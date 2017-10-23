
# global.R

library(shiny)
library(shinyjs)
library(dplyr)

# devtools::install_github("Appsilon/shiny.collections")
library(shiny.collections)

## TODO
# reset text
# back/forward to line x


# helper: template as first argument to place the 
# content into the template
.replace_placeholder <- function(template, content, placeholder) {
    sub(pattern = placeholder, replacement = content, x = template)
}

# helper: get last value from the db
.get_last_value_in_db_from_db <- function(coll) {
    db <- coll$collection 
    
    # if a new db starts up, it will be empty
    if (nrow(db) < 1) return(" ")
    
    res <- db %>% arrange(time) %>% filter(row_number() == n()) %>% select(text)
    return(as.character(res))
}


# Start `rethinkdb.exe`
system(paste0(getwd(), "/rethinkdb.exe"), wait = FALSE)

# Start a connection to the db
connection <- shiny.collections::connect() 

# Our dummy-template. Later we will replace 'intro' with the content from the editor.
template <- c("Your text from the editor fits into the template:
              <br> ---intro--- <br>
              Great! And here ends the template.")


# js code for SimpleMDE
jsCode  <- '
        shinyjs.get_editor_text = function() {
            Shiny.onInputChange("textfield", intro_text_MDE.value());
        }
        shinyjs.start_editor = function(initial_value) {
            intro_text_MDE = new SimpleMDE({
                element: document.getElementById("intro"),
    	        initialValue: initial_value[0],
                spellChecker: false, 
                autosave: {enabled: false}
            });
        }
        shinyjs.update_editor = function(content) {
                intro_text_MDE.value(content[0]);
        }
'


ui <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),    
    tags$head(HTML('<link rel="stylesheet" type="text/css" href="simplemde.min.css">')),
    tags$style(HTML("
                    body { margin: 20px;}
                    .CodeMirror, .CodeMirror-scroll {
                        min-height: 100px;
                    }
                    .footer { font-size: 0.9em; color: gray; }
                    ")
    ),    
    title = "Markdown Editor",
    fluidRow(
        h1("Markdown Editor"),
        h4("Enter some Text"),
        helpText("It will be integrated into the template."),
        HTML('<textarea id="intro" rows="4" cols="75"></textarea>'),
        # textAreaInput("intro", "", cols = "75"),
        actionButton("apply_changes_button", label = "Apply Changes"),
        hr(), 
        h3("The final text"), 
        helpText("This text will be part of  a .Rmd report."), 
        htmlOutput("introtext"),
        hr(),
        h4("Raw content from the db"),
        verbatimTextOutput("content_db"),
        hr(),
        HTML('<div class="footer">2017, Tinu Schneider, 
                <a href="mailto:tinu@tinuschneider.ch">tinu@tinuschneider.ch</a>
             </div>'),
        HTML('<div class="footer">Editor: <a href="https://simplemde.com/" target="_blank">SimpleMDE</a></div>'),
        HTML('<script src="simplemde.min.js"></script>'),
        HTML('<script>
             var intro_text_MDE; // init SimpleMDE
             </script>'
             )
        )
)



server <- shinyServer(
    function(input, output, session) {
        
        who_clicked <- reactiveValues(
                        token   = "NULL", 
                        clicked = FALSE)
        
        the_content <- shiny.collections::collection("content", connection)
        
        # get the value from the editor as reactive
        value_in_editor <- reactive({
            value <- input$textfield
            if (is.null(value)) value <- " "
            list(value = value)
        })
        
        # get the last value in the db as reactive
        last_value_in_db <- reactive({
            list(value = .get_last_value_in_db_from_db(the_content))
        })
        
        # start the editor with last entry from db
        startvalue <- isolate(.get_last_value_in_db_from_db(the_content))
        js$start_editor(startvalue)
        
        # why do we need this? init the js?
        js$get_editor_text()
        
        # Button clicked
        onclick("apply_changes_button", {
            js$get_editor_text() # refresh
            who_clicked$token <- session$token
            who_clicked$clicked <- TRUE
            # print(who_clicked$token)
        })
        
        # update the db AFTER the button is clicked
        observe({
            actual <- value_in_editor()$value
            last   <- last_value_in_db()$value

            # write to db if content in the editor changed, 
            if (actual != last) {
                if (session$token == who_clicked$token & who_clicked$clicked) {
                    shiny.collections::insert(the_content, list(
                            who  = session$token,
                            time = lubridate::now(), 
                            text = actual) )
                } 
            } 
            who_clicked$clicked <- FALSE
        })
        
        
        
        # update the other editors
        observe({
            last <- last_value_in_db()$value
            if (session$token != who_clicked$token ) {
                js$update_editor(last)
            }
        })
        
        # observe({
        #     value_in_editor()$value
        #     js$get_editor_text()
        # })
        
        output$introtext <- renderText({
            # read from db and insert the content into the template
            content_to_insert <- last_value_in_db()$value
            .replace_placeholder(template, content_to_insert, "---intro---")            
        })
        
        # show the content in the db (or part of it)
        output$content_db <- renderPrint({
            the_content$collection %>% select(who,  text, time) %>% arrange(desc(time))
        })
        
})
    


shinyApp(ui = ui, server = server)
