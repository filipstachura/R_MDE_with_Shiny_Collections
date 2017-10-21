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


# Start `rethinkdb.exe`
system(paste0(getwd(), "/rethinkdb.exe"), wait = FALSE)

# Start a connection to the db
connection <- shiny.collections::connect() 

# Our dummy-template. Later we will replace 'intro' with the content from the editor.
template <- c("Your text from the editor fits into the template:<br><br> intro <br><br>Great!")


# js code for SimpleMDE
jsCode  <- '
        shinyjs.get_editor_text = function() {
            var intro_value  = intro_text_MDE.value();
            Shiny.onInputChange("textfield", intro_value);
        }

        shinyjs.start_editor = function(initial_value) {
            intro_text_MDE = new SimpleMDE({
                element: document.getElementById("intro"),
    	        initialValue: initial_value[0],
                spellChecker: false, 
                autosave: {enabled: false}
        });     
    }
'


ui <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),    
    # theme = shinythemes::shinytheme("flatly"),
    tags$head(HTML('<link rel="stylesheet" type="text/css" href="simplemde.min.css">')),
    tags$style(HTML("
                    body { margin: 20px;}
                    .CodeMirror, .CodeMirror-scroll {
                        min-height: 150px;
                    }
                    .footer { font-size: 0.9em; color: gray; }
                    ")
    ),    
    title = "Markdown Editor",
    fluidRow(
        h1("Markdown Editor"),
        br(),
        h4("Enter some Text"),
        helpText("It will be integrated into the template."),
        HTML('<textarea id="intro" rows="4" cols="75"></textarea>'), 
        actionButton("apply_changes_button", label = "Apply Changes"),
        hr(), br(),
        h3("The final text"), 
        htmlOutput("introtext"),
        br(), hr(),
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
        
        the_content <- shiny.collections::collection("content", connection)
        
        
        # helper
        .get_last_value_from_db <- function() {
            res <- the_content$collection %>% arrange(time) %>% filter(row_number() == n()) %>% select(text)
            return(as.character(res))
        }
        
        
        # get the value from the editor as reactive
        value_in_editor <- reactive({
            list(value = input$textfield)
        })
        
        #☺ get the last value in the db as reactive
        # last_value_in_db <- reactive({
        #     res <- the_content$collection %>% arrange(time) %>% filter(row_number() == n()) %>% select(text)
        #     list(value_in_db = as.character(res[1, 1]))
        # })
        last_value_in_db <- reactive({
            list(value_in_db = .get_last_value_from_db())
        })
        
        
        
        # js$start_editor("Initial Text for MDE")
        # startvalue <- isolate(value_in_editor()$value_in_db ) %>%  print()
        # res <- isolate(the_content$collection %>% arrange(time) %>% filter(row_number() == n()) %>% select(text))
        # startvalue <- as.character(res) %>%  print()
        startvalue <- isolate(.get_last_value_from_db())
        js$start_editor(startvalue)
        
        # why do we need this? init the js?
        js$get_editor_text()
        

        
        
        
        onclick("apply_changes_button", {
            
            # refresh
            js$get_editor_text()
            # print(input$textfield)

            actual <- value_in_editor()$value
            last   <- last_value_in_db()$value_in_db

            cat("clicked", input$apply_changes_button, "\n")
            cat("value in editor:    ", actual, "\n")
            cat("value in db before: ", last, "\n")

            # write to db if content in the editor changed
            if (actual != last) {
                shiny.collections::insert(the_content, list(text = actual, time = now()) )
                
                # js$start_editor(actual)
                
                cat("value in db after : ", last_value_in_db()$value_in_db, "\n\n")
            } else {
                # nothing
                cat("content didn't change\n\n")
            }
            
    cat("\nWe have to update the value in the editor, too!\n\n")
    # simplemde.value("This text will appear in the editor");
        })
        
        
        output$introtext <- renderText({
            # read from db
            # content_to_insert <- the_content$collection %>% arrange(time) %>% filter(row_number() == n()) %>% select(text)
            content_to_insert <- last_value_in_db()$value_in_db
            .replace_placeholder(template, content_to_insert, "intro")            
        })
        
        
        output$content_db <- renderPrint({
            the_content$collection
        })
        
})
    


shinyApp(ui = ui, server = server)
