library(shiny)
library(shinyjs)

# devtools::install_github("Appsilon/shiny.collections")
library(shiny.collections)


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
        shinyjs.get_intro_text = function() {
            var intro_value  = intro_text_MDE.value();
            Shiny.onInputChange("textfield", intro_value);
        }

        shinyjs.start_editor = function() {
            intro_text_MDE = new SimpleMDE({
                element: document.getElementById("intro"),
    	        initialValue: "Hello world!",
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
        actionButton("apply_intro_changes_button", label = "Apply Changes"),
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
        
        
        js$start_editor()
        js$get_intro_text()
        
        # input_values <- reactive({
        #     list(intro  = input$textfield)
        # })
        
        ## write content of db into editor on startup
        
        
        onclick("apply_intro_changes_button", {
            js$get_intro_text()
            # write to db
            shiny.collections::insert(the_content, list(text = input$textfield, time = now()) )
        })
        
        output$introtext <- renderText({
            # content <-  input$textfield # input_values()$intro
            # read from db
            content_to_insert <- the_content$collection[2]
            .replace_placeholder(template, content_to_insert, "intro")            
        })
        
        
        output$content_db <- renderPrint({
            the_content$collection})
        
})
    


shinyApp(ui = ui, server = server)
