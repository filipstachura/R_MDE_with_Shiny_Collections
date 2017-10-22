

library(shiny)
library(shinyjs)

# js code for SimpleMDE
jsCode  <- '
        shinyjs.get_editor_text = function() {
            Shiny.onInputChange("textfield", intro_text_MDE.value());
        }
        shinyjs.start_editor = function() {
            intro_text_MDE = new SimpleMDE({
                element: document.getElementById("intro"),
    	        initialValue: "initial value",
                spellChecker: false, 
                autosave: {enabled: false}
            });
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
        HTML('<textarea id="intro" rows="4" cols="75"></textarea>'),
        actionButton("apply_changes_button", label = "Apply Changes"),
        hr(), 
        h3("The text in the editor"), 
        htmlOutput("introtext"),
        HTML('<script src="simplemde.min.js"></script>'),
        HTML('<script>
             var intro_text_MDE; // init SimpleMDE
             </script>'
             )
        )
)



server <- shinyServer(
    function(input, output, session) {
        
        clicked <- reactiveValues(value = 0)

        # get the value from the editor as reactive
        value_in_editor <- reactive({
            list(value = input$textfield)
        })
        
        # start the editor
        js$start_editor()
        
        # init the connection to the editor
        js$get_editor_text()
        
        # Button clicked, get content from editor
        onclick("apply_changes_button", {
            js$get_editor_text()
            clicked$value <- input$apply_changes_button
            # cat("button clicked: ", clicked$value, "\n")
        })
        
        # 
        observe({
            cat("hello", clicked$value, "\ngot:", value_in_editor()$value, "\n")
        })

        # compose output
        output$introtext <- renderText({
            paste("We have the follwing text: <br>", 
                value_in_editor()$value         )
        })
        
})
    


shinyApp(ui = ui, server = server)
