
# global.R

library(shiny)
library(shinyjs)
library(dplyr)

# devtools::install_github("Appsilon/shiny.collections")
library(shiny.collections)

# Start `rethinkdb.exe` on windows; we assume it is in the same dir like the app
# system(paste0(getwd(), "/rethinkdb.exe"), wait = FALSE)

# Start a connection to the db
connection <- shiny.collections::connect()

# Our dummy-template. Later we will replace '---intro---' with the content from the editor.
template <- c("Your text from the editor fits into the template:
              <br> ---intro--- <br>
              Great! And here ends the template.")

# js code for SimpleMDE
jsCode  <- '
        shinyjs.start_editor = function() {
            console.log("Starting editor")
            intro_text_MDE = new SimpleMDE({
                element: document.getElementById("intro"),
                spellChecker: false
            });
            intro_text_MDE.codemirror.on("change", function(){
              Shiny.onInputChange("textfield", intro_text_MDE.value());
            });
        }
        shinyjs.update_editor = function(content) {
          console.log("Update called");
          console.log(content);
          intro_text_MDE.value(content[0]);
        }
'


ui <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    tags$head(HTML('<link rel="stylesheet" type="text/css" href="simplemde.min.css">')), # is in www/
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
        h3("The final text"),
        htmlOutput("introtext"),
        hr(),
        h4("Raw content from the db"),
        verbatimTextOutput("session_token"),
        verbatimTextOutput("content_db"),
        hr(),
        HTML('<div class="footer">2017, Tinu Schneider,
                <a href="mailto:tinu@tinuschneider.ch">tinu@tinuschneider.ch</a>,
                Editor: <a href="https://simplemde.com/" target="_blank">SimpleMDE</a>
             </div>'),
        # start simplemde
        HTML('<script src="simplemde.min.js"></script>'), # is in www/
        HTML('<script>
                var intro_text_MDE; // init SimpleMDE globally
             </script>'
             )
        )
)



server <- shinyServer(
    function(input, output, session) {

        the_content <- shiny.collections::collection("content", connection)

        # get the value from the editor as reactive, return a string
        value_in_editor <- reactive({
            value <- input$textfield
            if (is.null(value)) value <- " "
            list(value = value)
        })

        # get the last value in the db as reactive, return a string
        last_value_in_db <- reactive({
            db <- the_content$collection

            # if a new empty db starts up, it will be a 0x0 tibble
            if (nrow(db) < 1)  {
                res <- " "
            } else {
                res <- db %>%
                        arrange(time) %>%
                        filter(row_number() == n()) %>%
                        select(text) %>%
                        as.character()
            }
            return(list(value = res))
        })

        # start the editor
        js$start_editor()

        # Button clicked
        onclick("apply_changes_button", {
          print("click")
          actual <- value_in_editor()$value
          last   <- last_value_in_db()$value

          # write to db if content in the editor changed
          if (actual != last) {
            shiny.collections::insert(the_content, list(
                    token = session$token,
                    time  = lubridate::now(),
                    text  = actual) )
          }
        })

        # update the other editors; works on startup, too
        # BUT FAILS, if we have more than one browser instance and change the
        # input from one to the other. So the whole app is basically useless...
        observe({
          last <- last_value_in_db()$value
          js$update_editor(last)
        })

        # show the session token
        output$session_token <- renderText({
            paste("This session token: ", session$token)
        })

        # put the text from the editor into the template
        # works fine through with multiple instances
        output$introtext <- renderText({
            # read from db and insert the content into the template
            content_to_insert <- last_value_in_db()$value
            sub(pattern = "---intro---", replacement = content_to_insert, x = template)
        })

        # show the content in the db (or part of it)
        output$content_db <- renderPrint({
            if (nrow(the_content$collection) < 1) {
                "db is empty -- nothing to show"
            } else {
                the_content$collection %>% select(text, time) %>% arrange(desc(time))
            }
        })
})


shinyApp(ui = ui, server = server)
