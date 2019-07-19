## GUI - package_interactive_session
fromSession <- function(){

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Dockerfile creation"),
    miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::textInput(inputId = "text",
                         label = NULL,
                         value = file.path(getwd(), "Dockerfile")),
        save_button(),
        height = "50px"
      ),
      shiny::checkboxInput("saveimage", "Save global R objects to Dockerfile", TRUE)
    )
  )

  server <- function(input, output, session){
    shiny::observeEvent(eventExpr = input$save,
                        handlerExpr = {
                          volumes <- c("Working directory" = getwd(), "Home directory" = "~")
                          shinyFiles::shinyFileSave(input = input,
                                                    id = "save",
                                                    roots = volumes,
                                                    session = session)
                          fileinfo <- shinyFiles::parseSavePath(roots = volumes,
                                                                selection = input$save)
                          if (length(fileinfo$datapath) != 0) {
                            shiny::updateTextInput(session = session,
                                                   inputId = "text",
                                                   value = fileinfo$datapath)
                          }
                        })

    shiny::observeEvent(eventExpr = input$done,
                        handlerExpr = {
                          # Exit app to ensure that the gadget is closed after 'done' is clicked.
                          shiny::stopApp()

                          # Create Dockerfile and write to desired path
                          dockerfile_object <- containerit::dockerfile(save_image = input$saveimage)
                          containerit::write(dockerfile_object, file = input$text)

                          print_docker_instructions(input$text)
                        })
  }

  viewer <- shiny::dialogViewer(dialogName = "containerit")
  shiny::runGadget(app = ui, server = server, viewer = viewer)
}

fromSession()
