## GUI - package_expressions
fromExpressions <- function(){

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Dockerfile creation"),
    miniUI::miniContentPanel(
      shiny::p(shiny::strong("Create a Dockerfile from a list of expressions")),
      shiny::p("Input a vector of expressions. For example;"),
      shiny::p(shiny::code(
                "c( expression(library(sp)),
                expression(data(meuse)),
                expression(mean(meuse[[\"zinc\"]])) )")),
      shiny::p("For more information see documentation at https://o2r.info/containerit/articles/containerit.html."),
      shiny::textAreaInput(inputId = "expressions",
                           label = "List of Expressions",
                           value = "",
                           height = "240px",
                           width = "500px"),
      shiny::fillRow(
        shiny::textInput(inputId = "text",
                         label = NULL,
                         value = file.path(getwd(), "Dockerfile")),
        save_button(),
        height = "50px"
      )
    )
  )

  server <- function(input, output, session){
    shiny::observeEvent(input$save,{
      shinyFiles::shinyFileSave(input, "save",
                                roots = file_dialog_volumes,
                                session = session)
      fileinfo <- shinyFiles::parseSavePath(roots = file_dialog_volumes,
                                            selection = input$save)
      if (length(fileinfo$datapath) != 0) {
        shiny::updateTextInput(session, "text", value = fileinfo$datapath)
      }
    })

    shiny::observeEvent(input$done, {
      # Exit app to ensure that the gadget is closed after 'done' is clicked.
      shiny::stopApp()

      # Create the session object
      session <- containerit::clean_session(input$expressions, echo = TRUE)

      # Create docker file and write to desired path
      dockerfile_object <- containerit::dockerfile(from = session)
      containerit::write(dockerfile_object, file = input$text)

      print_docker_instructions(input$text)
    })
  }

  viewer <- shiny::dialogViewer(dialogName = "containerit")
  shiny::runGadget(app = ui, server = server, viewer = viewer)
}

fromExpressions()
