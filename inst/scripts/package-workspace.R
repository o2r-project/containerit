## GUI - package workspace directory
# modified shinyDir button to fit inside miniPage
fromWorkspaceDirectory <- function(){

  ui <- miniUI::miniPage(
    scrollable = TRUE,
    miniUI::gadgetTitleBar("Docker file creation"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        shiny::fillRow(
          shiny::actionButton(inputId = "choose_dir",
                              label = "Choose directory"),
          height = "50px"),
        shiny::fillRow(
          shiny::fillCol(shiny::p("Directory to package:"),
                         shiny::textOutput("outputfilepath"))
        ),
        shiny::fillRow(shiny::p("Choose the location of the directory you wish to package. Containerit searches for the first occurence of an R script, or otherwise the first occurence of an R Markdown file. It then proceeds to package this file along with all other resources in the directory."),
                       height = "30px")
      )
    )
  )

  server <- function(input, output, session){
    path <- shiny::reactiveValues(data = NULL) #set to wd?
    dockerfilename <- shiny::reactiveValues(data = NULL)

    shiny::observeEvent(input$choose_dir, {
      path$data <- rstudioapi::selectDirectory()
      dirname <- basename(path$data)
      dockerfilename$data <- file.path(path$data, paste0(dirname, ".Dockerfile"))
      output$outputfilepath <- shiny::renderText({path$data})
    })

    shiny::observeEvent(input$done, {
      if (!is.null(path$data) && !is.null(dockerfilename$data)) {
        shiny::stopApp()

        # Create docker file and output to desired path
        dockerfile_object <- containerit::dockerfile(from = path$data,
                                                     copy = basename(path$data))
        containerit::write(dockerfile_object, file = dockerfilename$data)

        print_docker_instructions(dockerfilename$data)
      }
    })
  }

  viewer <- shiny::dialogViewer(dialogName = "containerit", height = 200)
  shiny::runGadget(app = ui, server = server, viewer = viewer)
}

fromWorkspaceDirectory()
