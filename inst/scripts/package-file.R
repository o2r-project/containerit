## GUI - package_load_file
fromFile <- function(){

  # This function takes a fileinfo object returned from shinyFiles::parseFilePaths(..)
  # and, depending on the file type, a filename with .dockerfile extension
  # and the other required calls to containerit::dockerfile,
  # returns a list of the arguments
  determineDockerFunctionArguments <- function(input_filename) {
    output <- list()

    output[["output_filename"]] <- "Dockerfile"

    # Determine the file type and create CMD accordingly
    if (grepl(".R$", input_filename)) {
      output[['output_filename']] <- gsub(".R$",".dockerfile",input_filename)
      output[["cmd"]] <- containerit::CMD_Rscript(basename(input_filename))
    } else if (grepl(".Rmd$",input_filename)) {
      output[['output_filename']] <- gsub(".Rmd$",".dockerfile",input_filename)
      output[["cmd"]] <- containerit::CMD_Render(basename(input_filename))
    } else if (grepl("RData$|Rdata$",input_filename)) {
      output[['output_filename']] <- gsub(".RData|.Rdata$",".Dockerfile",input_filename)
      output[["cmd"]] <- containerit::Cmd("R")
    } else {
      stop("File type not recognised")
    }

    return(output)
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Dockerfile creation"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(1,2,1,1,1),
        shiny::p(shiny::strong("Create a"), shiny::code("Dockerfile"), shiny::strong("from a workflow file")),
        shiny::p(shiny::em("Accepted inputs:"), shiny::br(),
                 "R scripts (.R)", shiny::br(),
                 "R markdown files (.Rmd)", shiny::br(),
                 "Stored sessionInfo (R object named 'info' in a file sessionInfo.RData, sessioninfo.RData, or session_info.RData)"),
        shiny::p("For more information see the ", shiny::a(href = "https://o2r.info/containerit/articles/containerit.html", "package Vignette"), "."),
        shiny::fillRow(
          shiny::textInput(inputId = "filename",
                           label = NULL),
          shinyFiles::shinyFilesButton(id = "load",
                                       label = "Select file",
                                       title = "Load file",
                                       multiple = F),
          height = "50px"
        ),
        shiny::textOutput("outputfile")
      )
    )
  )

  server <- function(input, output, session){
    shiny::observe({
      shinyFiles::shinyFileChoose(input = input,
                                  id = 'load',
                                  roots = file_dialog_volumes,
                                  filetypes = c("R","Rmd","Rdata","RData"))
      fileinfo <- shinyFiles::parseFilePaths(roots = file_dialog_volumes,
                                             selection = input$load)
      if (length(fileinfo$datapath) != 0) {
        shiny::updateTextInput(session = session,
                               inputId = "filename",
                               value = fileinfo$datapath)
        docker_output <- determineDockerFunctionArguments(fileinfo$datapath)
        output$outputfile <- shiny::renderText({
          paste("Output will be written to:", file.path(dirname(fileinfo$datapath), docker_output[['output_filename']]))
        })
      }
    })

    shiny::observeEvent(input$done, {
      # Exit app to ensure that the gadget is closed after 'done' is clicked.
      shiny::stopApp()

      # Throw error if nothing entered
      if (nchar(input$filename) == 0) {
        stop("No file selected")
      }

      # Convert to an output file
      function_arguments <- determineDockerFunctionArguments(input$filename)
      # Store current directory
      curr_dir <- getwd()
      # Change to script directory
      setwd(dirname(input$filename))
      # Create docker file
      dockerfile_object <- containerit::dockerfile(from = basename(input$filename),
                                                   copy = "script",
                                                   cmd = function_arguments[['cmd']])
      # Output to desired path
      containerit::write(dockerfile_object, file = function_arguments[['output_filename']])
      # Change back to original directory
      setwd(curr_dir)

      print_docker_instructions(function_arguments[['output_filename']])
    })

  }

  viewer <- shiny::dialogViewer(dialogName = "containerit", height = 400)
  shiny::runGadget(app = ui, server = server, viewer = viewer)
}

fromFile()
