## GUI - package_load_file
fromFileAddIn <- function(){
  
  determineDockerFunctionArguments <- function(input_filename) {
    # This function takes fileinfo, returned from parseFilePaths
    # and, depending on the file type, a filename with .dockerfile extension
    # and the other required calls to containerit::dockerfile
    # Return type is a list of the arguments
    
    # Determine the file type
    output <- list()
    if (grepl(".R$",input_filename)) {
      output[['output_filename']] <- gsub(".R$",".dockerfile",input_filename)
      output[['cmd']] <- containerit::CMD_Rscript(basename(input_filename))
      
    } else if (grepl(".Rmd$",input_filename)) {
      output[['output_filename']] <- gsub(".Rmd$",".dockerfile",input_filename)
      output[['cmd']] <- containerit::CMD_Render(input_filename)
      
    } else if (grepl("RData$",input_filename)) {
      output[['output_filename']] <- gsub(".Rdata$",".dockerfile",input_filename)
      output[['cmd']] <- containerit::Cmd("R")
    }
    else {
      stop("File type not recognised")
    }
    return(output)
  }
  
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Dockerfile creation"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(1,2,1,8),
        shiny::p(shiny::strong("Create a .dockerfile file from a file input.")),
        shiny::p("Inputs accepted are; R scripts (.R), R markdown files (.Rmd), 
          or stored sessionInfo (sessionInfo.RData, sessioninfo.RData, 
          or session_info.RData). For more information see the getting
          started page at https://o2r.info/containerit/articles/containerit.html."),
      shiny::fillRow(
        shiny::textInput("filename",NULL),
        shinyFiles::shinyFilesButton("load", "Select file", "Load file",multiple=F),
        height = '50px'
      ),
      shiny::textOutput('outputfile')
      )
    )
  )
  
  
  
  server <- function(input, output, session){
    volumes <- c("Working directory"=getwd(),"Home Directory"="~")
    shiny::observe({
      shinyFiles::shinyFileChoose(input,'load', roots = volumes,
                                  filetypes=c("R","Rmd","Rdata"))
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$load)
      if (length(fileinfo$datapath) != 0) {
        shiny::updateTextInput(session, "filename", value = fileinfo$datapath)
        docker_output <- determineDockerFunctionArguments(fileinfo$datapath)
        output$outputfile <- shiny::renderText({
          paste("Output will be written to:",docker_output[['output_filename']])
        })
      }
    })
    shiny::observeEvent(input$done, {
      
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      
      # Throw error if nothing entered
      if (nchar(input$filename) == 0) {
         stop("No file selected")
       }

      # Convert to an output file
      fn_args <- determineDockerFunctionArguments(input$filename)
      # Store current directory
      curr_dir <- getwd()
      # Change to script directory
      setwd(dirname(input$filename))
      # Create docker file
      dockerfile_object <- containerit::dockerfile(from=input$filename,
                                                   copy = "script",
                                                   cmd=fn_args[['cmd']]
                                                   )
      # Output to desired path
      containerit::write(dockerfile_object, file = fn_args[['output_filename']])
      # Change back to original directory
      setwd(curr_dir)
      # Output docker instructions
      cat("\nInstructions to run docker container from command line:\n")
      print(
        c(paste("docker build -t [tag] -f",basename(fn_args[['output_filename']])),
        "docker run -it [tag]") )
      # Exit app
      shiny::stopApp()
    })
    
  }
  
  
  viewer <- shiny::dialogViewer(dialogName = "containerit")
  shiny::runGadget(ui, server, viewer = viewer)
}

fromFileAddIn()