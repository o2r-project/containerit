## GUI - package workspace directory


# modified shinyDir button to fit inside miniPage

workspaceDirectory_addin <- function(){
  
  ui <- miniUI::miniPage(
    scrollable = TRUE,
    miniUI::gadgetTitleBar("Docker file creation"),
    miniUI::miniContentPanel(
      shiny::fillCol(
        shiny::fillRow(
          shiny::actionButton("choose_dir", "Choose Directory"),
          height = '50px'),
        shiny::fillRow(
          shiny::fillCol(shiny::p("Directory to package:"), 
                         shiny::textOutput('outputfilepath'))
        ),
        shiny::fillRow(shiny::p("Choose the location of the directory you wish to package. Containerit searches for the first occurence of an R script, or otherwise the first occurence of an R markdown file. It then proceeds to package this file along with all other resources in the directory."), height = "30px")
      )
    )
  )
  
  server <- function(input, output, session){
    
    path <- shiny::reactiveValues(data = NULL) #set to wd?
    dockerfilename <- shiny::reactiveValues(data = NULL) 
    
    shiny::observeEvent(input$choose_dir, {
      path$data <- rstudioapi::selectDirectory() # capture user's path entry
      filename<- gsub(".*/","",path$data) # extract the name of the dir
      dockerfilename$data <- paste0(path$data,"/",filename,".dockerfile")
      output$outputfilepath <- shiny::renderText({path$data})
    })
    
    
    shiny::observeEvent(input$done, {
      
      if(!is.null(path$data)){
        # Exit app first
        shiny::stopApp()
        # Create docker file
        print(path$data)
        dockerfile_object <- containerit::dockerfile(from = path$data)
        print(dockerfile_object)
        # Output to desired path
        containerit::write(dockerfile_object, file = dockerfilename$data)
        
        # Output docker instructions
        cat("\nInstructions to run docker container from command line:\n")
        print(
          c(paste("docker build . -t [tag] -f",basename(dockerfilename$data)),
            "docker run -it [tag]") )
      } #stop("Please Choose Directory Path")
      
    })
    
  }
  viewer <- shiny::dialogViewer(dialogName = "containerit",  width = 800, height = 700)
  shiny::runGadget(ui, server, viewer = viewer)
}

workspaceDirectory_addin()