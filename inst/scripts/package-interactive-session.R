## GUI - package_interactive_session
containeritAddIn <- function(){
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Docker file creation"),
    miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::textInput("text",NULL, 
                  value = paste0(getwd(),"/dockerfile.dockerfile")),
        shinyFiles::shinySaveButton("save", "Select file", "Save file as ...", 
                  filetype=list(dockerfile="dockerfile")),
        height = '50px'
        ),
        shiny::checkboxInput("saveimage", "Save global R objects to dockerfile", TRUE)
      )
    )
  
  
  
  server <- function(input, output, session){
    shiny::observeEvent(input$save,{
      volumes <- c("Working directory"=getwd(),"Home Directory"="~")
      shinyFiles::shinyFileSave(input, "save", roots=volumes, session=session)
      fileinfo <- shinyFiles::parseSavePath(volumes, input$save)
      if(length(fileinfo$datapath)!=0) {
        shiny::updateTextInput(session, "text", value = fileinfo$datapath)
      }
    })
    shiny::observeEvent(input$done, {
      
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      
      # Exit app first
      shiny::stopApp()
      # Create docker file
      dockerfile_object <- containerit::dockerfile(save_image=input$saveimage)
      # Output to desired path
      containerit::write(dockerfile_object, file = input$text)
      
    })
    
  }

  
  viewer <- shiny::dialogViewer(dialogName = "containerit")
  shiny::runGadget(ui, server, viewer = viewer)
}

containeritAddIn()