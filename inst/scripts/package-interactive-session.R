## GUI - package_interactive_session

containeritAddIn <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Docker file creation"),
    miniContentPanel(
      textInput("text",NULL, 
                value = paste0(getwd(),"/dockerfile.dockerfile")),
      shinySaveButton("save", "Select file", "Save file as ...", 
                filetype=list(dockerfile="dockerfile"))
    )
  )
  
  
  server <- function(input, output, session){
    observeEvent(input$save,{
      volumes <- c("UserFolder"=getwd())
      shinyFileSave(input, "save", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$save)
      updateTextInput(session, "text", value = fileinfo$datapath)
    })
    observeEvent(input$done, {
      
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      
      # Create docker file
      dockerfile_object <- dockerfile()
      # Output to desired path
      write(dockerfile_object, file = input$text)
      # Exit
      stopApp()
    })
    
  }

  
  viewer <- dialogViewer(dialogName = "containerit")
  runGadget(ui, server, viewer = viewer)
}