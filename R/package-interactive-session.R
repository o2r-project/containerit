## GUI - package_interactive_session

containeritAddIn <- function(){
  
  ui <- miniPage(
    gadgetTitleBar("Docker file creation"),
    miniContentPanel(
      textInput(dockerfilename, label, value = "", width = NULL, placeholder = NULL)
      # fileInput("dockerfilename", "Save .dockerfile", accept=".dockerfile")
    )
  )
  
  
  server <- function(input, output, session){
    shinyFileSave()
    observeEvent(input$done, {
      
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })
    
  }

  
  viewer <- dialogViewer(dialogName = "containerit")
  runGadget(ui, server, viewer = viewer)
}