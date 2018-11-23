## GUI - package_expressions
expressionsAddIn <- function(){
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Dockerfile creation"),
    miniUI::miniContentPanel(
      shiny::p(shiny::strong("Create a .dockerfile file from a list of expressions")),
      shiny::p("Input a vector of expressions. For example;"),
      shiny::p(shiny::code(
                "c( expression(library(sp)),
                expression(data(meuse)), 
                expression(mean(meuse[[\"zinc\"]])) )")),
      shiny::p("For more information see the getting
               started page at https://o2r.info/containerit/articles/containerit.html."),
      shiny::textAreaInput("expressions", "List of Expressions", "", height="240px", width="500px"),
      shiny::fillRow(
        shiny::textInput("text",NULL, 
                  value = paste0(getwd(),"/dockerfile.dockerfile")),
        shinyFiles::shinySaveButton("save", "Save as...", "Save file as ...", 
                  filetype=list(dockerfile="dockerfile")),
        height = '50px'
        )
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
      
      # Create the session object
      session <- containerit::clean_session(input$expressions, echo = TRUE)
      # Create docker file
      dockerfile_object <- containerit::dockerfile(from=session)
      # Output to desired path
      containerit::write(dockerfile_object, file = input$text)
      
      # Exit app
      shiny::stopApp()
    })
    
  }

  
  viewer <- shiny::dialogViewer(dialogName = "containerit")
  shiny::runGadget(ui, server, viewer = viewer)
}

expressionsAddIn()