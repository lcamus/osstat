

function(input, output, clientData, session) {
  
  object_choice_def <- "(choose an object)"
  method_choice_def <- "(choose a method)"
  method_options <- method_choice_def
  
  observe({
    
    if (input$object==object_choice_def)
      method_options <- method_choice_def
    else
      method_options <- c(method_choice_def,names(d[[input$object]]))

    output$table <- DT::renderDataTable(DT::datatable({

      if (input$object != object_choice_def & input$method != method_choice_def) {
        print("hello")
        print(input$object)
        print(input$method)
        data <- d[[input$object]][[input$method]]
        print(data)
        data
      }
      # if (input$cyl != "All") {
      #   data <- data[data$cyl == input$cyl,]
      # }
      # if (input$trans != "All") {
      #   data <- data[data$trans == input$trans,]
      # }
    }))
    
    updateSelectInput(session,"method", choices=method_options)
    
  })
  
}
