

function(input, output, clientData, session) {
  
  object_choice_def <- "(choose an object)"
  method_choice_def <- "(choose a method)"
  variable_choice_def <- "(choose a variable)"
  filtervalue_choice_def <- "(choose a value)"
  
  get_df <- reactive({
    if (input$filtervalue==filtervalue_choice_def)
      res <- d[[input$object]][[input$method]]
    else
      res <- d[[input$object]][[input$method]][d[[input$object]][[input$method]][[input$variable]]==input$filtervalue,]
    res <- subset(res, as.Date(date) %in% seq(from=as.Date(input$dateRange[1]), to=as.Date(input$dateRange[2]), by='days'))
    res <- res[order(-res$date),]
    return(res)
  })
  
  get_methods <- reactive({
      if (input$object==object_choice_def)
        method_options <- method_choice_def
      else
        method_options <- c(method_choice_def,names(d[[input$object]]))
      return(method_options)
  })
  
  get_variables <- reactive({
      if (input$method==method_choice_def)
        variable_options <- variable_choice_def
      else
        variable_options <- c(variable_choice_def,names(d[[input$object]][[input$method]]))
      return(sort(variable_options))
  })
  
  get_filtervalues <- reactive({
    if (input$variable==variable_choice_def)
      filtervalue_options <- filtervalue_choice_def
    else
      filtervalue_options <- c(filtervalue_choice_def,sort(as.character(unique(d[[input$object]][[input$method]][,c(input$variable)]))))
    return(filtervalue_options)
  })
  
  observe({
    updateSelectInput(session,"method", choices=get_methods())  
  })
  
  observe({
    updateSelectInput(session,"variable", choices=get_variables())
  })
  
  observe({
    updateSelectInput(session,"filtervalue", choices=get_filtervalues())
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$object != object_choice_def & input$method != method_choice_def) {
      data <- get_df()
      data
    }
  }))
  
}
