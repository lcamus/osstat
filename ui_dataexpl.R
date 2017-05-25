
library(shiny)

fluidPage(
  
  titlePanel("'Our statistics' traffic data"),
  
  fluidRow(
    column(2,
           dateRangeInput('dateRange',
                          label = "Time period",
                          start = Sys.Date() - 7, end = Sys.Date() - 1,
                          min = "2015-01-01", max = Sys.Date(),
                          separator = " - ", format = "yy-mm-dd",
                          startview = 'year', language = 'en', weekstart = 1
           )           
    ),
    column(2,
           selectInput("object",
                       "Module",
                       c("(choose an object)",
                         sort(names(d)))
           )
    ),
    column(3,
           selectInput("method",
                       "Method",
                       c("(choose a method"))
    ),
    column(2,
           selectInput("variable",
                       "Variable",
                       c("(choose a variable)"))
    ),
    column(2,
           selectInput("filtervalue",
                       "Filter value",
                       c("(choose a value)"))
    )
  ),
  
  fluidRow(
    DT::dataTableOutput("table")
  )
)

