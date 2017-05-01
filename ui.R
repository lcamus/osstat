
library(shiny)

fluidPage(
  
  titlePanel("'Our statistics' traffic data"),
  
  fluidRow(
    column(3,
           selectInput("object",
                       "Object",
                       c("(choose an object)",
                         names(d))
           )
    ),
    column(3,
           selectInput("method",
                       "Method",
                       c("(choose a method"))
    ),
    column(3,
           selectInput("variable",
                       "Variable",
                       c("(choose a variable)"))
    ),
    column(3,
           selectInput("filtervalue",
                       "Filter value",
                       c("(choose a value)"))
    )
  ),
  
  
  fluidRow(
    DT::dataTableOutput("table")
  )
)

