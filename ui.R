
library(shiny)

fluidPage(
  
  titlePanel("'Our statistics' traffic data"),
  
  fluidRow(
    column(4,
           selectInput("object",
                       "Object",
                       c("(choose an object)",
                         names(d))
           )
    ),
    column(4,
           selectInput("method",
                       "Method",
                       c("(choose a method"))
    )
  ),
  
  
  fluidRow(
    DT::dataTableOutput("table")
  )
)

