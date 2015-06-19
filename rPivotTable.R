library(shiny)
library(shinydashboard)
library(rpivotTable)
library(ggplot2)

data(diamonds)

header <- dashboardHeader(title = "Data Profiler")

sidebar <- dashboardSidebar()

body <- dashboardBody(
  tags$head(tags$style(
    type = 'text/css',
    '#test{ overflow-x: scroll; }'
  )),
  h1("this is just a test")
  ,h2("this is more testing")
  ,rpivotTableOutput("test",height= 550)
)


shinyApp(
  ui = dashboardPage(
    header, sidebar, body),
  server = function(input, output) {
    
    output$test <- rpivotTable::renderRpivotTable({
      rpivotTable(data = diamonds)
    })
    
  }
)