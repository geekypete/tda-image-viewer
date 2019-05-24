rm(list=ls())
source('./utility-functions.R')
source('./processing.R')

ui <- dashboardPage(
  dashboardHeader(title="TDA Image Viewer"),
  dashboardSidebar(
      sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard"),
          menuItem("Distance Measures", tabName = "distance")
      )),
    dashboardBody()
  )

server <- function(input, output, session) {}

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)
runApp(shinyApp(ui, server))
