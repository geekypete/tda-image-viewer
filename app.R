rm(list=ls())
source('./utility-functions.R')
#source('./processing.R')
persistence_diagrams <- readRDS('./perstdiag.rds')
ui <- dashboardPage(
  dashboardHeader(title="TDA Image Viewer"),
  dashboardSidebar(
  selectInput("file", "ROI", files, width=200)),
  dashboardBody(
      fluidRow(
          box(title = "Persistence",  status = "primary", plotOutput("cycleA"))
      )
  ))

server <- function(input, output, session) {

  output$cycleA<- renderPlot({
      img_index <- match(input$file, files)
      print(img_index)
      perstImage(paste(image_path, input$file, sep=''), persistence_diagrams[[img_index]][[2]], .1, 1, betti=1, image='thresh')
  },height=500, width=700)
}

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)
runApp(shinyApp(ui, server))
