rm(list=ls())
source('./utility-functions.R')
#source('./processing.R')
persistence_diagrams <- readRDS('./perstdiag.rds')
ui <- dashboardPage(
  dashboardHeader(title="TDA Image Viewer"),
  dashboardSidebar(
  selectInput("file", "ROI", files, width=200),
  sliderInput("min_perst","Minimum Persistence Bound", min=0, max=1, value=0, width=200),
  sliderInput("max_perst","Maximum Persistence Bound", min=0, max=1, value=1, width=200),
  selectInput("img_type", "Image Type", c("Color", "Grayscale", "Threshold"), width=200)),
  dashboardBody(
      fluidRow(
          box(status = "primary", plotOutput("cycleA"), width=5, height=520)
      ),
      fluidRow(
          box(title = "Persistence", status="primary", plotOutput("perst"), width=3, height=600)
      )
  ))

server <- function(input, output, session) {

  output$cycleA<- renderPlot({
      img_index <- match(input$file, files)
      perstImage(paste(image_path, input$file, sep=''), persistence_diagrams[[img_index]][[2]], input$min_perst, input$max_perst, betti=1, image=input$img_type)
  },height=500, width=700)

output$perst<- renderPlot({
    img_index <- match(input$file, files)
    pd_at_index <- persistence_diagrams[[img_index]][[2]]$diag
    pd_betti <- pd_at_index[which(pd_at_index[,1] == 1),]
    pd_df <- data.frame(Birth=pd_betti[,2], Persistence = pd_betti[,3]-pd_betti[,2])
    pd_df <- pd_df[which(pd_df$Persistence > input$min_perst),]
    ggplot(pd_df, aes(Birth, Persistence)) + geom_point() +
        scale_x_continuous(limits = c(0,1),breaks=seq(0,1,.2)) +
        scale_y_continuous(limits = c(0,1),breaks=seq(0,1,.2)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=12), axis.title = element_text(size = 14))
},height=500, width=500)
}

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)
shinyApp(ui, server)

