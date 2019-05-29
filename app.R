rm(list=ls())
source('./utility-functions.R')
#source('./processing.R')
persistence_diagrams <- readRDS('./perstdiag.rds')
ui <- dashboardPage(
    dashboardHeader(title="TDA Image Viewer"),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
        fluidRow(
            column(width = 7,
                   box(status = "primary", plotOutput("cycleA"), width=12, height=520),
                   box(title = "Persistence", status="primary", plotOutput("perst"), width=10, height=600)
                   ),
            column(width = 4, 
                   box(title = "Parameters", status="warning",
                       selectInput("file", "ROI", files, width=200),
                       sliderInput("min_perst","Minimum Persistence Bound", min=0, max=1, value=0, width=200),
                       sliderInput("max_perst","Maximum Persistence Bound", min=0, max=1, value=1, width=200),
                       sliderInput("birth","Minimum Birth", min=0, max=1, value=1, width=200),
                       radioButtons("img_type", "Image Type", c("Color", "Grayscale", "Threshold"), selected="Color"),
                       checkboxInput("show_cycles", "Show Representative Cycles", TRUE),
                       checkboxInput("show_cc", "Show Representative Connected Components", FALSE),
                       checkboxInput("show_bg", "Show Background Image", TRUE)
                       )
                   )
        )
    )
)

server <- function(input, output, session) {

    img <-readPNG(paste(image_path, files[1], sep=''))
    img_height = nrow(img[,1,])*1.2
    img_width = nrow(img[2,,])*1.2
    output$cycleA<- renderPlot({
        img_index <- match(input$file, files)
        perstImage(paste(image_path, input$file, sep=''), persistence_diagrams[[img_index]][[2]], input$min_perst, input$max_perst, birth=input$birth, image=input$img_type, show_cycles = input$show_cycles, show_cc = input$show_cc, show_bg = input$show_bg)
    },height=img_height, width=img_width)

    output$perst<- renderPlot({
        img_index <- match(input$file, files)
        pd_betti <- persistence_diagrams[[img_index]][[2]]$diag
        pd_df <- data.frame(Birth=pd_betti[,2], Persistence = pd_betti[,3]-pd_betti[,2], Birth = pd_betti[,2], Betti = factor(pd_betti[,1]))
        pd_df <- pd_df[which(pd_df$Persistence > input$min_perst & pd_df$Birth < input$birth),]
        ggplot(pd_df, aes(Birth, Persistence)) + geom_point(aes(colour = Betti)) +
            scale_x_continuous(limits = c(0,1),breaks=seq(0,1,.2)) +
            scale_y_continuous(limits = c(0,1),breaks=seq(0,1,.2)) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=12), axis.title = element_text(size = 14))
    },height=500, width=550)
}

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)
shinyApp(ui, server)

