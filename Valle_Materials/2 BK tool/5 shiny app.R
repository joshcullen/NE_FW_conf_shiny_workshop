# Load libraries
library(shiny)

# Load data:
setwd('U:\\uf\\Shiny course\\2022 NE FWC\\2 BK tool')
dat1=read.csv('calc all1.csv',as.is=T)
uni=sort(unique(dat1$reg))

# Define UI 
ui <- fluidPage(
    
  # Application title.
  titlePanel("Probability of malaria infection"),
  
  #side bar layout
  sidebarLayout(
    #define what will be shown in side bar panel
    sidebarPanel(
      width=3,
      selectInput(inputId="region1", 
                  label="Choose region:",
                  choices = uni)
    ),

    #define what will be shown in main panel
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic

server <- function(input, output) {

    # compartmentalize server for ready debugging.
    # process data based on user inputs
    data <- reactive({
      cond=dat1$fever2wk==0 & dat1$male==0 & 
        dat1$reg==input$region1 & dat1$urban==0
      dat2=dat1[cond,]
    })
    
    # Make outputs based on data processed per
    # user inputs
    output$plot <- renderPlot({
      dat3 <- data()

      #draw graph
      plot(pinf~age,data=dat3,ylim=c(0,1),pch=19,
           xlab='Age',ylab='Probability of infection')
      points(pinf.rdt0~age,data=dat3,pch=19,col='blue')
      points(pinf.rdt1~age,data=dat3,pch=19,col='red')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
