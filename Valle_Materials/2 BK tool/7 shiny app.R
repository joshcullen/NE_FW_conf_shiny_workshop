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
  
  #side bar panel
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectInput(inputId="region", 
                  label="Choose region:",
                  choices = uni),
      selectInput(inputId="urban", 
                  label="Urban?",
                  choices = c('yes','no')),
      radioButtons(inputId='fever2wk',
                   label='Fever in past 2 weeks?',
                   choices=c('yes','no')),
      sliderInput(inputId='ager',
                  label='Age range',
                  min=0,max=5,
                  value=c(0,5),
                  step=0.5)
    ),

    #main panel
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
      urb=0
      if(input$urban=='yes') urb=1
      fever=0
      if(input$fever2wk=='yes') fever=1
      
      cond=dat1$fever2wk==fever & dat1$male==0 & 
        dat1$reg==input$region & dat1$urban==urb &
        dat1$age>= input$ager[1] & dat1$age <=input$ager[2]
      dat2=dat1[cond,]
    })
    
    # Make outputs based on data processed per
    # user inputs
    output$plot <- renderPlot({
      dat3 <- data()

      #draw graph
      par(mfrow=c(1,1),mar=c(5,6,1,1))
      plot(pinf~age,data=dat3,ylim=c(0,1),pch=19,
           xlab='Age',ylab='Probability of infection',cex=2,
           cex.lab=2,cex.axis=1.5)
      points(pinf.rdt0~age,data=dat3,pch=19,col='blue',cex=2)
      points(pinf.rdt1~age,data=dat3,pch=19,col='red',cex=2)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
