library(shiny)
library(JGTeach)
# Define UI for app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Genetic Drift"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "n",
                  label = "Population size",
                  min = 10,
                  max = 1000,
                  step = 50,
                  value = 100)
,
    
    sliderInput(inputId = "g",
                label = "Number of generations",
                min = 100,
                max = 1000,
                step = 100,
                value = 200)
    
, 
  
  sliderInput(inputId = "r",
              label = "Number of replicate populations",
              min = 10,
              max = 200,
              step = 10,
              value = 100)
  
,
sliderInput(inputId = "p0",
            label = "Initial frequency",
            min = 0.001,
            max = 0.999,
            step = 0.1,
            value = 0.5)

),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      textOutput('text'),
      plotOutput(outputId = "distPlot")
      
      
      
    )
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    drift(input$n,input$g,input$p0,input$r,PlotIt = T) -> d  
    lines(apply(d,1,mean)~seq(1,input$g),lwd=2, col='red') # mean line
    mtext(paste0("# of replicates that fixed the allele = ",
                 round(sum(ifelse(d[input$g,]==1,1,0)) / input$r ,2)),
          side=3)
  })
  output$text <- renderText({
    "Don't drift too far"
  })
}
shinyApp(ui = ui, server = server)
