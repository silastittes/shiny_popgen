library(tidyverse)

#Haploid
fA_t <- function(t, fA_0, s){
  fA_0 / (fA_0 + (1-s)^t * (1-fA_0))
}


#user interface
ui <- pageWithSidebar( 
  
  headerPanel = headerPanel("Selection - single locus haploid model"),
  
  sidebarPanel(
    
    sliderInput(inputId = "s", label = "Selection", value = 0.5, 
                min = -1, max = 1, step = 0.05),
    
    sliderInput(inputId = "fA_0", label = "Initial fA", value = 0.5, 
                min = 0, max = 1, step = 0.05),
    
    sliderInput(inputId = "gen", label = "Numer of generations", value = 100, 
                min = 2, max = 200, step = 1)
    
  ), 
  
  mainPanel =  mainPanel(
    plotOutput(outputId = 'freq')
  )
)

#back end code and response to user input
server <- function(input, output){
  
  sim_A <- reactive({
    
    #parameters
    s = input$s
    fA_0 = input$fA_0
    gen = input$gen
    t = seq(1, gen, length.out = 1000)
    
    return(data.frame(t=t, fA = fA_t(t, fA_0, s)))
    
  })
  
  
  output$freq <- renderPlot({
    
    sim_A() %>%
      ggplot(aes(x = t, y = fA)) +
      geom_line() +
      xlab("generations") +
      ylab(expression(f[A])) +
      ylim(0, 1) + 
      theme(panel.background =  
              element_rect(fill =  rgb(30, 144, 255, 25, 
                                       maxColorValue = 255)),
            text = element_text(size=16, family= "Times"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)