#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)

drift <- function(N, p0, gen, reps, bottle_t = 0, bottle_N = N){
  two_n <- 2*N
  out <- data.frame(
    replicate(n = reps, expr = {
      sims <- rep(NA, gen)
      sims[1] <- p0
      for(i in 2:gen){
        sims[i] <- rbinom(n = 1, size = two_n, prob = sims[i-1]) /
          two_n
        
        if(i >= bottle_t){
          sims[i] <- rbinom(n = 1, size = 2*bottle_N, prob = sims[i-1] ) / 
            (2*bottle_N)
        }        
      }
      return(sims)
    })
  ) %>% mutate(generation = 1:gen) %>% 
    gather(sim, freq, -generation)
}


#user interface
ui <- pageWithSidebar( 
  
  headerPanel = headerPanel("Genetic Drift and Bottlenecks"),
  
  sidebarPanel(
    
    numericInput(inputId = "N", label = "Population size (N) (1-1e5)", value = 100, 
                 min = 1, max = 1e5, step = 10),
    numericInput(inputId = "p0", label = "Initial allele freq", value = .5, 
                 min = 0, max = 1, step = .01),
    numericInput(inputId = "gen", label = "Generations (1-1e4)", value = 100, 
                 min = 1, max = 1e4, step = 10),
    numericInput(inputId = "bottle_t", label = "Bottleneck time", value = 100, 
                 min = 1, max = 1e4, step = 10),
    numericInput(inputId = "bottle_N", label = "Bottleneck pop. size (N bottleneck) (1-1e5)", value = 100, 
                 min = 1, max = 1e5, step = 10),
    sliderInput(inputId = "reps", label = "Number of replicates", value = 5, 
                min = 1, max = 100, step = 5),
    actionButton("goButton", "GO"),
    #plotOutput(outputId = "freq"),
    downloadButton(outputId = 'drift_out.csv', label = 'Download')
  ), 
  mainPanel =  mainPanel(
    plotOutput(outputId = 'freq')
  )
)

#back end code and response to user input
server <- function(input, output){
  
  #each time user hits "go"
  rand <- eventReactive(input$goButton, {
    
    #parameters
    N = input$N
    p0 = input$p0
    gen = input$gen
    reps = input$reps
    bottle_t = input$bottle_t
    bottle_N = input$bottle_N
    
    #package data for plotting
    return(drift(N, p0, gen, reps, bottle_t, bottle_N))
  })
  
  
  output$freq <- renderPlot({
    data_out <- rand()
    data_out %>%
      ggplot() +
      geom_line(aes(x = generation, y = freq, group = sim, colour = sim), 
                alpha = 0.9) +
      geom_hline(yintercept = data_out$freq[1], lty = 2) +
      ylim(0,1) +
      xlab("Generation (t)") +
      ylab(expression(f[A](t))) +
      theme(panel.background =  
              element_rect(fill =  rgb(30, 144, 255, 25, 
                                       maxColorValue = 255)),
            text = element_text(size=16, family= "Times"))
  })
  
  output$drift_out.csv <- downloadHandler(
    filename = function() {'drift-sim.csv'},
    content = function(file) {
      write.csv(rand(), file, row.names = F, quote = F)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
