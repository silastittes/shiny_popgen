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

sim_sel <- function( n_gen = 10,
                     n_reps = 5,
                     p0 = 0.5,
                     N = 100,
                     wAA = 0.6,
                     wAa = 1,
                     waa = 0.2
){
  p_ <- matrix(NA, nrow = n_gen, ncol = n_reps)
  colnames(p_) <- paste0("sim", 1:n_reps)
  p_[1,] <- rep(p0, n_reps)
  
  all_t <- 2:n_gen
  for(t in all_t){
    for(j in 1:n_reps){
      ave_W <- wAA * (p_[t-1,j])^2 +
        wAa * 2 * p_[t-1,j] * (1 - p_[t-1,j]) +
        waa * (1 - p_[t-1,j])^2
      det_p <- (wAA * (p_[t-1,j])^2 + wAa * p_[t-1,j] * (1 - p_[t-1,j])) / ave_W
      p_[t,j] <- rbinom(n = 1, N, prob = det_p) / N
    }
  }
  data.frame(
    cbind(
      time = c(1,all_t), 
      fA = p_
    )
  ) %>%
    gather(sim, fA, -time)
}

#user interface
ui <- pageWithSidebar( 
  
  headerPanel = headerPanel("Diploid selection with finite population size"),
  
  sidebarPanel( 
    
    actionButton("goButton", "GO"),
    
    sliderInput(inputId = "N", label = "Number of individuals", value = 50,
                min = 1, max = 1000, step = 20),
    sliderInput(inputId = "n_reps", label = "Number of replicates", value = 5,
                min = 1, max = 100, step = 5),
    sliderInput(inputId = "n_gen", label = "Number of generations", value = 20,
                min = 1, max = 100, step = 5),
    sliderInput(inputId = "wAA", label = "Fitness of AA", value = 1,
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "wAa", label = "Fitness of Aa", value = 0.5,
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "waa", label = "Fitness of aa", value = 0.2,
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "p0", label = "Initial allele frequencies", value = 0.5,
                min = 0, max = 1, step = 0.05),
    
    downloadButton(outputId = 'sim_out.csv', label = 'Download')
  ), 
  mainPanel =  mainPanel(
    plotOutput(outputId = 'fitness')
  )
)

#back end code and response to user input
server <- function(input, output){
  
  #each time user hits "go"
  rand <- eventReactive(input$goButton, {
    
    sim_sel(input$n_gen, input$n_reps, input$p0, input$N, input$wAA, input$wAa, input$waa)
    
  })
  
  output$fitness <- renderPlot({
    
    out <- rand()
    
    out %>%
      ggplot(aes(time, fA, colour = sim)) +
      geom_line() +
      xlab("generation") +
      ylab(expression(f[A])) +
      ylim(0, 1) +
      geom_hline(yintercept = out$fA[1], lty = 2) +
      theme(panel.background = element_rect(
        fill =  rgb(30, 144, 255, 25, maxColorValue = 255)),
        text = element_text(size=16, family= "Times"
        )
      )
  })
  
  output$sim_out.csv <- downloadHandler(
    filename = function() {'sim-out.csv'},
    content = function(file) {
      write.csv(rand(), file, row.names = F, quote = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)