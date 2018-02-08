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

##Forward time simulation
for_sim <- function(n, gen, show_coal = T){
  #n <- 50
  #gen <- 200
  samps <- 1:(n)
  sim_forward <- replicate(n = gen-1, sort(sample(samps, replace = T)))
  
  par(mar=c(0,0,0,0))
  
  plot(NA, NA, xlim = c(1, n), ylim = c(1, gen))
  invisible(1:gen %>% map(~ points(1:(n), rep(.x, n))))
  
  connect_gen1 <- rep(gen:2, each = n)
  connect_gen2 <- connect_gen1-1
  
  segments(x0 = sim_forward, y0 = connect_gen1, 
           x1 = samps, y1 = connect_gen2, col = ifelse(show_coal, "grey90", "grey50"), lwd = 2)
  
  t0 <- c(1:n)
  c_gen <- gen-1
  while(c_gen > 0 & length(unique(t0)) > 1){
    t0 <- sim_forward[t0, c_gen]
    c_gen <- c_gen-1
  }
  
  if(show_coal){
    if(length(unique(t0)) == 1){
      points(unique(t0), (gen - c_gen), pch = 19, col = "blue", cex = 2)
    }
    
    sim_forward[, c_gen:(gen-1)]
    segments(x0 = samps, y0 = 1, x1 = sim_forward[,gen-1], y1 = 2, col = "blue", lwd = 2)
    group_gen <- unique(sim_forward[,gen-1])
    points(samps, rep(1, length(samps)), pch = 19, col = "blue")
    points(group_gen, rep(2, length(group_gen)), pch = 19, col = "blue")
    for(i in 1:(gen - c_gen - 2)){
      segments(x0 = group_gen, y0 = i+1, x1 = sim_forward[,gen-(i+1)][group_gen], y1 = i+2, col = "blue", lwd = 2)
      points(group_gen, rep(i+1, length(group_gen)), pch = 19, col = "blue")
      points(sim_forward[,gen-(i+1)][group_gen], 
             rep(i+2, length(sim_forward[,gen-(i+1)][group_gen])), pch = 19, col = "blue")
      group_gen <- unique(sim_forward[,gen-(i+1)][group_gen])
    }
    
  }
}

#for_sim(n = 10, gen = 20, show_coal = F)

ui <- fluidPage(pageWithSidebar( 
  
  headerPanel = headerPanel("Discrete time and coalescence"),
  
  sidebarPanel(
    
    sliderInput(inputId = "n", label = "n", value = 5, 
                min = 2, max = 100, step = 1),
    sliderInput(inputId = "gen", label = "Generations", value = 2, 
                min = 2, max = 100, step = 1),
    
    checkboxInput("checkbox", label = "show coalescence", value = TRUE),
    
    actionButton("goButton", "GO"),
    
    helpText(
      a("More apps and source code", 
        target="_blank", cex = 0.5,href="https://github.com/silastittes/shiny_popgen"
      )
    )
    
  ), 
  
  mainPanel =  mainPanel(
    plotOutput(outputId = 'viz')
  )
))


#back end code and response to user input
server <- function(input, output){
  
  rand <- eventReactive(input$goButton, {
    return(list(n = input$n,
                geners = input$gen))
    }
  )

  output$viz <- renderPlot({
    out <- rand()
    for_sim(n = out$n, gen = out$geners, show_coal = input$checkbox)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


