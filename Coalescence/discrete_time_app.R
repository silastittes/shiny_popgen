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
for_sim <- function(n, sample_frac, gen) {
  # sample_frac <- 0.2
  # n <- 10
  # gen <- 20
  samps <- 1:(n)
  sim_forward <-
    replicate(n = gen - 1, sort(sample(samps, replace = T)))
  
  par(mar = c(0, 0, 0, 2.5))
  
  plot(NA, NA, xlim = c(1, n), ylim = c(1, gen))
  invisible(1:gen %>% map( ~ points(1:(n), rep(.x, n))))
  
  connect_gen1 <- rep(gen:2, each = n)
  connect_gen2 <- connect_gen1 - 1
  
  segments(
    x0 = sim_forward,
    y0 = connect_gen1,
    x1 = samps,
    y1 = connect_gen2,
    col = "grey90",
    lwd = 2
  )
  
  sample_sz <- as.integer(sample_frac * n)
  sample_sz <- ifelse(sample_sz < 2, 2, sample_sz)
  samps <- sample(1:n, size = sample_sz, replace = F)
  t0 <- samps
  c_gen <- gen - 1
  
  while (c_gen > 0 & length(unique(t0)) > 1) {
    t0 <- sim_forward[t0, c_gen]
    c_gen <- c_gen - 1
  }
  
  colz <- "dodgerblue"
  
  if (length(unique(t0)) == 1) {
    points(
      unique(t0),
      (gen - c_gen),
      pch = 19,
      col = colz,
      cex = 2
    )
    mtext(
      text = (gen - c_gen),
      side = 4,
      at = (gen - c_gen),
      las = 1,
      cex = 1.5,
      line = 0.5
    )
  }
  
  
  segments(
    x0 = samps,
    y0 = 1,
    x1 = sim_forward[samps, gen - 1],
    y1 = 2,
    col = colz,
    lwd = 2
  )
  group_gen <- unique(sim_forward[samps, gen - 1])
  points(samps, rep(1, length(samps)), pch = 19, col = colz)
  points(group_gen, rep(2, length(group_gen)), pch = 19, col = colz)
  samps <- sim_forward[samps, gen - 2]
  i <- 1
  #for(i in 1:(gen - c_gen - 2)){
  while (length(group_gen) > 1 & i < gen - 1) {
    segments(
      x0 = group_gen,
      y0 = i + 1,
      x1 = sim_forward[group_gen, gen - (i + 1)],
      y1 = i + 2,
      col = colz,
      lwd = 2
    )
    
    points(group_gen,
           rep(i + 1, length(group_gen)),
           pch = 19,
           col = colz)
    points(sim_forward[group_gen, gen - (i + 1)],
           rep(i + 2, length(sim_forward[group_gen, gen - (i + 1)])),
           pch = 19,
           col = colz)
    group_gen <- unique(sim_forward[group_gen, gen - (i + 1)])
    i <- i + 1
    
  }
}


ui <- fluidPage(pageWithSidebar(
  headerPanel = headerPanel("Discrete time and coalescence"),
  
  sidebarPanel(
    sliderInput(
      inputId = "n",
      label = "Population size",
      value = 20,
      min = 2,
      max = 100,
      step = 2
    ),
    sliderInput(
      inputId = "sample_frac",
      label = "Sample (as fraction of Population size)",
      value = 0.2,
      min = 0,
      max = 1,
      step = 0.1
    ),
    sliderInput(
      inputId = "gen",
      label = "Generations",
      value = 10,
      min = 2,
      max = 200,
      step = 2
    ),
    
    actionButton("goButton", "GO"),
    
    helpText(
      a(
        "More apps and source code",
        target = "_blank",
        cex = 0.5,
        href = "https://github.com/silastittes/shiny_popgen"
      )
    )
    
  ),
  
  mainPanel =  mainPanel(plotOutput(outputId = 'viz'))
))


#back end code and response to user input
server <- function(input, output) {
  rand <- eventReactive(input$goButton, {
    return(list(
      n = input$n,
      sample_frac = input$sample_frac,
      geners = input$gen
    ))
  })
  
  output$viz <- renderPlot({
    out <- rand()
    for_sim(
      n = out$n,
      sample_frac = out$sample_frac,
      gen = out$geners
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)