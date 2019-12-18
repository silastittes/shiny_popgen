library(shiny)
library(ggplot2)

MU <- 3e-8

linked_sel <- function(N, r, nu, s){
  
  syn_pi <- 4*N*MU
  tau <- 4*log(2*N)/s
  
  pi <- syn_pi * (r/(4*N*nu/tau + r))
  
  return(data.frame(r, pi = pi))
}

bgs <- function(N, mu, r){
  
  syn_pi <- 4*N*MU
  
  pi <- syn_pi * exp(-mu/r)
  return(data.frame(r = r, pi = pi))
}


#user interface
ui <- fluidPage(
  
  pageWithSidebar( 
    
    mainPanel =  mainPanel(
      plotOutput(outputId = 'ls')),
    
    
    headerPanel = headerPanel("Fast drift"),
    
    sidebarPanel(

      sliderInput(inputId = "N", label = "Population size", value = 1e4, 
                  min = 100, max = 1e5, step = 1e2),
      
      sliderInput(inputId = "s", label = "Selection coefficient (sweeps)", value = 1e-3,
                  min = 0, max = 0.1, step = 1e-3),
      
      sliderInput(inputId = "mu", label = "Deleterious mutation rate (bgs)", value = 0.1, 
                  min = 0, max = 1, step = 1e-3),
      
      sliderInput(inputId = "nu", label = "Sweep initiation rate", value = 0.1, 
                  min = 0, max = 1, step = 0.01)
      
      # sliderInput(inputId = "r", label = "Recombination rate maximum", value = 2, 
      #             min = 1, max = 5, step = 0.5)
    )))


#back end code and response to user input
server <- function(input, output){
  
  output$ls <- renderPlot({
    
    N <- input$N
    #r_seq <- seq(0, input$r, length.out = 1000)
    r_seq <- seq(0, 5, length.out = 1000)
    nu <- input$nu 
    s <- input$s
    mu <- input$mu
    
    link_df <- linked_sel(N, r_seq, nu, s)
    bgs_df <- bgs(N, mu, r_seq)
    
    ggplot() +
      geom_line(data = link_df, mapping = aes(x = r, y = pi, colour = "black"), lwd = 3) +
      geom_line(data = bgs_df,mapping = aes(x = r, y = pi, colour = "dodgerblue"), lwd = 3, lty = 3) +
      ylab("π") +
      xlab("r (cM/Mb)") +
      scale_colour_manual(name = '', 
                          values =c('black'='black','dodgerblue'='dodgerblue'), 
                          labels = c('Hitchhiking','BGS')) +
      theme_classic(base_size = 30)
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)