library(shiny)
library(ggplot2)
library(scales)

#sweep back of envelope
tau <- function(N,s){
  4*log(2*N)/s
}

L <- function(N,s, r, recovery_fraction){
  -log(1-recovery_fraction)/(r*tau(N, s))
}

e_pi <- function(r, N, s){
  (1 - exp(-r*tau(N, s)))
}

#user interface
ui <- fluidPage(
  
  pageWithSidebar( 
    
    mainPanel =  mainPanel(
      
      plotOutput(outputId = 'sweep')),
    
    headerPanel = headerPanel("Classic selective sweep size and duration"),
    
    sidebarPanel(
      
      sliderInput(inputId = "N", label = "Population size (log10 scale)", value = 4, 
                  min = 1, max = 6, step = 0.5),
      
      sliderInput(inputId = "s", label = "Selection coefficient (log10 scale)", value = -3,
                  min = -6, max = -1, step = 0.5),
      
      sliderInput(inputId = "r", label = "Recombination rate (log10 scale)", value = -8, 
                  min = -10, max = -1, step = 0.5),
      
      #sliderInput(inputId = "f", label = "Fraction of diversity recovered", value = 0.99, 
      #            min = 0.01, max = 0.99, step = 0.1),
      
      textOutput("sojourn_time"),
      textOutput("sweep_region"),
      tags$head(tags$style("#sojourn_time{color: black;
                                 font-size: 23px;
                                 font-style: bold;
                                 }", 
                            "#sweep_region{color: black;
                                 font-size: 23px;
                                 font-style: bold;
                                 }"
                          )
      ),
  
      url <- a("Please visit Chapter 13 of POPULATION AND QUANTITATIVE GENETICS for more info.", href="https://github.com/cooplab/popgen-notes/releases"),
    )))


#back end code and response to user input
server <- function(input, output){
  
  output$sweep <- renderPlot({
    
    N <- 10^input$N
    s <- 10^input$s
    r <- 10^input$r
    f <- 0.99#input$f
    sweep_gen <- tau(N, s)
    L_out <- L(N = N, s = s, r = r, recovery_fraction = f)
    
    
    #output$sojourn_time <- renderText(paste("Sojourn time:", round(sweep_gen, 0) + 1, " generations"))
    #output$sweep_region <- renderText(paste("Sweep region:", round(L_out, 0) + 1, " bps"))
    
    dists <- seq(-L_out, L_out, length.out = 1000)
    sweep_path <- e_pi(r = abs(dists*r), N = N, s = s)
    ggplot(data.frame()) +
      geom_line(aes(dists, sweep_path), lwd = 1.1) +
      theme_classic(20) +
      ylab(expression(pi/theta)) +
      ylim(0, 1) +
      xlab("Position (bp)") +
      ggtitle(label = paste0(
        "Sojourn time: ", scientific(sweep_gen, digits = 2), " generations\n", 
        "Sweep region: ", scientific(L_out, digits = 2), " bps")
      )
  })
}



# Run the application 
shinyApp(ui = ui, server = server)