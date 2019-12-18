library(tidyverse)

cwd <- 4

#user interface
ui <- fluidPage(pageWithSidebar( 
  
  headerPanel = headerPanel("Selection - single locus diploid model"),
  
  sidebarPanel(
    
    sliderInput(inputId = "wAA", label = "wAA", value = 1, 
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "wAa", label = "wAa", value = 0.5, 
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "waa", label = "waa", value = 0.1, 
                min = 0, max = 1, step = 0.05),
    
    sliderInput(inputId = "fA_0", label = "Initial fA", value = 0.5, 
                min = 0, max = 1, step = 0.05),
  
    sliderInput(inputId = "gen", label = "Numer of generations", value = 100, 
                min = 2, max = 200, step = 1)
    
  ), 
  
  mainPanel =  mainPanel(
    plotOutput(outputId = 'viz'),
    
    #column(width = cwd,
    #checkboxInput(inputId = "w_plot",
    #              label = strong("Show Average fitness by fA"),
    #              value = FALSE)
    #),
    
    #column(width = cwd, 
    #checkboxInput(inputId = "delta_plot",
    #              label = strong("Show change in fA by fA"),
    #              value = FALSE)
    #),
    
    #column(width = cwd,
    #checkboxInput(inputId = "time_plot",
    #              label = strong("Show fA by generation"),
    #              value = FALSE)
    #),
    
    selectInput("select", label = "Plot options", 
                 choices = list("Average Fitness by fA" = 1, "Change in fA by fA" = 2,
                                "fA by generation" = 3), selected = 1)
    
    
  )
))

#back end code and response to user input
server <- function(input, output){
  
  output$viz <- renderPlot({
    
    p <- seq(0, 1, length.out = 1000)
    #parameters
    wAA = input$wAA
    wAa = input$wAa
    waa = input$waa
    fA_0 = input$fA_0
    gen = input$gen
    t = seq(1, gen, length.out = 1000)

    #if(input$w_plot){
    if(input$select == 1){
      W <- p^2*wAA + 2*p*(1-p)*wAa + (1-p)^2*waa
      data.frame(p=p, W=W) %>%
        ggplot(aes(x = p, y = W)) +
        geom_line() +
        xlab(W) +
        xlab(expression(f[A])) +
        xlim(0, 1) +
        xlim(0, 1) + 
        theme(panel.background =  
                element_rect(fill =  rgb(30, 144, 255, 25, 
                                         maxColorValue = 255)),
              text = element_text(size=16, family= "Times"))
      
    #} else if(input$delta_plot){
    } else if(input$select == 2){
      W <- p^2*wAA + 2*p*(1-p)*wAa + (1-p)^2*waa
      delta_p <- (p^2*wAA + p*(1-p)*wAa) / W - p
      data.frame(p=p, delta_p=delta_p) %>%
        ggplot(aes(x = p, y = delta_p)) +
        geom_line() +
        geom_hline(yintercept = 0, lty = 2) +
        ylab(expression(paste(Delta,f[A]))) +
        xlab(expression(f[A])) +
        xlim(0, 1) +
        theme(panel.background =  
                element_rect(fill =  rgb(30, 144, 255, 25, 
                                         maxColorValue = 255)),
              text = element_text(size=16, family= "Times"))
      
      
    #} else if(input$time_plot){
    } else if(input$select == 3){
      
      p_t <- rep(NA, gen)
      p_t[1] <- fA_0
      for(i in 2:gen){
        W <- p_t[i-1]^2*wAA + 2*p_t[i-1]*(1-p_t[i-1])*wAa + (1-p_t[i-1])^2*waa
        p_t[i] <- (p_t[i-1]^2 * wAA + p_t[i-1] * (1-p_t[i-1]) * wAa) / W
      }
      
      data.frame(t = 1:gen, p_t = p_t) %>%
        ggplot(aes(x = t, y = p_t)) +
        geom_line() +
        xlab("generation") +
        ylab(expression(f[A])) +
        ylim(0, 1) + 
        theme(panel.background =  
                element_rect(fill =  rgb(30, 144, 255, 25, 
                                         maxColorValue = 255)),
              text = element_text(size=16, family= "Times"))
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
