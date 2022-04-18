#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

duel_rect <- function(x, y, size_x, size_y, col1 = NA, col2 = NA, ...){
  
  rect(xleft = x - size_x, 
       ybottom = y - size_y, 
       xright = x, 
       ytop = y + size_y, col = col1, ...
  )  
  rect(xleft = x, 
       ybottom = y - size_y, 
       xright = x + size_x, 
       ytop = y + size_y, col = col2, ...
  )  
}

#duel_rect(x = 0, 0, .1, .1, col1 = "red", col2 = "blue")


het <- function(x){
  2 * mean(x == 1) * mean(x == 2)
}

fst <- function(x1, x2){
  HS <- mean(c(het(x1), het(x2)))
  HT <- het(c(x1, x2))
  FST <- round((HT - HS)/HT, 3)
  return(FST)
}

ellipse <- function(x){
  2*sqrt(1 - (x)^2)
}



#user interface
ui <- pageWithSidebar( 
  
  headerPanel = headerPanel("FST in 2 Demes"),
  
  sidebarPanel(
    
    sliderInput(inputId = "N", label = "Number of individuals", value = 5, 
                min = 1, max = 50, step = 1),
    sliderInput(inputId = "A", label = "fA Deme 1", value = 0.5, 
                min = 0, max = 1, step = 0.05),
    sliderInput(inputId = "a", label = "fA Deme 2", value = 0.5, 
                min = 0, max = 1, step = 0.05),
  
    actionButton("goButton", "GO")
    
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
    freqA = input$A
    freqa = input$a
    
    return(list(N = N, freqA = freqA, freqa = freqa))

  })
  
  output$freq <- renderPlot({
    data_out <- rand()
    
    ell_col <- rgb(30, 144, 255, 25, 
                   maxColorValue = 255)
    
    ell_x <- seq(-1, 1, length.out = 1000)
    ell_y <- ellipse(ell_x)
    coords <- data.frame(c(ell_x, rev(ell_x)),
                         c(ell_y, -ell_y))
    
    spp <- 3
    plot(coords[[1]], coords[[2]], type ="n", asp = 0.8, axes = F, 
         xlab = "", ylab = "")
    polygon(coords[[1]] - spp, coords[[2]], col = ell_col, lwd =2)
    polygon(coords[[1]] + spp, coords[[2]], col = ell_col, lwd =2)
    text(-spp-1, 1.85, "Deme 1", cex = 1.2, font = 2)
    text( spp+1, 1.85, "Deme 2", cex = 1.2, font = 2)
    
    
    freq_d1 <- c(data_out$freqA, 1-data_out$freqA)
    freq_d2 <- c(data_out$freqa, 1-data_out$freqa)
    
    
    adj1 <- 0.1
    sz <- 3
    shpe <- c(22, 22)
    
    colr <- c(rgb(1, 0, 0, .9),
              rgb(0, 0, 1, .9))
    
    alleles_d1 <- matrix(sample(1:2, size = 2*data_out$N, prob = freq_d1, replace = T), 
                         ncol = 2)
    alleles_d2 <- matrix(sample(1:2, size = 2*data_out$N, prob = freq_d2, replace = T), 
                         ncol = 2)
    locations <- seq(0.01, 0.8, length.out = data_out$N)
    loc1 <- sample_n(coords, data_out$N)*locations
    loc2 <- sample_n(coords, data_out$N)*locations
    
    #points(loc1[[1]] - spp, loc1[[2]], pch = shpe[alleles_d1[,1]], 
    #       bg = colr[alleles_d1[,1]], cex = sz)
    
    #points((loc1[[1]] - spp) - adj1, loc1[[2]], pch = shpe[alleles_d1[,2]], 
    #       bg = colr[alleles_d1[,2]], cex = sz)
    
    #points((loc2[[1]] + spp) - adj1, loc2[[2]], pch = shpe[alleles_d2[,2]], 
    #       bg = colr[alleles_d2[,2]], cex = sz)
    
    #points(loc2[[1]] + spp, loc2[[2]], pch = shpe[alleles_d2[,1]], 
    #       bg = colr[alleles_d2[,1]], cex = sz)
    sz <- .1
    duel_rect(loc1[[1]] - spp, loc1[[2]], col1 = colr[alleles_d1[,1]], col2 = colr[alleles_d1[,2]], size_x = sz, size_y = sz)
    duel_rect(loc2[[1]] + spp, loc2[[2]], col1 = colr[alleles_d2[,1]], col2 = colr[alleles_d2[,2]], size_x = sz, size_y = sz)
    
    hs <- round(mean(c(het(alleles_d1), het(alleles_d2))),3)
    ht <- round(het(c(alleles_d1, alleles_d2)),3)
    fst <- fst(x1 = alleles_d1, x2 = alleles_d2)

    #legend(-4.5, -0.5, bquote(H[T] = .(ht) ,
     #                             H[S] = .(hs),
      #                            F[ST] = .(fst)),
       #    merge=TRUE, bty = "n")
    
    legend(-5.6, -0.5,
           c(eval(substitute( expression(H[S] == hs))),
             eval(substitute( expression(H[T] == ht))),
             eval(substitute( expression(F[ST] == fst)))
             ),
           bty = "n", cex = 1.65, text.font = 2)
           
  })

}

# Run the application 
shinyApp(ui = ui, server = server)