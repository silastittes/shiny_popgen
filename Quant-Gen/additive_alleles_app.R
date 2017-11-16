#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

pheno.dist <- function(n.loci, allele.types) {
  n.alleles <- 2 * n.loci #2 alleles for each locus
  class.counts <- choose(n.alleles,
                         0:n.alleles)
  # turn counts into proportions
  class.freqs <- class.counts/sum(class.counts)
  #class.freqs <- class.counts
  class.types <- NULL #storage
  for (i in 0:n.alleles) {
    
    type <- i * allele.types[1] +
      (n.alleles - i) * allele.types[2]
    class.types <- c(class.types,
                     type) #add to storage
  }
  return(list(class.types = class.types,
              class.freqs = class.freqs))
  
}

#for_sim(n = 10, gen = 20, show_coal = F)

ui <- fluidPage(pageWithSidebar( 
  
  headerPanel = headerPanel("Additive alleles"),
  
  sidebarPanel(
    
    sliderInput(inputId = "n", label = "Number of independent loci", value = 1, 
                min = 1, max = 100, step = 1),
    sliderInput(inputId = "A", label = "Phenotypic contribution of A alleles", value = 10, 
                min = 0, max = 100, step = 1),
    sliderInput(inputId = "a", label = "Phenotypic contribution of a alleles", value = 0, 
                min = 0, max = 100, step = 1)

  ), 
  
  mainPanel =  mainPanel(
    plotOutput(outputId = 'viz')
  )
))


#back end code and response to user input
server <- function(input, output){
  
  output$viz <- renderPlot({
    out <- pheno.dist(input$n, c(input$A, input$a))
    plot(out$class.types, out$class.freqs,
         type = "h", lwd = 3, ylab = "Frequency of phenotypic class",
         xlab = "Phenotypic classes", ylim = c(0, max(out$class.freqs, na.rm = F)*1.05))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
