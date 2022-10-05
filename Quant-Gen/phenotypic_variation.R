#
# This is a Shiny web application You can run the application by clicking
# the 'Run App' button above
#
# Find out more about building applications with Shiny here:
#
#    http://shiny_rstudio_com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)
theme_set(theme_classic(15))

pheno_dist <- function(n_individuals, n_loci, allele_types, v_env) {
  n_alleles <- 2 * n_loci #2 alleles for each locus
  class_counts <- choose(n_alleles, 0:n_alleles)
  # turn counts into proportions
  class_freqs <- class_counts/sum(class_counts)
  
  class_types <- (0:n_alleles) * allele_types[1] + 
                 (n_alleles - (0:n_alleles)) * allele_types[2]
  
  additive_phenotype <- sample(class_types, size = n_individuals, prob = class_counts, replace = T)
  if(length(n_loci) < 1){
    phenotype <- rnorm(n=n_individuals, mean = 0, sd = v_env)
  } else {
    phenotype <- rnorm(n=n_individuals, mean = additive_phenotype, sd = v_env)  
  }
  
  
  return(
    tibble(
      additive_phenotype = additive_phenotype,
      phenotype = phenotype
    )
  )
}

#pheno_dist(n_loci = 10, allele_types = c(0, 10), 2)$phenotype

ui <- fluidPage(pageWithSidebar( 
  
  headerPanel = headerPanel("Phenotypic variation"),
  
  sidebarPanel(
    
    sliderInput(inputId = "N", label = "Number of individuals", value = 100, 
                min = 1, max = 500, step = 10),
    sliderInput(inputId = "n", label = "Number of independent loci", value = 1, 
                min = 0, max = 14, step = 1),
    sliderInput(inputId = "A", label = "Phenotypic contribution of A alleles", value = 10, 
                min = 0, max = 10, step = 1),
    sliderInput(inputId = "a", label = "Phenotypic contribution of a alleles", value = 0, 
                min = 0, max = 10, step = 1),
    sliderInput(inputId = "e", label = "Environmental contribution to phenotype variance", value = 0, 
                min = 0, max = 20, step = .2)
  ), 
  mainPanel =  mainPanel(
    plotOutput(outputId = 'viz')
  )
))


#back end code and response to user input
server <- function(input, output){
  
  output$viz <- renderPlot({
    out <- pheno_dist(input$N, input$n, c(input$A, input$a), input$e)
    p1 <- ggplot(data = out) +
      geom_histogram(aes(x=additive_phenotype), binwidth = 1) +
      ylab("Frequency") +
      xlab("Additive Phenotype")
    p2 <- ggplot(data = out) +
      geom_histogram(aes(x=phenotype), binwidth = 1) +
      ylab("Frequency") +
      xlab("Phenotype")
    p1 + p2 + plot_layout(ncol = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

