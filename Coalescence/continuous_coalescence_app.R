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


r_coal <- function(n_pop, theta = 2){
  
  append_node <- function(node_i, x){
    d_idx <- which(nodes == node_i)
    nodes <- append(nodes, node_i, after = x-1)
    nodes <- nodes[-d_idx]
    return(nodes)
  }
  #n_pop <- 14
  k <- n_pop:2
  leaves <- 1:n_pop
  nodes <- (n_pop+1):(n_pop + (n_pop-1))
  nodes_const <- nodes
  mx_nodes <- max(nodes)
  
  tree <- list(
    topology = matrix(
      rep(NA, 2*max(nodes)), 
      ncol = 2
    ),
    branch_lengths = rep(0, max(nodes)),
    nod_pos = rep(0, max(nodes))
  )
  
  for(i in seq(1, mx_nodes, 2)){
    if(i == mx_nodes){
      tree$topology[i, 1] <- mx_nodes
    } else {
      #i <- 1
      r_leaves <- sample(leaves, 2)
      r_node <- nodes[1]
      tree$topology[i:(i+1), 2] <- r_leaves
      tree$topology[i:(i+1), 1] <- r_node
      leaves <- c(r_node, leaves[-match(r_leaves, leaves)])
      nodes <- nodes[-1]
    }
  }
  
  
  exp_mu <- (k*(k-1))/2
  coal_t <- rexp(n_pop-1, rate = exp_mu)
  tree$branch_lengths[(n_pop+1):(n_pop + (n_pop-1))] <- (cumsum(coal_t)*theta)/2
  #tree$branch_lengths[(n_pop+1):(n_pop + (n_pop-1))] <- coal_t
  
  nodes <- nodes_const
  
  topo <- tree$topology[tree$topology[,2] > n_pop,]
  u_topo <- unique(topo)
  if( mean(is.na(u_topo)/length(u_topo)) == 1 ){
    topo <- 3
  } else {
    topo <- topo[-which(is.na(topo)),]  
  }
  
  
  for(j in rev(nodes_const)){
    node_idx <- which(nodes == j)
    
    if(length(dim(topo) > 0)){
      daughters <- topo[topo[,1] == j, 2]  
    } else{
      daughters <- topo[topo[1] == j][2]  
    }
    daughters <- daughters[!is.na(daughters)]
    
    if(length(daughters)){
      
      mn_daughters <- min(daughters)
      mx_daughters <- max(daughters)
      
      node_check1 <- nodes[(node_idx-1)] == mn_daughters
      if(length(node_check1)){
        if(!node_check1){
          nodes <- append_node(mn_daughters, node_idx)
        }  
      }
      node_check2 <- nodes[(node_idx-1)] == mx_daughters
      if(length(node_check2)){
        if(!node_check2){
          nodes <- append_node(mx_daughters, node_idx)
        }  
      }
    }
  }
  
  tree$nod_pos = rep(NA, max(nodes))
  tip_order <- rep(NA, n_pop)
  tip_count <- 1
  j <- 1
  i <- 2
  for(i in seq_along(nodes)){
    k <- sort(tree$topology[which(tree$topology[,1] == nodes[i])[1:2],2])
    if(k[1] <= n_pop){
      tip_order[tip_count:(tip_count)] <- k[1]
      tip_count <- tip_count + 1
    }
    if(k[2] <= n_pop){
      tip_order[tip_count] <- k[2]
      tip_count <- tip_count + 1
    }
    
    if(is.na(tree$nod_pos[k[1]])){
      tree$nod_pos[k[1]] <- j
      j <- j+1
    }
    
    if(is.na(tree$nod_pos[k[2]])){
      tree$nod_pos[k[2]] <- j
      j <- j+1
    }
    
    if(is.na(tree$nod_pos[nodes[i]])){
      tree$nod_pos[nodes[i]] <- mean(tree$nod_pos[k])
    } 
  }
  
  tree$nodes <- nodes
  tree$tip_order <- tip_order
  tree$n_pop <- n_pop
  return(tree)
}

plot_coal <- function(tree, node = F){
  ell_col <- rgb(30, 144, 255, 25, 
                 maxColorValue = 255)
  ell_col <- "white"
  
  mny <- -max(tree$branch_lengths)*.04
  
  par(las = 1)
  par(mar=c(0, 4, 1, 0))
  par(mfrow=c(2,1))
  
  #my_asp <- 2
  plot(NA, NA, xlim=c(1, max(tree$nod_pos)), 
       ylim = c(mny, max(tree$branch_lengths)),
       axes = F, xlab = "", ylab = "")
  axis(side = 2)
  mtext(text = "T", side = 2, line = 3, las = 1)
  abline(h = unique(tree$branch_lengths), lty = 2, col = "grey", lwd = 0.25)
  
  #rect(xleft = 1*buff, ybottom = mny*(buff+1), 
  #     xright = max(tree$nod_pos)*(buff+1), ytop = max(tree$branch_lengths)*(buff+1),
  #     col = ell_col, border = F)
  
  invisible(
    seq_along(tree$nodes) %>% purrr::map( ~ {
      #  .x <- 1
      #for(.x in seq_along(tree$nodes)){
      k <- tree$topology[which(tree$topology[,1] == tree$nodes[.x])[1:2],2]
      
      segments(x0 = tree$nod_pos[k], 
               y0 = tree$branch_lengths[k], 
               x1 = tree$nod_pos[tree$nodes[.x]], 
               y1 = tree$branch_lengths[tree$nodes[.x]],
               lwd = 2, col = rgb(0, 0, 0, 1))
      
      #text(tree$nod_pos[k[1]], mny, k[1])
      #text(tree$nod_pos[k[2]], mny, k[2])
      #text(tree$nod_pos[tree$nodes[.x]], tree$branch_lengths[tree$tree$nodes[.x]], tree$nodes[.x])
      
      if(k[1] <= tree$n_pop){
        text(tree$nod_pos[k[1]], mny, k[1])  
        #""
      }
      
      if(k[2] <= tree$n_pop){
        text(tree$nod_pos[k[2]], mny, k[2])
        #""
      }
      
    })
  )
  
  if(node){
    node_show <- tree$nod_pos[tree$branch_lengths > 0]
    branch_show <- tree$branch_lengths[tree$branch_lengths > 0]
    points(node_show, 
           branch_show, 
           pch = 15, col = "white", cex = 4.5)
    
    
    text(node_show, branch_show, 
         round(branch_show,2))
  }
}

###################
## add mutations ##
###################
paint_mutants <- function(tree, viz = F, theta = 2){
  #ne <- 1e8
  #mu <- 1e-5
  #theta <- 2
  #mx_nodes <- length(tree$branch_lengths)
  #text(1.5, tree$branch_lengths[mx_nodes], 
  #     eval(bquote(expression(theta ~  "=" ~ .(theta)))))
  
  count <- 1
  buffer <- 10000
  aln <- matrix(NA, nrow = tree$n_pop, ncol = buffer)
  rownames(aln) <- 1:tree$n_pop
  
  for(c_node in rev(tree$nodes)){
    
    #[1] on next line only considers on sub-branch
    c_dots <- tree$topology[tree$topology[,1] == c_node, 2] ###!!!!
    br <- tree$branch_lengths[c_node] - tree$branch_lengths[c_dots]
    
    n_muts <- rpois(2, lambda = br)
    #n_muts <- rpois(2, lambda = (theta*br)/2)
    
    for(k in 1:2){
      if(n_muts[k] > 0){
        
        if(viz){
          points((tree$nod_pos[c_node] + tree$nod_pos[c_dots[1]])/2, 
                 (tree$branch_lengths[c_node] + tree$branch_lengths[c_dots[1]])/2, 
                 bg = "black", pch = 22, cex = 1.75)
          
          text((tree$nod_pos[c_node] + tree$nod_pos[c_dots[1]])/2, 
               (tree$branch_lengths[c_node] + tree$branch_lengths[c_dots[1]])/2, 
               n_muts[k], cex = 1, col = "white")
        }
        all_dots <- rep(NA, length(tree$nodes))
        x <- c_dots[1]
        j <- 1
        j2 <- (j+length(x)-1)
        all_dots[j:j2] <- x
        j <- j2 + 1
        while(length(x)){
          x <- tree$topology[tree$topology[,1] %in% x, 2]  
          j2 <- (j+length(x)-1)
          if(length(x)){
            all_dots[j:j2] <- x
          }
          j <- j2 + 1
        }
        
        mutated <- all_dots[all_dots <= tree$n_pop]
        mutations <- rep(0, tree$n_pop)
        mutations[mutated] <- 1
        aln[,count:(count+n_muts[k]-1)] <- matrix(rep(mutations, n_muts[k]), 
                                                  ncol = n_muts[k], byrow = F)
        count <- count+n_muts[k]
      }
      
    }
  }
  
  #aln <- matrix(aln, nrow = sum(aln))
  aln <- aln[,-which(aln %>% apply(MARGIN = 2, function(x){
    as.logical(sum(is.na(x))/length(x))
  }))]
  return(aln)
}

plot_aln <- function(tree, aln){
  if(length(dim(aln)) > 0 ){
    aln <- aln[tree$tip_order,]  
  } else {
    aln <- aln[tree$tip_order]  
  }
  
  if(is.vector(aln)) aln <- matrix(aln)
  
  #aln <- t(aln)
  sz <- dim(aln)
  #aln[,]
  
  marg <- -0.5
  
  if(!is.null(sz)){
    if(sz[2] > 0){
      #par(mar=c(0, 0, 0, 0))
      plot(NA, NA, xlim = c(-marg, sz[1]+marg), ylim = c(sz[2], 0),
           axes = F, xlab = "", ylab = "")
      
      for(j in 1:ncol(aln)){
        for(i in 1:nrow(aln)){
          rect(xleft = (i-1), ybottom = (j-1), 
               xright = i, ytop = j, border = F,#, lwd = 4,
               col = aln[i,j]+8)
          
          text(x = (i-1 + i)/2, y = (j-1 + j)/2, round(aln[i,j], 2), cex = 1,
               col = "white")
        }
      }
    }
  }
}





cwd <- 2
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Continuous time coalescence"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         sliderInput("n", "Number of samples",
                     min = 2, max = 50,
                     value = 8, step = 1),
         
         sliderInput("theta", label = HTML("&theta;"),
                     min = 0, max = 5,
                     value = 2, step = 0.05),
  
     
            isolate(checkboxInput(inputId = "tree",
                                  label = strong("Show geneology"), 
                                  value = TRUE)),
     
 
            isolate(checkboxInput(inputId = "mutations",
                                  label = strong("Show mutations"),
                                  value = TRUE)),
     
  
            isolate(checkboxInput(inputId = "aln",
                                  label = strong("Show alignment"),
                                  value = TRUE)),
         
           isolate(checkboxInput(inputId = "nodes",
                                 label = strong("Show node ages"),
                                 value = TRUE)),
       
         actionButton("goButton", "GO"),
         
         helpText(
           a("More apps and source code", 
             target="_blank", cex = 0.5,href="https://github.com/silastittes/shiny_popgen"
           )
         )
      ),

      mainPanel(
        
          column(11, align="center",
                 plotOutput("distPlot")


        )
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  #each time user hits "go"
  rand <- eventReactive(input$goButton, {
    n = input$n
    theta = input$theta
    return(list(n=n, theta = theta, tree = r_coal(n, theta = theta)))
  })
  
  
   output$distPlot <- renderPlot({
     
     data_out <- rand()
     
     par(mfrow=c(2,1))
     
     if (input$tree) plot_coal(data_out$tree, node = input$nodes)
      
     aln <- paint_mutants(data_out$tree, data_out$theta, viz = input$mutations)
     
      if (input$aln) plot_aln(data_out$tree, aln)
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
