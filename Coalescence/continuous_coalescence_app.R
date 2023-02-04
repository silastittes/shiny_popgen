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
  tree$mutations <- rep(0, nrow(tree$topology))
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

paint_mutations <- function(tree, viz = F, theta = 2){
  n_nodes <- nrow(tree$topology)
  for(i in 1:(n_nodes-1)){
    #parent branch is first column
    parent_branch <- tree$topology[i, 1]
    child_branch <- tree$topology[i, 2]
    
    br <- tree$branch_lengths[parent_branch] - tree$branch_lengths[child_branch]
    n_muts <- rpois(1, lambda = br)
    
    #tree$topology[tree$topology[,1] == parent_branch, 2]
    tree$mutations[i] <- tree$mutations[i] + n_muts
    
    if(n_muts > 0){
      if(viz){
        
        #midpoint along x and y axis to label mutations on tree
        mut_pos_x <- (tree$nod_pos[parent_branch] + tree$nod_pos[child_branch])/2
        mut_pos_y <- (tree$branch_lengths[parent_branch] + tree$branch_lengths[child_branch])/2
        
        points(mut_pos_x, mut_pos_y,bg = "black", pch = 22, cex = 1.75)
        text(mut_pos_x, mut_pos_y, n_muts, cex = 1, col = "white")
      }
    }
  }
  return(tree)
}

get_daughters <- function(tree, nodes){
  tree$topology[tree$topology[,1] %in% nodes, 2]  
}

collect_daughters <- function(tree, mutation_index){
  all_daughters <- c()
  daughters <- tree$topology[mutation_index,2]
  all_daughters <- c(all_daughters, daughters)
  while(length(daughters) > 0){
    if(length(get_daughters(tree, daughters)) > 0){
      daughters <- get_daughters(tree, daughters)
      all_daughters <- c(all_daughters, daughters)
    } else {
      break
    }
  }
  all_daughters[all_daughters %in% tree$tip_order]
}


make_aln <- function(tree, viz = F){
  n_mutations <- sum(tree$mutations)
  #make alignment mutations X tips
  aln <- matrix(rep(0, n_mutations * tree$n_pop), ncol = tree$n_pop)
  locus <- 1
  for(mut_index in seq_along(tree$mutations)){
    n_mutations <- tree$mutations[mut_index]
    if(n_mutations > 0){
      children <- collect_daughters(tree, mut_index)
      #mutation sites at current row
      aln[locus:(locus + n_mutations - 1), which(tree$tip_order %in% children)] <- 1
      #update where along alignment to add more mutations
      locus <- locus + n_mutations 
    }
  }
  
  if(viz){
    plot_aln(tree, aln)
  }
  return(aln)
}


plot_aln <- function(tree, aln){
  dimz <- dim(aln)
  if(min(dimz) >0 ){
    plot(NA, NA, ylim = c(1, dimz[1]), xlim = c(1, dimz[2]), axes = F, ylab = "", xlab = "")
    gridz <- expand.grid(dimz[1]:1, 1:dimz[2])
    points(gridz[,2], gridz[,1],pch = 22, bg = aln)
  }
}


cwd <- 2
# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Continuous time coalescence"),
   
   # Sidebar
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


# Define server logic
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
    
    data_out$tree <- paint_mutations(data_out$tree, data_out$theta, viz = input$mutations)
    
    if (input$aln) make_aln(data_out$tree, viz = TRUE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# n <- 4; theta <- 2;
# tree = r_coal(n, theta)
# tree
# plot_coal(tree)
# tree <- paint_mutations(tree, viz = TRUE, theta = theta)
# tree
# aln <- make_aln(tree, viz = T)
# dimz <- dim(aln)
# plot(NA, NA, ylim = c(1, dimz[1]), xlim = c(1, dimz[2]), axes = F, ylab = "", xlab = "")
# gridz <- expand.grid(dimz[1]:1, 1:dimz[2])
# points(gridz[,2], gridz[,1],pch = 22, bg = aln)
# children <- collect_daughters(tree, 7)
