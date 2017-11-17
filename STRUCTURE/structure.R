#get package names
library(tidyverse)
library(wesanderson)

get_qmat <- function(infile, buffer = 10e3){
  
  #subset structure output to q matrix lines
  qmat <- rep(NA, buffer)
  con = file(infile, "r")
  match <- "        Label (%Miss) Pop:  Inferred clusters"
  stop_match <- "Estimated Allele Frequencies in each cluster"
  mark <- 0
  index <- 1
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    if(line == match) mark <- 1
    if(line == stop_match) mark <- 0
    if(mark){
      qmat[index] <- line
      index <- index + 1
    }
  }
  
  close(con)
  
  #process strings from file
  q_na <- na.omit(qmat)
  q_data <- q_na[2:(length(q_na)-2)]
  q_df <- q_data %>% map_df(function(x){
    pop_string <- unlist(strsplit(unlist(strsplit(x, split = "\\("))[1], split = " "))
    pop_name <- as.character(pop_string[pop_string != ""][2])
    if(is.na(pop_name)) pop_name <- "fake"
    
    first_split <- unlist(strsplit(unlist(strsplit(x, split = ":"))[2], split = " "))
    split <- as.numeric(first_split[first_split != ""])
    df_line <- data.frame(pop_name, rbind(split), stringsAsFactors = F)
    df_line
  })
  return(q_df)
}

structure_plot <- function(qtable, colours, list_pops = F){
  pops <- sapply(
    strsplit(x = toupper(qtable$pop_name), split = "[[:digit:]]"),
    function(x)x[1])
  qtable <- qtable %>% mutate(pops = pops) %>% arrange(pops)
  
  #create barplot
  barplot(t(qtable[,-c(1,ncol(qtable))]), col = colours, 
          space=0, border=NA,xlab = "", ylab = "q")
  
  #get counts of individuals within each population
  pops <- table(pops)
  
  #create positioning for labels and population separators
  pop_labels <- cumsum(pops)
  pop_pos <- c(1, pop_labels)
  
  pop_adj <- 0.5*(na.omit(lag(pop_pos )) +  na.omit(lead(pop_pos)))
  
  #add separators                
  abline(v = pop_labels)
  
  #add population text
  if(list_pops){
    mtext(text = names(pop_labels), side = 1, at = pop_adj, cex = 0.7)  
    mtext(text = "population", side = 1, line = 2, cex = 1.2)  
  }
}


run_files <- function(path, colours){
  file_list <- list.files(path, full.names = T)
  order_files <- file_list[order(nchar(file_list), file_list)]
  last_file <- order_files[seq_along(order_files) %% 3 == 0]
  total <- length(last_file)
  invisible(suppressWarnings(
    seq_along(last_file) %>% map(function(x){
      qtable <- get_qmat(last_file[x])
      if(x == total){
        structure_plot(qtable, colours = colours, 
                       list_pops = T)  
      } else {
        structure_plot(qtable, colours = colours, 
                       list_pops = T)    
      }
    })
  ))
}


names(wes_palettes)
pal <- wes_palette("Zissou")
#structure_plot(get_qmat("~/Documents/courses/popgen/STRUCTURE/Results/burn10K_iter100K_run_11_f"),
#               colours = pal, list_pops = T)

run_files("PATH TO FOLDER", colours = pal)

