EDBqgraph_orig <- function(age, inventory, rule, set, gamma){
  # setup for different inventories
  nitem <- str_remove_all(inventory, "[A-Z a-z]") %>% as.numeric()
  
  loadRData <- function(inventory, set, age){
    #loads an RData file, and returns it
    path <- sprintf("~/Box/networks/SAPA/01-data/%s/%s/%s.RData"
                    , inventory, set, age)
    load(path)
    get(ls()[ls() == "d"])
  }
  x <- loadRData(inventory, set, age)
  
  grps <- if(nitem == 20) ipipgroup20 else if(nitem == 50) ipipgroup50 else ipipgroup100
  iinfo <- if(nitem == 20) ItemInfo20 else if(nitem == 50) ItemInfo50 else ItemInfo100
  
  # setup for sample sizes using different rules
  # pwise_n_mat <- matrix(rep(NA, nitem*nitem), ncol = nitem)
  # for (i in 1:(nitem - 1)){
  #   for (k in (i+1):nitem){
  #     pwise_n_mat[i,k] <- sum(complete.cases(x[,c(i,k)]))
  #   }
  # }
  # pwise_n <- switch (rule,
  #   mean = ceiling(mean(pwise_n_mat, na.rm = T))
  #   , min = min(pwise_n_mat, na.rm = T)
  #   , max = max(pwise_n_mat, na.rm = T)
  #   , median = ceiling(median(pwise_n_mat, na.rm = T))
  # )
  
  pwise_n_vec <- colSums(!is.na(x))
  
  pwise_n <- switch (rule,
                     mean   = ceiling(mean(pwise_n_vec  , na.rm = T))
                     , min    =         min(pwise_n_vec   , na.rm = T)
                     , max    =         max(pwise_n_vec   , na.rm = T)
                     , median = ceiling(median(pwise_n_vec, na.rm = T))
  )
  
  # graph setups
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  
  # run the correlations
  cor <- qgraph::cor_auto(x, missing = "pairwise", forcePD = T)
  
  # gamma <- .5 # ifelse(n > 1000, .5, .25)
  
  # call qgraph on the correlations
  graph <- qgraph::qgraph(cor
                          , graph = "glasso"
                          , sampleSize = pwise_n
                          , tuning = gamma
                          , layout = "spring"
                          , node.width = 1
                          , edge.width = 2
                          , label.font = 2
                          , label.cex = .7
                          , legend.cex = .25
                          , nodeNames = iinfo$Item
                          , labels = T
                          , esize = 3
                          , DoNotPlot = T
                          , groups = grps
                          , color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  # 02-results <- list(cor, graph)
  save(graph, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s/network/%s-%s-%s-%s.RData",
                               set, inventory, age, rule, str_remove(gamma, "[.]")))
  save(cor, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s/cor/%s-%s-%s-%s.RData",
                             set, inventory, age, rule, str_remove(gamma, "[.]")))
  cent <- qgraph::centrality_auto(graph)
  save(cent, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s/centrality/%s-%s-%s-%s.RData"
                            , set, inventory, age, rule, str_remove(gamma, "[.]")))
  return(T)
}

EDBqgraph_n500 <- function(age, inventory, rule, set, gamma){
  # setup for different inventories
  nitem <- str_remove_all(inventory, "[A-Z a-z]") %>% as.numeric()
  
  loadRData <- function(inventory, set, age){
    #loads an RData file, and returns it
    path <- sprintf("~/Box/networks/SAPA/01-data/%s/%s/%s.RData"
                    , inventory, set, age)
    load(path)
    get(ls()[ls() == "d"])
  }
  x <- loadRData(inventory, set, age)
  
  sz <- if(nrow(x) < 500) nrow(x) else 500
  
  set.seed(4)
  xl <- lapply(1:100, function(z) x[sample(nrow(x), size = sz, replace = F),])
  
  grps <- if(nitem == 20) ipipgroup20 else if(nitem == 50) ipipgroup50 else ipipgroup100
  iinfo <- if(nitem == 20) ItemInfo20 else if(nitem == 50) ItemInfo50 else ItemInfo100
  
  # setup for sample sizes using different rules
  
  pwise_n_vec <- lapply(xl, function(z) colSums(!is.na(z)))
  pwise_n <- sapply(pwise_n_vec, function(z){
    switch (rule,
            mean   = ceiling(mean(z  , na.rm = T))
            , min    =         min(z   , na.rm = T)
            , max    =         max(z   , na.rm = T)
            , median = ceiling(median(z, na.rm = T))
    )
  })
  
  # graph setups
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  
  # run the correlations
  cor <- lapply(xl, function(z) qgraph::cor_auto(z, missing = "pairwise", forcePD = T))
  
  # gamma <- .5 # ifelse(n > 1000, .5, .25)
  
  # call qgraph on the correlations
  graph <- lapply(1:100, function(z) {
    g <- qgraph::qgraph(cor[[z]]
                   , graph = "glasso"
                   , sampleSize = pwise_n[z]
                   , tuning = gamma
                   , layout = "spring"
                   , node.width = 1
                   , edge.width = 2
                   , label.font = 2
                   , label.cex = .7
                   , legend.cex = .25
                   , nodeNames = iinfo$Item
                   , labels = T
                   , esize = 3
                   , DoNotPlot = T
                   , groups = grps
                   , color = ipipcolors)
    g$graphAttributes$Edges$lty[g$Edgelist$weight < 0] <- 2
    g$graphAttributes$Edges$color <- ifelse(g$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
    dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
    g$graphAttributes$Nodes$label.color[g$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
    return(g)
  })
  
  # 02-results <- list(cor, graph)
  save(graph, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s-500/network/%s-%s-%s-%s.RData",
                             set, inventory, age, rule, str_remove(gamma, "[.]")))
  save(cor, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s-500/cor/%s-%s-%s-%s.RData",
                           set, inventory, age, rule, str_remove(gamma, "[.]")))
  cent <- lapply(graph, function(z) qgraph::centrality_auto(z))
  save(cent, file = sprintf("~/Box/networks/SAPA/02-results/multitrait-%s-500/centrality/%s-%s-%s-%s.RData"
                            , set, inventory, age, rule, str_remove(gamma, "[.]")))
  return(T)
}

EDBqgraph_ST <- function(x, age, trait, inventory, rule, gamma){
  print(paste(inventory, trait, age, rule))
  ncol <- dim(x)[2]-1
  x <- x %>% select(-RID)
  nitem <- (str_remove_all(inventory, "[A-Z a-z]") %>% as.numeric())/5
  # pwise_n_mat <- matrix(rep(NA, nitem*nitem), ncol = nitem)
  # for (i in 1:(nitem - 1)){
  #   for (k in (i+1):nitem){
  #     pwise_n_mat[i,k] <- sum(complete.cases(x[,c(i,k)]))
  #   }
  # }
  # pwise_n <- switch (rule,
  #                    mean = ceiling(mean(pwise_n_mat, na.rm = T))
  #                    , min = min(pwise_n_mat, na.rm = T)
  #                    , max = max(pwise_n_mat, na.rm = T)
  #                    , median = ceiling(median(pwise_n_mat, na.rm = T))
  # )
  pwise_n_vec <- colSums(!is.na(x))
  
  pwise_n <- switch (rule,
                     mean   = ceiling(mean(pwise_n_vec  , na.rm = T))
                     , min    =         min(pwise_n_vec   , na.rm = T)
                     , max    =         max(pwise_n_vec   , na.rm = T)
                     , median = ceiling(median(pwise_n_vec, na.rm = T))
  )
  # gamma <- .5 # ifelse(n > 1000, .5, .25)
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise", forcePD = T)
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = pwise_n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          label.fill.vertical = 1, label.fill.horizontal = 1, 
                          esize = 3, DoNotPlot = TRUE)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  # 02-results <- list(cor, graph)
  # return(02-results)
  
  save(graph, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-age/network/%s-%s-%s-%s-%s.RData",
                             inventory, age, trait, rule, str_remove(gamma, "[.]")))
  save(cor, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-age/cor/%s-%s-%s-%s-%s.RData",
                            inventory, age, trait, rule, str_remove(gamma, "[.]")))
  cent <- qgraph::centrality_auto(graph)
  save(cent, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-age/centrality/%s-%s-%s-%s-%s.RData"
                            , inventory, age, trait, rule, str_remove(gamma, "[.]")))
  return(T)
}

EDBqgraph_ST_n500 <- function(x, age, trait, inventory, rule, gamma){
  print(paste(inventory, trait, age, rule))
  ncol <- dim(x)[2]-1
  x <- x %>% select(-RID)
  nitem <- (str_remove_all(inventory, "[A-Z a-z]") %>% as.numeric())/5
  # pwise_n_mat <- matrix(rep(NA, nitem*nitem), ncol = nitem)
  # for (i in 1:(nitem - 1)){
  #   for (k in (i+1):nitem){
  #     pwise_n_mat[i,k] <- sum(complete.cases(x[,c(i,k)]))
  #   }
  # }
  # pwise_n <- switch (rule,
  #                    mean = ceiling(mean(pwise_n_mat, na.rm = T))
  #                    , min = min(pwise_n_mat, na.rm = T)
  #                    , max = max(pwise_n_mat, na.rm = T)
  #                    , median = ceiling(median(pwise_n_mat, na.rm = T))
  
  
  # )
  
  sz <- if(nrow(x) < 500) nrow(x) else 500
  
  set.seed(4)
  xl <- lapply(1:100, function(z) x[sample(nrow(x), size = sz, replace = F),])
  
  pwise_n_vec <- lapply(xl, function(z) colSums(!is.na(z)))
  pwise_n <- sapply(pwise_n_vec, function(z){
    switch (rule,
            mean   = ceiling(mean(z  , na.rm = T))
            , min    =         min(z   , na.rm = T)
            , max    =         max(z   , na.rm = T)
            , median = ceiling(median(z, na.rm = T))
    )
  })
  
  # gamma <- .5 # ifelse(n > 1000, .5, .25)
  edgecolors <- c("orchid2", "#08519C")
  # run the correlations
  cor <- lapply(xl, function(z) qgraph::cor_auto(z, missing = "pairwise", forcePD = T))
  
  # gamma <- .5 # ifelse(n > 1000, .5, .25)
  
  # call qgraph on the correlations
  graph <- lapply(1:100, function(z) {
    g <- qgraph::qgraph(cor[[z]]
                        , graph = "glasso"
                        , sampleSize = pwise_n[z]
                        , tuning = gamma
                        , layout = "spring"
                        , node.width = 1
                        , edge.width = 2
                        , label.font = 2
                        , label.fill.vertical = 1
                        , label.fill.horizontal = 1
                        , esize = 3
                        , DoNotPlot = TRUE)
    g$graphAttributes$Edges$lty[g$Edgelist$weight < 0] <- 2
    g$graphAttributes$Edges$color <- ifelse(g$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  g$graphAttributes$Nodes$label.color[g$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  return(g)
  })
  # 02-results <- list(cor, graph)
  # return(02-results)
  
  save(graph, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-500/network/%s-%s-%s-%s-%s.RData",
                             inventory, age, trait, rule, str_remove(gamma, "[.]")))
  save(cor, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-500/cor/%s-%s-%s-%s-%s.RData",
                           inventory, age, trait, rule, str_remove(gamma, "[.]")))
  cent <- lapply(graph, function(z) qgraph::centrality_auto(z))
  save(cent, file = sprintf("~/Box/networks/SAPA/02-results/singletrait-500/centrality/%s-%s-%s-%s-%s.RData"
                            , inventory, age, trait, rule, str_remove(gamma, "[.]")))
  return(T)
}

# original functions no longer used because these were compiled into 
# more flexible code in EDBqgraph_orig
EDBqgraph_communities <- function(x, bordercolors){
  edgecolors <- c("orchid1", "lightblue")
  graph <- qgraph(x
                  , border.color = bordercolors
                  , legend = F
                  , border.width = 4
                  , DoNotPlot = TRUE
                  , minimum = .075
                  )
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <-  ifelse(graph$Edgelist$weight < 0, 
                                              "gray", "gray")
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  return(graph)
}
