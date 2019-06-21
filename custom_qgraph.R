# N <- matrix(rep(NA, 3*50), ncol = 3)
# for (j in 1:50){
#   n <- matrix(rep(NA, 50*50), ncol = 50)
#   x <- datalist50[[j]]
#   for (i in 1:49){
#     for (k in (i+1):50){
#       n[i,k] <- sum(complete.cases(x[,c(i,k)]))
#     }
#   }
#   N[j,] <- c(min(n,na.rm = T), max(n, na.rm = T), mean(n, na.rm = T))
# }

EDBqgraph_trait <- function(cor,n){
  edgecolors <- c("orchid2", "#08519C")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          label.fill.vertical = 1, label.fill.horizontal = 1, 
                          esize = 3, DoNotPlot = TRUE)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  return(graph)
}

EDBqgraph2 <- function(x, age, trait, inventory){
  print(paste(inventory, trait, age, sep = " "))
  ncol <- dim(x)[2]-1
  n <- matrix(rep(NA, ncol^2), ncol = ncol)
  for (i in 1:(ncol-1)){
    for (k in (i+1):ncol){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(as.matrix(x), missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
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
  results <- list(cor, graph)
  return(results)
}

EDBqgraph_communities <- function(x, bordercolors){
  edgecolors <- c("orchid1", "lightblue")
  graph <- qgraph(x, border.color = bordercolors, legend = F, border.width = 4, DoNotPlot = TRUE, minimum = .05)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  return(graph)
}

# define personal theme function for graphs
EDBqgraph50nCircle <- function(x){
  n <- matrix(rep(NA, 50*50), ncol = 50)
  for (i in 1:49){
    for (k in (i+1):50){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, ifelse(n > 500, .25, .125))
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, lambda.min.ratio = .0075,
                               tuning = gamma, layout = "groups",
                               node.width = 1, edge.width = 2, label.font = 2, 
                               #label.fill.vertical = 1, label.fill.horizontal = 1, 
                               label.cex = .7, legend.cex = .25, nodeNames = ItemInfo50$Item, 
                               labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup50, 
                               color = ipipcolors, legend = F)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                                   edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                               %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph50n <- function(x){
  n <- matrix(rep(NA, 50*50), ncol = 50)
  for (i in 1:49){
    for (k in (i+1):50){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo50$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup50, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

EDBqgraph120n <- function(x){
  n <- matrix(rep(NA, 120*120), ncol = 120)
  for (i in 1:119){
    for (k in (i+1):120){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo120$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup120, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph100n <- function(x){
  n <- matrix(rep(NA, 100*100), ncol = 100)
  for (i in 1:99){
    for (k in (i+1):100){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo100$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup100, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

EDBqgraph300n <- function(x){
  n <- matrix(rep(NA, 299*299), ncol = 299)
  for (i in 1:298){
    for (k in (i+1):299){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo300$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup300, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph135 <- function(cor,n){
  SPI135group <- list(Adaptability = 1:5, Anxiety = 6:10, ArtAppreciation = 11:15,
                      AttentionSeeking = 16:20, Authoritarianism = 21:25, Charisma = 26:30,
                      Compassion = 31:35, Conformity = 36:40, Conservatism = 41:45, 
                      Creativity = 46:50, EasyGoingness = 51:55,EmotionalExpressiveness = 56:60,
                      EmotionalStability = 61:65, Honesty = 66:70, Humor = 71:75,
                      Impulsivity = 76:80, Industry = 81:85, Intellect = 86:90, Introspection = 91:95,
                      Irritability = 96:100, Order = 101:105, Perfectionism = 106:110, 
                      SelfControl = 111:115, SensationSeeking = 116:120, Sociability = 121:125,
                      Trust = 126:130, WellBeing = 131:135)       
  spi135colors <- c(RColorBrewer::brewer.pal(12,"Set3"), 
                    RColorBrewer::brewer.pal(9,"Set1"), 
                    RColorBrewer::brewer.pal(6,"Pastel2"))
  edgecolors <- c("orchid2", "#08519C")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .3, #nodeNames = SPI.ItemInfo$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = SPI135group, 
                          color = spi135colors, min = .1)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  return(graph)
}

# define personal theme function for graphs
EDBqgraph135n <- function(x){
  n <- matrix(rep(NA, 135*135), ncol = 135)
  for (i in 1:134){
    for (k in (i+1):135){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  SPI135group <- list(Adaptability = 1:5, Anxiety = 6:10, ArtAppreciation = 11:15,
                      AttentionSeeking = 16:20, Authoritarianism = 21:25, Charisma = 26:30,
                      Compassion = 31:35, Conformity = 36:40, Conservatism = 41:45, 
                      Creativity = 46:50, EasyGoingness = 51:55,EmotionalExpressiveness = 56:60,
                      EmotionalStability = 61:65, Honesty = 66:70, Humor = 71:75,
                      Impulsivity = 76:80, Industry = 81:85, Intellect = 86:90, Introspection = 91:95,
                      Irritability = 96:100, Order = 101:105, Perfectionism = 106:110, 
                      SelfControl = 111:115, SensationSeeking = 116:120, Sociability = 121:125,
                      Trust = 126:130, WellBeing = 131:135)       
  spi135colors <- c(RColorBrewer::brewer.pal(12,"Set3"), 
                    RColorBrewer::brewer.pal(9,"Set1"), 
                    RColorBrewer::brewer.pal(6,"Pastel2"))
  edgecolors <- c("orchid2", "#08519C")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .3, #nodeNames = SPI.ItemInfo$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = SPI135group, 
                          color = spi135colors, min = .1)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph20n <- function(x){
  n <- matrix(rep(NA, 20*20), ncol = 20)
  for (i in 1:19){
    for (k in (i+1):20){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 1000, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = gamma, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo20$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup20, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

EDBqgraphAvLayout <- function(x, ItemInfo, ipipgroup){
  n <- matrix(rep(NA, 50*50), ncol = 50)
  for (i in 1:49){
    for (k in (i+1):50){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- max(n, na.rm = T)
  gamma <- ifelse(n > 500, .5, .25)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- list()
  for (i in 1:1000) {
    print(i)
    graph[[i]] <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                            tuning = gamma, layout = "spring",
                            node.width = 1, edge.width = 2, label.font = 2, 
                            #label.fill.vertical = 1, label.fill.horizontal = 1, 
                            label.cex = .7, legend.cex = .25, nodeNames = ItemInfo$Item, 
                            labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup, 
                            color = ipipcolors)
    graph[[i]]$graphAttributes$Edges$lty[graph[[i]]$Edgelist$weight < 0] <- 2
    graph[[i]]$graphAttributes$Edges$color <- ifelse(graph[[i]]$Edgelist$weight < 0, 
                                                edgecolors[1], edgecolors[2])
    dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
    graph[[i]]$graphAttributes$Nodes$label.color[graph[[i]]$graphAttributes$Nodes$color 
                                            %in% dark_colors] <- "white"
  }
  avlayout <- qgraph::averageLayout(graph)
  graph <- qgraph::qgraph(cor, cor, graph = "glasso", sampleSize = n, 
                    tuning = .5, layout = avlayout,
                    node.width = 1, edge.width = 2, label.font = 2, 
                    #label.fill.vertical = 1, label.fill.horizontal = 1, 
                    label.cex = .7, legend.cex = .25, nodeNames = ItemInfo$Item, 
                    labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup, 
                    color = ipipcolors)
    graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
    graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                                     edgecolors[1], edgecolors[2])
    dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
    graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                                      %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

EDBqgraphAvLayout2 <- function(x){
  n <- matrix(rep(NA, 135*135), ncol = 135)
  for (i in 1:134){
    for (k in (i+1):135){
      n[i,k] <- sum(complete.cases(x[,c(i,k)]))
    }
  }
  n <- median(n, na.rm = T)
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- list()
  for (i in 1:500) {
    print(i)
    graph[[i]] <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                                 tuning = .25, layout = "spring")}
  avlayout <- qgraph::averageLayout(graph)
  graph <- qgraph::qgraph(cor, cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = avlayout,
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, #nodeNames = ItemInfo50$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = SPI135group, 
                          color = spi135colors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

EDBNCT <- function(x){
  age_1 <- unique(x$age2)[1]
  age_2 <- unique(x$age2)[2]
  data1 <- x %>% dplyr::filter(age2 == age_1) %>% dplyr::select(-age2)
  data2 <- x %>% dplyr::filter(age2 == age_2) %>% dplyr::select(-age2)
  NCT <- NetworkComparisonTest::NCT(data1, data2, it = 1000, gamma = .25)
}

smallworldness_EDB <- function (x, B = 1000, up = 0.995, lo = 0.005) 
{
  require(qgraph)
  require(igraph)
  require(pbapply)
  x <- getWmat(x)
  if (is.list(x)) {
    return(lapply(x, smallworldness, B = B, up = up, lo = lo))
  }
  A <- x != 0
  A <- igraph::graph.adjacency(A, mode = "undirected", diag = F, weighted = NULL)
  N <- vcount(A)
  m <- ecount(A)
  clusttrg <- transitivity(A, type = "global", isolates = "zero")
  lengthtrg <- average.path.length(graph = A, directed = F, 
                                   unconnected = F)
  deg.dist <- igraph::degree(A, mode = "all", loops = F)
  no_cores <- detectCores() - 1
  system.time(rndA <- pblapply(1:B, function(i) degree.sequence.game(deg.dist, 
                                   method = "simple.no.multiple"), cl = no_cores))
  rndA <- lapply(1:B, function(x) degree.sequence.game(deg.dist, 
                                                       method = "simple"))
  clustrnd <- sapply(rndA, transitivity, type = "global", 
                     isolates = "zero")
  clustrnd_m <- mean(clustrnd)
  clustrnd_lo <- quantile(clustrnd, lo)
  clustrnd_up <- quantile(clustrnd, up)
  lengthrnd <- sapply(rndA, average.path.length, directed = F, 
                      unconnected = F)
  lengthrnd_m <- mean(lengthrnd)
  lengthrnd_lo <- quantile(lengthrnd, lo)
  lengthrnd_up <- quantile(lengthrnd, up)
  sigma <- (clusttrg/clustrnd_m)/(lengthtrg/lengthrnd_m)
  c(smallworldness = sigma, trans_target = clusttrg, averagelength_target = lengthtrg, 
    trans_rnd_M = clustrnd_m, trans_rnd_lo = unname(clustrnd_lo), 
    trans_rnd_up = unname(clustrnd_up), averagelength_rnd_M = lengthrnd_m, 
    averagelength_rnd_lo = unname(lengthrnd_lo), averagelength_rnd_up = unname(lengthrnd_up))
}

# define personal theme function for graphs
EDBqgraph50 <- function(x){
  n <- nrow(x)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo50$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup50, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph100 <- function(x){
  n <- nrow(x)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo100$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup100, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}

# define personal theme function for graphs
EDBqgraph20 <- function(x){
  n <- nrow(x)
  ipipcolors <- RColorBrewer::brewer.pal(5,"Set3")
  edgecolors <- c("orchid2", "#08519C")
  cor <- qgraph::cor_auto(x, missing = "pairwise")
  graph <- qgraph::qgraph(cor, graph = "glasso", sampleSize = n, 
                          tuning = .25, layout = "spring",
                          node.width = 1, edge.width = 2, label.font = 2, 
                          #label.fill.vertical = 1, label.fill.horizontal = 1, 
                          label.cex = .7, legend.cex = .25, nodeNames = ItemInfo20$Item, 
                          labels = T, esize = 3, DoNotPlot = T, groups = ipipgroup20, 
                          color = ipipcolors)
  graph$graphAttributes$Edges$lty[graph$Edgelist$weight < 0] <- 2
  graph$graphAttributes$Edges$color <- ifelse(graph$Edgelist$weight < 0, 
                                              edgecolors[1], edgecolors[2])
  dark_colors <- c("#9E9AC8", "#6A51A3", "#3F007D")
  graph$graphAttributes$Nodes$label.color[graph$graphAttributes$Nodes$color 
                                          %in% dark_colors] <- "white"
  results <- list(cor, graph)
  return(results)
}
