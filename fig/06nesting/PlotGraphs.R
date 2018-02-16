setwd("/Users/vasistas/Dropbox/Thesis/terVersion/imgs/03introduction")

library(igraph)
library(dplyr)
library(ggplot2)

converting <- function(fn,msgV) {
  fg <- read.csv(fn, sep = " ",header = FALSE)[,c(1,3)]
  names(fg) <- c("s","t")
  fg <- merge(fg,msgV,all.x = TRUE,by.x = "s",by.y = "msgV")
  fg <- merge(fg,msgV,by.x = "t",by.y = "msgV")
  fg <- merge(fg,msgV,by.x = "s",by.y = "msgV")[,c(3,2)]
  names(fg) <- c("s","t")
  fg <- merge(fg,msgV,by.x = "t",by.y = "msgV")[,c(2,3)]
  names(fg) <- c("s","t")
  fg <- fg[with(fg,order(s,t)),]
  return(fg)
}

aggregateGraph <- function(msg, mc) {
  k <- merge(msg,mc,by.x="s",by.y="v",all.x = TRUE)[,c(3,2)]
  names(k) <- c("s","t")
  k <- merge(k,mc,by.x="t",by.y="v",all.x = TRUE)[,c(2,3)]
  names(k) <- c("s","t")
  k <- merge(msg,mc,by.x="s",by.y="v",all.x = TRUE)[,c(3,2)]
  names(k) <- c("s","t")
  k <- merge(k,mc,by.x="t",by.y="v",all.x = TRUE)[,c(2,3)]
  names(k) <- c("s","t")
  k <- distinct(k)
  k <- k[with(k,order(s,t)),]
  return(k)
}

set.seed(1)

msg <- read.csv("msg_s_edges.txt", sep = " ",header = FALSE)[,c(1,3)]
msgx <- (sort(unique(combine(msg$V1,msg$V3))))
msgx <- as.data.frame(msgx)
names(msgx) <- c("msgV")
msgx$new <- as.numeric(rownames(msgx))




fg <- converting("friend.csv",msgx)
msg <- converting("msg_s_edges.txt",msgx)

ggg <- barabasi.game(max(msgx$new), power=1)
eb <- edge.betweenness.community(ggg)
ggg.groups <- max(eb$membership)
COLOR <- topo.colors(ggg.groups)

pdf("friendgraph.pdf")
plot(ggg,
     vertex.color= COLOR[eb$membership],
     vertex.size=10, vertex.label = NA,
     edge.arrow.size=.2)
dev.off()

mc <- as.data.frame(eb$membership)
names(mc) <- c("g")
mc$v <- rownames(mc)
names(msg) <- c("s","t")

k <- aggregateGraph(msg,mc)

htmp <- as.data.frame(get.edgelist(ggg))
names(htmp) <- c("s","t")
h <- aggregateGraph(htmp,mc)

plotgraph <- function(z,x,isi,j) {
  if (j) {
    graph <- graph.data.frame(z, directed = TRUE)
  } else {
    graph <- z
  }
   
  # Re-generate dataframes for both nodes and edges, now containing
  # calculated network attributes
  node_list <- get.data.frame(graph, what = "vertices")
  
  # Determine a community for each edge. If two nodes belong to the
  # same community, label the edge with that community. If not,
  # the edge community value is 'NA'
  edge_list <- get.data.frame(graph, what = "edges") %>%
    inner_join(node_list %>% select(name), by = c("from" = "name")) %>%
    inner_join(node_list %>% select(name), by = c("to" = "name")) #%>%
  #mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())
  
  # Create a character vector containing every node name
  all_nodes <- sort(node_list$name)
  
  # Adjust the 'to' and 'from' factor levels so they are equal
  # to this complete list of node names
  plot_data <- edge_list %>% mutate(
    to = factor(to, levels = all_nodes),
    from = factor(from, levels = all_nodes))
  
  l <- layout.fruchterman.reingold(graph)
  
  
  pdf(x)
  if (isi) {
    plot(simplify(graph),
         vertex.color = COLOR,
         vertex.size=10, 
         layout=l, 
         edge.arrow.size=.2)
  } else {
    plot(simplify(graph),
         vertex.color= COLOR[eb$membership],
         vertex.size=10, layout=l, 
         edge.arrow.size=.2)
  }
  
  dev.off()
}

#set.seed(1)
#plotgraph(fg,"friendgraph.pdf")

set.seed(1)
plotgraph(msg,"msggraph.pdf",FALSE,TRUE)

set.seed(1)
plotgraph(k,"aggregated.pdf",TRUE,TRUE)

set.seed(1)
plotgraph(h,"aggregatedSocial.pdf",TRUE,TRUE)
