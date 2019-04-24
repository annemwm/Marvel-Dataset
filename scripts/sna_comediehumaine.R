#### COMEDIE HUMAINE ANALYSIS ####
library("igraph")
library("splitstackshape")
library("RColorBrewer")

#### Set up
# read in Masterlist csv with metadata on filenames
masterlist <- read.csv("balzac_comediehumaine.csv", header=T)

# loop through each row of masterlist, find the corresponding distinct csv, pull out those
# characters and assign them all of the values that are found in that row of the masterlist
setwd("~/comediehumaine/")
files <- list.files('chars_clean')
# loop through files
char_masterlist <- NULL
for (i in 1:length(files)){
  char_list <- NULL
  # pull out corresponding row in masterlist
  metadata <- masterlist[(which(masterlist$filename == paste(strsplit(files[i], '.csv'), '.txt', sep=""))),]
  # go into file, add row to each char
  setwd("~/comediehumaine/chars_clean")
  chars <- read.csv(files[i], header=T)
  char_list <- cbind(chars[,1:3], metadata)
  char_masterlist <- rbind(char_masterlist, char_list)
}

#### Social network analysis

# create nodes list 
nodes.all<-char_masterlist[,2:ncol(char_masterlist)]
colnames(nodes.all) <- c("NAME", "APPEARANCES", "GENDER")

# create edge list
edge.all <- NULL
firstchar <- 1
while (firstchar < nrow(char_masterlist)){ 
  work <- char_masterlist$filename[firstchar]
  while (char_masterlist$filename[firstchar+1] == work && firstchar < nrow(char_masterlist)){
    char1 <- char_masterlist$entity[firstchar]
    secondchar <- firstchar + 1 #loop through first character to make their connections to everyone below them 
    while (secondchar <= nrow(char_masterlist) & char_masterlist$filename[secondchar] == work){
      edge.all <- rbind(edge.all, data.frame(char1, char_masterlist$entity[secondchar], char_masterlist$filename[secondchar]))
      secondchar <- secondchar+1 #loop up through characters to match to first character
    }
    firstchar <- firstchar+1
  }
  firstchar <- secondchar
}

colnames(edge.all) <- c("NAME.1", "NAME.2", "filename")

#subset only by characters (source and target)
edge1<-data.frame(source=edge.all$NAME.1, target=edge.all$NAME.2)

#make graph object g
ch.g<-graph.data.frame(edge1, directed=FALSE, vertices=NULL)
#make a second that collapses all interactions into unique interactions (weighted edges)
ch.g2 <- graph.adjacency(get.adjacency(ch.g),mode=c("undirected"), weighted=TRUE)

# Degree centrality
degree(ch.g)
degree(ch.g2)

# Eigenvector centrality
evcent(ch.g, directed=FALSE) # if a directed graph, set to TRUE

# Closeness
sort(closeness(ch.g, vids=V(ch.g), mode = c("all"), weights = NULL, normalized = FALSE), TRUE)

# Strength
graph.strength(ch.g2, vids=V(ch.g2), mode = c("all"))

# Betweeness
sort(betweenness(ch.g), TRUE)

# Diameter
diameter(ch.g2, directed = FALSE, weights=E(ch.g2)$weight)
farthest.nodes(ch.g2, weights=E(ch.g2)$weight)

# Transitivity
transitivity(ch.g2, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

# Graph Density
edge_density(ch.g2)

# Community Detection

# Louvain clusters
comm1<-cluster_louvain(ch.g2, weights = E(ch.g2)$weight)
#find modularity score
max(comm1$modularity)
#find who is in what community
comm1.df<-data.frame(comm1$names, comm1$membership)

# Assortativity
nodegender <- as.character(nodes.all$GENDER[match(V(ch.g)$name,nodes.all$NAME)])
V(ch.g)$gender <- nodegender
V(ch.g2)$gender <- nodegender

# Robustness
robustness_vector<-vector()
for (j in 1:100) {
  random.nodes<-sample(V(ch.g), length(V(ch.g)), replace = FALSE, prob = NULL)
  ch.g.robust<-as.undirected(ch.g)
  for (i in 1:length(V(ch.g))) {
    ch.g.robust<-delete.vertices(ch.g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(ch.g.robust)==FALSE)
      break
  }
  robustness_vector<-append(robustness_vector, i/length(V(ch.g)))
}

# Vulnerability
deg<-names(sort(degree(ch.g2),TRUE))
ch.g.vulnerable<-as.undirected(ch.g2)
for (i in 1:length(V(ch.g2))) {
  ch.g.vulnerable<-delete.vertices(ch.g.vulnerable, v=deg[[i]])
  if (is.connected(ch.g.vulnerable)==FALSE)
    break
}
#it prints the number of nodes needed to be removed
print(i)  
#and the percentage of the network
print(vulnerability<-i/length(V(ch.g2)))

#### Graph

# assign colours to edges by work
ch.colpal <- colorRampPalette(brewer.pal(11,"BrBG"))(87)

filename <- ch.edge.all$filename[1]
ch.edgecol <- ch.colpal[1]
j=2
for (i in 2:nrow(ch.edge.all)){
  if (ch.edge.all$filename[i] == filename){
    ch.edgecol <- rbind(ch.edgecol, ch.edgecol[i-1])
  } else {
    ch.edgecol <- rbind(ch.edgecol, ch.colpal[j])
    filename <- ch.edge.all$filename[i]
    j<-j+1
  }
}

E(ch.g)$color <- mar.edgecol
V(ch.g)$name

plot.igraph(ch.g, vertex.label.color = "black", vertex.frame.color=NA,
            vertex.label = ifelse(degree(ch.g) > 150, V(ch.g)$name, NA),
            vertex.label.cex=c(0.75))









