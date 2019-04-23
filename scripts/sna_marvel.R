library("igraph")
library("splitstackshape")
library("stringr")

# read in edge list. --> this is set up so each marvel movie was its own edge list
# so it reads them in a glob and then merges into one data frame. Alter as necessary. 
setwd("C:/Users/Anne/Desktop/all_edges_nodes/marvel_edges")
marvel_edges <- data.frame(stringsAsFactors = FALSE)
tmp = list.files(pattern="*.csv")
marvel_dfe = lapply(tmp, read.csv)
for (df in marvel_dfe){
  marvel_edges<-rbind(marvel_edges, df)
}

# node list. same scenario. 
setwd("C:/Users/Anne/Desktop/all_edges_nodes/marvel")
marvel_chars <- data.frame()
tmp = list.files(pattern="*.csv")
marvel_df = lapply(tmp, read.csv)
for (df in marvel_df){
  marvel_chars<-rbind(marvel_chars, df)
}

# skip this for other data sets, but this is how you can exclude certain films from a multifilm franchise.
for (i in (1:length(marvel_dfe))){
  df <- marvel_dfe[[i]]
  df2 <- marvel_df[[i]]
  if (df2[1,]$TITLE != "Avengers: Infinity War"){
    marvel_edges<-rbind(marvel_edges, df)
  }
}

# run this INSTEAD of the above for marvel_edges if you want to add that factor.
# TITLE is a factor in the character metadata csvs. 
for (i in (1:length(marvel_dfe))){
  df <- marvel_dfe[[i]]
  df2 <- marvel_df[[i]]
  df <- cbind(df, "movie"=as.character(df2$TITLE[1]))
  marvel_edges<-rbind(marvel_edges, df)
}

# 20 = # of colors in the range to generate.
colpal <- colorRampPalette(brewer.pal(11,"BrBG"))(20)

# hard coded because i was too lazy to figure out why the auto encoded version wasn't working :^/
marvel_edges$color[marvel_edges$movie =="Ant-Man"] <- colpal[1]
marvel_edges$color[marvel_edges$movie =="Ant-Man and the Wasp"] <- colpal[2]
marvel_edges$color[marvel_edges$movie =="Avengers: Age of Ultron"] <- colpal[3]
marvel_edges$color[marvel_edges$movie =="Avengers: Infinity War"] <- colpal[4]
marvel_edges$color[marvel_edges$movie =="Black Panther"] <- colpal[5]
marvel_edges$color[marvel_edges$movie =="Captain America: Civil War"] <- colpal[6]
marvel_edges$color[marvel_edges$movie =="Captain America: The First Avenger"] <- colpal[7]
marvel_edges$color[marvel_edges$movie =="Captain America: The Winter Soldier"] <- colpal[8]
marvel_edges$color[marvel_edges$movie =="Doctor Strange"] <- colpal[9]
marvel_edges$color[marvel_edges$movie =="Guardians of the Galaxy"] <- colpal[10]
marvel_edges$color[marvel_edges$movie =="Guardians of the Galaxy Vol. 2"] <- colpal[11]
marvel_edges$color[marvel_edges$movie =="Iron Man"] <- colpal[12]
marvel_edges$color[marvel_edges$movie =="Iron Man 2"] <- colpal[13]
marvel_edges$color[marvel_edges$movie =="Iron Man 3"] <- colpal[14]
marvel_edges$color[marvel_edges$movie =="Spider-Man: Homecoming"] <- colpal[15]
marvel_edges$color[marvel_edges$movie =="The Avengers"] <- colpal[16]
marvel_edges$color[marvel_edges$movie =="The Incredible Hulk"] <- colpal[17]
marvel_edges$color[marvel_edges$movie =="Thor"] <- colpal[18]
marvel_edges$color[marvel_edges$movie =="Thor: Ragnarok"] <- colpal[19]
marvel_edges$color[marvel_edges$movie =="Thor: The Dark World"] <- colpal[20]


# this is important for another script. not relevant to graphics. 
all_mcu <- data.frame()
for (i in (1: length(marvel_df))){
  all_mcu <- rbind(all_mcu, marvel_df[[i]])
}
dups <- all_mcu[duplicated(all_mcu$CHARACTER),]
all_mcu<-all_mcu[!duplicated(all_mcu$CHARACTER),]


marvel_nodes <- data.frame(mega_data$CHARACTER)

g_all<-graph.data.frame(marvel_edges, directed = FALSE, vertices = NULL)
marvel_g <- graph.data.frame(marvel_edges, directed = FALSE, vertices = NULL) # here's the important one. all viz at bottom.
g2 <- graph.adjacency(get.adjacency(g), mode=c("undirected"), weighted=TRUE)

V(g)$label <- as.character(all_mcu$CHARACTER)

length(V(g))
length(V(g2))

length(E(g))
length(E(g2))

sort(degree(g), decreasing = T)[1:21]

sort(degree(g2), decreasing = T)[1:36]

evc <- evcent(g, directed=FALSE)
sort(evc$vector, TRUE)
sort(evcent(g2)$vector, TRUE)[1:36]


sort(closeness(g, vids=V(g), mode = c("all"), weights= NULL, normalized = FALSE), TRUE)[1:10]

graph.strength(g2, vids=V(g2), mode = c("all"))
sort(graph.strength(g2, vids=V(g2), mode = c("all")), TRUE)[1:10]


diameter(g2, directed = FALSE, weights= E(g2)$weight)
farthest.nodes(g2, weights=E(g2)$weight)

#percentage of length 2 paths: how many closed loops there are 
transitivity(g, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

# how interconnected graph is out of all possible connects
edge_density(g)

#assortativity
#this measures how often nodes from the same group are connected to each other

#first give attributes to your nodes and edges
char.list<-sort(as_ids(V(g)), decreasing = F)
nodes.all<-mega_data[order(mega_data$CHARACTER),]
V(g)$gender<-nodes.all$GENDER
V(g2)$gender<-nodes.all$GENDER

#calculate assortativity
assortativity.nominal(g, as.factor(V(g)$gender), directed=FALSE)

#what if we condition on the top 20 characters
deg<-names(sort(degree(g),TRUE))
deg.top20<-deg[1:20]
deg.top36 <- deg[1:36]
deg.remove<-deg[21:length(deg)]
g.reduced<-delete.vertices(g, v=deg.remove)
char.list<-sort(as_ids(V(g.reduced)), decreasing = F)
nodes.red<-nodes.all[nodes.all$CHARACTER %in% char.list,]
nodes.red<-nodes.red[order(nodes.red$CHARACTER),]
V(g.reduced)$gender<-nodes.red$GENDER
assortativity.nominal(g.reduced, as.factor(V(g.reduced)$gender), directed=FALSE)


## good
char.list<-sort(as_ids(V(g)), decreasing = F)
nodes.all<-mega_data[order(mega_data$ALIGNMENT),]
V(g)$alignment<-nodes.all$ALIGNMENT
V(g2)$alignment<-nodes.all$ALiGNMENT


#Community Detection
#Here are two ways of identifying communities in your network

#Louvain Clusters (maximizes modularity)
comm1<-cluster_louvain(g, weights = E(g)$weight)

#find modularity score
max(comm1$modularity)
#find who is in what community
comm1.df<-data.frame(comm1$names, comm1$membership)
c1 <- comm1.df[comm1.df$comm1.membership==1,] # ant man
nrow(c1) # 37
c2 <- comm1.df[comm1.df$comm1.membership==2,] # Guardians
nrow(c2) #53
c3 <- comm1.df[comm1.df$comm1.membership==3,] # Thor
nrow(c3) #63
c4 <- comm1.df[comm1.df$comm1.membership==4,] # Captain America + Hulk, Hawkeye, Black Widow, misc. avengers

nrow(c4)#64
c5 <- comm1.df[comm1.df$comm1.membership==5,] # Iron Man
nrow(c5) # 66
c6 <- comm1.df[comm1.df$comm1.membership==6,] # spider man
nrow(c6) # 33
c7 <- comm1.df[comm1.df$comm1.membership==7,] # Black panther + bonus cast of hulk ????
nrow(c7) #39
#leading eigenvector method
comm2<-cluster_leading_eigen(g)
max(comm2$modularity)
comm2.df<-data.frame(comm2$names, comm2$membership)


#calculate assortativity
assortativity.nominal(g, as.factor(V(g)$alignment), directed=FALSE)

robustness_vector_marvel<-vector()
for (j in 1:1000){
  random.nodes<-sample(V(g), length(V(g)), replace = FALSE, prob= NULL)
  g.robust<-as.undirected(g)
  for (i in 1:length(V(g))) {
    g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(g.robust)==FALSE)
      break
  }
  robustness_vector_marvel<-append(robustness_vector_marvel, i/length(V(g)))
}

# vulnerability
deg<-names(sort(degree(g), TRUE))
g.vulnerable<-as.undirected(g)
for (i in 1:length(V(g))){
  g.vulnerable<-delete.vertices(g.vulnerable, v=deg[[i]])
  if (is.connected(g.vulnerable)==FALSE)
    break
}
vuln <- i
print(vuln)
vuln/length(V(g))

# this is the nice one. 
plot.igraph(marvel_g, layout=layout_with_fr, edge.width=E(marvel_g)$weight, vertex.size=degree(marvel_g)/10,
            vertex.color=adjustcolor("bisque",alpha.f=1),vertex.label.color="black", vertex.frame.color="white",
            edge.color = E(marvel_g)$color, vertex.label=NA)

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color="white",vertex.label.color="black", vertex.label = ifelse(degree(g) > 150, V(g)$label, NA),
            vertex.frame.color="white")




edge1 <- read.csv("edge1.csv")
edge1<- edge1[,2:3]
g3 <- graph.data.frame(edge1, directed = FALSE, vertices = NULL)
plot.igraph(g3, layout=lay, edge.width=E(g)$weight, vertex.size=degree(g)/50,
            vertex.color="white",vertex.label.color="black", vertex.frame.color="white")


dates <- levels(mega_data$RELEASE_DATE)
dates <- as.integer(str_sub(dates, -2,-1))
hist(dates, breaks=20)
