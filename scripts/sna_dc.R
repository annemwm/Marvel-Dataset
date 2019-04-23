library("igraph")
library("splitstackshape")

setwd("C:/Users/Anne/Desktop/all_edges_nodes/dc_edges")
dc_edges <- data.frame(stringsAsFactors = FALSE)

tmp = list.files(pattern="*.csv")
dc_dfe = lapply(tmp, read.csv)
for (df in dc_dfe){
  dc_edges<-rbind(dc_edges, df)
}

setwd("C:/Users/Anne/Desktop/all_edges_nodes/dc")
dc_chars <- data.frame()
tmp = list.files(pattern="*.csv")
dc_df = lapply(tmp, read.csv)
for (df in dc_df){
  dc_chars<-rbind(dc_chars, df)
}

for (i in (1:length(dc_dfe))){
  df <- dc_dfe[[i]]
  df2 <- dc_df[[i]]
  df <- cbind(df, "movie"=as.character(df2$TITLE[1]))
  dc_edges<-rbind(dc_edges, df)
}

colpal <- colorRampPalette(brewer.pal(11,"BrBG"))(10)

dc_edges$color[dc_edges$movie =="Aquaman"] <- colpal[1]
dc_edges$color[dc_edges$movie =="Batman Begins"] <- colpal[2]
dc_edges$color[dc_edges$movie =="Batman v Superman: Dawn of Justice"] <- colpal[3]
dc_edges$color[dc_edges$movie =="Green Lantern"] <- colpal[4]
dc_edges$color[dc_edges$movie =="Justice League"] <- colpal[5]
dc_edges$color[dc_edges$movie =="Man of Steel"] <- colpal[6]
dc_edges$color[dc_edges$movie =="Suicide Squad"] <- colpal[7]
dc_edges$color[dc_edges$movie =="The Dark Knight"] <- colpal[8]
dc_edges$color[dc_edges$movie =="The Dark Knight Rises"] <- colpal[9]
dc_edges$color[dc_edges$movie =="Wonder Woman"] <- colpal[10]



#g_all<-graph.data.frame(marvel_edges, directed = FALSE, vertices = NULL)
dc_g <- graph.data.frame(dc_edges, directed = FALSE, vertices = NULL)
g2 <- graph.adjacency(get.adjacency(g), mode=c("undirected"), weighted=TRUE)

all_dc <- data.frame()
for (i in (1: length(dc_df))){
  all_dc <- rbind(all_dc, dc_df[[i]])
}
dups <- all_dc[duplicated(all_dc$CHARACTER),]
all_dc<-all_dc[!duplicated(all_dc$CHARACTER),]

V(g)$label <- as.character(all_dc$CHARACTER)

length(V(g))
length(V(g2))

length(E(g))
length(E(g2))

sort(degree(g), decreasing = T)[1:19]

sort(degree(g2), decreasing = T)[1:36]

evc <- evcent(g, directed=FALSE)
sort(evc$vector, TRUE)[1:20]
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

dc_robustness_vector<-vector()
for (j in 1:1000){
  random.nodes<-sample(V(g), length(V(g)), replace = FALSE, prob= NULL)
  g.robust<-as.undirected(g)
  for (i in 1:length(V(g))) {
    g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(g.robust)==FALSE)
      break
  }
  dc_robustness_vector<-append(dc_robustness_vector, i/length(V(g)))
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

plot.igraph(dc_g, layout=layout_with_fr, edge.width=E(dc_g)$weight, vertex.size=degree(dc_g)/10,
            vertex.color=adjustcolor("bisque",alpha.f=1),vertex.frame.color="white",
            edge.color = E(dc_g)$color, vertex.label = V(dc_g)$label)

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color="white",vertex.label.color="black", vertex.label = ifelse(degree(g) > 82, V(g)$label, NA),
            vertex.frame.color="white")



cluster <- clusters(g)
gs <- induced.subgraph(g, cluster$membership==order(-cluster$csize)[1])

edge1 <- read.csv("edge1.csv")
edge1<- edge1[,2:3]
g3 <- graph.data.frame(edge1, directed = FALSE, vertices = NULL)
plot.igraph(g3, layout=lay, edge.width=E(g)$weight, vertex.size=degree(g)/50,
            vertex.color="white",vertex.label.color="black", vertex.frame.color="white")

dates <- levels(mega_data$RELEASE_DATE)
dates <- as.integer(str_sub(dates, -2,-1))
hist(dates, breaks=20)