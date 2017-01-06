setwd("D:/Stevens/summer 2016/BIA 658/Final/airline-twitter-sentiment")

# install.packages("dplyr")
# install.packages("caret")
# install.packages("knitr")
# install.packages("plotrix")
# install.packages("igraph")

# LOAD PACKAGES:
library(plyr)
library(dplyr)
library(caret)
library(knitr)
library(ggplot2)
library(plotrix)
library(igraph)
library(RColorBrewer)
library(xtable)
library(wordcloud)

# DEFINE FUNCTIONS
# BUILD THE CROSS PRODUCT OF TWO VECTORS OF STRINGS
string_prod3 <- function(v_1,v_2){
  l_1 <- length(v_1)
  l_2 <- length(v_2)
  v_1 <- v_1[1:ceiling(l_1/5)]
  v_2 <- v_2[1:ceiling(l_2/5)]
  l_1 <- length(v_1)
  l_2 <- length(v_2)
  intersections <- 0
  for (i in 1:l_1){
    for (j in 1:l_2){
      idx <- as.numeric(nchar(Reduce(intersect, strsplit(c(v_1[i],v_2[j])," "))))
      intersections <- intersections + length(which(idx>2))
    }
  }
  return(intersections/(l_1+l_2))
}

# DEFINE EDGE-FILTRATION FUNCTION FOR THE NETWORKS
filter <- function(cutoff,edge_matrix,vertex_colors,vertex_names, vertex_size) {
  
  # get the definitions
  cut <- cutoff
  adj <- edge_matrix
  adj[adj<cut] <- 0
  adj_0 <- adj
  
  # define the filtered graph
  g <- graph.adjacency(adj_0,mode="undirected",weighted=TRUE)
  V(g)$color <- vertex_colors
  
  # filter to degree > 0 eliminate isolated vertices
  g_f <- delete.vertices(g,V(g)[degree(g)==0])
  v_g_f <- setdiff(V(g),V(g)[degree(g)==0])
  V(g_f)$name <- vertex_names[v_g_f]
  V(g_f)$color <- vertex_colors[v_g_f]
  V(g_f)$size <- vertex_size[v_g_f]
  
  return(g_f)
}

data <- read.csv("tweets.csv", header = TRUE, sep =",")
data <- data[data$name %in% c("JetBlueNews","kbosspotter","_mhertz","throthra","rossj987","weezerandburnie",
                                  "MeeestarCoke","GREATNESSEOA","scoobydoo9749","jasemccarty","georgetietjen","ElmiraBudMan",
                                  "flemmingerin","Aero0729","ThatJasonEaton","thomashoward88","chagaga2013","SMHillman",
                                  "worldwideweg","patrick_maness","heyheyman","arthurhasher","luvthispayne","Allisonjones704",
                                  "Evan_Flay","ColtSTaylor","Heavenlychc9","davidgoodson71","geekstiel","BernardLeCroix",
                                  "riricesq","farfalla818","urno12","DontenPhoto","alanbestnyc","Old_bauer",
                                  "AlMehairiAUH","gwen1013","JackieFerrari3","emptynester25","CLChicosky","lj_verde",
                                  "tannapistolis","DAngel082","TomFutureforall","Whosgunz","c4pyro","BritishAirNews",
                                  "davisesq212","MerrickRealtor","Trufflebaby2","JenRomanoff","smaguire2","MeganStephens74",
                                  "LindsaySweeting","nickpasculli","jacquelinewins6","HaileyUrban"),]

data$name <- as.character(data$name)
data$text <- as.character(data$text)

names <- unique(data$name)
# ids <- unique(data[,c("name","name")])
data <- data[,c("name","text")]
edgem <- matrix(0,nrow = length(names), ncol = length(names))
for (i in 1:(length(names)-1)){
  for (j in (i+1):length(names)){
    text_i <- data[data[,1] %in% names[i],2]
    text_j <- data[data[,1] %in% names[j],2]
    edgem[i,j] <- string_prod3(text_i,text_j)
  }
}

# BUILD THE SYMMETRIC TABLE
for (i in 1:(length(names)-1)){
  for (j in (i+1):length(names)){
    edgem[j,i] <- edgem[i,j]
  }
}

# SET THE MAIN DIAGONAL TO 0, NO SELF-INTERSECTIONS.
for (k in 1:length(names)){
  edgem[k,k] <- 0
}

# BUILD THE ISIS TWEET NETWORK
g <- graph.adjacency(adjmatrix = edgem, 
                     mode = "undirected",
                     weighted = TRUE)
V(g)$color <- rep("SkyBlue2",length(V(g)))
V(g)$size <- graph.strength(g)
V(g)$name <- as.character(names)

# ISIS TWEET ASSOCIATION NETWORK, FILTERED BY ?% PERCENTILE
cut50 <- quantile(as.vector(edgem),0.90)
g_f <- filter(cutoff = cut50,
              edge_matrix = edgem,
              vertex_colors = V(g)$color,
              vertex_names = V(g)$name,
              vertex_size = V(g)$size)

# FITLER TO THE GIANT COMPONENT
# g_f <- giant_comp(graph = g_f,
#                   vertex_colors = V(g_f)$color,
#                   vertex_names = V(g_f)$name,
#                   vertex_size = V(g_f)$size)

# ADJUST THE NODE SIZE
V(g_f)$size <- 0.04*(graph.strength(g_f))

# FILTER LABELS TO THE HIGHEST WEIGHTED DEGREE NODES
for (k in 1:vcount(g_f)){
  if (graph.strength(g_f)[k]<quantile(graph.strength(g_f),0.8)){
    V(g_f)$name[k] <- NA
  }
}

# PLOT THE NETWORK
plot(g_f,
     layout = layout.sphere(g_f),
     vertex.color = V(g_f)$color,
     vertex.size = V(g_f)$size,
     vertex.label = V(g_f)$name, 
     vertex.label.color = "red", 
     vertex.label.font = 2, 
     vertex.label.cex = 1.2,
     vertex.label.dist = 0.5,
     edge.width = 0.07*(E(g_f)$weight),
     edge.curved = TRUE,
     edge.color = gray.colors(1),
     main = "WEIGHTED TWEET ASSOCIATION NETWORK WITH 90.0% FILTRATION")
legend("topleft",
       "TWEETER NAME",
       fill = "SkyBlue2",
       bty = "n")

