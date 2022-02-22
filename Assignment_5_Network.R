library(multiplex)
library(igraph)

#1 A basic plot of the network
AN <- read.gml('adjnoun.gml')

set.seed(824)
AN_graph <- graph_from_data_frame(d = AN, directed = FALSE)
plot(AN_graph)

plot(AN_graph, edge.arrow.size = 10, edge.curved=.1,
     vertex.size=7, vertex.label.cex=0.6)

#2 The 5 nodes with the highest degrees, and a histogram for the degrees in the network
deg <- degree(AN_graph)
deg
hist(deg, xlab='Degree', main='Histogram of Degree', col='lightblue', ylim=c(0,50))
plot(deg, xlab='Degree', main='Scattorplot of Degree')

deg1 <- degree(AN_graph, mode = "all")
sort(deg1, decreasing = T)[1:5]

#3 The 5 nodes with the highest betweenness centrality and eigenvector centrality scores
eigenCent <- evcent(AN_graph)$vector
sort(eigenCent,decreasing=TRUE)[1:5]

betweenCent <- betweenness(AN_graph)
sort(betweenCent,decreasing=TRUE)[1:5]

#4 The correlation of betweenness centrality to eigenvector centrality.
cor(betweenCent,eigenCent)
betweenCent
eigenCent

#5 A plot with all “gatekeeper vertices” marked in red
colorVals <- rep("white", length(betweenCent))
colorVals[which(eigenCent < 0.36 & betweenCent > 100)] <- "red"
V(AN_graph)$color <- colorVals
plot.igraph(AN_graph, vertex.label=NA, vertex.size=5)
plot.igraph(AN_graph, vertex.label.cex=0.5, vertex.size=5)

#6 Two plots showing subsets of the data: one for a subset centered on a single node, and one including more than one central node
target <- c('little')
AN_subset_rownumbers <- 
  which((AN$S %in% target) | (AN$R %in% target))
AN_subset_nodes <- AN$S[AN_subset_rownumbers]
AN_subgraph <- induced.subgraph(AN_graph, AN_subset_nodes)

plot(AN_subgraph, edge.arrow.size = 0.2, 
     vertex.size=5, vertex.label.cex=0.5, 
     vertex.color="white", vertex.frame.color="light gray",
     main=paste("All words connected to", target, sep=" ") )

mtarget <- c('little', 'man')
AN_subset_rownumbers <- 
  which((AN$S %in% mtarget) | (AN$R %in% mtarget))
AN_subset_nodes <- AN$S[AN_subset_rownumbers]
AN_subgraph <- induced.subgraph(AN_graph, AN_subset_nodes)

plot(AN_subgraph, edge.arrow.size = 0.2, 
     vertex.size=5, vertex.label.cex=0.5, 
     vertex.color="white", vertex.frame.color="light gray",
     main=paste("All words connected to", mtarget, sep=" ") )

#7 A plot of one neighborhood
gn <- graph.neighborhood(AN_graph, order = 1)
head(gn)

plot(gn[[14]],
     edge.arrow.size = 0.2, vertex.size=(degree(gn[[71]])/2), vertex.label.cex=0.5, 
     vertex.color="white", vertex.frame.color="light gray")

#8 A plot with all communities of greater than 3 members (or a reasonable number for your dataset) color-coded
w <- edge.betweenness.community(AN_graph)
sort(table(w$membership))

V(AN_graph)$color <- rep("white", length(w$membership))
keepTheseCommunities <- names(sizes(w))[sizes(w) > 3]
matchIndex <- match(w$membership, keepTheseCommunities) 
colorVals <- rainbow(6)[matchIndex[!is.na(matchIndex)]]
V(AN_graph)$color[!is.na(matchIndex)] <- colorVals

plot(AN_graph, edge.arrow.size = 0.2, 
     vertex.size=5, vertex.label.cex=0.3, 
     vertex.frame.color="light gray")

#9 A plot of a subgraph with all communities of greater than 3 members (or a reasonable number for your dataset) color-coded
w2 <- edge.betweenness.community(AN_subgraph)
w2total <- nrow(sizes(w2))
w2total_large <- nrow(sizes(w2)[sizes(w2) > 3] == "TRUE")

plot(AN_subgraph, edge.arrow.size = 0.2, 
     vertex.size=5, vertex.label.cex=0.5, 
     vertex.frame.color="light gray",
     main=paste("All characters words to", mtarget, "\n\n",
                w2total_large, "Large Communities,", w2total, "Total", sep=" ") )

