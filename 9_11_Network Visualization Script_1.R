
#LOADING PACKAGES
library (igraph)
library (readr)
library (haven)
library (ggplot2)


C_Network <- read.csv('D:\\Documents\\Social Networks\\9_11_hijackers.csv')

D_Network <- read.csv('D:\\Documents\\Social Networks\\9_11_hijackers.csv')

#####################################################################
# Formatting the Data to Create an iGraph Style Edge List and Graph #
#####################################################################



#Creating an iGraph Style Edgelist
C_EdgeList <- C_Network
#Creating a Graph Obeject for Subsequent Analyses
  #Importantly, we define here whether the graph will be directed or not
C_Graph=graph.data.frame(C_EdgeList, directed=TRUE)

#Creating an iGraph Style Edgelist
D_EdgeList <- D_Network
#Creating a Graph Obeject for Subsequent Analyses
  #Importantly, we define here whether the graph will be directed or not
D_Graph=graph.data.frame(D_EdgeList, directed=TRUE)

#####################
#  Visualizations   #
#####################

#C Network: First Try
  
  #Layout Options
    set.seed(3952)  # set seed to make the layout reproducible
    layout1 <- layout.fruchterman.reingold(C_Graph) #Creating a layout object to tell iGraph what layout I want
    
  #Node or Vertex Options: Color
    V(C_Graph)$color <- "grey"
    V(C_Graph)[degree(C_Graph, mode="in")>8]$color <- "yellow"  #Destinguishing High Degree Nodes as yellow
    
  #Edge Options: Siz
    E(C_Graph)$color <- "grey"
    
#Plotting 
    plot(C_Graph)

#Verdict: Pretty Ugly, let's try to fix this.
    

    
    ##########################
    #  D Networks   #
    ##########################
    
    #Removing Self-Loops (Repondents Nominating Themselves)
    D_Graph2<-simplify(D_Graph, remove.multiple=TRUE, remove.loops=TRUE)
   
    #Layout Options
    set.seed(3952)  # set seed to make the layout reproducible
    layout1 <- layout.fruchterman.reingold(D_Graph2,niter=500)
    #Node or Vetex Options: Size and Color
    V(D_Graph2)$size=degree(D_Graph, mode = "in")/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
    #V(D_Graph2)$color <- ifelse(D_Attributes[V(D_Graph2), 2] == "Researcher", "blue", "red")
    
    #Edge Options: Color
    E(D_Graph2)$color <- "grey"
    
    #Plotting, Now Specifying an arrow size and getting rid of arrow heads
    #We are letting the color and the size of the node indicate the directed nature of the graph
    plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
  #Verdict, Maybe a different layout?
  
    

    
  #Verdict: Maybe a bit better, but maybe not??
    
#######################################################################
#  Thinking about the Network in Terms of Connectivity and Position   #
#######################################################################
    
   #CONNECTIVITY
    
    #Density
    graph.density(D_Graph2,loop=FALSE)
    graph.density(C_Graph2, loop=FALSE)
    
    #Average Path Length
    mean_distance(D_Graph2)
    mean_distance(C_Graph2)
    
    #Degree Distribution
    degree_distribution(D_Graph2)
    D_DegreeDis <- degree_distribution(D_Graph2)   #Turns this into a data object we can export
    
    D_DegreeDis2 <- as.data.frame(D_DegreeDis)
    
    qplot(D_DegreeDis, data=D_DegreeDis2, geom="histogram", binwidth=.001)
    
    #Clustering Coefficeint 
    transitivity(D_Graph2)
    transitivity(C_Graph2)
    
    D_Trans <- transitivity(D_Graph2)
    
    #POSITION
    
    #Degree: In, Out, All Centrality
    D_OutDegree <- degree(D_Graph2, mode = "out")
    D_OutDegree <- as.data.frame(D_OutDegree)
    
    
    degree(D_Graph2, mode = "in")
    D_InDegree <- degree(D_Graph2, mode = "in")
    D_InDegree <- as.data.frame(D_InDegree)
    
    #Layout Options
    set.seed(3952)  # set seed to make the layout reproducible
    layout1 <- layout.fruchterman.reingold(D_Graph2,niter=500)
    #Node or Vetex Options: Size and Color
    V(D_Graph2)$size=degree(D_Graph, mode = "all")/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
  #  V(D_Graph2)$color <- ifelse(D_Attributes[V(D_Graph2), 2] == "Researcher", "blue", "red")
    
    #Edge Options: Color
    E(D_Graph2)$color <- "grey"
    
    #Plotting, Now Specifying an arrow size and getting rid of arrow heads
    #We are letting the color and the size of the node indicate the directed nature of the graph
    plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
    #Closeness Centrality
    closeness(D_Graph2, mode="in")
    
    D_InCloseness <- closeness(D_Graph2, mode="in")
    D_InCloseness <- as.data.frame(D_InCloseness)
    
    
    D_OutCloseness <- closeness(D_Graph2, mode="out")
    D_OutCloseness <- as.data.frame(D_OutCloseness)
    
    closeness(D_Graph2, mode="out")
    
    closeness(D_Graph2, mode="all")
    
    #Layout Options
    set.seed(3952)  # set seed to make the layout reproducible
    layout1 <- layout.fruchterman.reingold(D_Graph2,niter=500)
    #Node or Vetex Options: Size and Color
    V(D_Graph2)$size=closeness(D_Graph, mode = "out")/.05#because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
  #  V(D_Graph2)$color <- ifelse(D_Attributes[V(D_Graph2), 2] == "Researcher", "blue", "red")
    
    #Edge Options: Color
    E(D_Graph2)$color <- "grey"
    
    #Plotting, Now Specifying an arrow size and getting rid of arrow heads
    #We are letting the color and the size of the node indicate the directed nature of the graph
    plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
    
    #Betweeness Centrality
    D_Betweeness <- betweenness(D_Graph2)
    D_Betweeness <- as.data.frame(D_Betweeness)
  
    
    #Layout Options
    set.seed(3952)  # set seed to make the layout reproducible
    layout1 <- layout.fruchterman.reingold(D_Graph2,niter=500)
    #Node or Vetex Options: Size and Color
    V(D_Graph2)$size=betweenness(D_Graph)/200 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
    V(D_Graph2)$color <- ifelse(D_Attributes[V(D_Graph2), 2] == "Researcher", "blue", "red")
    
    #Edge Options: Color
    E(D_Graph2)$color <- "grey"
    
    #Plotting, Now Specifying an arrow size and getting rid of arrow heads
    #We are letting the color and the size of the node indicate the directed nature of the graph
    plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
    #Eigen Vector Centrality
    
   eigen_centrality(D_Graph2)
   D_EigenCentrality <- eigen_centrality(D_Graph2)
   
   D_EigenCentrality <- as.data.frame(D_EigenCentrality)
   
   #Layout Options
   set.seed(3952)  # set seed to make the layout reproducible
   layout1 <- layout.fruchterman.reingold(D_Graph2,niter=500)
   #Node or Vetex Options: Size and Color
   V(D_Graph2)$size=eigen_centrality(D_Graph)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
   V(D_Graph2)$color <- ifelse(D_Attributes[V(D_Graph2), 2] == "Researcher", "blue", "red")
   
   #Edge Options: Color
   E(D_Graph2)$color <- "grey"
   
   #Plotting, Now Specifying an arrow size and getting rid of arrow heads
   #We are letting the color and the size of the node indicate the directed nature of the graph
   plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
    
######################################################
# Segue to Advanced Network Visualization Techniques #
######################################################

    
    GNC <- cluster_edge_betweenness(D_Graph2, weights = NULL)
    V(D_Graph2)$color <-membership(GNC)              #Plot setting specifying the coloring of vertices by community
    D_Graph2$palette <- diverging_pal(length(GNC))   #Plot setting specifying the color pallette I am using (iGraph supports 3)
    plot(D_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)
    
  #Verdict: This is one answer, probably not the best one, but more on that next time.
    
    
    