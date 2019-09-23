library(arules)
library(arulesViz)
library(data.table)

fileName = "tbl_cafe"
groceries = read.transactions(paste0(fileName, ".csv"), sep=',', rm.duplicates = T)
itemFrequency(groceries)
# itemFrequencyPlot(groceries,support = 0.05, main = "Item Frequency with S = 0.1",ylab = "Relative Frequency")
rule = apriori(groceries, parameter = list(support = 0.006,confidence = 0.3))
rule1 <- rule[!is.redundant(rule)]
# summary(rule1)
  
#Sort
ruleSort_byLift = sort(rule1, by = "lift")
ruleSort_byCount = sort(ruleSort_byLift, by = "count")

###########################################################################
recordList = inspect(ruleSort_byCount)
df <- as.data.frame(recordList)
df <- df[df$lift > 1.0000001,]
df[c('lhs', 'rhs')] <- apply(df[c('lhs', 'rhs')], 2, function(x)
  substr(x, start=2,stop=nchar(x)-1))

df1 = df[!grepl(',', df[,1]),] # rm lhs containing 2 item
a = unique(df1[,1])
b = unique(df1[,3])
c = append(a,b)
item <- c[!duplicated(c)]
x = matrix(nrow = length(item), ncol = length(item))
rownames(x) = item
colnames(x) = item
x = as.data.frame(x)

ii = 1
jj = 1
for (i in 1:nrow(x)){
  for (j in 1:ncol(x)){
    if(item[i] == item[j]){
      x[i,j] <- 0
    }else if (length(df1[(df1$lhs == item[i] & df1$rhs == item[j]), 7]) == 0)
      x[i,j] <- 0
    else
      x[i,j] <- df1[(df1$lhs == item[i] & df1$rhs == item[j]), 7]
  }
}

for (i in 1:ncol(x)){
  for (j in 1:nrow(x)){
    if (length(df1[(df1$lhs == item[i] & df1$rhs == item[j]), 7]) == 0)
      next
    else
      x[j,i] <- df1[(df1$lhs == item[i] & df1$rhs == item[j]), 7]
  }
}

# g <- graph.adjacency(x, weighted=T, mode = "undirected")
# g <- simplify(g)
# V(g)$label <- V(g)$name
# V(g)$degree <- degree(g)
# V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
# V(g)$label.color <- rgb(0, 0, .2, .8)
# V(g)$frame.color <- NA
# egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
# E(g)$color <- rgb(.5, .5, 0, egam)
# E(g)$width <- egam
# # plot the graph in layout1
# plot(g, layout=layout1)


library(igraph)
library(networkD3)

source <- df1$lhs
target <- df1$rhs
networkData <- data.frame(source, target)
simpleNetwork(networkData,
              height = 480,                     # height of frame area in pixels
              width = 480,
              linkDistance = 120,               # distance between node. Increase this value to have more space between nodes
              charge = -480,                    # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 27,                    # size of the node names
              linkColour = "black",             # colour of edges, MUST be a common colour for the whole graph
              nodeColour = "red",             # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.8,                    # opacity of nodes. 0=transparent. 1=no transparency
              zoom = TRUE,
              fontFamily = "sans-serif")


# x[x>=1] <- 1
# x <- as.matrix(x)
# network=graph_from_adjacency_matrix(x)

# transform Igraph format in something readable by networkD3
# network=igraph_to_networkD3(network)

# plot
# simpleNetwork(network,
#               height = 480,                     # height of frame area in pixels
#               width = 480,
#               linkDistance = 120,               # distance between node. Increase this value to have more space between nodes
#               charge = -480,                    # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
#               fontSize = 27,                    # size of the node names
#               linkColour = rgb(0.1,0.9,0.1,0.3),# colour of edges, MUST be a common colour for the whole graph
#               nodeColour = "forestgreen",       # colour of nodes, MUST be a common colour for the whole graph
#               opacity = 0.9                   # opacity of nodes. 0=transparent. 1=no transparency
# )




# 
# library(igraph)
# library(visNetwork)
# m=as.matrix(x)
# net=graph.adjacency(m ,mode="undirected", weighted=TRUE,diag=FALSE)
# summary(net)
# plot.igraph(net,vertex.label=V(net)$name,
#             layout=layout.fruchterman.reingold, 
#             vertex.label.color="black",
#             edge.color="black",
#             edge.width=E(net)$weight/15, 
#             edge.arrow.size=1.5)
# 
# links=data.frame(
#   source=df1$lhs,
#   target=df1$rhs,
#   importance=(sample(1:2, nrow(df1), replace=T))
# )
# 
# nodes=data.frame(
#   name=item
# )
# network=graph_from_data_frame(d=links, vertices=nodes, directed=F) 
# par(bg="grey32", mar=c(0,0,0,0))
# plot(network)
# 
# plot(network, layout=layout.random, main="random")
# 
# 
# ###########################################################################  
# rules_dt <- data.table( lhs = labels( lhs(rule1) ),  rhs = labels( rhs(rule1) ), quality(rule1) )[ order(-lift), ]
# write.csv(rules_dt, file = paste0("rule_", fileName, ".csv"), row.names = FALSE) # output rule file (by Count)
# 
# p <- plot(rule1, method="graph",
#           measure = "confidence",
#           nodeCol = "#F0F0F0",
#           itemCol	= "#EE9B9B",
#           control=list(verbose = TRUE),
#           max = nrow(rules_dt),
#           engine = "html",
#           igraphLayout = "layout_with_lgl")
#   
#   
#   p$x$options$width <- "200%"
#   p$x$options$height <- "200%"
#   p$x$idselection$main <- "請選擇餐點"
#   p$x$idselection$style <- "width: 300px; height: 50px"
#   p$x$options$nodes$shape <- ""
#   # p$x$options$manipulation$enabled <- TRUE # 可以修改 Node 跟 Edge
#   # p$x$options$physics$stabilization <- TRUE
#   
#   
#   p$x$nodes
#   pChar <- as.list.data.frame(p)
#   
# 
#   htmlwidgets::saveWidget(p, paste0("~/public_html/", fileName,"_rules.html"), selfcontained = FALSE)
# 

  
  
  
  
  # plot(rule1, method="grouped matrix", engine = "interactive")
  
  # subrules2 <- head(sort(rule1, by="confidence"),20)
  # ig <- plot( subrules2, method="graph")
  # ig_df <- as.list.data.frame( ig, what = "both" )
  
  
  # w <- visNetwork(p$x$nodes, p$x$edges)%>%
  #     visLegend() %>%
  #     visOptions(selectedBy = "group") %>%
  #     visEdges(arrows = "from") %>%
  #     visInteraction(navigationButtons = TRUE) %>%
  #     visEdges(smooth = FALSE) %>%
  #     visGroups(groupname = "item", color = "#EE9B9B", shape = "square", 
  #               shadow = list(enabled = FALSE), square = list(size = 75)) %>%
  #     visGroups(groupname = "rule", color = "#F0F0F0", shape = "circle", 
  #               shadow = list(enabled = FALSE), circle = list(size = 200)) %>%
  #     addFontAwesome() %>%
  #     visNodes(shape = "square", title = "I'm a node", borderWidth = 1)
  #   visConfigure(enabled = TRUE)
  # visHierarchicalLayout(direction = "LR", levelSeparation = 50)
  
  