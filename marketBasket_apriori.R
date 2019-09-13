library(arules)
library(arulesViz)
# library(GREP)

# connected to mySQL
library(DBI)
library(gWidgets)
library(visNetwork)
library(RODBC)  
library(dbConnect)  
library(RMySQL) 

connect = dbConnect(MySQL(), 
                    dbname = "basket_data",
                    username = "bellalien", 
                    password = "", 
                    host = "", 
                    encoding = "utf8"
                    )

out_connect = dbConnect(MySQL(), 
                    dbname = "basket_recommend",
                    username = "bellalien", 
                    password = "", 
                    host = "", 
                    encoding = "utf8"
)

dbListTables(connect)
dbSendQuery(connect,"set names utf8")
# data_original = dbGetQuery(connect ,"select * from tbl_cafe_original")
# data = dbGetQuery(connect ,"select * from tbl_cafe")
tables<-dbListTables(connect)


for (i in 1:length(tables)){
  # temp<-(dbReadTable(connect, tables[i]))
  # temp[is.na(temp)] <- NULL
  # write.table(temp, row.names=F)
  intputfileName <- paste0(tables[i],".csv")
  basket = dbReadTable(connect, tables[i])
  write.csv(basket, file = intputfileName, row.names = FALSE) # input file
  
  groceries = read.transactions(intputfileName, sep=',', rm.duplicates = T)
  
  itemFrequency(groceries)
  # itemFrequencyPlot(groceries,support = 0.05, main = "Item Frequency with S = 0.1",ylab = "Relative Frequency")
  rule = apriori(groceries, parameter = list(support = 0.006,confidence = 0.3))
  rule1 <- rule[!is.redundant(rule)]
  # summary(rule1)
  
  #Sort
  ruleSort_byLift = sort(rule1, by = "lift")
  ruleSort_byCount = sort(ruleSort_byLift, by = "count")
  # recordList = inspect(ruleSort_byCount)
  # df <- as.data.frame(recordList)
  # df <- df[df$lift > 1.0000001,]
  # df[c('lhs', 'rhs')] <- apply(df[c('lhs', 'rhs')], 2, function(x) 
  #   substr(x, start=2,stop=nchar(x)-1))
  
  rules_dt <- data.table( lhs = labels( lhs(rule1) ),  rhs = labels( rhs(rule1) ), quality(rule1) )[ order(-lift), ]
  
  write.csv(rules_dt, file = paste0("rule_", tables[i], ".csv"), row.names = FALSE) # output rule file (by Count)

  # library("RColorBrewer")
  # itemFrequencyPlot(groceries,
  #                           topN=20,
  #                           col=brewer.pal(8,'Pastel2'),
  #                           main='商品購買頻率統計',
  #                           type="relative",
  #                           ylab="商品購買頻率統計 (相關性)")
  # plotly_arules(rule1)
  
  # rules的lift & support
  # sel <- plot(rule1, measure=c("support", "lift"),
  #             shading = "confidence",
  #             interactive = TRUE)
  
  
  # dbWriteTable(out_connect, paste0(tables[i],"_recommend"), df)
  # dbListTables(out_connect)

  p <- plot(rule1, method="graph",
            measure = "confidence",
            nodeCol = "#F0F0F0",
            itemCol	= "#EE9B9B",
            control=list(verbose = TRUE),
            max = nrow(rules_dt),
            engine = "html",
            igraphLayout = "layout_with_lgl")
  
  plot(rule1, method="grouped matrix", engine = "interactive")
  
  subrules2 <- head(sort(rule1, by="confidence"),20)
  ig <- plot( subrules2, method="graph")
  ig_df <- as.list.data.frame( ig, what = "both" )
  
  
  w <- visNetwork(p$x$nodes, p$x$edges)%>%
    visLegend() %>%
    visOptions(selectedBy = "group") %>%
    visEdges(arrows = "from") %>%
    visInteraction(navigationButtons = TRUE) %>%
    visEdges(smooth = FALSE) %>%
    visGroups(groupname = "item", color = "#EE9B9B", shape = "square", 
              shadow = list(enabled = FALSE), square = list(size = 75)) %>%
    visGroups(groupname = "rule", color = "#F0F0F0", shape = "circle", 
              shadow = list(enabled = FALSE), circle = list(size = 200)) %>%
    addFontAwesome() %>%
    visNodes(shape = "square", title = "I'm a node", borderWidth = 1)
  #   visConfigure(enabled = TRUE)
    # visHierarchicalLayout(direction = "LR", levelSeparation = 50)
    
    
  
  p$x$options$width <- "200%"
  p$x$options$height <- "200%"
  p$x$idselection$main <- "請選擇餐點"
  p$x$idselection$style <- "width: 300px; height: 50px"
  p$x$options$nodes$shape <- ""
  # p$x$options$manipulation$enabled <- TRUE # 可以修改 Node 跟 Edge
  # p$x$options$physics$stabilization <- TRUE

  
  p$x$nodes
  pChar <- as.list.data.frame(p)
  
  # 
  # p <- plot(ruleSort_byCount, method="graph",
  #           measure = "confidence",
  #           control = list(type="items"),
  #           max = 5,
  #           engine = "html",
  #           igraphLayout = "layout_in_circle")
  htmlwidgets::saveWidget(p, paste0("~/public_html/",tables[i],"_rules.html"), selfcontained = FALSE)
  # browseURL(paste0(tables[i],"_rules.html"))
}

all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

# groceries = read.transactions(tables, sep=' ', rm.duplicates = T)
# itemFrequency(groceries)
# 
# #itemFrequencyPlot(groceries,topN = 10)
# #itemFrequencyPlot(groceries,topN = 10,type = "absolute")
# #itemFrequencyPlot(groceries,topN = 10,horiz = T, main = "Item Frequency",xlab = "Relative Frequency")
# itemFrequencyPlot(groceries,support = 0.05, main = "Item Frequency with S = 0.1",ylab = "Relative Frequency")
# 
# #Apriori Algorithm 
# rule1 = apriori(groceries,parameter = list(support = 0.006,confidence = 0.3))
# summary(rule1)
# 
# # # Show 1st~20th (lift=1.5 > 1，表示了這個規則相當具有正相關)
# # inspect(rule1[1:20])
# # test = inspect(head(sort(rule1, by="count"), 100))
# 
# #Sort
# ruleSort_byLift = sort(rule1, by = "lift")
# ruleSort_byCount = sort(ruleSort_byLift, by = "count")
# recordList = inspect(ruleSort_byCount)
# df <- as.data.frame(recordList)

# df[c('lhs', 'rhs')] <- apply(df[c('lhs', 'rhs')], 2, function(x)
#   substr(x, start=2,stop=nchar(x)-1))
# df
# 
# dbWriteTable(connect,"tbl_cafe_recommend", df)
# dbListTables(connect)
# 
# w <- readline()
# recommend <- df[which(df['lhs']==w),]
# recommend[,3:7]
# recommend['rhs']




# subset.matrix=is.subset(ruleSort_byCount,ruleSort_byCount)
# subset.matrix
# redundant=colSums(subset.matrix) > 1
# which(redundant)
#   rulepruned=ruleSort_byCount[redundant]
# rulepruned=ruleSort_byCount[!redundant]
# inspect(rulepruned)

# Heat map (熱圖)
# plot(ruleSort_byCount)




# #Balloon plot (氣球圖)
# # tmp = asS4(recordList)
# plot(ruleSort_byCount, method="grouped")
# 
# 
# plot(ruleSort_byCount, method="paracoord",control = list(reorder=TRUE))

# Graph (網路圖)
# ptmp <- plot(ruleSort_byCount, method="graph",
#      measure = "confidence",
#      control = list(type="items"),
#      max = 5,
#      engine = "htmlwidget",
#      igraphLayout = "layout_in_circle")
# 
# p <- plot(ruleSort_byCount, method="graph",
#           measure = "confidence",
#           control = list(type="items"),
#           max = 5,
#           engine = "html",
#           igraphLayout = "layout_in_circle")
# htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
# browseURL("arules.html")
# plotl(ruleSort_byCount)
# plot(ruleSort_byCount, method="grouped matrix", measure = "confidence", control = list(type="items"), max = 5, engine = "interactive")

# Parallel coordinates plot (平行座標圖)
# plot(rulepruned, method = "paracoord", control = list(reorder = TRUE))

