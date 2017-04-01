

library(networkD3)
library(movieNetwork)

# data(MisLinks)
# data(MisNodes)
# 
# # Plot
# forceNetwork(Links = MisLinks, Nodes = MisNodes,
#              Source = "source", Target = "target",
#              Value = "value", NodeID = "name",
#              Group = "group", opacity = 0.8)


source <- c(1,1,0,0)
target <- c(0,2,2,3)
value <- c(1,1,1,1)
links <- as.data.frame(cbind(source, target, value))

name <- c("TJ", "TC", "TX", "TZ")
group <- c(1,1,2,2)
size <- c(1,1,1,1)
nodes <- as.data.frame(cbind(name, group, size))

forceNetwork(Links = links, Nodes = nodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8, fontSize = 24)




