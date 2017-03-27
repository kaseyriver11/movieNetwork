
#build("C:\\Users\\kaseyriver11\\Desktop\\networkD3")
#install("C:\\Users\\kaseyriver11\\Desktop\\networkD3")

#devtools::install_github('wjburton/Movie_Network')
library(movieNetwork)
library(tidyverse)
library(networkD3) # You'll need my local version 

df <- create_network_df("6df77c0d4d734469b206f490ea084869", 
                              c("Comedy"), "2000-06-01", "2016-06-01", movie_request_lim = 100)

# Look at only the cast members
cast <- df[df$cast_crew == "cast",]

movies <- unique(df$title)
## We want only the first K actors per movie: 
k = 7
firstK <- cast[cast$title == "nothinginging", ]
for(i in 1:length(movies)){ 
  dframe <- cast[cast$title == movies[i],]
  dframe <- dframe[1:(min(k, dim(dframe)[1])),]
  firstK <- rbind(firstK, dframe)
} 

# Make the connections for every pair of actors that are in the same movie 
frame <- data.frame(source = character(), target = character())
for(i in 1:length(movies)){
    currentDF <- firstK[firstK$title == movies[i],]
    # Data is too large - look at top K actors only
    columns <- dim(currentDF)[1]
    count <- columns
    for(j in 1:(columns-1)){
        count = count - 1
        source <- rep(currentDF[j,"name"], (columns-j)) 
        target <- currentDF$name[(j+1):columns]
        c <- as.data.frame(cbind(source,target))
        frame <- rbind(frame,c)
    }
}
# Convert to character 
frame$source <- as.character(frame$source)
frame$target <- as.character(frame$target)
# Sort each row alphabetically 
for(i in 1:length(frame)){
  a <- sort(frame[i,])
  frame[i,1] <- a[1]
  frame[i,2] <- a[2]
}

# Grab the Unique names of the actors
keep <- unique(c(frame$source, frame$target))
names <- unique(df$name[(df$name %in% keep)])
ID <- 0:(length(names)-1)


convertDF <- data.frame(Names=names, ID = ID)

frame$source  <- convertDF$ID[match(frame$source, convertDF$Names)]
frame$target  <- convertDF$ID[match(frame$target, convertDF$Names)]

dupframe <- frame[!(duplicated(frame)), ]



name <- as.character(convertDF$Names)
count <- as.numeric()
#name <- paste0(" ., Actor: ", name, )
for(i in 1:length(name)){
  dframe <- firstK[firstK$name == name[i],]
  name[i] <- paste0("., ", "Actor: ", name[i], ", Movies: ", paste(dframe$title, collapse = "; "))
  count[i] <- dim(dframe)[1]
}


# Make Links
value <- rep(1,dim(dupframe)[1])
links <- as.data.frame(cbind(dupframe$source, dupframe$target, value))
colnames(links) <- c("source", "target", "value")

# Make Nodes
group <- count
size <- rep(1,length(name))
nodes <- as.data.frame(cbind(name, group, size))
#nodes <- nodes[order(nodes$group),]

forceNetwork(Links = links, Nodes = nodes, #height = 800, width = 1000,
             Source = "source", Target = "target", 
             linkWidth = JS("function(d) { return Math.sqrt(d.value/1.5); }"), # width of links
             linkDistance = JS("function(d){return d.value * 10}"), # tightness of movie clusters
             Value = "value", NodeID = "name", zoom = TRUE,
             charge = -5, legend = TRUE, 
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             Group = "group", opacity = 0.8, fontSize = 12)


