library(jsonlite)

movie_data <- read.csv("network_data.csv", header = TRUE)

subset = TRUE

# Remove data with missing director
movie_data <- movie_data[-which(movie_data$director_name == ''),] 

# Differentiate between persons in both Director and Actor role
actor_dir_intersect <- which(movie_data$actor_name %in% intersect(movie_data$director_name,movie_data$actor_name))
movie_data$actor_name <- as.character(movie_data$actor_name)
movie_data$actor_name[actor_dir_intersect] <- paste(movie_data$actor_name[actor_dir_intersect], '(Ac)')

# Calculate no.of movies per director
x <- aggregate(movie_data$director_name, by=list(movie_data$director_name), function(x) length(x)/3)
colnames(x) <- c('director_name', 'dir_movie_count')
movie_data <- merge(movie_data, x, by='director_name', all.x=TRUE)

# Remove data with missing actor
movie_data <- movie_data[-which(movie_data$actor_name == ''),]

# Calculate no.of movies per actor
x <- aggregate(movie_data$actor_name, by=list(movie_data$actor_name), function(x) length(x))
colnames(x) <- c('actor_name', 'actor_movie_count')
movie_data <- merge(movie_data, x, by='actor_name', all.x=TRUE)

# Calculate dir_actor occurences
movie_data$dir_actor <- paste(movie_data$director_name, movie_data$actor_name, sep = " ")
x <- aggregate(movie_data$dir_actor, by=list(movie_data$dir_actor), function(x) length(x))
colnames(x) <- c('dir_actor', 'strength')
movie_data <- merge(movie_data, x, by='dir_actor', all.x=TRUE)

# Find directors with strength > 1
significant_dir <- unique(movie_data$director_name[which(movie_data$strength > 1)])
movie_data <- movie_data[which(movie_data$director_name %in% significant_dir),]

# Subset for testing
if (subset) {
  movie_data <- movie_data[1:1000,] 
}

# Get nodes for the graph
nrow(movie_data)
length(unique(movie_data$director_name))
length(unique(movie_data$actor_name))

dir_nodes <- cbind(as.character(movie_data$director_name), movie_data$dir_movie_count, 'Director')
actor_nodes <- cbind(as.character(movie_data$actor_name), movie_data$actor_movie_count, 'Actor')

nodes <- rbind(dir_nodes, actor_nodes)
nodes <- unique(nodes)
nrow(nodes)

nodes_df <- data.frame(id=nodes[,1], movie_count=as.numeric(nodes[,2]), type=nodes[,3])
write.csv(nodes_df, "person.csv", row.names = FALSE)

# Get links for the graph
link <- data.frame(source=as.character(movie_data$director_name), target=as.character(movie_data$actor_name),
                   strength=movie_data$strength)
link <- unique(link)
nrow(link)
write.csv(link, "links.csv", row.names = FALSE)

# Create the JSON file
network <- list(nodes=nodes_df, links=link)
json_file <- toJSON(network, pretty = TRUE)
write(json_file, "network.json")
