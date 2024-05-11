getwd()

# Get the directory path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the directory containing the script
setwd(script_dir)

data_dir = "../data"
setwd(data_dir)
# assure that you are in interust/data now, uncomment to run:
# getwd()

library(igraph)
library(ggplot2)
library(dplyr)

rm(list = ls()) #clear the workspace

df = read.csv("dependencies.csv",header = TRUE, stringsAsFactors = FALSE)
head(df)
class(df)
unique(df$timestamp) # months covered 
print(length(unique(df$timestamp)))

# create the list of networks#####################################################################
graph_list <- list()
unique_dates <- unique(df$timestamp)
for (date in unique_dates) {
  # Subset the data for the current date
  subset_data <- df[df$timestamp == date, -1]
  
  # Create a graph from the subsetted data
  g <- graph_from_data_frame(subset_data, directed = TRUE, vertices = unique(c(subset_data$repo_id, subset_data$dep_id)))
  
  # Store the graph in the list
  graph_list[[date]] <- g
}

# g <- graph_from_data_frame(df, directed = TRUE, vertices = NULL)
# plot(g)


plot(graph_list[["2022-06-01"]],layout = layout_with_fr, edge.arrow.size = 0.3)
# Simplify the graph before plotting
simplified_graph <- simplify(graph_list[["2022-06-01"]], remove.multiple = TRUE, remove.loops = TRUE)
plot(simplified_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, edge.width = 1)

######################################################trial#####
extract_last_component <- function(names) {
  split_names <- strsplit(names, "/")
  last_components <- sapply(split_names, tail, 1)
  return(last_components)
}
trial = df[df$timestamp == "2022-06-01", ]
relations <- data.frame(
  from = extract_last_component(trial$repo_name),
  to = extract_last_component(trial$dep_name)
)
trial.g = graph_from_data_frame(relations)
plot(trial.g, vertex.label.cex= 0.6, edge.arrow.size = 0.2, edge.width = 1)


###########################################################################
# layouts
# Fruchterman-Reingold layout (layout_with_fr)
# Kamada-Kawai layout (layout_with_kk)
# Large Graph Layout (layout_nicely)
# Circular layout (layout_in_circle)
# Random layout (layout_random)
# Grid layout (layout_grid)
# Sugiyama layout (layout_as_tree)

selected_graph = graph_list[["2022-09-01"]]

node_color <- "skyblue"  
# edge_color <- "lightblue"
# edge_color <- "cornflowerblue"
edge_color = "dodgerblue"
edge_width <- 1           # Width of edges
edge_curvature <- 0.2     # Curvature of edges
node_size <- 8            # Size of nodes
label_size <- 0.5       # Size of node labels
layout_algorithm <- layout_with_fr  # Fruchterman-Reingold layout algorithm

# Plot the graph
plot(selected_graph, 
     layout = layout_algorithm, 
     vertex.color = node_color, 
     vertex.size = node_size,
     vertex.label.cex = label_size,
     edge.color = edge_color,
     edge.width = edge_width,
     edge.curved = edge_curvature,
     edge.arrow.size = 0.1,
     main = "Network Graph",
     vertex.label.family = "sans",
     vertex.label.dist = 1, # distance between the node and its label
     vertex.label.color = "black")

# Let's visualize the degree distribution 
# full_graph = graph_from_data_frame(df[, -1])
# degree_distribution <- degree(full_graph)
simplify(selected_graph, remove.multiple = T, remove.loops = T)
degree_distribution <- degree(selected_graph)

hist(degree_distribution,
     main = "Degree Distribution",
     xlab = "Degree",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


# The histogram looks like a power law distribution, let's try to fit with a 
# power law distribution
# install.packages("poweRlaw")
library(poweRlaw)

# Fit a power law model to the degree distribution
fit <- power.law.fit(degree_distribution)
fit2 <- displ$new(degree_distribution)
# plot(fit2)
plot(fit2, col = "red", lwd = 2, add = TRUE, xlab = "Degree", ylab = "Probability Density", main = "Fitted Power-law Distribution")

# Add legend
legend("topright", legend = "Power-law fit", col = "red", lwd = 2)
# to add a log-log scale:
loglog <- loglog.fnpowerlaw(fit, data = TRUE)


# Perform the goodness-of-fit test
result <- fit$KS
print(result)
# the result is 0.1062375, which is > 0.05, so we do not reject the null hypothesis
print(fit$alpha)
# alpha is 3.649571, so average_degree ⟨k⟩ ∝ N^(1 - α)   
print(fit$xmin)
# xmin is 201, indicating that the power law is observed for degree>=201


###############################################################
# let's visualize more than 1 network
months = c("2021-12-01", "2022-03-01", "2022-06-01", "2022-09-01")
par(mfrow=c(2,2), mar=c(1,1,1,1))

node_color <- "skyblue"  
# edge_color <- "lightblue"
# edge_color <- "cornflowerblue"
edge_color = "dodgerblue"
edge_width <- 1           # Width of edges
edge_curvature <- 0.2     # Curvature of edges
node_size <- 8            # Size of nodes
label_size <- 0.5       # Size of node labels
layout_algorithm <- layout_with_fr  # Fruchterman-Reingold layout algorithm

for(month in months){
# Plot the graph
plot(graph_list[[month]], 
     layout = layout_algorithm, 
     vertex.color = node_color, 
     vertex.size = node_size,
     vertex.label.cex = label_size,
     edge.color = edge_color,
     edge.width = edge_width,
     edge.curved = edge_curvature,
     edge.arrow.size = 0.1,
     main = paste("Network Graph at", month),
     vertex.label.family = "sans",
     vertex.label.dist = 1, # distance between the node and its label
     vertex.label.color = "black")
}

####degree distributions are almost identical
par(mfrow=c(2,2), mar=c(2,2,1,1))
for(month in months){
  degree_distribution <- degree(graph_list[[month]])

  hist(degree_distribution,
       main = paste("Degree Distribution at", month),
       xlab = "Degree",
       ylab = "Frequency",
       col = "skyblue",
       border = "black")
}



