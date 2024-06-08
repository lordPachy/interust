getwd()

# Get the directory path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)


setwd(script_dir)

# to use the sample dataset: full_dir = "../data"
fulldir = "../rust_repos_2022_09_07/rust_repos_2022_09_07/dumps/postgresql/data"
setwd(fulldir)

library(igraph)
library(ggplot2)
library(dplyr)
library(data.table)
library(forecast)
library(poweRlaw)
library(reshape2)
# library(zoo)


###############contributions analysis################################
contributions <- read.csv("contributions.csv")
# contributions = contributions[, timestamp = as.Date(timestamp)]
contributions$timestamp <- as.Date(contributions$timestamp)
head(contributions)
list_bipartite <- list()
unique_dates <- unique(contributions$timestamp)
for (date in unique_dates) {
  contrib <- contributions[contributions$timestamp == date, -1]
  
  edges = data.frame("from" = contrib$user_id, "to"=contrib$repo_id, stringsAsFactors = F)
  
  g = graph_from_data_frame(edges, directed=F)
  
  V(g)$type = V(g)$name %in% edges[, 2]
  V(g)$color = ifelse(V(g)$type, "salmon", "lightblue") # lightblue for packages
  # salmon for users (edges[, 2] are user_ids)
  V(g)$shape <- ifelse(V(g)$type, "circle", "square") #packages square, users circle
  E(g)$color <- "lightgray"
  
  list_bipartite[[date]] <- g
}

# pdf("user_vs_repo_bipartite.pdf") # run just the middle block if you want to
# plot it directly in Rstudio


# plot(list_bipartite[[as.Date('2014-09-01')]], vertex.label=NA, 
#     layout = layout_as_bipartite)

g_contrib = list_bipartite[[as.Date('2020-09-01')]]
plot(g_contrib, vertex.label.cex = 0.2, 
     vertex.label.color = "black",
     vertex.label = NA,
     edge.width=0.5,
     edges.size = 0.1,
     vertex.size = 0.1,
     edge.lty = 2,
     edge.curved = TRUE,
     layout=layout_as_bipartite
)


# dev.off()

########################## timeseries analysis on contributions ################
# Aggregate data: number of contributions per year
contributions_per_year <- aggregate(user_id ~ format(timestamp, "%Y"), contributions, length)
colnames(contributions_per_year) <- c("Year", "Count")

# Convert to time series object
contributions_ts <- ts(contributions_per_year$Count, 
                start = as.numeric(contributions_per_year$Year[1]), frequency = 1)

# Plot the time series
plot(contributions_ts, main = "Number of Contributions per Year", xlab = "Year",
     ylab = "Number of Contributions", col = "blue", lwd = 2)

# ggplot2 alternative
contributions_per_year$Year <- as.Date(paste0(contributions_per_year$Year, "-01-01"))

ggplot(contributions_per_year, aes(x = Year, y = Count)) +
  geom_line(color = "blue") +
  labs(title = "Number of Contributions per Year", x = "Year", y = "Number of Contributions") +
  theme_minimal()


# using a simple exponential smoothing model to forecast
contributions_ts_forecast <- forecast(contributions_ts, h = 5) # Forecasting the next 5 years
plot(contributions_ts_forecast, main = "Forecast of Contributions per Year",
     xlab = "Year", ylab = "Number of Contributions")





############ let's do some basic analysis on the dependencies network ##########
dependencies = read.csv("dependencies.csv")
head(dependencies)
dependencies_2022 = dependencies[dependencies$timestamp == '2022-09-01', -1]
packages = read.csv("packages.csv")
head(packages)

# Perform a join operation on the package_id
merged_data <- merge(packages, dependencies_2022, by = "repo_id", all.x = TRUE)


dependencies_2022 = as.data.table(dependencies_2022)
# dependencies_2022 <- dependencies_2022[!is.na(dependencies_2022$repo_id) &
#                                          !is.na(dependencies_2022$dep_id)]

# Create an edge list from the dependencies data
edges_dep <- data.frame(from = dependencies_2022$repo_id, 
                        to = dependencies_2022$dep_id, stringsAsFactors = FALSE)

# Construct the dependencies graph
g_dep <- graph_from_data_frame(edges_dep, directed = FALSE)

##### analyze the largest connected component
lcc = largest_component(g_dep)
lcc = simplify(lcc, remove.loops = T, remove.multiple = T)
g_dep = lcc


# Let's plot the degree distribution
degree_values <- degree(g_dep)
hist(degree_values, breaks = 50, main = "Degree Distribution", xlab = "Degree", ylab = "Frequency")

# Log-log plot for degree distribution
plot(degree_distribution(g_dep), log = "xy", main = "Log-Log Degree Distribution", xlab = "Degree", ylab = "Frequency")


# make the degree distribution nicer with points size proportional to the frequency
# Create a data frame for degree and frequency
degree_freq <- as.data.frame(table(degree_values))
colnames(degree_freq) <- c("Degree", "Frequency")

# Convert to numeric
degree_freq$Degree <- as.numeric(as.character(degree_freq$Degree))
degree_freq$Frequency <- as.numeric(degree_freq$Frequency)

# Plot using ggplot2
ggplot(degree_freq, aes(x = Degree, y = Frequency)) +
  geom_point(aes(size = Frequency), alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Log-Log Degree Distribution",
       x = "Degree",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(1, 10))




# Network Centrality Measures
betweenness_values <- betweenness(g_dep)
closeness_values <- closeness(g_dep)
eigenvector_values <- eigen_centrality(g_dep)$vector

par(mfrow = c(2, 2))
hist(betweenness_values, breaks = 50, main = "Betweenness Centrality", xlab = "Betweenness")
hist(closeness_values, breaks = 50, main = "Closeness Centrality", xlab = "Closeness")
hist(eigenvector_values, breaks = 50, main = "Eigenvector Centrality", xlab = "Eigenvector")

# communities
par(mfrow=c(1,1))
# Reset margins to default
par(mar = c(5.1, 4.1, 4.1, 2.1))
communities <- cluster_louvain(g_dep)
plot(communities, g_dep, vertex.size = 5, vertex.label = NA, main = "Community Detection")

# plot the average number of degrees over time
edges_temp <- data.frame(from = merged_data$repo_id, to = merged_data$dep_id, stringsAsFactors = FALSE)
edges_temp <- edges_temp[!is.na(edges_temp$to), ]

g_dep_temp <- graph_from_data_frame(edges_temp, directed = TRUE)

valid_repo_ids <- V(g_dep_temp)$name
# filtered_data <- merged_data[repo_id %in% valid_repo_ids]
filtered_data <- merged_data[merged_data$repo_id %in% valid_repo_ids, ]

filtered_data = as.data.table(filtered_data)


# Make sure `repo_id` is character type to match vertex names in `g_dep_temp`
filtered_data[, repo_id := as.character(repo_id)]

# Calculate the degree for each repo_id in g_dep_temp
filtered_data[, degree := degree(g_dep_temp, v = repo_id)]

# Convert created_at to Date if necessary
filtered_data[, created_at := as.Date(created_at)]

# Aggregate by the date part of created_at to get average degree
temporal_analysis <- filtered_data[, .(average_degree = mean(degree, na.rm = TRUE)), by = created_at]

# Check the result to ensure it has enough data points
print(head(temporal_analysis))

ggplot(temporal_analysis, aes(x = created_at, y = average_degree)) +
  geom_line(color = "blue") +
  labs(title = "Average degree VS Creation date",
       x = "Creation Date",
       y = "Average Degree") +
  theme_minimal()

deg_dis = degree_distribution(g_dep)
max(degree_values)

fit <- fit_power_law(deg_dis)
print(fit)
fit2 <- displ$new(degree_values)
# plot(fit2)
plot(fit2, col = "red", lwd = 2, add = TRUE, xlab = "Degree", ylab = "Probability Density", main = "Fitted Power-law Distribution")

# Add legend
legend("topright", legend = "Power-law fit", col = "red", lwd = 2)
# to add a log-log scale:
# loglog <- loglog.fnpowerlaw(fit2, data = TRUE)

print(fit2)

# Estimate the xmin parameter and the power-law exponent
est <- estimate_xmin(fit2)
fit2$setXmin(est)
fit2$pars <- estimate_pars(fit2)
print(fit2)
## pars: power law exponent, we have 2.50895, an exponent between 2 and 3, 
## which is typical for many real-world networks, indicating a heavy-tailed 
## distribution where high-degree nodes (hubs) are relatively common.



#### Let's try also to fig a log normal #########
# Create log-normal distribution object
fit_log_normal <- dislnorm$new(degree_values)

# Estimate the xmin parameter and the log-normal parameters
est_log_normal <- estimate_xmin(fit_log_normal)
fit_log_normal$setXmin(est_log_normal)
fit_log_normal$pars <- estimate_pars(fit_log_normal)

print(fit_log_normal)
# xmin: 1
# meanlog: 1.351015
# sdlog: 1.084699



# fit_power_law(degree_distribution(g_dep))
# fit_power_law(degree(g_dep))

# Assuming degree_values contains the degree of nodes in your network
degree_values <- degree_distribution(g_dep) + 0.000001  # Replace with actual degree values

# Create power-law and log-normal distribution objects
# fit_power_law <- displ$new(degree_values)
# fit_log_normal <- dislnorm$new(degree_values)
fit_power_law <- conpl$new(degree_values)
fit_log_normal <- conlnorm$new(degree_values)


# Estimate the xmin parameter and the parameters for both distributions
est_power_law <- estimate_xmin(fit_power_law)
fit_power_law$setXmin(est_power_law)
fit_power_law$pars <- estimate_pars(fit_power_law)

est_log_normal <- estimate_xmin(fit_log_normal)
fit_log_normal$setXmin(est_log_normal)
fit_log_normal$pars <- estimate_pars(fit_log_normal)

# Print fitting results
print(fit_power_law)
print(fit_log_normal)

# Calculate p-values using bootstrap
bs_power_law <- bootstrap_p(fit_power_law, no_of_sims = 30)
bs_log_normal <- bootstrap_p(fit_log_normal, no_of_sims = 30)

p_value_power_law <- bs_power_law$p
p_value_log_normal <- bs_log_normal$p

print(bs_power_law$gof)
print(bs_log_normal$gof)

print(p_value_power_law)
print(p_value_log_normal)

# Generate empirical data for plotting
hist_data <- hist(deg, breaks = 100, plot = F)
empirical_data <- data.frame(
  Degree = hist_data$mids,
  Density = hist_data$density,
  Type = "Empirical"
)

# Generate power-law fitted values
deg = degree(g_dep)
power_law_x <- round(seq(min(deg), max(deg), length.out = 100), 0)
power_law_y <- dist_pdf(fit_power_law, power_law_x)

# Generate log-normal fitted values
log_normal_x <- round(seq(min(deg), max(deg), length.out = 100), 0)
log_normal_y <- dist_pdf(fit_log_normal, log_normal_x)

# Ensure lengths match for plotting
if (length(power_law_y) < length(power_law_x)) {
  power_law_x <- power_law_x[1:length(power_law_y)]
} else {
  power_law_y <- power_law_y[1:length(power_law_x)]
}

if (length(log_normal_y) < length(log_normal_x)) {
  log_normal_x <- log_normal_x[1:length(log_normal_y)]
} else {
  log_normal_y <- log_normal_y[1:length(log_normal_x)]
}

# Create data frames for the fitted values
power_law_data <- data.frame(
  Degree = power_law_x,
  Density = power_law_y,
  Type = "Power-Law"
)

log_normal_data <- data.frame(
  Degree = log_normal_x,
  Density = log_normal_y,
  Type = "Log-Normal"
)

# Combine all data for plotting
combined_data <- rbind(empirical_data, power_law_data, log_normal_data)

ggplot(combined_data, aes(x = Degree, y = Density, color = Type, shape = Type)) +
  geom_point(data = empirical_data, aes(x = Degree, y = Density, color = "Empirical")) +
  geom_line(data = power_law_data, aes(x = Degree, y = Density, color = "Power-Law")) +
  geom_line(data = log_normal_data, aes(x = Degree, y = Density, color = "Log-Normal")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Degree Distribution with Power-Law and Log-Normal Fits",
       x = "Degree",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_shape_manual(values = c(16, NA, NA), name = "Type") +
  scale_color_manual(values = c("black", "blue", "red"), name = "Type") +
  scale_linetype_manual(values = c("solid", "solid", "solid"), name = "Type")



#################### Network Growth Analysis

# edges_temp <- data.frame(from = merged_data$repo_id, to = merged_data$dep_id, stringsAsFactors = FALSE)
# 
# # Remove rows where dep_id is NA
# edges_temp <- edges_temp[!is.na(edges_temp$to), ]
# 
# g_dep_temp <- graph_from_data_frame(edges_temp, directed = TRUE)
# 
# # Verify that the vertex IDs are valid
# valid_repo_ids <- V(g_dep_temp)$name
# 
# # Filter merged_data to include only valid repo_id
# filtered_data <- merged_data[repo_id %in% valid_repo_ids]
# 
# # Calculate degree only for valid repo_ids
# filtered_data[, degree := degree(g_dep_temp, v = as.character(repo_id))]
# 
# # Perform temporal analysis
# temporal_analysis <- filtered_data[, .(average_degree = mean(degree, na.rm = TRUE)), by = created_at]
# 
# # Plot the results
# ggplot(temporal_analysis, aes(x = created_at, y = average_degree)) +
#   geom_line(color = "blue") +
#   labs(title = "Average Degree of Packages Over Time",
#        x = "Creation Date",
#        y = "Average Degree") +
#   theme_minimal()


############### average degrees over time ########

# Get unique dates
unique_dates <- unique(dependencies$timestamp)
average_degree <- numeric(length(unique_dates))

# Calculate average degree for each date
for (i in seq_along(unique_dates)) {
  date <- unique_dates[i]
  # Subset the data for the current date
  subset_data <- df[df$timestamp == date, c("repo_id", "dep_id")]
  
  # Create a graph from the subsetted data
  g <- graph_from_data_frame(subset_data, directed = TRUE, vertices = 
                               unique(c(subset_data$repo_id, subset_data$dep_id)))
  
  # Calculate average degree
  average_degree[i] <- mean(degree(g))
}

# Create a data frame with the results
avg_degrees <- data.frame(unique_dates = as.Date(unique_dates), 
                          average_degree = average_degree)

# Plot using ggplot2
ggplot(avg_degrees, aes(x = unique_dates, y = average_degree)) +
  geom_line(color = "blue") +
  labs(title = "Average Degree of the Network Over Time",
       x = "Year",
       y = "Average Degree") +
  theme_minimal()

# Optionally, plot using points with rounded average degrees
ggplot(avg_degrees, aes(x = unique_dates, y = round(average_degree, 1))) +
  geom_point() +
  labs(title = "Average Degree of the Network Over Time",
       x = "Year",
       y = "Average Degree") +
  theme_minimal()

##  plots of dependencies network 

selected_graph = lcc

node_color <- "skyblue"  
# edge_color <- "lightblue"
# edge_color <- "cornflowerblue"
edge_color = "dodgerblue"
edge_width <- 1           # Width of edges
edge_curvature <- 0.2     # Curvature of edges
node_size <- 8            # Size of nodes
label_size <- 0.5       # Size of node labels
layout_algorithm <- layout_with_fr  # Fruchterman-Reingold layout algorithm

## Plot the graph (will take 10 mins to plot)
# plot(selected_graph, 
#      layout = layout_algorithm, 
#      vertex.color = node_color, 
#      vertex.size = node_size,
#      vertex.label.cex = label_size,
#      edge.color = edge_color,
#      edge.width = edge_width,
#      edge.curved = edge_curvature,
#      edge.arrow.size = 0.1,
#      main = "Network Graph",
#      vertex.label.family = "sans",
#      vertex.label.dist = 1, # distance between the node and its label
#      vertex.label.color = "black", 
#      vertex.label = NA)


## a better way to plot it 
# plot(simplify(lcc), vertex.size= 0.01,edge.arrow.size=0.001,vertex.label.cex = 0.75,
#      vertex.label.color = "black"  ,vertex.frame.color = adjustcolor("white",
#       alpha.f = 0),vertex.color = adjustcolor("white", alpha.f = 0),
#      edge.color=adjustcolor(1, alpha.f = 0.15),display.isolates=FALSE,
#      # edge.color=edge_color,display.isolates=FALSE,
#      vertex.label=ifelse(page_rank(lcc)$vector > 0.1 , 
#                          "important nodes", NA))


## we can try to export it and plot it in gephi
selected_graph <- lcc  # Your largest component graph

# Export  and edges
nodes <- data.frame(id = V(selected_graph)$name)
write.csv(nodes, "nodes.csv", row.names = FALSE)
edges <- get.data.frame(selected_graph, what = "edges")
write.csv(edges, "edges.csv", row.names = FALSE)

summary(nodes)
summary(edges)


########################### as reference let's see how these metrics look like in a Barabasi #########################
## Albert Model and in a Erdos Renyi Model
# Generate a scale-free network using the Barabási-Albert model
set.seed(42)
g_ba <- barabasi.game(n = 1000, m = 5, directed = FALSE)

# Calculate centrality measures
ba_betweenness <- betweenness(g_ba)
ba_closeness <- closeness(g_ba)
ba_eigenvector <- eigen_centrality(g_ba)$vector

# Plot histograms of centrality measures
par(mfrow = c(2, 2))
hist(ba_betweenness, breaks = 50, main = "BA Betweenness Centrality", xlab = "Betweenness", col = "lightblue")
hist(ba_closeness, breaks = 50, main = "BA Closeness Centrality", xlab = "Closeness", col = "lightblue")
hist(ba_eigenvector, breaks = 50, main = "BA Eigenvector Centrality", xlab = "Eigenvector", col = "lightblue")


##Random Network: Erdos Renyi 
# Generate a random network using the Erdős-Rényi model
g_er <- erdos.renyi.game(n = 1000, p = 0.01, directed = FALSE)

# Calculate centrality measures
er_betweenness <- betweenness(g_er)
er_closeness <- closeness(g_er)
er_eigenvector <- eigen_centrality(g_er)$vector

# Plot histograms of centrality measures
par(mfrow = c(2, 2))
hist(er_betweenness, breaks = 50, main = "ER Betweenness Centrality", xlab = "Betweenness", col = "salmon")
hist(er_closeness, breaks = 50, main = "ER Closeness Centrality", xlab = "Closeness", col = "salmon")
hist(er_eigenvector, breaks = 50, main = "ER Eigenvector Centrality", xlab = "Eigenvector", col = "salmon")




# Plot degree distribution for both networks
par(mfrow = c(1, 2))
plot(degree.distribution(g_ba), main = "BA Degree Distribution", xlab = "Degree", ylab = "Frequency", col = "blue", type = "h")
plot(degree.distribution(g_er), main = "ER Degree Distribution", xlab = "Degree", ylab = "Frequency", col = "red", type = "h")




# 4. Average Degree Over Time (for BA Model)
# The Barabási-Albert model grows over time, so let's simulate and plot the average degree over time.


# Initialize parameters
n_steps <- 100
n_nodes <- 1000
m <- 5
avg_degree_ba <- numeric(n_steps)
avg_degree_er <- numeric(n_steps)

# Simulate and calculate average degree for each step
for (i in 1:n_steps) {
  g_ba <- barabasi.game(n = i * 10, m = m, directed = FALSE)
  avg_degree_ba[i] <- mean(degree(g_ba))
  
  g_er <- erdos.renyi.game(n = i * 10, p = 2 * m / (i * 10), directed = FALSE)
  avg_degree_er[i] <- mean(degree(g_er))
}

# Plot average degree over time
par(mfrow = c(1, 1))
plot(1:n_steps, avg_degree_ba, type = "l", col = "blue", ylim = c(0, max(avg_degree_ba, avg_degree_er)),
     xlab = "Steps", ylab = "Average Degree", main = "Average Degree Over Time")
lines(1:n_steps, avg_degree_er, col = "red")
legend("topright", legend = c("BA Model", "ER Model"), col = c("blue", "red"), lty = 1)











########################### fitness analysis ##############################################
# ecount(g_dep)/(vcount(g_dep)*(vcount(g_dep)-1))

nrow(merged_data)
head(merged_data)
merged_data = as.data.table(merged_data, T)
class(merged_data)
head(merged_data)

# merged_data = unique(merged_data)   # already unique
# nrow(merged_data)

## Remove rows with NA in created_at
# merged_data <- merged_data[!is.na(created_at)]


#print.POSIXct <- function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))
merged_data = merged_data[, created_at := as.Date(created_at)]
min(merged_data$created_at, na.rm = TRUE)


# discard useless columns
merged_data = merged_data[,c("repo_name", "rn",
                             "source_id", "insource_id", "url_id"):=NULL]
head(merged_data)

head(contributions)
class(contributions)


contributions_dt = as.data.table(contributions)

############# 1: Number of Contributors VS Degree ##############################

# # Step 1: Create the dependencies network
# dependencies <- as.data.table(dependencies)
# dependencies <- dependencies[!is.na(repo_id) & !is.na(dep_id)]
# 
# # Create an edge list from the dependencies data
# edges_dep <- data.frame(from = dependencies$repo_id, 
#                         to = dependencies$dep_id, stringsAsFactors = FALSE)
# 
# # Construct the dependencies graph
# g_dep <- graph_from_data_frame(edges_dep, directed = FALSE)

# Step 2: Calculate the degree for each package in the dependencies network
package_degree <- degree(g_dep, mode = "all")

# Convert package_degree to a data frame
degree_df <- data.frame(package_id = names(package_degree), degree = package_degree)

# Step 3: Calculate the number of unique contributors per package
contributions_dt <- as.data.table(contributions)
package_contributors <- contributions_dt[, .(num_contributors = uniqueN(user_id)), by = repo_id]

# Step 4: Merge degree and contributor count
degree_contributors <- merge(degree_df, package_contributors, by.x = "package_id",
                             by.y = "repo_id", all.x = TRUE)

# Step 5: Plot the correlation between degree and number of contributors
ggplot(degree_contributors, aes(x = num_contributors, y = degree)) +
  geom_point(color = "blue") +
  # geom_smooth(method = "loess", formula = y ~ x) +
  # stat_smooth(method = "lm",
  #             formula = y ~ x,
  #             geom = "smooth") +
  labs(title = "Correlation between Package Degree and Number of Contributors",
       x = "Number of Contributors",
       y = "Package Degree") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))

# let's see the linear regression results
linear_model = lm(degree ~ num_contributors, data = degree_contributors)
summary(linear_model)


# Calculate the correlation coefficient
correlation <- cor(degree_contributors$degree, degree_contributors$num_contributors, use = "complete.obs")
print(correlation)




# Apply log transformation to degree and num_contributors to see better the data
degree_contributors$log_degree <- log1p(degree_contributors$degree)
degree_contributors$log_num_contributors <- log1p(degree_contributors$num_contributors)

# Plot the log-transformed data
ggplot(degree_contributors, aes(x = log_num_contributors, y = log_degree)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  labs(title = "Correlation between Log-transformed Package Degree and Number of Contributors",
       x = "Log(Number of Contributors)",
       y = "Log(Package Degree)") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))

# Linear regression on log-transformed data
lm_log <- lm(log_degree ~ log_num_contributors, data = degree_contributors)
summary(lm_log)

# Calculate the correlation coefficient on log-transformed data
correlation_log <- cor(degree_contributors$log_degree, degree_contributors$log_num_contributors, use = "complete.obs")
print(correlation_log)


# use jittering: Add a small amount of random noise to the data points to reduce overplotting.
ggplot(degree_contributors, aes(x = log_num_contributors, y = log_degree)) +
  geom_point(color = "blue", position = position_jitter(width = 0.1, height = 0.1)) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  labs(title = "Correlation between Log-transformed Package Degree and Number of Contributors",
       x = "Log(Number of Contributors)",
       y = "Log(Package Degree)") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))

# alpha blending: adjust the transparency 
ggplot(degree_contributors, aes(x = log_num_contributors, y = log_degree)) +
  geom_point(color = "blue", alpha = 0.3, position = position_jitter(width = 0.1, height = 0.1)) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  labs(title = "Correlation between Log-transformed Package Degree and Number of Contributors",
       x = "Log(Number of Contributors)",
       y = "Log(Package Degree)") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))





#################### 2: Age of the repository VS Degree #########################
merged_data <- merged_data[!is.na(created_at)]
merged_data[, age := as.numeric(difftime(Sys.Date(), created_at, units = "days")) / 365]


# Create an edge list from the dependencies data
edges_dep <- data.frame(from = dependencies_2022$depending_version, 
                        to = dependencies_2022$depending_on_package, stringsAsFactors = FALSE)

# we already have the dependencies graph (g_dep) and degree (package_degree)
package_degree <- degree(lcc, mode = "all")
degree_df <- data.frame(package_id = names(package_degree), degree = package_degree)

# Merge degree and age
age_degree <- merge(degree_df, merged_data[, .(repo_id, age)], by.x = "package_id", by.y = "repo_id", all.x = TRUE)

# Remove rows with NA age
# age_degree <- age_degree[!is.na(age)]

# Plot the correlation between age and degree
ggplot(age_degree, aes(x = age, y = degree)) +
  geom_point(color = "blue") +
  # geom_smooth(method = "loess", formula = y ~ x) +
  stat_smooth(method = "lm",
              formula = y ~ x,color="red",
              geom = "smooth") +
  labs(title = "Correlation between Repository Age and Package Degree",
       x = "Repository Age (years)",
       y = "Package Degree") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))

# linear regression:
summary(lm(degree ~ age, data = age_degree))

# Calculate the correlation coefficient
correlation_age_degree <- cor(age_degree$age, age_degree$degree, use = "complete.obs")
print(correlation_age_degree)



##################### 3: Number of Stars, commits, forks, developers vs degree ######

usage = read.csv('usage.csv')
head(usage)


# Convert data frames to data.tables for efficient operations
age_degree_dt <- as.data.table(age_degree)
degree_contributors_dt <- as.data.table(degree_contributors)

# Convert package_id to character to ensure consistent data types
age_degree_dt[, package_id := as.character(package_id)]
degree_contributors_dt[, package_id := as.character(package_id)]

# Merge datasets
attributes_dt <- merge(age_degree_dt, degree_contributors_dt, by = "package_id", all.x = TRUE)
attributes_dt[is.na(attributes_dt)] <- 0
usage_dt <- as.data.table(usage)
usage_dt[, repo_id := as.character(repo_id)]
usage_dt[, timestamp := as.Date(timestamp)]

# Select the latest entry for each repo_id
usage_latest_dt <- usage_dt[, .SD[which.max(timestamp)], by = repo_id]

# Merge attributes with usage data, ensuring unique package_id
attributes_l_dt <- merge(attributes_dt, usage_latest_dt, by.x = "package_id", by.y = "repo_id", all.x = TRUE)

# Remove duplicates and keep the most recent entry per package_id if necessary
attributes_l_dt <- attributes_l_dt[order(package_id, -timestamp)]
attributes_l_dt <- attributes_l_dt[!duplicated(package_id)]

# Convert back to data frame
unique_attributes <- as.data.frame(attributes_l_dt)

# Inspect the resulting unique attributes data
head(unique_attributes)

rm(attributes_dt, attributes_l_dt)  # free some RAM space 

unique_attributes$degree.y <- NULL


















# Calculate correlation coefficients
correlations <- sapply(unique_attributes[, c("stars", "forks", "commits", "commits_cumul", 
                                             "developers", "active_developers", "downloads", 
                                             "issues", "issues_closed", "pullrequests", 
                                             "pullrequests_merged")],
                       function(x) cor(x, unique_attributes$degree.x, use = "complete.obs"))

# Print correlation results
print(correlations)

# Plotting each attribute against degree
attributes_list <- c("stars", "forks", "commits", "commits_cumul", "developers", 
                     "active_developers", "downloads", "issues", "issues_closed", 
                     "pullrequests", "pullrequests_merged")

for (attribute in attributes_list) {
  p <- ggplot(unique_attributes, aes_string(x = attribute, y = "degree.x")) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
    labs(title = paste("Correlation between", attribute, "and Degree"),
         x = attribute,
         y = "Degree") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))
  print(p)
}

# Display the correlation coefficients
correlations




attributes_temp = unique_attributes

# Apply log transformation to attributes to handle outliers
for (attribute in attributes_list) {
  attributes_temp[[paste0("log_", attribute)]] <- log1p(attributes_temp[[attribute]])
}

# Plotting log-transformed attributes against log-transformed degree
for (attribute in attributes_list) {
  p <- ggplot(attributes_temp, aes_string(x = paste0("log_", attribute), y = "log_degree")) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
    labs(title = paste("Correlation between Log-transformed", attribute, "and Log-transformed Degree"),
         x = paste0("Log(", attribute, ")"),
         y = "Log(Degree)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5, vjust = 2))
  print(p)
}

# Recalculate correlation coefficients for log-transformed data
log_correlations <- sapply(attributes_temp[, paste0("log_", attributes_list)], 
                           function(x) cor(x, attributes_temp$log_degree, use = "complete.obs"))
print(log_correlations)






# Create a correlation matrix
correlation_matrix <- cor(unique_attributes[, c("degree.x", "age", "num_contributors", "stars", "forks", 
                                                "commits", "commits_cumul", "developers", "active_developers", 
                                                "downloads", "issues", "issues_closed", "pullrequests", 
                                                "pullrequests_merged")], use = "complete.obs")

# Convert the matrix to a long format for ggplot
correlation_long <- reshape2::melt(correlation_matrix)

# Plot the heatmap
ggplot(data = correlation_long, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))



# Fit a linear regression model
model <- lm(degree.x ~ age + num_contributors + stars + forks + commits + 
              commits_cumul + developers + active_developers + downloads + 
              issues + issues_closed + pullrequests + pullrequests_merged, data = unique_attributes)

# Summarize the model
summary(model)

unique_attributes$fitness <- with(unique_attributes,
                                  -4.709 * age - 1.188 * num_contributors - 0.001735
                                  * stars + 0.04683 * forks +
                                    0.6711 * commits - 0.004562 * commits_cumul 
                                  + 1.481 * developers - 0.4346 * active_developers +
                                    6.989e-06 * downloads - 0.1147 * issues + 
                                    0.08547 * issues_closed + 0.006538 * pullrequests
                                  + 44.28
)

# Plot fitness distribution
hist(unique_attributes$fitness, breaks = 50, main = "Fitness Distribution", xlab = "Fitness Score")

# # Check if fitness follows a power-law distribution (use the continuous version of 
# # fitting powerlaw since fitness scores are continuos)
# fitness_pl <- conpl$new((unique_attributes$fitnes))
# est <- estimate_xmin(fitness_pl)  ## commented because this line takes too much time
# fitness_pl$setXmin(est)
# plot(fitness_pl)
# lines(fitness_pl, col="red")

fit_power_law(unique_attributes$fitness) ##(takes around 10minutes to finish)



### some variables seems redundant or linearly dependent: 
# install.packages('car')
library(car)
# Calculate VIF for each predictor
vif_values <- vif(model)
print(vif_values)


### remove variables that are highly linearly dependent (with VIF values > 5)
final_model <- lm(degree.x ~ age + stars + forks + commits + 
                    downloads + issues_closed, data = unique_attributes)

# Summary of the final model
summary(final_model)

# Calculate VIF for the final model
final_vif <- vif(final_model)
print(final_vif)



########################## polynomial regression model #########################

# Create polynomial terms
unique_attributes$age2 <- unique_attributes$age^2
unique_attributes$stars2 <- unique_attributes$stars^2
unique_attributes$forks2 <- unique_attributes$forks^2
unique_attributes$commits2 <- unique_attributes$commits^2
unique_attributes$downloads2 <- unique_attributes$downloads^2

# Convert columns to numeric to avoid overflow
unique_attributes$downloads <- as.numeric(unique_attributes$downloads)
unique_attributes$developers <- as.numeric(unique_attributes$developers)

# Create interaction terms
unique_attributes$age_stars <- unique_attributes$age * unique_attributes$stars
unique_attributes$commits_forks <- unique_attributes$commits * unique_attributes$forks
unique_attributes$downloads_developers <- unique_attributes$downloads * unique_attributes$developers

# Fit the polynomial regression model with interaction terms
model_poly <- lm(degree.x ~ age + age2 + num_contributors + stars + stars2 + forks + forks2 + commits + commits2 +
                   commits_cumul + developers + active_developers + downloads + downloads2 + issues + issues_closed + 
                   pullrequests + pullrequests_merged + age_stars + commits_forks + downloads_developers, data = unique_attributes)

# Summarize the model
summary(model_poly)




########################## Gradient Boosting Machine ##########################
library(gbm)
# install.packages("gbm")
# Set seed for reproducibility
set.seed(42)

# Fit a Gradient Boosting model
model_gbm <- gbm(
  formula = degree.x ~ age + num_contributors + stars + forks + commits + 
    commits_cumul + developers + active_developers + downloads + 
    issues + issues_closed + pullrequests + pullrequests_merged,
  data = unique_attributes,
  distribution = "gaussian",
  n.trees = 5000,  # Number of trees
  interaction.depth = 4,  # Depth of each tree
  shrinkage = 0.01,  # Learning rate
  cv.folds = 5  # Cross-validation folds
)

# Summary of the model
summary(model_gbm)
summary_gbm <- summary(model_gbm)

# Normalize the relative influence for color scaling
summary_gbm$color_intensity <- summary_gbm$rel.inf / max(summary_gbm$rel.inf)

# Plot the relative influence with gradient color
ggplot(summary_gbm, aes(x = reorder(var, rel.inf), y = rel.inf, fill = color_intensity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Relative Influence of Variables",
       x = "Variable",
       y = "Relative Influence") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE)  # Remove the legend for fill

# Get the best number of trees
best_iter <- gbm.perf(model_gbm, method = "cv")

# Predict on the training data
predictions <- predict(model_gbm, newdata = unique_attributes, n.trees = best_iter)

# Calculate R-squared
ss_total <- sum((unique_attributes$degree.x - mean(unique_attributes$degree.x))^2)
ss_residual <- sum((unique_attributes$degree.x - predictions)^2)
r_squared <- 1 - ss_residual / ss_total

# Print R-squared
cat("R-squared:", r_squared, "\n")

# Calculate Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE)
mae <- mean(abs(predictions - unique_attributes$degree.x))
rmse <- sqrt(mean((predictions - unique_attributes$degree.x)^2))

# Print MAE and RMSE
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot(unique_attributes$degree.x, predictions, xlab = "Actual Degree", 
     ylab = "Predicted Degree", main = "Actual vs Predicted Degrees")
abline(0, 1, col = "red")


residuals <- unique_attributes$degree.x - predictions
plot(predictions, residuals, xlab = "Predicted Degree", ylab = "Residuals", main = "Residuals Plot")
abline(h = 0, col = "red")


hist(predictions, breaks = 50, main = "Predicted Degree Distribution", xlab = "Predicted Degree")
hist(unique_attributes$degree.x, breaks = 50, main = "Actual Degree Distribution", 
     xlab = "Actual Degree", col = "blue", add = TRUE, alpha = 0.5)


top_predicted_nodes <- unique_attributes[order(-predictions), ]
top_actual_nodes <- unique_attributes[order(-unique_attributes$degree.x), ]
head(top_predicted_nodes)
head(top_actual_nodes)


####### it seems that a simple linear regression model is not able to capture 
#####   the variability in degree, lets's use a randomForest
library(randomForest)
# install.packages("randomForest")

# # Fit a random forest model (super super slow)
# model_rf <- randomForest(degree.x ~ age + num_contributors + stars + forks + commits + 
#                            commits_cumul + developers + active_developers + downloads + 
#                            issues + issues_closed + pullrequests + pullrequests_merged, 
#                          data = unique_attributes, importance = TRUE, ntree = 500)


# let's fit a random forest model with reduced number of trees and limited depth
model_rf <- randomForest(
  degree.x ~ age + num_contributors + stars + forks + commits + 
    commits_cumul + developers + active_developers + downloads + 
    issues + issues_closed + pullrequests + pullrequests_merged, 
  data = unique_attributes, 
  importance = TRUE, 
  ntree = 100,          # Reduce the number of trees
  maxnodes = 30,        # Limit the maximum number of nodes in a tree
  mtry = 3,             # Number of variables tried at each split (you can tune this parameter)
  do.trace = 50         # Print progress every 50 trees
)


print(model_rf)

# Get variable importance
importance(model_rf)

# Convert importance to data frame
importance_df <- as.data.frame(importance(model_rf))
importance_df$Variable <- rownames(importance_df)


# %IncMSE: The increase in Mean Squared Error (%IncMSE) if the variable is left out.
# Higher values indicate that the variable is more important for prediction.
# IncNodePurity: The total decrease in node impurity (measured by the residual sum
# of squares) that results from splits over that variable. Higher values indicate 
# greater importance.

# Plot %IncMSE
ggplot(importance_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance (Random Forest)",
       x = "Variables",
       y = "% Increase in MSE") +
  theme_minimal()

# Plot IncNodePurity
ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance (Random Forest)",
       x = "Variables",
       y = "Increase in Node Purity") +
  theme_minimal()

# new_data <- data.frame(
#   age = c(5, 6),
#   num_contributors = c(10, 12),
#   stars = c(50, 60),
#   forks = c(5, 6),
#   commits = c(100, 110),
#   commits_cumul = c(150, 160),
#   developers = c(3, 4),
#   active_developers = c(2, 3),
#   downloads = c(5000, 6000),
#   issues = c(1, 2),
#   issues_closed = c(0, 1),
#   pullrequests = c(10, 12),
#   pullrequests_merged = c(8, 10)
# )
# 
# predictions <- predict(model_rf, new_data)
# print(predictions)



fitness_metric <- predict(model_rf, unique_attributes)
tail(fitness_metric, 20)
tail(unique_attributes$degree.x, 20)
# unique_attributes$fitness_metric <- fitness_metric

### Let's see the performance of the model: 
residuals <- unique_attributes$degree.x - fitness_metric
hist(residuals, breaks = 50, main = "Histogram of Residuals", xlab = "Residuals")
plot(fitness_metric, residuals, main = "Residuals vs Predicted", xlab = "Predicted Degree", ylab = "Residuals")

plot(unique_attributes$degree.x, fitness_metric, 
     xlab = "Actual Degree", ylab = "Predicted Degree", 
     main = "Actual vs Predicted Degree")
abline(0, 1, col = "red")




library(pdp)
# install.packages('pdp')

# Extract feature importances
importance_matrix <- importance(model_rf)

# Extract the MeanDecreaseGini or MeanDecreaseAccuracy column (depends on your focus)
# Here, we use MeanDecreaseGini as an example
importance_values <- importance_matrix[, "IncNodePurity"]

# Sort the importance values in decreasing order and extract the feature names
sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)
print(important_features)

# par(mfrow = c(2, 3))  # Set up the plot layout
# for (feature in important_features) {
#   pdp <- partial(model_rf, pred.var = feature, grid.resolution = 50)
#   plotPartial(pdp, main = feature)
# }          ### super slow and take hours

# Fit a simplified linear model using the top important features
top_features <- important_features[1:5]
formula <- as.formula(paste("fitness ~", paste(top_features, collapse = " + ")))
simplified_model <- lm(formula, data = unique_attributes)
summary(simplified_model)
# Print coefficients of the simplified model
coefficients <- summary(simplified_model)$coefficients
print(coefficients)

par(mfrow=c(2,2))
plot(simplified_model)
par(mfrow=c(1,1))

unique_attributes$new_fitness <- with(unique_attributes,
                                  -2.561078e+00 * num_contributors  + 4.650633e-02 * forks +
                                  2.618262e+00 * developers +
                                  7.032726e-06 * downloads -2.863112e-02 * issues + 
                                    2.622250e+01
)

summary(lm(degree.x ~ num_contributors + forks +
                developers +  downloads + 
                issues, data = unique_attributes))
head(unique_attributes)

hist(unique_attributes$new_fitness, breaks = 50, main = "Fitness Distribution (RF model)"
     , xlab = "Fitness Score")
# fit_power_law(unique_attributes$fitness)  # 10mins, skip if not needed



# compare_distributions(deg_dis, unique_attributes$new_fitness)


######### Grwoth analysis and preferential attachment ##################

setDT(dependencies)
dependencies[, timestamp := as.Date(timestamp)]

# Extract year and month
dependencies[, year := year(timestamp)]
dependencies[, month := month(timestamp)]
monthly_networks <- list()

# Create monthly networks from 2014 to 2022
for (yr in 2014:2022) {
  for (mnth in 1:12) {
    subset_data <- dependencies[year == yr & month == mnth, .(repo_id, dep_id)]
    
    if (nrow(subset_data) > 0) {
      g <- graph_from_data_frame(subset_data, directed = TRUE, vertices = unique(c(subset_data$repo_id, subset_data$dep_id)))
      
      # Store the graph in the list with a year-month key
      monthly_networks[[paste0(yr, "-", sprintf("%02d", mnth))]] <- g
    }
  }
}



dates <- names(monthly_networks)
num_nodes <- sapply(monthly_networks, vcount)
num_edges <- sapply(monthly_networks, ecount)


network_growth <- data.table(
  Date = as.Date(paste0(dates, "-01")),
  Nodes = num_nodes,
  Edges = num_edges
)

# Plot the number of nodes and edges over time
ggplot(network_growth, aes(x = Date)) +
  geom_line(aes(y = Nodes, color = "Nodes")) +
  geom_line(aes(y = Edges, color = "Edges")) +
  labs(y = "Count", color = "Metric") +
  theme_minimal() +
  ggtitle("Network Growth Over Time")
# Edges Outpacing Nodes: The number of edges grows faster than the number of nodes.
# This indicates that each node (repository) is, on average, forming more 
# dependencies over time. It implies a denser network, where repositories are 
# increasingly interconnected.




# Analyze preferential attachment by Analyzing the degree distribution for a specific month
target_month <- "2022-09"
g <- monthly_networks[[target_month]]

# Fit power-law model
degree_values <- degree(g, mode = "in")
degree_values <- degree_values[degree_values > 0]
fit_power_law <- displ$new(degree_values)
fit_power_law$setXmin(estimate_xmin(fit_power_law))
fit_power_law$pars <- estimate_pars(fit_power_law)

# Create empirical data
empirical_data <- data.frame(
  Degree = sort(unique(degree_values)),
  Density = as.numeric(table(degree_values) / length(degree_values))
)

# Generate power-law fitted values
power_law_x <- seq(min(degree_values), max(degree_values), length.out = 100)
power_law_y <- dist_pdf(fit_power_law, power_law_x)

# Plot the empirical degree distribution and the power-law fit
ggplot(empirical_data, aes(x = Degree, y = Density)) +
  geom_point() +
  geom_line(data = data.frame(Degree = power_law_x, Density = power_law_y), aes(x = Degree, y = Density), color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Degree", y = "Density", title = "Degree Distribution with Power-law Fit") +
  theme_minimal() 
  # annotation_logticks()




### network structure metrics change over time ###
monthly_metrics <- list()
unique_dates <- unique(dependencies$timestamp)

# Calculate metrics for each month
for (date in unique_dates) {
  subset_data <- dependencies[timestamp == date, .(repo_id, dep_id)]
  g <- graph_from_data_frame(subset_data, directed = TRUE)
  
  # num_nodes <- vcount(g)
  # num_edges <- ecount(g)
  avg_degree <- mean(degree(g))
  transitivity <- transitivity(g, type = "average")
  avg_path_length <- average.path.length(g, directed = TRUE)
  
  monthly_metrics[[as.character(date)]] <- list(
    Date = date,
    # Nodes = num_nodes,
    # Edges = num_edges,
    AvgDegree = avg_degree,
    Transitivity = transitivity,
    AvgPathLength = avg_path_length
  )
}

# Convert to data.table
metrics_dt <- rbindlist(monthly_metrics)

# Ensure 'Date' column is of Date type
metrics_dt[, Date := as.Date(Date, origin = "1970-01-01")]

# Reshape data for plotting
metrics_long <- melt(metrics_dt, id.vars = "Date", variable.name = "Metric", value.name = "Value")

# Plot metrics over time
ggplot(metrics_long, aes(x = Date, y = Value, color = Metric)) +
  geom_line() +
  labs(title = "Network Metrics Over Time", y = "Value") +
  scale_color_manual(name = "Metric", values = c(
    # "Nodes" = "blue", "Edges" = "red",
  "AvgDegree" = "green", "Transitivity" = "purple", "AvgPathLength" = "orange")) +
  theme_minimal()
# The transitivity remains relatively flat and close to zero throughout the observed period.
# This implies that despite the increasing number of connections, the network does 
# not form many tightly-knit clusters or triads. The structure of the network may 
# be more tree-like or have a more hierarchical nature with few closed loops.


#### degree growth over time ######
degree_growth <- list()

for (i in seq_along(unique_dates)) {
  date <- unique_dates[i]
  subset_data <- dependencies[timestamp == date, .(repo_id, dep_id)]
  g <- graph_from_data_frame(subset_data, directed = TRUE)
  
  if (i == 1) {
    previous_degrees <- degree(g)
    next
  }
  
  current_degrees <- degree(g)
  degree_diff <- current_degrees - previous_degrees
  
  degree_growth[[as.character(date)]] <- data.table(
    Date = date,
    Node = names(degree_diff),
    DegreeDiff = degree_diff,
    PreviousDegree = previous_degrees[names(degree_diff)]
  )
  
  previous_degrees <- current_degrees
}

# Combine all data
degree_growth_dt <- rbindlist(degree_growth)
degree_growth_dt <- degree_growth_dt[PreviousDegree > 0]

# Plot degree growth
ggplot(degree_growth_dt, aes(x = PreviousDegree, y = DegreeDiff)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  # geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Previous Degree", y = "Degree Growth", title = "Preferential Attachment Analysis") +
  theme_minimal()
# There is a clear positive correlation between the previous degree and degree growth.
# This indicates that nodes with higher degrees tend to gain more connections over time,
# which is a characteristic of preferential attachment.
# "the rich get richer."



#### TODO: still need to debug this part 
# # ######### fitness
# # 
# # # 
# # # # Convert timestamp to Date type
# # dependencies$timestamp <- as.Date(dependencies$timestamp)
# # 
# # # Extract unique nodes
# # nodes <- unique(c(dependencies$repo_id, dependencies$dep_id))
# # node_data <- data.table(node_id = nodes, degree = rep(0, length(nodes)), fitness = rep(1, length(nodes)))
# # 
# # # Function to calculate fitness scores
# # calculate_fitness <- function(dependencies, node_data) {
# #   unique_timestamps <- unique(dependencies$timestamp)
# #   
# #   for (timestamp in unique_timestamps) {
# #     current_data <- dependencies[timestamp == !!timestamp]
# #     
# #     # Update degrees
# #     node_data[current_data$repo_id, degree := degree + 1, on = .(node_id)]
# #     node_data[current_data$dep_id, degree := degree + 1, on = .(node_id)]
# #     
# #     # Update fitness scores iteratively
# #     fitness_sum <- sum(node_data$fitness * node_data$degree, na.rm = TRUE)
# #     if (fitness_sum > 0) {
# #       node_data[, prob := (fitness * degree) / fitness_sum]
# #       node_data[prob > 0, fitness := fitness + (degree / prob)]
# #       node_data[, prob := NULL] # Remove the temporary column
# #     }
# #     print(paste("Processed timestamp:", timestamp))
# #   }
# #   return(node_data)
# # }
# # 
# # # Apply the function to calculate fitness
# # node_data <- calculate_fitness(dependencies, node_data)
# # 
# # # Ensure fitness values are not NA
# # node_data[is.na(fitness), fitness := 1]
# # 
# # # Print a sample of the resulting node_data
# # print(head(node_data))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Ensure consistent data types
# age_degree_dt[, package_id := as.character(package_id)]
# degree_contributors_dt[, package_id := as.character(package_id)]
# usage_dt[, repo_id := as.character(repo_id)]
# usage_dt[, timestamp := as.Date(timestamp)]
# 
# # Merge age and degree data
# attributes_dt <- merge(age_degree_dt, degree_contributors_dt, by = "package_id", all.x = TRUE)
# attributes_dt[is.na(attributes_dt)] <- 0
# 
# # Check the structure of attributes_dt and ensure degree is numeric
# str(attributes_dt)
# 
# # Ensure the degree column is numeric
# attributes_dt[, degree := as.numeric(degree.y)]
# attributes_dt[, degree.y := NULL]
# 
# # Check for any non-numeric values and convert them if needed
# attributes_dt[, degree := as.numeric(as.character(degree))]
# 
# # Verify the structure again
# str(attributes_dt)
# 
# # Define the function to fit the degree growth model
# fit_degree_growth <- function(degree, timestamp) {
#   numeric_timestamp <- as.numeric(difftime(timestamp, min(timestamp), units = "days"))
#   
#   log_degree <- log(degree)
#   log_timestamp <- log(numeric_timestamp + 1)  # +1 to avoid log(0)
#   
#   model <- lm(log_degree ~ log_timestamp)
#   return(model)
# }
# 
# # Calculate fitness values for each package_id
# calculate_fitness <- function(df) {
#   fitness <- rep(NA, nrow(df))
#   unique_packages <- unique(df$package_id)
#   
#   for (package_id in unique_packages) {
#     subset_data <- df[df$package_id == package_id, ]
#     
#     if (nrow(subset_data) > 1) {
#       model <- fit_degree_growth(subset_data$degree, subset_data$timestamp)
#       beta <- coef(model)[2]
#       eta <- beta
#       fitness[df$package_id == package_id] <- eta
#     }
#   }
#   return(fitness)
# }
# 
# # Ensure df has package_id and timestamps for the calculation
# df <- usage_dt
# 
# # Merge df with attributes_dt while allowing cartesian joins if needed
# df <- merge(df, attributes_dt[, .(package_id, degree)], by.x = "repo_id", by.y = "package_id", all.x = TRUE, allow.cartesian = TRUE)
# df <- df[order(repo_id, timestamp), ]
# 
# # Check the structure of df and ensure degree is numeric
# str(df)
# 
# # Ensure the degree column is numeric in df
# df[, degree := as.numeric(degree)]
# 
# # Verify the structure again
# str(df)
# 
# 
# 
# # Define the function to fit the degree growth model
# fit_degree_growth <- function(degree, timestamp) {
#   numeric_timestamp <- as.numeric(difftime(timestamp, min(timestamp), units = "days"))
#   
#   # Print debug information
#   # print(paste("Degree: ", degree))
#   # print(paste("Timestamp: ", numeric_timestamp))
#   
#   # Ensure we avoid log(0) and -Inf values
#   log_degree <- log(degree + 1)
#   log_timestamp <- log(numeric_timestamp + 1)
#   
#   # Print debug information
#   # print(paste("Log Degree: ", log_degree))
#   # print(paste("Log Timestamp: ", log_timestamp))
#   
#   model <- lm(log_degree ~ log_timestamp)
#   return(model)
# }
# 
# # Calculate fitness values for each package_id
# calculate_fitness <- function(subset_data) {
#   fitness <- rep(NA, nrow(subset_data))
#   
#   # Print debug information
#   # print("Subset Data:")
#   # print(subset_data)
#   
#   if (nrow(subset_data) > 1) {  # Ensure there are multiple timestamps
#     model <- fit_degree_growth(subset_data$degree, subset_data$timestamp)
#     beta <- coef(model)[2]
#     eta <- beta
#     
#     # Print debug information
#     print(paste("Beta: ", beta))
#     print(paste("Eta: ", eta))
#     
#     fitness <- eta
#   }
#   return(fitness)
# }
# 
# # Apply the fitness calculation across the unique repo_id values
# unique_repos <- unique(df$repo_id)
# fitness_values <- lapply(unique_repos, function(repo_id) {
#   subset_data <- df[repo_id == repo_id, ]
#   calculate_fitness(subset_data)
# })
# 
# # Flatten the list of fitness values
# fitness_values <- unlist(fitness_values)
# 
# # Assign the fitness values back to the original data table
# df[, fitness := fitness_values, by = repo_id]
# 
# # Check the resulting dataframe
# head(df)
# summary(df$fitness)
# 
# 
# 
# 
# 
# # # Create a fitness column initialized with NA
# # df[, fitness := NA_real_]
# # df[, fitness := as.numeric(fitness)]
# # head(df)
# # # Get unique repo_ids
# # unique_repos <- unique(df$repo_id)
# # 
# # # Iterate over each unique repo_id and calculate fitness
# # for (repo_id in unique_repos) {
# #   subset_data <- df[repo_id == df$repo_id, ]
# #   fitness_value <- calculate_fitness(subset_data)
# #   
# #   # Assign fitness value to the corresponding rows in df
# #   df[repo_id == df$repo_id, fitness := fitness_value]
# # }
# # 
# # # Check the resulting dataframe
# # head(df)
# # summary(df$fitness)
# 
# # Plot a histogram with more details
# hist(df$fitness, breaks = 50, main = "Histogram of df$fitness", xlab = "Fitness", col = "grey")
# 
# 
# # Correlation between fitness and growth measures
# cor(df$fitness, df$stars, use = "complete.obs")
# cor(df$fitness, df$forks, use = "complete.obs")
# cor(df$fitness, df$commits, use = "complete.obs")