library(dplyr)
library(igraph)
library(tibble) 

####All clusters, one condition ----

#Load the combined kmeans results
combined_kmeans_result<- readRDS(file.choose())

#Convert the kmeans results to a dataframe and create a timepoint column from row names (necessary as the original format is a tibbl which is harder to work with)
data<- as.data.frame(combined_kmeans_result$cluster) %>%
  rownames_to_column(var = "timepoint")

#Renaming the cluster column to be consistent after the transformation from tibbl to data frame
colnames(data)[2]<- "cluster"

#Filter data for a specific condition, e.g., "D7_R"
#If you wish to filter by time point add D7_R_T1
data<- data %>%
  filter(grepl("D7_R", timepoint))

#adding a "leading cluster" column, so we can see which clusters/nodes/vertices are interacting 
data<- data %>%
  mutate(lead_cluster = lead(cluster, order_by = timepoint))

#Filtering out any NAs arising from above transformation
data<- na.omit(data)

#Creating an edge list for the specified condition
edge_list<- data %>%
  group_by(cluster, lead_cluster) %>%
  summarise(count = n(), .groups = "drop")

#Creating a graph from said edge list
g_filtered<- graph_from_data_frame(edge_list, directed = TRUE)


#Calculating centrality scores (useful for setting vertex size as it is proportional to the number of interactions)
centrality_scores<- degree(g_filtered)
print(centrality_scores)

#Setting max and min size for nodes/vertices
max_size<- 18
min_size<- 6

#Normalizing vertex size to within range of max and min size
normalised_scores<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_size - min_size) + min_size

#Setting max and min size for node/vertex label size
max_label_size<- 1.4
min_label_size<- 0.7
normalised_label_sizes<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_label_size - min_label_size) + min_label_size

#Choosing a layout algorithm
#layout_with_mds is good for expressing meaning through edge length
#layout_in_circle is nice for visualising busy plots
layout<- layout_in_circle(g_filtered)
plot(g_filtered, layout = layout, edge.arrow.size = 0.2, vertex.size = normalised_scores,
     vertex.label.cex = normalised_label_sizes)

#Finding communities and re-plotting with said communities
#As with the layouts, there are multiple choices for communities
#Note: Communities are not always relevant, for example if we use layout_in_circle, we can't expect much meaning if we overlay communities on that plot
communities<- cluster_walktrap(g_filtered)
plot(communities, g_filtered, vertex.size = normalised_scores, edge.arrow.size = 0.2, 
     vertex.label.cex = normalised_label_sizes, vertex.label.color = "black")






####Common clusters, one condition ----

#Load the combined kmeans results
combined_kmeans_result<- readRDS(file.choose())


#Convert the kmeans results to a dataframe and create a timepoint column from row names (necessary as the original format is a tibbl which is harder to work with)
data<- as.data.frame(combined_kmeans_result$cluster) %>%
  rownames_to_column(var = "timepoint")

#Renaming the cluster column to be consistent after the transformation from tibbl to data frame
colnames(data)[2]<- "cluster"

#Filter data for a specific condition, e.g., "D7_R"
#If you wish to filter by time point add D7_R_T1
data<- data %>%
  filter(grepl("D8_R", timepoint))

#adding a "leading cluster" column, so we can see which clusters/nodes/vertices are interacting 
data<- data %>%
  mutate(lead_cluster = lead(cluster, order_by = timepoint))

#Filtering out any NAs arising from above transformation
data<- na.omit(data)

#Insert the common clusters from part 3.
common_clusters<- c(7, 8, 13, 18, 21, 24, 33, 35, 39, 44, 50, 55, 61, 65, 70, 77, 80, 82, 95, 97, 109, 113, 116, 121, 126, 128, 130, 131, 132, 133, 135, 143, 149, 155, 157, 166, 167, 170, 171, 176, 185, 188, 211, 219, 221, 224, 227, 228, 234, 240, 242, 247, 263, 275, 293, 294, 304, 305, 309, 318, 323, 325, 328, 333)

#Creating an edge list for the specific condition
edge_list<- data %>%
  group_by(cluster, lead_cluster) %>%
  summarise(count = n(), .groups = "drop")


#Filtering the edge list for common clusters ONLY
filtered_edge_list<- edge_list %>%
  filter(cluster %in% common_clusters & lead_cluster %in% common_clusters)

#Creating a graph from said edge list
g_filtered<- graph_from_data_frame(filtered_edge_list, directed = TRUE)


#Calculating centrality scores (useful for setting vertex size as it is proportional to the number of interactions)
centrality_scores<- degree(g_filtered)
print(centrality_scores)

#Setting max and min size for nodes/vertices
max_size<- 18
min_size<- 6

#Normalizing vertex size to within range of max and min size
normalised_scores<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_size - min_size) + min_size

#Setting max and min size for node/vertex label size
max_label_size<- 1.4
min_label_size<- 0.7
normalised_label_sizes<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_label_size - min_label_size) + min_label_size

#Choosing a layout algorithm
#layout_with_mds is good for expressing meaning through edge length
#layout_in_circle is nice for visualising busy plots
layout<- layout_in_circle(g_filtered)
plot(g_filtered, layout = layout, edge.arrow.size = 0.2, vertex.size = normalised_scores, 
     vertex.label.cex = normalised_label_sizes)



#Finding communities and re-plotting with said communities
#As with the layouts, there are multiple choices for communities
#Note: Communities are not always relevant, for example if we use layout_in_circle, we can't expect much meaning if we overlay communities on that plot
communities<- cluster_walktrap(g_filtered)
plot(communities, g_filtered, vertex.size = normalised_scores, edge.arrow.size = 0.2,
     vertex.label.cex = normalised_label_sizes, vertex.label.color = "black")






####Colour select clusters ----

#Convert the kmeans results to a dataframe and create a timepoint column from row names (necessary as the original format is a tibbl which is harder to work with)
data<- as.data.frame(combined_kmeans_result$cluster) %>%
  rownames_to_column(var = "timepoint")


#Renaming the cluster column to be consistent after the transformation from tibbl to data frame
colnames(data)[2]<- "cluster"

#Optional, select which data to show. In grep, use " | " for OR. 
data<- data %>%
  filter(grepl("D8_R", timepoint))

#adding a "leading cluster" column, so we can see which clusters/nodes/vertices are interacting 
data<- data %>%
  mutate(lead_cluster = lead(cluster, order_by = timepoint))

#Filtering out any NAs arising from above transformation
data<- na.omit(data)


#Creating an edge list for the specific condition
edge_list<- data %>%
  group_by(cluster, lead_cluster) %>%
  summarise(count = n(), .groups = "drop")



#Creating a graph from said edge list
g<- graph_from_data_frame(edge_list, directed = TRUE)


#Calculating centrality scores (useful for setting vertex size as it is proportional to the number of interactions)
centrality_scores<- degree(g)
print(centrality_scores)

#Setting max and min size for nodes/vertices
max_size<- 18
min_size<- 6

#Normalizing vertex size to within range of max and min size
normalised_scores<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_size - min_size) + min_size

#Setting max and min size for node/vertex label size
max_label_size<- 1.4
min_label_size<- 0.7
normalised_label_sizes<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_label_size - min_label_size) + min_label_size


colour_clusters<- c(7, 8, 13, 18, 21, 24, 33, 35, 39, 44, 50, 55, 61, 65, 70, 77, 80, 82, 95, 97, 109, 113, 116, 121, 126, 128, 130, 131, 132, 133, 135, 143, 149, 155, 157, 166, 167, 170, 171, 176, 185, 188, 211, 219, 221, 224, 227, 228, 234, 240, 242, 247, 263, 275, 293, 294, 304, 305, 309, 318, 323, 325, 328, 333)

V(g)$color<- ifelse(V(g)$name %in% colour_clusters, "red", "grey")

#Color edges connected to colour_clusters red
#First, find the edges where either or both vertices are in colour_clusters
edges_to_color<- which(sapply(E(g), function(e) {
  ends<- ends(g, e, names = TRUE)
  any(ends %in% colour_clusters)
}))

E(g)$color<- "grey" #Default color for all edges
E(g)$color[edges_to_color]<- "red" #Color the selected edges red


#Choosing a layout algorithm
#layout_with_mds is good for expressing meaning through edge length
#layout_in_circle is nice for visualising busy plots
layout<- layout_in_circle(g)
plot(g, layout = layout, edge.arrow.size = 0.2, vertex.size = normalised_scores, vertex.label.color = "black",
     vertex.label.cex = normalised_label_sizes, vertex.color = V(g)$color, edge.color = E(g)$color)





####Get community network map ----
data<- as.data.frame(combined_kmeans_result$cluster) %>%
  rownames_to_column(var = "timepoint")



#Renaming the cluster column to be consistent after the transformation from tibbl to data frame
colnames(data)[2]<- "cluster"

#adding a "leading cluster" column, so we can see which clusters/nodes/vertices are interacting 
data<- data %>%
  mutate(lead_cluster = lead(cluster, order_by = timepoint))

#Filtering out any NAs arising from above transformation
data<- na.omit(data)


#Creating an edge list for the specific condition
edge_list<- data %>%
  group_by(cluster, lead_cluster) %>%
  summarise(count = n(), .groups = "drop")



#Creating a graph from said edge list
g<- graph_from_data_frame(edge_list, directed = TRUE)



#Communities
#_________________________________________________________________________
#Then we extract the community data and save it in a data frame

communities<- cluster_walktrap(g)
#Creating a new column named "Node" with values corresponding to the vertices from the original graph, g
community_data<- data.frame(node = V(g)$name, community = communities$membership)

#converting node to integer as our nodes are just numbered
community_data<- community_data %>%
  mutate(node = as.integer(node))

#Transforming the edge list for our original network into an edge list at the community level
#Assigning each node to its community and creating a new a new graph where each node represents a community
#left join technique is used a few times here, this means that every row from table x is 
#included in the final result, regardless if the tables being joined have matching rows
community_edges<- edge_list %>%
  left_join(community_data, by = c("cluster" = "node")) %>%
  #renaming to from_community so we can identify transitions
  rename(from_community = community) %>%
  left_join(community_data, by = c("lead_cluster" = "node")) %>%
  rename(to_community = community) %>%
  group_by(from_community, to_community) %>%
  #calculating an edge for each new "node" in the graph
  summarise(weight = n(), .groups = "drop")

community_graph<- graph_from_data_frame(community_edges, directed = TRUE)

#Calculating centrality scores in order to adjust vertex size based on interactions, centrality proportional to interactions
centrality_scores<- degree(community_graph)

#Setting max and min size for nodes/vertices
max_size<- 18
min_size<- 6

#Normalizing vertex size to within range of max and min size
normalised_scores<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_size - min_size) + min_size

#Setting max and min size for node/vertex label size
max_label_size<- 1.4
min_label_size<- 0.7
normalised_label_sizes<- (centrality_scores - min(centrality_scores)) / 
  (max(centrality_scores) - min(centrality_scores)) * 
  (max_label_size - min_label_size) + min_label_size

#setting max edge width
max_edge_width<- 15

#Combining the community edges and changing thickness to reflect aggregate size
community_edges_aggregated<- community_edges %>%
  group_by(from_community, to_community) %>%
  summarise(weight = sum(weight), .groups = "drop")

#Creating a new graph with the aggregated/filtered edge list
community_graph<- graph_from_data_frame(community_edges_aggregated, directed = TRUE)

#Normalize the edge weights for the new graph
normalised_edge_width<- community_edges_aggregated$weight / max(community_edges_aggregated$weight) * max_edge_width



layout<- layout_in_circle(community_graph)

plot(community_graph, layout = layout, vertex.size = normalised_scores, vertex.label.color = "black",
     edge.width = normalised_edge_width, edge.arrow.size = 0.2, vertex.label.cex = normalised_label_sizes)













####Sorting clusters ----

#This is a little automation to help combine different lists of clusters

set1<- c(1, 4, 5, 8, 9, 18, 25, 32, 36, 38, 39, 48, 49, 56, 58, 62, 67, 70, 71, 80, 82, 87, 88, 90, 92, 93, 98, 100, 101, 109, 124, 126, 127, 129, 133, 134, 135, 137, 141, 146, 147, 150, 153, 154, 156, 157, 159, 160, 163, 164, 165, 166, 173, 180, 181, 182, 192, 197, 198, 202, 203, 206, 207, 213, 214, 216, 226, 229, 230, 231, 232, 235, 240, 242, 253, 261, 262, 264, 273, 277, 278, 283, 289, 299, 300, 304, 308, 310, 313, 314, 316, 318, 319, 324, 327, 328, 329, 337, 342)
set2<- c(7, 8, 13, 18, 21, 24, 33, 35, 39, 44, 50, 55, 61, 65, 70, 77, 80, 82, 95, 97, 109, 113, 116, 121, 126, 128, 130, 131, 132, 133, 135, 143, 149, 155, 157, 166, 167, 170, 171, 176, 185, 188, 211, 219, 221, 224, 227, 228, 234, 240, 242, 247, 263, 275, 293, 294, 304, 305, 309, 318, 323, 325, 328, 333)

combined_sorted_set<- sort(unique(c(set1, set2)))
cat(paste(combined_sorted_set, collapse = ", "))