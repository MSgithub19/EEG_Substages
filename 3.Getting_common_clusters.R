#Reading in previous data ----
combined_kmeans_result<- readRDS(file.choose())

#### This will get clusters which are common between 2 conditions ----

#Function to get clusters based on condition and timepoint from the row name
extract_clusters_by_condition <- function(condition, cluster_list) {
  #Filtering the list by condition
  condition_clusters <- cluster_list[grep(condition, names(cluster_list))]
  #Returning the unique cluster assignments
  unique(unlist(condition_clusters))
}

#Function to find common clusters between conditions
find_common_clusters <- function(condition1, condition2, cluster_list) {
  #Extracting clusters for each condition 
  #This is for a two way comparison, if for whatever reason you want more you will have to add another line here
  clusters_condition1 <- extract_clusters_by_condition(condition1, cluster_list)
  clusters_condition2 <- extract_clusters_by_condition(condition2, cluster_list)
  
  #Finding the common clusters with intersect method
  intersect(clusters_condition1, clusters_condition2)
}


#This will find clusters common to D7_R and D8_R, modify as appropriate
common_clusters <- find_common_clusters("D7_R", "D8_R", combined_kmeans_result$cluster)
print(common_clusters)
print(length(common_clusters))




#### This will get common clusters within conditions----

#Function to extract common clusters across all timepoints for a given condition
find_common_clusters_with_threshold <- function(condition, cluster_list, min_timepoints = NULL) {
  #Filtering out the names that match the condition
  condition_names <- names(cluster_list)[grepl(condition, names(cluster_list))]
  
  #Getting unique timepoints
  timepoints <- unique(gsub(".*_(T\\d+)_.*", "\\1", condition_names))
  
  #N-2 style approach for selecting how many timepoints a cluster must be present in to be considered common
  #E.g if we set n-2 then a cluster is considered common if it is present in 5 out of 7
  if (is.null(min_timepoints)) {
    min_timepoints <- length(timepoints) - 2 #n-2
  }
  
  #Extracting cluster numbers for each timepoint and store them in a list
  clusters_per_timepoint <- lapply(timepoints, function(tp) {
    #Finding names that contain the specific timepoint
    tp_names <- condition_names[grepl(tp, condition_names)]
    #Returning the unique clusters for those names
    unique(cluster_list[tp_names])
  })
  
  #Creating a table of cluster frequencies across timepoints and finding clusters that are present in at least "min_timepoints" timepoints
  cluster_freq <- table(unlist(clusters_per_timepoint))
  clusters_present_enough <- names(cluster_freq[cluster_freq >= min_timepoints])
  as.numeric(clusters_present_enough)
}


#Finding common clusters for a given condition, with the threshold allowance
common_clusters_D7_R <- find_common_clusters_with_threshold("D8_FM", combined_kmeans_result$cluster)


print(paste(common_clusters_D7_R, collapse = ", "))
print(paste("Number of common clusters:", length(common_clusters_D7_R)))


#### Getting common clusters between conditions, alternative method----
#This method uses the common clusters identified above to match between two conditions

#Insert common clusters from above
common_clusters_D7_R <- c(1, 4, 5, 8, 9, 18, 25, 32, 36, 38, 39, 48, 49, 56, 58, 62, 67, 70, 71, 80, 82, 87, 88, 90, 92, 93, 98, 100, 101, 109, 124, 126, 127, 129, 133, 134, 135, 137, 141, 146, 147, 150, 153, 154, 156, 157, 159, 160, 163, 164, 165, 166, 173, 180, 181, 182, 192, 197, 198, 202, 203, 206, 207, 213, 214, 216, 226, 229, 230, 231, 232, 235, 240, 242, 253, 261, 262, 264, 273, 277, 278, 283, 289, 299, 300, 304, 308, 310, 313, 314, 316, 318, 319, 324, 327, 328, 329, 337, 342)

common_clusters_D8_R <- c(7, 8, 13, 18, 21, 24, 33, 35, 39, 44, 50, 55, 61, 65, 70, 77, 80, 82, 95, 97, 109, 113, 116, 121, 126, 128, 130, 131, 132, 133, 135, 143, 149, 155, 157, 166, 167, 170, 171, 176, 185, 188, 211, 219, 221, 224, 227, 228, 234, 240, 242, 247, 263, 275, 293, 294, 304, 305, 309, 318, 323, 325, 328, 333)

common_clusters_D7_FM <- c(5, 6, 8, 10, 11, 12, 13, 15, 16, 17, 19, 21, 22, 24, 25, 30, 33, 35, 36, 38, 39, 40, 41, 42, 44, 46, 47, 49, 50, 51, 53, 56, 58, 59, 62, 64, 65, 66, 67, 68, 69, 72, 75, 76, 77, 80, 82, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 95, 96, 98, 99, 100, 101, 106, 108, 109, 110, 111, 112, 113, 115, 116, 117, 118, 119, 120, 123, 124, 125, 126, 127, 129, 130, 131, 132, 133, 134, 135, 137, 139, 140, 143, 145, 146, 147, 149, 150, 154, 155, 158, 160, 161, 162, 163, 164, 167, 168, 171, 172, 174, 175, 176, 177, 178, 180, 182, 183, 184, 185, 188, 189, 190, 191, 192, 193, 194, 196, 197, 198, 201, 208, 210, 211, 214, 215, 216, 217, 220, 221, 222, 225, 226, 228, 229, 231, 232, 234, 235, 236, 239, 240, 241, 242, 245, 247, 249, 251, 253, 254, 259, 261, 262, 264, 269, 271, 272, 274, 275, 276, 278, 280, 283, 284, 288, 292, 293, 294, 295, 296, 297, 298, 300, 302, 304, 307, 308, 313, 317, 319, 320, 321, 323, 324, 326, 328, 330, 333, 335, 337, 338, 341, 342, 343, 344, 345)

common_clusters_D8_FM <- c(4, 5, 7, 8, 10, 11, 16, 18, 19, 21, 24, 26, 29, 36, 39, 40, 41, 42, 44, 45, 47, 48, 50, 52, 53, 57, 58, 60, 61, 62, 63, 65, 68, 69, 72, 73, 75, 76, 77, 80, 85, 87, 88, 91, 93, 94, 95, 97, 98, 99, 101, 102, 103, 105, 106, 108, 111, 113, 114, 116, 117, 121, 122, 124, 128, 132, 134, 136, 137, 143, 145, 146, 147, 148, 151, 153, 154, 156, 157, 161, 162, 164, 165, 166, 167, 170, 171, 173, 174, 175, 178, 180, 181, 184, 188, 189, 190, 191, 192, 200, 201, 204, 205, 206, 207, 208, 210, 211, 212, 214, 216, 217, 219, 221, 224, 225, 228, 230, 233, 234, 236, 239, 240, 241, 245, 246, 247, 248, 249, 250, 251, 255, 258, 259, 260, 262, 263, 268, 269, 271, 273, 275, 276, 278, 281, 284, 288, 289, 290, 292, 293, 295, 296, 299, 302, 304, 310, 313, 316, 317, 319, 320, 321, 324, 325, 326, 328, 329, 330, 332, 334, 335, 336, 337, 338, 342, 343, 344)



#Using intersect function will return values present in both vectors
common_clusters <- intersect(common_clusters_D8_FM, common_clusters_D8_R) #Change these depending on which common clusters you want to get

print(paste(common_clusters, collapse = ", "))
print(paste("Number of common clusters:", length(common_clusters)))


