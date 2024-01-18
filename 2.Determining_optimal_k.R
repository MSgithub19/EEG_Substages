library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)


#### Silhouette analysis for x replicates ----
#Load in the combined spectrogram
combined_spectrogram<- readRDS(file.choose())

#PCA analysis
pca_result<- prcomp(combined_spectrogram, center = TRUE, scale = TRUE)
#OR read in the result
pca_result<- readRDS(file.choose())

#Kaiser criterion for PC determination
eigenvalues<- pca_result$sdev^2
kaiser<- sum(eigenvalues > 1)

#Selecting principal components based on Kaiser criterion
PC<- pca_result$x[, 1:kaiser]
n_col<- as.integer(ncol(PC))
#Setting sample size to number of PCs
subsample_size<- n_col

#Variable to store the sum of optimal k values
sum_optimal_k<- 0

#Looping to get different subsamples
#The seed changes every time, so even though the code is identical every time the outcome will be different
optimal_k_values<- vector()
for (i in 1:50) {
  set.seed(i)
  sample_indices<- sample(1:ncol(combined_spectrogram), subsample_size, replace = FALSE)
  subsample<- combined_spectrogram[, sample_indices]
  
  #Only need silhouette here
  #Silhouette Plot
  silhouette_plot<- fviz_nbclust(subsample, FUN = kmeans, method = "silhouette", k.max = subsample_size, iter.max=30) +
    ggtitle(paste("Silhouette Plot - Repetition", i))
  #Adding breaks for readability
  breaks_seq<- seq(1, subsample_size, by = 10)
  print(silhouette_plot + scale_x_discrete(breaks = breaks_seq))
  
  #Finding and saving the optimal k value from the silhouette analysis
  optimal_k<- silhouette_plot$data
  optimal_k<- as.numeric(optimal_k$clusters[which.max(optimal_k$y)])
  sum_optimal_k<- sum_optimal_k + optimal_k
  #Appending the optimal k value for each iteration of my loop
  optimal_k_values<- c(optimal_k_values, optimal_k)
}

#Calculating and returning the average value of k
final_k<- sum_optimal_k / 50

final_k

saveRDS(final_k, file= "D:/Milosz_2023/Spectrograms/NN6/final_k.RDS")

saveRDS(pca_result, file = "D:/Milosz_2023/Spectrograms/NN6/pca_result.RDS")



#### Silhouette analysis where the data is too large for the above loop ----
#When combining multiple spectrograms and with a higher range of k.max it takes significantly longer
#to run the standard loop
#The below method will utilise all of the computers CPU cores to perform as many simultaneous k means calculations as possible
#May require some adjustment


library(doParallel)
library(foreach)

detectCores()
registerDoParallel(cores=8)

subsample_size <- 600

k_range <- 100:500
subsample_size <- 600

set.seed(123)
sample_indices <- sample(1:nrow(combined_spectrogram), subsample_size, replace = FALSE)
subsample <- combined_spectrogram[sample_indices, ]

#Performing parallel silhouette analysis
results <- foreach(k = k_range, .combine = rbind, .packages = c("cluster", "factoextra")) %dopar% {
  clust <- kmeans(subsample, centers = k, nstart = 25)
  silhouette_score <- silhouette(clust$cluster, dist(subsample))
  avg_width <- mean(silhouette_score[, "sil_width"])
  data.frame(k = k, avg_sil_width = avg_width)
}

#Storing the optimal k
optimal_k <- results$k[which.max(results$avg_sil_width)]





#### Performing the actual k-means operation ----

combined_kmeans_result <- kmeans(PC, centers=215, nstart=25, iter.max= 30)

saveRDS(combined_kmeans_result, file = "D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/combined_kmeans_result.RDS")