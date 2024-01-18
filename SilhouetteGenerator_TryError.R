#Imports ----
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)

###Setup ----

#Setting directory and creatign list of files
directory<- "D:/Milosz_2023/Spectrograms/NN22/Free Moving/D3/efg/Different Channels"
file_list<- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)

#Looping through every file in the given directory, transforming and scaling the data
for (file in file_list) {
  #try method for continuing the loop if an error occurs
  result<- try({
    data<- read.csv(file)
    tdata<- t(data)
    sdata<- scale(tdata, center = TRUE, scale = TRUE)
    
    #Setting subsample size, reduce if sample size exceeds number of columns (time points)
    subsample_size<- min(200, ncol(sdata))  
    
    #Getting the file name for every file in the directory, initialising a PDF with that name
    file_name<- tools::file_path_sans_ext(basename(file))
    pdf_filename<- paste0(directory, "/", file_name, ".PDF")
    pdf(pdf_filename)
    
    #Setting a variable to store the optimal k from the silhouette plot, this is necessary for appending the name to the PDF title late
    sum_optimal_k<- 0
    
    #Main Loop ----
    #Looping 200 times, to generate 200 sub samples
    #Each loop will correspond to number 1 through 200, this will determine the seed used and the title of all graphs for replicability
    for (i in 1:200) {
      set.seed(i)
      sample_indices<- sample(1:ncol(sdata), subsample_size, replace=FALSE)
      subsample<- sdata[, sample_indices]
      
      #PCA analysis
      pca_result<- prcomp(subsample, center=TRUE, scale=TRUE)
      
      #Scree plot of PCA result
      var_explained<- pca_result$sdev^2 / sum(pca_result$sdev^2)
      scree_data<- data.frame(PC = 1:subsample_size, Variance = var_explained)
      scree_plot<- ggplot(scree_data, aes(x = PC, y = Variance)) + 
        geom_line() + 
        geom_point() + 
        xlab("Principal Component") + 
        ylab("Variance Explained") +
        ggtitle(paste("Scree Plot - Repetition", i)) +
        ylim(0, 1)
      print(scree_plot)
      
      #Biplot of PCA result
      bi_plot<- autoplot(pca_result, data = subsample, label = FALSE, loadings = TRUE, loadings.label = FALSE, loadings.label.size = 0) + 
        ggtitle(paste("Biplot - Repetition", i))
      print(bi_plot)
      
      #Silhouette Plot
      silhouette_plot<- fviz_nbclust(subsample, FUN = kmeans, method = "silhouette", k.max = 200, iter.max=100) +
        ggtitle(paste("Silhouette Plot - Repetition", i))
      #Adding breaks for readability
      breaks_seq<- seq(1, 200, by = 10)
      print(silhouette_plot + scale_x_discrete(breaks = breaks_seq))
      
      #Finding and saving the optimal k value from the silhouette analysis
      optimal_k<- silhouette_plot$data
      optimal_k<- as.numeric(optimal_k$clusters[which.max(optimal_k$y)])
      sum_optimal_k<- sum_optimal_k + optimal_k
      

    }
    
    #Creating a variable for the average optimal k across all 200 subsamples
    final_k<- sum_optimal_k / 200
    
    #Closing the PDF and renaming it to include the optimal k value in the name
    dev.off()
    final_pdf_filename<- paste0(directory, "/", file_name, " ", final_k, "k.PDF")
    file.rename(pdf_filename, final_pdf_filename)
  }, silent = TRUE)
  
  #continuing the try error method to handle exceptions
  #This will print which file caused the error
  if (inherits(result, "try-error")) {
    cat("An error occurred with the following file: ", file, "\nContinuing with the next file in the directory.\n")
  }
}