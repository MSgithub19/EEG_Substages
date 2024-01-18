#In this script you will load in all of the data you wish to merge into the combined_spectrogram object

#### Condition 1 D7_R ----
D7_R_T1<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T1.csv', header = FALSE)
D7_R_T1<- t(D7_R_T1)
D7_R_T1<- scale(D7_R_T1, center = TRUE, scale = TRUE)

D7_R_T2<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T2.csv', header = FALSE)
D7_R_T2<- t(D7_R_T2)
D7_R_T2<- scale(D7_R_T2, center = TRUE, scale = TRUE)

D7_R_T3<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T3.csv', header = FALSE)
D7_R_T3<- t(D7_R_T3)
D7_R_T3<- scale(D7_R_T3, center = TRUE, scale = TRUE)

D7_R_T4<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T4.csv', header = FALSE)
D7_R_T4<- t(D7_R_T4)
D7_R_T4<- scale(D7_R_T4, center = TRUE, scale = TRUE)

D7_R_T5<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T5.csv', header = FALSE)
D7_R_T5<- t(D7_R_T5)
D7_R_T5<- scale(D7_R_T5, center = TRUE, scale = TRUE)

D7_R_T6<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T6.csv', header = FALSE)
D7_R_T6<- t(D7_R_T6)
D7_R_T6<- scale(D7_R_T6, center = TRUE, scale = TRUE)

D7_R_T7<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T7.csv', header = FALSE)
D7_R_T7<- t(D7_R_T7)
D7_R_T7<- scale(D7_R_T7, center = TRUE, scale = TRUE)

D7_R_T8<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D7/encode_NN6 R D7 T8.csv', header = FALSE)
D7_R_T8<- t(D7_R_T8)
D7_R_T8<- scale(D7_R_T8, center = TRUE, scale = TRUE)

#Set unique row identifiers to enable matching of rows from each timepoint to the combined_spectrograms
rownames(D7_R_T1)<- paste("D7_R_T1", rownames(D7_R_T1), sep = "_")
rownames(D7_R_T2)<- paste("D7_R_T2", rownames(D7_R_T2), sep = "_")
rownames(D7_R_T3)<- paste("D7_R_T3", rownames(D7_R_T3), sep = "_")
rownames(D7_R_T4)<- paste("D7_R_T4", rownames(D7_R_T4), sep = "_")
rownames(D7_R_T5)<- paste("D7_R_T5", rownames(D7_R_T5), sep = "_")
rownames(D7_R_T6)<- paste("D7_R_T6", rownames(D7_R_T6), sep = "_")
rownames(D7_R_T7)<- paste("D7_R_T7", rownames(D7_R_T7), sep = "_")
rownames(D7_R_T8)<- paste("D7_R_T8", rownames(D7_R_T8), sep = "_")


#### Condition 2 D8_R ----
D8_R_T2<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T2.csv', header = FALSE)
D8_R_T2<- t(D8_R_T2)
D8_R_T2<- scale(D8_R_T2, center = TRUE, scale = TRUE)

D8_R_T3<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T3.csv', header = FALSE)
D8_R_T3<- t(D8_R_T3)
D8_R_T3<- scale(D8_R_T3, center = TRUE, scale = TRUE)

D8_R_T4<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T4.csv', header = FALSE)
D8_R_T4<- t(D8_R_T4)
D8_R_T4<- scale(D8_R_T4, center = TRUE, scale = TRUE)

D8_R_T5<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T5.csv', header = FALSE)
D8_R_T5<- t(D8_R_T5)
D8_R_T5<- scale(D8_R_T5, center = TRUE, scale = TRUE)

D8_R_T6<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T6.csv', header = FALSE)
D8_R_T6<- t(D8_R_T6)
D8_R_T6<- scale(D8_R_T6, center = TRUE, scale = TRUE)

D8_R_T7<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T7.csv', header = FALSE)
D8_R_T7<- t(D8_R_T7)
D8_R_T7<- scale(D8_R_T7, center = TRUE, scale = TRUE)

D8_R_T8<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/R D8/encode_NN6 R D8 T8.csv', header = FALSE)
D8_R_T8<- t(D8_R_T8)
D8_R_T8<- scale(D8_R_T8, center = TRUE, scale = TRUE)

#Set unique row identifiers to enable matching of rows from each timepoint to the combined_spectrograms
#rownames(D8_R_T1)<- paste("D8_R_T1", rownames(D8_R_T1), sep = "_")
rownames(D8_R_T2)<- paste("D8_R_T2", rownames(D8_R_T2), sep = "_")
rownames(D8_R_T3)<- paste("D8_R_T3", rownames(D8_R_T3), sep = "_")
rownames(D8_R_T4)<- paste("D8_R_T4", rownames(D8_R_T4), sep = "_")
rownames(D8_R_T5)<- paste("D8_R_T5", rownames(D8_R_T5), sep = "_")
rownames(D8_R_T6)<- paste("D8_R_T6", rownames(D8_R_T6), sep = "_")
rownames(D8_R_T7)<- paste("D8_R_T7", rownames(D8_R_T7), sep = "_")
rownames(D8_R_T8)<- paste("D8_R_T8", rownames(D8_R_T8), sep = "_")


#### Condition 3 D7_FM ----
D7_FM_T1<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T1.csv', header = FALSE)
D7_FM_T1<- t(D7_FM_T1)
D7_FM_T1<- scale(D7_FM_T1, center = TRUE, scale = TRUE)

D7_FM_T2<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T2.csv', header = FALSE)
D7_FM_T2<- t(D7_FM_T2)
D7_FM_T2<- scale(D7_FM_T2, center = TRUE, scale = TRUE)

D7_FM_T3<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T3.csv', header = FALSE)
D7_FM_T3<- t(D7_FM_T3)
D7_FM_T3<- scale(D7_FM_T3, center = TRUE, scale = TRUE)

D7_FM_T4<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T4.csv', header = FALSE)
D7_FM_T4<- t(D7_FM_T4)
D7_FM_T4<- scale(D7_FM_T4, center = TRUE, scale = TRUE)

D7_FM_T5<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T5.csv', header = FALSE)
D7_FM_T5<- t(D7_FM_T5)
D7_FM_T5<- scale(D7_FM_T5, center = TRUE, scale = TRUE)

D7_FM_T6<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T6.csv', header = FALSE)
D7_FM_T6<- t(D7_FM_T6)
D7_FM_T6<- scale(D7_FM_T6, center = TRUE, scale = TRUE)

D7_FM_T7<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D7/encode_NN6 FM D7 T7.csv', header = FALSE)
D7_FM_T7<- t(D7_FM_T7)
D7_FM_T7<- scale(D7_FM_T7, center = TRUE, scale = TRUE)

#Set unique row identifiers to enable matching of rows from each timepoint to the combined_spectrograms
rownames(D7_FM_T1)<- paste("D7_FM_T1", rownames(D7_FM_T1), sep = "_")
rownames(D7_FM_T2)<- paste("D7_FM_T2", rownames(D7_FM_T2), sep = "_")
rownames(D7_FM_T3)<- paste("D7_FM_T3", rownames(D7_FM_T3), sep = "_")
rownames(D7_FM_T4)<- paste("D7_FM_T4", rownames(D7_FM_T4), sep = "_")
rownames(D7_FM_T5)<- paste("D7_FM_T5", rownames(D7_FM_T5), sep = "_")
rownames(D7_FM_T6)<- paste("D7_FM_T6", rownames(D7_FM_T6), sep = "_")
rownames(D7_FM_T7)<- paste("D7_FM_T7", rownames(D7_FM_T7), sep = "_")
#rownames(D7_FM_T8)<- paste("D7_FM_T8", rownames(D7_FM_T8), sep = "_")

#### Condition 4 D8_FM ----
D8_FM_T1<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T1.csv', header = FALSE)
D8_FM_T1<- t(D8_FM_T1)
D8_FM_T1<- scale(D8_FM_T1, center = TRUE, scale = TRUE)

D8_FM_T2<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T2.csv', header = FALSE)
D8_FM_T2<- t(D8_FM_T2)
D8_FM_T2<- scale(D8_FM_T2, center = TRUE, scale = TRUE)

D8_FM_T3<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T3.csv', header = FALSE)
D8_FM_T3<- t(D8_FM_T3)
D8_FM_T3<- scale(D8_FM_T3, center = TRUE, scale = TRUE)

D8_FM_T4<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T4.csv', header = FALSE)
D8_FM_T4<- t(D8_FM_T4)
D8_FM_T4<- scale(D8_FM_T4, center = TRUE, scale = TRUE)

D8_FM_T5<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T5.csv', header = FALSE)
D8_FM_T5<- t(D8_FM_T5)
D8_FM_T5<- scale(D8_FM_T5, center = TRUE, scale = TRUE)

D8_FM_T6<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T6.csv', header = FALSE)
D8_FM_T6<- t(D8_FM_T6)
D8_FM_T6<- scale(D8_FM_T6, center = TRUE, scale = TRUE)

D8_FM_T7<- read.csv('D:/Milosz_2023/Spectrograms/NN6/Autoencoder data/FM D8/encode_NN6 FM D8 T7.csv', header = FALSE)
D8_FM_T7<- t(D8_FM_T7)
D8_FM_T7<- scale(D8_FM_T7, center = TRUE, scale = TRUE)


#Set unique row identifiers to enable matching of rows from each timepoint to the combined_spectrograms
rownames(D8_FM_T1)<- paste("D8_FM_T1", rownames(D8_FM_T1), sep = "_")
rownames(D8_FM_T2)<- paste("D8_FM_T2", rownames(D8_FM_T2), sep = "_")
rownames(D8_FM_T3)<- paste("D8_FM_T3", rownames(D8_FM_T3), sep = "_")
rownames(D8_FM_T4)<- paste("D8_FM_T4", rownames(D8_FM_T4), sep = "_")
rownames(D8_FM_T5)<- paste("D8_FM_T5", rownames(D8_FM_T5), sep = "_")
rownames(D8_FM_T6)<- paste("D8_FM_T6", rownames(D8_FM_T6), sep = "_")
rownames(D8_FM_T7)<- paste("D8_FM_T7", rownames(D8_FM_T7), sep = "_")
#rownames(D8_FM_T8)<- paste("D8_FM_T8", rownames(D8_FM_T8), sep = "_")

#### Combine ----

#Combine the spectrograms into one object
#Make sure to edit this as not all experiments will have the same timepoints
combined_spectrograms <- rbind(D7_R_T1, D7_R_T2, D7_R_T3, D7_R_T4, D7_R_T5, D7_R_T6, D7_R_T7, D7_R_T8,
                               D7_FM_T1, D7_FM_T2, D7_FM_T3, D7_FM_T4, D7_FM_T5, D7_FM_T6, D7_FM_T7,
                               D8_R_T2, D8_R_T3, D8_R_T4, D8_R_T5, D8_R_T6, D8_R_T7, D8_R_T8,
                               D8_FM_T1, D8_FM_T2, D8_FM_T3, D8_FM_T4, D8_FM_T5, D8_FM_T6, D8_FM_T7)

saveRDS(combined_spectrograms, file= "D:/Milosz_2023/Spectrograms/NN6/combined_spectrograms.RDS")