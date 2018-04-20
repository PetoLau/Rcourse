# PCA and other dimensionality reduction methods ----
# Pendigits dataset used:
# https://archive.ics.uci.edu/ml/datasets/Pen-Based+Recognition+of+Handwritten+Digits

library(data.table)
library(ggplot2)

datas <- fread("Exercises/pendigits_train.csv")
dim(datas)
str(datas)
table(datas$V17)

data_all <- datas[, 1:16, with = FALSE]
class <- datas[, 17]

# PCA ----
# 1. step normalise data (substract mean from every feature)
data_norm <- scale(data_all, center = TRUE, scale = FALSE)

# 2. step compute covariance matrix
cov_mat <- cov(data_norm)
dim(cov_mat)

# 3. step compute eigenvalues and eigenvectors
eig_mat <- eigen(cov_mat)
eig_mat$values
dim(eig_mat$vectors)

# 4. step extract first two eigenvectors and multiply them by normalised dataset.
# Be careful with matrix multiplication and dimensions! Use t() for transpose.
pca_mat <- t(t(eig_mat$vectors)[1:2,] %*% t(data_norm))

# visualise dataset with added class (labels)
ggplot(data.table(pca_mat, class = factor(class$V17)),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# verify (check) solution
pca_orig <- prcomp(datas, center = TRUE, scale. = FALSE)$x[,1:2]

ggplot(data.table(pca_orig, class = factor(class$V17)),
       aes(PC1, PC2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# equal, just rotated, becasuse prcomp uses SVD

# Multidimensional scaling ----
# classical MDS
# works with dissimilarity matrix only!

ids_sample <- sample(nrow(data_all), 1000, replace = F)
d <- dist(data_all[ids_sample], method = "manhattan") # use manhattan distance for example

mds_classical <- cmdscale(d, eig = FALSE, k = 2) # very slow, be aware!

ggplot(data.table(mds_classical, class = factor(class$V17[ids_sample])),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# nonmetric MDS
library(MASS)

mds_nonmetric <- isoMDS(d, k = 2)

ggplot(data.table(mds_nonmetric$points, class = factor(class$V17[ids_sample])),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# t-SNE ----
# stochastic method, very powerful method for finding non-normal structure in data
# "just" for visualisations
# again works with dissimilarities - any distance metric can be used - useful for text data etc.
library(Rtsne)

# perplexity default = 30
d_tsne_30 <- Rtsne(d, dims = 2, perplexity = 30,
                theta = 0.5, check_duplicates = TRUE, pca = FALSE, max_iter = 1000,
                verbose = TRUE, is_distance = TRUE)

ggplot(data.table(d_tsne_30$Y, class = factor(class$V17[ids_sample])),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# play with perplexity - 10
d_tsne_10 <- Rtsne(d, dims = 2, perplexity = 10,
                   theta = 0.5, check_duplicates = TRUE, pca = FALSE, max_iter = 1000,
                   verbose = TRUE, is_distance = TRUE)

ggplot(data.table(d_tsne_10$Y, class = factor(class$V17[ids_sample])),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()

# 50
d_tsne_50 <- Rtsne(d, dims = 2, perplexity = 50,
                   theta = 0.5, check_duplicates = TRUE, pca = FALSE, max_iter = 1000,
                   verbose = TRUE, is_distance = TRUE)

ggplot(data.table(d_tsne_50$Y, class = factor(class$V17[ids_sample])),
       aes(V1, V2, color = class)) +
  geom_point(alpha = 0.75) +
  theme_bw()
