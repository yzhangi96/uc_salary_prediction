## -------------------------------------------
##
## Script name: eda
##
## Author: Ivy Zhang
##
## Date Last Modified: 2020-02-16
## Last Modified by: Ivy Zhang
##
## -------------------------------------------
##
## Notes:  
##  
##  
## -------------------------------------------

# Import packages ----
library(ggplot2)
library(gridExtra)
library(Rtsne)
library(ggcorrplot)
library(ggfortify)

# Local imports ----
cur_wd <- getwd()
source(paste0(cur_wd,"/constants.R"))
final_data <- read.csv(paste0(cur_wd,"/final_data.csv"))[,-1]

# Helper functions ----
'%!in%' <- function(x,y)!('%in%'(x,y))

# Salary vs covariates ----
final_data$pred_male <- as.factor(final_data$pred_male)

# 70% male
round(table(final_data$pred_male)/nrow(final_data),2)

# Pay vs citations
ranks <- unique(final_data$rank)
plots <- list()
for(i in 1:length(ranks)){
  subset_data <- final_data[final_data$rank == ranks[i],]
  plots[[i]] <- ggplot(subset_data, 
         aes(x=cited_by, y=Total.Pay, shape=pred_male, color=pred_male)) +
    geom_point() +
    labs(title=paste0("Pay vs citations by gender for rank ", ranks[i]),
         x="Total Google Scholar Citations", y = "2021 Pay")
}
plots[[length(ranks)+1]] <- 
  ggplot(final_data, 
         aes(x=cited_by, y=Total.Pay, shape=pred_male, color=pred_male)) +
  geom_point() +
  labs(title="Pay vs citations by gender (ALL)",
       x="Total Google Scholar Citations", y = "2021 Pay")

do.call(grid.arrange,plots)

# Pay vs h-index
plots <- list()
for(i in 1:length(ranks)){
  subset_data <- final_data[final_data$rank == ranks[i],]
  plots[[i]] <- ggplot(subset_data, 
                       aes(x=h_index, y=Total.Pay, shape=pred_male, 
                           color=pred_male)) + geom_point() +
    labs(title=paste0("Pay vs h-index for rank ",
                      ranks[i]),
         x="h-index", y = "2021 Pay")
}
plots[[length(ranks)+1]] <- 
  ggplot(final_data, 
         aes(x=h_index, y=Total.Pay, shape=pred_male, color=pred_male)) +
  geom_point() +
  labs(title="Pay vs h-index by gender (ALL)",
       x="h-index", y = "2021 Pay")

do.call(grid.arrange,plots)

# Pay vs count_pub
plots <- list()
for(i in 1:length(ranks)){
  subset_data <- final_data[final_data$rank == ranks[i],]
  plots[[i]] <- ggplot(subset_data, 
                       aes(x=count_pub, y=Total.Pay, shape=pred_male, 
                           color=pred_male)) + geom_point() +
    labs(title=paste0("Pay vs total publications for rank ",
                      ranks[i]),
         x="Total publications", y = "2021 Pay")
}
plots[[length(ranks)+1]] <- 
  ggplot(final_data, 
         aes(x=count_pub, y=Total.Pay, shape=pred_male, color=pred_male)) +
  geom_point() +
  labs(title="Pay vs total publications yr by gender (ALL)",
       x="Total publications", y = "2021 Pay")

do.call(grid.arrange,plots)

# Correlation
# Linear reg likely won't give great results
tenure_data <- final_data[final_data$tenure_track == "True",]
num_data <- scale(apply(
  subset(tenure_data, select= c(num_covariates, y)), 2, as.numeric
))
num_data_m <- num_data[tenure_data$pred_male == 1,]
num_data_f <- num_data[tenure_data$pred_male == 0,]

male_cor <- ggcorrplot(cor(num_data_m),
                           type = "lower", lab = TRUE,
) + labs(title = "Male") + theme(axis.text.y=element_blank())

fem_cor <- ggcorrplot(cor(num_data_f),
                           type = "lower", lab = TRUE,
) + labs(title = "Female") + theme(axis.text.y=element_blank())

grid.arrange(fem_cor, male_cor, nrow=1)


# K-means clustering

clusters <- kmeans(x = num_data, centers = 3)
num_data_y <- data.frame(num_data, pred_male = factor(tenure_data$pred_male))
num_data_y$cluster <- factor(clusters$cluster)

# Only one in cluster 3. Exclude for visualizations
table(num_data_y$cluster)
num_data_c <- droplevels(num_data_y[num_data_y$cluster != 3,])

# Cluster cleanly separates on scaled # of publications / h-index (related)...
all_count_pub <- ggplot() +
  geom_point(data = num_data_c, 
             mapping = aes(x = Total.Pay, 
                           y = count_pub, 
                           colour = cluster)) +
  labs(y = "Total publications", x = "2021 Pay")

# .. But not so much on other covariates
all_yr_pub <- ggplot() +
  geom_point(data = num_data_c, 
             mapping = aes(x = Total.Pay, 
                           y = year_first_pub, 
                           colour = cluster)) +
  labs(y = "Year of first publication", x = "2021 Pay")

grid.arrange(all_count_pub, all_yr_pub, nrow=1)

# Cluster v predicted gender proportions
# 10% difference between men and women in cluster 1
prop.table(table(num_data_y$cluster, num_data_y$pred_male), 1)

# Dimension reduction visualizations ----

# While these are difficult to interpret, can get a sense of whether there 
# might be separability by gender that isn't easily seen from 1x1 plots

# Linear visualization 
# Also not much

autoplot(
  prcomp(num_data), 
  data = num_data_y, 
  colour = 'pred_male'
  ) + labs(title ="PCA")

# Non linear visualization
set.seed(123)
plots <- list()

perp <- c(5, 10, 15, 20, 25, 30)

# Tune perplexity 
for(i in 1:length(perp)){
  tsne_out <- Rtsne(num_data, perplexity = i, max_iter = 5000)
  tsne_data <- data.frame(
    x = tsne_out$Y[,1], 
    y = tsne_out$Y[,2], 
    pred_male = factor(tenure_data$pred_male)
    )
  plots[[i]] <-ggplot(tsne_data, 
                      aes(x=x, y=y, color=pred_male)) +
    geom_point() +
    labs(title=paste0("TSNE perplexity: ",perp[i]))
}

# Not a clear separation by gender
do.call(grid.arrange,plots)


# Appendix ----

# Pay vs first publication year (age proxy)
plots <- list()
for(i in 1:length(ranks)){
  subset_data <- final_data[final_data$rank == ranks[i],]
  plots[[i]] <- ggplot(subset_data, 
                       aes(x=year_first_pub, y=Total.Pay, shape=pred_male, 
                           color=pred_male)) + geom_point() +
    labs(title=paste0("Pay vs first publication yr for rank ",
                      ranks[i]),
         x="Year of first publication", y = "2021 Pay")
}
plots[[length(ranks)+1]] <- 
  ggplot(final_data, 
         aes(x=year_first_pub, y=Total.Pay, shape=pred_male, color=pred_male)) +
  geom_point() +
  labs(title="Pay vs first publication yr by gender (ALL)",
       x="Year of first publication", y = "2021 Pay")

do.call(grid.arrange,plots)