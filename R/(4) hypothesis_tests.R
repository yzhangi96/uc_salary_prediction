## -------------------------------------------
##
## Script name: hypothesis_test
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
library(comprehenr)
library(perm)

# Local imports ----
cur_wd <- getwd()
source(paste0(cur_wd,"/constants.R"))
final_data <- read.csv(paste0(cur_wd,"/final_data.csv"))[,-1]

# Average wages between men and women ----
final_data$pred_male <- factor(final_data$pred_male)

# Is there a statistically significant difference
# in average wages between men and women?

table(final_data$pred_male)
hist(final_data$Total.Pay) # Approx normal?

# p-value = 0.1421. Fail to reject the null that the average difference is 0
t.test(x = final_data$Total.Pay[which(final_data$pred_male == 1)], 
       y = final_data$Total.Pay[which(final_data$pred_male == 0)])

# What if we only included tenure track?
tenure <- final_data[which(final_data$tenure_track == "True"),]

# p-value = 0.01401 Reject the null that the average difference is 0
t.test(x = tenure$Total.Pay[tenure$pred_male == 1], 
       y = tenure$Total.Pay[tenure$pred_male == 0])
hist(tenure$Total.Pay) # Not exactly normal... a bit right skewed 

# Using exact permutation test (estimated by MC)
# p-value = 0.02 Reject the null. Similar result as from t-test
permTS(tenure$Total.Pay ~ tenure$pred_male, alternative="two.sided", exact=T)

# What if we only looked at the highest quartile h-index?
hindex_q4 <- quantile(tenure$h_index)[4]

# p-value = 0.04207. Reject the null
permTS(tenure$Total.Pay[tenure$h_index >= hindex_q4] ~ 
         tenure$pred_male[tenure$h_index >= hindex_q4], 
       alternative="two.sided", exact=T)

# What if we only looked at the youngest / oldest age group?
first_pub_q4 <- quantile(tenure$year_first_pub)[4]
first_pub_q1 <- quantile(tenure$year_first_pub)[1]

# p-value = 0.074. Fail to reject the null for oldest age group
permTS(tenure$Total.Pay[tenure$year_first_pub >= first_pub_q4] ~ 
         tenure$pred_male[tenure$year_first_pub >= first_pub_q4], 
       alternative="two.sided", exact=T)

# p-value = 0.02. Reject the null for youngest age group
permTS(tenure$Total.Pay[tenure$year_first_pub >= first_pub_q1] ~ 
         tenure$pred_male[tenure$year_first_pub >= first_pub_q1], 
       alternative="two.sided", exact=T)

# Gender ratio ----

# Is there a statistically significant difference
# in number of men and women in top 2 (rank 2 and 13)
# vs the lower ranked schools?

final_data$top_2 <- with(final_data, ifelse(rank < 15, 1, 0))

# p-value = 0.02725. Reject the null that there is no relationship between
# tenure faculty gender ratio and top 2 v bottom 2 rank
fisher.test(table(final_data$pred_male, final_data$top_2))
