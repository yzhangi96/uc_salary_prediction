## -------------------------------------------
##
## Script name: prep_data2
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

# Local imports ----
cur_wd <- getwd()
source(paste0(cur_wd,"/constants.R"))
final_data <- read.csv(paste0(cur_wd,"/google_scholar_uc_faculty.csv"))[,-c(1,2)]
additional <- read.csv(paste0(cur_wd,"/additional.csv"))[,-1]

# Helper functions ----
'%!in%' <- function(x,y)!('%in%'(x,y))

# Final clean up ----
colnames(final_data)[colnames(final_data) == "pred"] <- "pred_male"

# Manual adjustments for missing sex prediction
final_data$pred_male[final_data$name == "Adityanand Guntuboyina"] <- 1
final_data$pred_male[final_data$name == "Jane-Ling Wang"] <- 0
final_data$pred_male[final_data$name == "Jungseock Joo"] <- 1
final_data$pred_male[final_data$name == "Xiucai Ding"] <- 1
final_data$pred_male <- as.factor(as.numeric(final_data$pred_male))

# Exclude no salary and no google scholar (loss of 31)
final_data <- final_data[!(is.na(final_data$cited_by) | is.na(final_data$Total.Pay)),]

# Correct data from Google scholar / remove incorrect 
final_data$year_first_pub[final_data$year_first_pub == min(final_data$year_first_pub)] <- 2004

cols_to_update <- colnames(additional)
for(j in 1:nrow(additional)){
  for(i in 2:length(cols_to_update)){
    final_data[which(final_data$name == names_to_add[j]), cols_to_update[i]] <- additional[j,i]
  }
}

final_data <- final_data[final_data$name %!in% incorrect_gc,]

# Let's exclude the outlier, whose pay is high because of administrative role
# as Provost / Dean (Jennifer Chayes)
final_data <- final_data[final_data$Total.Pay != max(final_data$Total.Pay),]

write.csv(final_data, paste0(cur_wd,"/final_data.csv"))
