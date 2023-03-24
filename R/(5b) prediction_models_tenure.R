## -------------------------------------------
##
## Script name: prediction_models
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
library(ranger)
library(parallel)
library(caret)
library(dplyr)

# Local imports ----
cur_wd <- getwd()
n_cores <- detectCores()
source(paste0(cur_wd,"/constants.R"))
final_data <- read.csv(paste0(cur_wd,"/final_data.csv"))[,-1]

# Helper functions ----
'%!in%' <- function(x,y)!('%in%'(x,y))

# RF hyperparam tuning ----

set.seed(305)
train_index <- sample.int(nrow(final_data), round(nrow(final_data)*train_perc,0))
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

train_data <- train_data[train_data$tenure_track == "True",]
test_data <- test_data[test_data$tenure_track == "True",]

grid <- expand.grid(
  mtry        = c(2,3,4,5),
  num_trees   = c(100, 250, 500),
  min_node    = c(2,5,8,10),
  mse = 0
)

# Store fold level mse
temp_mse <- NULL

# 5 fold cross validation
set.seed(123)
cvIndex <- createFolds(train_data$pred_male, 5, returnTrain = T)

for(i in 1:nrow(grid)) {
  
  for (k in 1:length(cvIndex)){
    
    # Split training and validation
    
    traincv_data <- train_data[cvIndex[[k]],]
    traincv_data <- traincv_data[,colnames(traincv_data) %in% 
                                   c(num_covariates, "pred_male", y)]
    
    valcv_data <- train_data[-cvIndex[[k]],]
    
    # Train model
    model <- ranger(
      formula         = Total.Pay ~ .,
      data            = traincv_data,
      num.trees       = grid$num_trees[i],
      min.node.size   = grid$min_node[i],
      mtry            = grid$mtry[i],
      sample.fraction = 0.632,
      num.threads     = n_cores-1,
      oob.error       = F,
      verbose         = F,
      seed            = 123
    )
    
    # Predict on each model and calculate MSE
    pred <- predict(model, valcv_data)
    mse <- mean((valcv_data$Total.Pay - pred$predictions)^2)
    
    # Calculate MSE for each fold
    temp_mse  <- rbind(temp_mse, mse)
    print(paste("Done with fold ",k))
  }
  
  # Calculate overall MSE with avg of folds
  grid$mse[i]  <- mean(temp_mse)
  print(grid$mse[i])
  print(paste("Done with model ",i ))
}

results <- grid %>%
  arrange((mse))%>%
  head(5)

print(results)

# Final model fitting ----

final_model <- ranger(
  formula         = Total.Pay ~ .,
  data            = train_data[,colnames(train_data) %in% 
                                 c(num_covariates, "pred_male", y)],
  num.trees       = results$num_trees[1],
  min.node.size   = results$min_node[1],
  mtry            = results$mtry[1],
  sample.fraction = 0.632,
  num.threads     = n_cores-1,
  oob.error       = F,
  verbose         = F,
  seed            = 123,
  importance      = "impurity"
)

var_imp <- data.frame(importance(final_model))
var_imp$var <- rownames(var_imp)
sort(importance(final_model), decreasing = T)

ggplot(var_imp, 
       aes(x=reorder(var,importance.final_model.), 
           y=importance.final_model.,fill=importance.final_model.))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("RF Variable Importance (Impurity) - Tenure")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue") +
  theme_minimal()

# Final RF model evaluation ----

pred_rf <- predict(final_model, test_data)
test_mse_rf <- mean((test_data$Total.Pay - pred_rf$predictions)^2)
print(test_mse_rf)

# Fit glm for prediction---

# Given the approx normal distribution use gaussian link (OLS)
ols <- glm(
  Total.Pay ~ .,,
  data=train_data[,colnames(train_data) %in% 
                    c(num_covariates, "pred_male", y)],
  family=gaussian)

summary(ols)

ols_1 <- update(ols, . ~ . - cited_by)
ols_2 <- update(ols_1, . ~ . - year_last_pub)
ols_3 <- update(ols_2, . ~ . - year_first_pub)
ols_4 <- update(ols_3, . ~ . - count_pub)

pred_ols <- predict(ols_4, test_data)
test_mse_ols <- mean((test_data$Total.Pay - pred_ols)^2)
print(test_mse_ols)

# OLS has lower MSE, surprisingly!

# Visualization

predictions_ols <- data.frame(name = test_data$name, Total.Pay = pred_ols)
predictions_rf <- data.frame(name = test_data$name, Total.Pay = pred_rf$predictions)

pred_tenure <- ggplot() +
  geom_point(data = test_data, aes(y = Total.Pay, x = name, color = "black"))+
  geom_point(data = predictions_ols, aes(y = Total.Pay, x = name, color = "red"))+
  geom_point(data = predictions_rf, aes(y = Total.Pay, x = name, color = "blue"), show.legend = TRUE)+
  scale_color_manual(values = c("black","red", "blue"), labels = c("Actual","Pred - OLS", "Pred - RF"), name = "") +
  theme(axis.text.x=element_blank()) + 
  labs(y = "2021 Pay", x = "Tenure Faculty", title = "Predictions on Test Set - Tenure")

# Fit glm for just tenure track on all data to understand covariates ---

ols <- glm(
  Total.Pay ~ .,,
  data=tenure_data[,colnames(tenure_data) %in% 
                     c(num_covariates, "pred_male", y)],
  family=gaussian)
summary(ols)

