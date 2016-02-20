library(magrittr)
library(qpcR)
library(dplyr)
library(FNN)

setwd("C:/Users/Dean/Desktop/Tableau_MindStorm")

data <- read.csv("WA_schools_data.csv")

# Get average data over 2008-2010 to minimize missing data loss
grouped_data <- data %>% group_by(School, SchoolCategory, District, SchoolType) %>%
  summarise(mathNLImet = mean(mathNLImet, na.rm=TRUE), 
            mathLImet = mean(mathLImet, na.rm=TRUE),
            LowIncome = mean(LowIncome, na.rm=TRUE),
            ELL = mean(ELL, na.rm=TRUE),
            SpecialEd = mean(SpecialEd, na.rm=TRUE),
            Enrollment = mean(Enrollment, na.rm=TRUE),
            NLIGradRate = mean(NLIGradRate, na.rm=TRUE),
            LIGradRate = mean(LIGradRate, na.rm=TRUE)
            )

# Grad rate data has way too much missing data. 
# For this project, I will only look at low income pass rates.
final_data <- grouped_data %>% select(-NLIGradRate,-LIGradRate,-mathNLImet)
dim(final_data)

# Get rid of observations with missing data
final_data %<>% na.omit()
dim(final_data)

# Schools with no low income kids do not have a legit response variable for this project
final_data_2 <- final_data %>% filter(LowIncome != 0)
dim(final_data_2)

# Run regression. Special Ed % is not significant - special ed students probably aren't counted
# in standardized testing pass rates.
M1 <- lm(mathLImet ~ LowIncome + ELL + Enrollment + SpecialEd, data=final_data_2)
summary(M1)

knn_data <- final_data_2 %>% mutate(LowIncome=LowIncome/IQR(final_data_2$LowIncome),
                                    ELL=ELL/IQR(final_data_2$ELL),
                                    SpecialEd=SpecialEd/IQR(final_data_2$SpecialEd),
                                    Enrollment=Enrollment/IQR(final_data_2$Enrollment))

# I decided not to use special ed % as a predictor
knn_training <- knn_data[c(6,7,9)]
knn_training_response <- final_data_2["mathLImet"]

# KNN model without a separate training data set
school_pred <- knn.reg(train=knn_training,test=knn_training,y=knn_training_response$mathLImet,k=3)

# computes RMSE of a knn model
knn_rmse <- function(knn_model,test_response){
  df.knn_model <- knn_model$pred %>% as.matrix() %>% as.data.frame()
  d <- bind_cols(df.knn_model,test_response)
  colnames(d) <- c("pred","actual")
  d %<>% mutate(error=pred-actual)
  mse <- (1/length(d$error))*sum(d$error^2)
  rmse <- sqrt(mse)
  return(rmse)
  
}

# KNN model with k=3 has better training RMSE than linear regression
knn_rmse(school_pred,knn_training_response)
RMSE(M1)

# Create training (2/3) and test (1/3) data sets
set.seed(76)
ind <- sample(2, nrow(knn_data), replace=TRUE, prob=c(0.67, 0.33))

knn_training <- knn_data[ind==1,c(6,7,9)]
knn_test <- knn_data[ind==2,c(6,7,9)]
knn_training_response <- final_data_2[ind==1,"mathLImet"]
knn_test_response <- final_data_2[ind==2,"mathLImet"]

lm_training <- final_data_2[ind==1,]
lm_test <- final_data_2[ind==2,]

# Fit the linear model
lm_model <- lm(mathLImet~LowIncome + ELL + Enrollment, data=lm_training)
summary(lm_model)

# compute RMSE
lm_model_test <- predict(lm_model,newdata=lm_test)
lm_model_rmse <- sqrt((1/length(lm_model_test)) * sum((lm_model_test-lm_test$mathLImet)^2))
lm_model_rmse

# Create dataframe of to store performance of various KNN models with different k's
lowest <- data.frame(NA,NA) %>% na.omit()
colnames(lowest) <- c("k","rmse")

# Try k from 1 to 30
for (k in 1:30){
  knn_model <- knn.reg(train=knn_training, test=knn_test, y=knn_training_response$mathLImet, k=k)
  rmse <- knn_rmse(knn_model,knn_test_response)
  lowest %<>% bind_rows(data.frame(k,rmse))
}

# KNN model outperforms on most k
lowest
lowest %>% arrange(rmse)

write.csv(final_data_2, file="WA_Schools_Shinyapp_Data.csv", row.names=FALSE)
