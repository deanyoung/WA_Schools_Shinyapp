library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)


data <- read.csv("WA_Schools_Shinyapp_Data.csv")
data$School <- as.character(data$School)
data$District <- as.character(data$District)
# Normalize predictors by IQR
data %<>% mutate(norm_LowIncome=LowIncome/IQR(data$LowIncome),
                  norm_ELL=ELL/IQR(data$ELL),
                  norm_Enrollment=Enrollment/IQR(data$Enrollment),
                  ID = paste(School,District))
neg <- "No"
