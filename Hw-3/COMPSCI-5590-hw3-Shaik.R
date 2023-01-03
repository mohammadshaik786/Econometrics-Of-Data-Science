# Get current Working Directory to fetch dataset
getwd()
# Read CSV file
CarData_df <- read.csv("hw3_car_data.csv", header=TRUE)
# View Dataset
View(CarData_df)
# Row count of Dataset
nrow(CarData_df)
# Reading First 10 Rows from the dataframe
CarData_df[1:10,]
# head(data, 3)
#summary(two-way or contingency table)
table(CarData_df$name, CarData_df$foreign)[1:10,]
with(CarData_df, table(foreign, modelyr))

# 1. Generate summary statistics for these variables 

# summary provides the statistics for each column of the entire dataset
CarData_df <- CarData_df[c(-9)]
smry = summary(CarData_df)
# Writing statistics as a CSV file
library(pastecs)
write.csv(smry,"summary.csv")

# Standard deviation for the dataset
#sd(CarData_df,na.rm=TRUE)
apply(CarData_df,2,sd, na.rm=TRUE) 

# summary statistics for the selected column
summary(CarData_df$cylinders)


# mean statistics for the selected column
mean(CarData_df$cylinders)
# mean(data $speed_before, na.rm=TRUE)


# 2. State and justify your binary logit model explaining “foreign”
#fit logistic regression model
bin_logit_model1 <- glm( foreign ~., data = CarData_df, family = binomial)


# 3. Estimate a binary logit model that includes binary variables for model year

# Summarize the model
summary(bin_logit_model1)
#exp(coef(bin_logit_model))
#write.csv(exp(coef(bin_logit_model)),"odds.csv")

#fit 2nd logistic regression model
bin_logit_model2=glm(foreign~ mpg+cylinders+displacement+hp+weight+acceleration, data=CarData_df, family = binomial)
# Summarize the model
summary(bin_logit_model2)

#fit 3rd logistic regression model
bin_logit_model3=glm(foreign~ mpg+cylinders+displacement+hp+weight+acceleration+modyr80+modyr81+modyr82, data=CarData_df, family = binomial)
#Summary of model
summary(bin_logit_model3)

#fit 4th logistic regression model
bin_logit_model4=glm(foreign~ mpg+cylinders+displacement+weight+ hp+acceleration+modyr70+modyr71+modyr72+modyr73+
             modyr74+modyr75+modyr76+modyr77+modyr78+modyr79+modyr80+modyr81+modyr82, data=CarData_df, family = binomial)
#Summary of model
summary(bin_logit_model4)

#fit 5th logistic regression model
bin_logit_model5=glm(foreign~ mpg+cylinders+displacement+weight+modyr70+modyr71+modyr72+modyr73+
             modyr74+modyr75+modyr76+modyr77+modyr78+modyr79+modyr80, data=CarData_df, family = binomial)
#Summary of model
summary(bin_logit_model5)
#smry_mdl = summary(bin_logit_model5)
#write.csv(smry_mdl,"bin_logit_model5.csv")


#modyr70 = CarData_df$modyr70+0.1
#fit 6th logistic regression model
#bin_logit_model6=glm(foreign~ mpg+cylinders+displacement+weight+ hp+acceleration+log(modyr70)+modyr71+modyr72+modyr73+
 #                      modyr74+modyr75+modyr76+modyr77+modyr78+modyr79+modyr80+modyr81+modyr82, data=CarData_df, family = binomial)
#Summary of model
#summary(bin_logit_model6)


exp(coef(bin_logit_model5))
write.csv(exp(coef(bin_logit_model5)),"odds.csv")

# Make predictions
library(dplyr)
library(tidyverse)
probabilities1 <- bin_logit_model5 %>% predict(CarData_df, type = "response")
predicted.classes <- ifelse(probabilities1 > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == CarData_df$foreign)

# 5.	Discuss goodness-of-fit measures
install.packages("ResourceSelection")
library(ResourceSelection)

h1 <- hoslem.test(CarData_df$foreign, fitted(bin_logit_model5), g=15)
print(h1)
