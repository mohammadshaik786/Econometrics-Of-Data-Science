library(dplyr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVPlots)

#install.packages('car')
#install.packages('alr4')
#install.packages('faraway')

library(car)
library(alr4)
library(faraway)

#Loading the dataset
CarData<-read.csv("/Users/mohammadshaik/Desktop/UMKC/Econometrics of Data Science/Hw-2/hw2_car_data.csv")
sample(CarData, 5)
#Viewing the data
View(CarData)
#Dimensions of the data
dim(CarData)
#Structure of the data
str(CarData)

#summary from the dataset
summary(CarData)

# Data Cleaning
CarData[duplicated(CarData), ]

# Remove Duplicates
CarData <- CarData %>% distinct()

# Check Missing Values
colSums(is.na(CarData))

dataset<- CarData[c(-9)]
View(dataset)
model1=lm(mpg~.,data=dataset)
#regression=lm(mpg~+ cylinders+ displacement+hp+weight+acceleration+ modelyr+origin+foreign, data=dataset)

#Summary of reggresion model
summary(model1)



# Correlation Plot
install.packages('GGally')
library(GGally)
ggcorr(CarData %>% mutate_if(is.factor, as.numeric), label = TRUE)


x <- ggplot(CarData, aes(cylinders, mpg)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

y <- ggplot(CarData, aes(displacement, mpg)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

z <- ggplot(CarData, aes(hp, mpg)) +
  geom_jitter(color = "yellow", alpha = 0.5) +
  theme_light()

w <- ggplot(CarData, aes(weight, mpg)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()



p <- plot_grid(x, y, z, w) 
title <- ggdraw() + draw_label("1. Correlation between mpg and cylinders / displacement / hp / weight", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


x <- ggplot(CarData, aes(acceleration, mpg)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(CarData, aes(modelyr, mpg)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

z <- ggplot(CarData, aes(origin, mpg)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

w <- ggplot(CarData, aes(foreign, mpg)) +
  geom_jitter(color = "yellow", alpha = 0.5) +
  theme_light()


p <- plot_grid(x, y, z, w) 
title <- ggdraw() + draw_label("2. Correlation between mpg and acceleration/modelyr/origin/foreign", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

x <- ggplot(CarData, aes(modyr80, mpg)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(CarData, aes(modyr81, mpg)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

z <- ggplot(CarData, aes(modyr82, mpg)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y, z) 
title <- ggdraw() + draw_label("3. Correlation between mpg and modyr80/modyr81/modyr82", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

model2=lm(mpg~ cylinders+displacement+hp+weight+acceleration+ modelyr+origin+foreign, data=dataset)

#Summary of reggresion model
summary(model2)

FinalData <- dataset %>% select(mpg,cylinders,displacement,hp,weight,acceleration, modelyr,origin,foreign,modyr80,modyr81,modyr82)

#Summary statistics of the selected dataset
summary(FinalData)
smry <- summary(FinalData)
library(pastecs)
write.csv(smry,"summary.csv")

#data2 <- dataset$cylinders
#sd(data2)
apply(FinalData,2,sd)  

model3=lm(mpg~ cylinders+displacement+hp+weight+acceleration+ modelyr+origin+foreign+modyr80+modyr81+modyr82, data=FinalData)

#Summary of reggresion model
summary(model3)

model4=lm(mpg~ displacement+weight+ modelyr+origin+foreign+modyr80+modyr81+modyr82, data=dataset)

#Summary of reggresion model
summary(model4)


#installing the requried packages
install.packages("BSDA")

#importing the libraries
library(BSDA)
library("dplyr")
library(dplyr)

#filtering the data of foreign countries
foreign=CarData%>%dplyr::filter(foreign==1)
foreign1=CarData%>%dplyr::filter(foreign==1)%>%dplyr::summarise(mean=mean(mpg),obs=n(), stad=var(weight)^0.5)
nonforeign=CarData%>%dplyr::filter(foreign==0)
nonforeign1=CarData%>%dplyr::filter(foreign==0)%>%dplyr::summarise(mean=mean(mpg),obs=n(), stad=var(mpg)^0.5)

#printing the z-test
z.test(x=foreign$mpg, y=nonforeign$mpg, mu=0, sigma.x=foreign1$stad, sigma.y=nonforeign1$stad,alternative = "less")

#plot(model3)

#linear regression model
#fit=unlist(model3$fitted.values)
#model3$residuals



# Scatterplot matrix
scatterplotMatrix(~ log(mpg) + foreign+ log(hp) + log(weight)+log(modelyr), data = FinalData)

# outliers Identification
res.std <- rstandard(model3)

plot(res.std, ylab="Standardized Residual", ylim=c(-3.5,3.5))

abline(h =c(-3,0,3), lty = 2)

# print the index number of respective observation in order to remove them
# In the below case I printed the observations of Horse power
index <- which(res.std > 3 | res.std < -3)
text(index-20, res.std[index] , labels = FinalData$hp[index])



#leverage plot

h <- influence(model3)$hat

halfnorm(influence(model3)$hat, ylab = "Leverage Value")


#influential observations:- cooks distance

cutoff <- 4/((nrow(FinalData)-length(model3$coefficients)-2))
plot(model3, which = 4, cook.levels = cutoff)
abline(h = cutoff)

# checking homoscedasticity

plot(model3$residuals ~ model3$fitted.values)

abline(h = 0, lty = 10)

#linearity checking

residualPlots(model3)

library(car)
crPlot(model3, variable = "foreign")

#indepedence
plot(model3$residuals ~ FinalData$weight)

#normality 
qqnorm(model3$residuals)  
qqline(model3$residuals) 


library(MASS)
sresid <- studres(model3)
hist(sresid, freq=FALSE,main=" Studentized Residuals Distribution",xlab="Residuals of model")
xfit <- seq(min(sresid),max(sresid),length=40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# Multicollinearity check
vif(model3)

#Model specification
avPlots(model3)


