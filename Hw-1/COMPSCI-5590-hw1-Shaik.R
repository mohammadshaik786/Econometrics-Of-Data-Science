# Get current Working Directory to fetch dataset
getwd()
# Read CSV file
data <- read.csv("hw1_speed_data.csv", header=TRUE)
# Reading First 10 Rows from the dataframe
data[1:10,]
# head(data, 3)

# 1. Generate summary statistics for vehicular speeds data

# summary provides the statistics for each column of the entire dataset
summary(data)

# summary statistics for the selected column
summary(data$speed_before)

# mean statistics for the selected column
mean(data$speed_before)
# mean(data $speed_before, na.rm=TRUE)

# 2. Generate and interpret box plots for vehicular speeds data

# boxplot() which takes numeric vectors, draws a plot for any of the vector.
# boxplot for "speed_before" column
boxplot(data$speed_before,data=data, main="Plotting Vehicular Speeds for 'speed_before' ",
        xlab="Vehicular Speeds", ylab="Speed before",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# boxplot for "speed_after" column
boxplot(data$speed_after,data=data, main="Plotting Vehicular Speeds for 'speed_after' ",
        xlab="Vehicular Speeds", ylab="Speed after",
        col = "red",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# boxplot comparision for entire dataset
boxplot(data$speed_before,data$speed_after,
        main = "Multiple boxplots for comparision",
        at = c(1,3),
        names = c("Speed before", "Speed after"),
        las = 0.1,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# 3. Generate and interpret histograms for vehicular speeds data

# histogram is plotted by using hist() which takes a vector of values
# histogram with added parameters for "speed_before"
hist(data$speed_before,
     main="Plotting Vehicular Speeds for 'speed_before' ",
     xlab="Vehicular Speeds",
     col="skyblue",
     border="black",
     freq=TRUE
)

# histogram with added parameters for "speed_after"
hist(data$speed_after,
     main="Plotting Vehicular Speeds for 'speed_after' ",
     xlab="Vehicular Speeds",
     #col=c("pink","black"),
     col="skyblue",
     border="black",
     freq=TRUE
)


# 4. mean and median values of the after-speed data for those particular vehicles whose speeds before the repeal were greater than 60 mph

#mean for after-speed data by filtering speeds before > 60
mean(data[data$speed_before>60,'speed_after'],na.rm = TRUE)

#median for after-speed data by filtering speeds before > 60
median(data[data$speed_before>60,'speed_after'],na.rm = TRUE)


# 5.frequency distribution of vehicular after-speed data

frequencydata <- data$speed_after
#Create a list of class boundaries
bins <- seq(45,75,by=5)
#Group the data into bins
freqdistdata <- cut(frequencydata,bins)
#Below produces the original table in a bad format
table(freqdistdata)

#Below produces the original table in a better format
transform(table(freqdistdata))

#Below appends a relative and cumulative table
freq <- table(freqdistdata)
transform(freqdistdata,Rel_Freq=prop.table(freq),Cum_Freq=cumsum(freq))

# 6. Generate 99% confidence intervals for mean vehicular after-speed data assuming the population variance is unknown

# 1st Method -------------->

# Calculate the mean of the sample data
mean_value <- mean(data$speed_after, na.rm=T)
mean_value

# Compute the size
n <- length(data$speed_after)

# Find the standard deviation
std_dev <- sd(data$speed_after, na.rm=T)

# Find the standard error
std_err <- std_dev / sqrt(n)
confidence_level=0.99
alpha = 1 - confidence_level
degrees_of_freedom = n - 1
t_score = qt(p=1- alpha/2, df=degrees_of_freedom,lower.tail=T)
margin_error <- t_score * std_err

# Calculating lower bound and upper bound
lower_bound <- mean_value - margin_error
upper_bound <- mean_value + margin_error

# Print the confidence interval
print(c(lower_bound,upper_bound))


# 2nd Method --------------->

#confidence interval for 99% 
t.test(data$speed_after, conf.level = 0.99)

# 3rd Method --------------->

# Calculate the mean and standard error
model <- lm(speed_after ~ 1, data)

# Find the confidence interval
confint(model, level=0.99)

# 7. Generate 95% confidence intervals for the variance of before-speed data. 

# Calculate the variance of the sample data
var_value <- var(data$speed_before)
var_value

# Compute the size
n <- length(data$speed_before)

# Find chi-squared distribution table
confidence_level=0.95
alpha = 1 - confidence_level
chi_1 = qchisq(1 - alpha/2, df=n-1)
chi_2 = qchisq(alpha/2, df=n-1)

# Calculating lower bound and upper bound 
lower_bound = (n-1)*var_value/chi_1
upper_bound = (n-1)*var_value/chi_2

# Print the confidence interval for 95%
print(c(lower_bound,upper_bound))


# 8.Test whether the mean speed is 55 mph before and 60 mph after at the £\=5% significance level.

# There are 2 methods to find the solution as shown in 6th question. It's easy with the t.test() method

# Testing t.test method for mean speed is 55 mph before
t.test(data$speed_before, mu = 55,conf.level=0.95)

# Testing t.test method for mean speed is 60 mph after
t.test(data$speed_after, mu = 60,conf.level=0.95)

# 9. Test whether the variance of after-speed data is less than 19 mph2 at the α=5% significance level.

# variance at alpha=5% significance level
install.packages("EnvStats")
library(EnvStats)
varTest(data$speed_after, alternative = "less", conf.level = 0.95, sigma.squared = 19)


# 10. Test that the mean vehicular speeds before and after are equal at the α=10% significance level

# Boxplot for mean vehicular speeds before and after 
boxplot(data$speed_before,data$speed_after,
        main = "Multiple boxplots for comparision",
        at = c(1,3),
        names = c("Speed before", "Speed after"),
        las = 0.1,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# Get the parameters details in t.test() by using help()
help(t.test)

# means at α=10% significance level by using t.test()
t.test(data$speed_before,data$speed_after,alternative="two.sided",mu=0,var.equal = FALSE,paired=F,conf=0.90)


# 11. Test that the vehicular speed variances before and after are equal at the α=5% significance level. 

# Get the parameters details in f test by using help()
help(var.test)

# variances at α=5% significance level by using f test
var.test(data$speed_before,data$speed_after, alternative = 'two.sided',conf.level = 0.95)


#12. Use a Mann-Whitney-Wilcoxon test to assess whether the speeds before and after are equal.

# Get the details for wilcox.test() by using help() function
help(wilcox.test)

# Consider,significance level of 0.05, to find distributions are equal 
wilcox.test(data$speed_before,data$speed_after,alternative = "two.sided",conf.int = T,conf.level = 0.95,,correct = T)

# Density Plots for the vehicular speed data before and after
d1 <- density(data$speed_before)
#x=data$speed_after
#NoMissing <- x[!is.na(x)]
#d2 <- density(NoMissing)
d2 <- density(data$speed_after, na.rm = T)
plot(d1, type = 'l',main="Kernel Density of Data for before and after Speed",xlab="speed(mph)",lwd =2,col="blue")
lines(d2,type = 'l',lwd = 2,col = 'red')
legend('topright',fill = c('blue','red'),legend = c('before_speed','after_speed'))


