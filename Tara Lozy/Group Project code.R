# read the CSV file for class project 
stocks <-read.csv("the data for your group project_MA541.csv")

# Part 1 ----

# summary statistics for each variable 

# sample mean for each variable
mean(stocks$Close_ETF)
mean(stocks$oil)
mean(stocks$gold)
mean(stocks$JPM)

# standard deviation for each variable 
sd(stocks$Close_ETF)
sd(stocks$oil)
sd(stocks$gold)
sd(stocks$JPM)

# sample correlations between each pair
cor(stocks$Close_ETF, stocks$oil)
cor(stocks$Close_ETF, stocks$gold)
cor(stocks$Close_ETF, stocks$JPM)
cor(stocks$oil, stocks$gold)
cor(stocks$oil, stocks$JPM)
cor(stocks$gold, stocks$JPM)

# Part 2 ----

# create histograms for each variable
hist(stocks$Close_ETF)
hist(stocks$oil)
hist(stocks$gold)
hist(stocks$JPM)

#time series plot for each variable 
plot(1:1000, stocks[, "Close_ETF"])
plot(1:1000, stocks[, "oil"])
plot(1:1000, stocks[, "gold"])
plot(1:1000, stocks[, "JPM"])

#time series plot for all variables 


# scatter plots 
plot(stocks[, 1:2])
plot(stocks[, "Close_ETF"], stocks[, "gold"])
plot(stocks[, "Close_ETF"], stocks[, "JPM"])

# Part 4 ----

# Step 8, Generate 50 simple random samples (with replacement), size = 20

rm(.Random.seed)
sample(stocks$Close_ETF, 20, replace=TRUE)

# Step 10, Generate 10 simple random samples (with replacement), size = 100 
sample(stocks$Close_ETF, 100, replace=TRUE)

# Part 5 ----

# Selection of one group from Part 4 Step 10

ETF6 <- c(142.16, 110.27, 125.41, 112.06, 128.17, 114.50, 116.53, 118.18, 121.23, 113.22,
          130.66, 102.83, 145.61, 127.38,  99.93, 104.57, 120.76, 127.10, 142.82, 116.88,
          114.86, 106.40, 104.99, 136.81, 111.54, 110.52, 111.81, 122.19,  97.75, 114.70,
          144.61, 126.60, 127.10, 128.17, 106.37, 103.54, 131.36, 123.65, 142.82, 120.58,
          120.37, 130.11, 109.27, 114.90, 109.71, 130.87, 117.46, 130.51, 142.34, 100.61,
          100.58, 123.82, 102.94, 118.58, 141.37, 123.50, 114.50, 135.27, 117.88, 142.82,
          119.24, 125.76, 110.45, 114.50, 129.00, 107.16, 100.73, 145.98, 113.20, 128.81,
          121.32, 113.92, 123.54, 140.38, 148.06, 138.25, 145.02, 111.52, 129.65, 103.36,
          128.44, 121.21, 123.99,  99.30, 141.82, 140.22, 136.54, 109.58, 121.32, 149.53,
          122.47, 126.36, 149.48, 139.47, 125.01, 126.60, 121.22, 128.81, 120.52, 127.51)

# Question 1 - 95% Confidence Intervals 
a <- mean(ETF6)
s <- sd(ETF6)
n <- 100
error <- qnorm(0.975) * s / sqrt(n)
left <- a - error 
right <- a + error 
left
right 

# Selection of one group from Part 4 Step 8
ETF26 <- c(130.64, 109.73, 132.36, 126.55, 141.95, 113.51, 117.11, 129.31, 108.90, 119.27,
           116.78, 119.83, 129.74, 127.07, 127.10, 124.72, 121.43, 127.41, 112.44, 136.41)

# Question 2 - 95% Confidence Intervals 

a1 <- mean(ETF26)
s1 <- sd(ETF26)
n1 <- 100
error1 <- qnorm(0.975) * s1 / sqrt(n1)
left1 <- a1 - error 
right1 <- a1 + error 
left1
right1 

# Part 6 ----

# 2 - sided T-test on Sample Set ETF6, H0: u = 100 at Confidence Level = 95%
t.test(ETF6, mu = 100)

# 2-sided T-test on Sample Set ETF26, H0: u = 100 at Confidence Level = 95%
t.test(ETF26, mu = 100)

# 2-sided Chi-Squared Test on Sample Set ETF26, sigma = 225 at Confidence Level = 95%
varTest(ETF26, sigma.squared = 225)

# 1-sided Chi-Sqaured Test on Sample Set ETF25, sigma = 225 at Conficence Level = 95%
varTest(ETF26, alternative = "less", sigma.squared = 225)

# Part 9 ----
fit <- lm(stocks$Close_ETF ~ stocks$oil + stocks$gold)
summary(fit)

# extract residuals
R <- residuals(fit)

# extract predicted values 
P <- predict(fit)

#Plot residuals vs predicted 
plot(P, R, ylab = "Residuals", xlab = "Predicted")
abline(0, 0)

#Independence Plot residuals vs row number
D <- c(1:1000)
plot(D, R, ylab = "Residuals", xlab = "Row Number")

# Plot residuals vs oil 
plot(stocks$oil, R, ylab = "Residuals", xlab = "Oil")
abline(0, 0)

# Plot residuals vs gold
plot(stocks$gold, R, ylab = "Residuals", xlab = "Gold")
abline(0, 0)

#Plot Normality Plot
qqnorm(R, ylab = "Residuals", xlab = "Normal Scores")
qqline(R)

R.Std <- rstandard(fit)
qqnorm(R.Std, ylab = "Standardized Residuals", xlab = "Normal Scores")
qqline(R.Std)
