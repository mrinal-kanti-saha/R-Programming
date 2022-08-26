library(tidyverse)
library(naniar)
library(GGally)
library(caret)
library(car)
library(dplyr)
library(reshape)
library(leaps)
library(MASS)
library(corrplot)

# set working directory
setwd("/home/msc1/R_Project/Dataset_Tips")
# check the directory
getwd()

# import the dataset and read the strings as factors
tips <- read.csv("./tips.csv", stringsAsFactors = TRUE)

### Exploratory Data Analysis
# Dimension of the data, no of rows and columns
dim(tips)
# First 6 rows
head(tips)
# Names of the columns
names(tips)
# Structure of the dataframe tips
str(tips)
# 6 point info about each feature
summary(tips)

# Do we have missing values ?
# Plot of missing values
vis_miss(tips)
# All the data values are present.

# Data frame of numerical features
num_cols <- unlist(lapply(tips, is.numeric))
tips_num <- tips[, num_cols]
head(tips_num)

### Data Visualization
# Pairplot
ggpairs(data = tips_num, title = "Pairplots of numerical features",
        upper = list(continuous = wrap("cor", size = 7)))
# The data seems linear between tip and total_bill

### Feature Engineering
tips %>%
  group_by(day) %>%
  summarise_at(vars(tip), list(name = mean))
# 1 Fri    2.73 1
# 2 Sat    2.99 3
# 3 Sun    3.26 4
# 4 Thur   2.77 2
# From this, we are trying to derive an ordering for the day variable.

tips$day <- as.factor(ifelse(tips$day=="Thur", 2,
                      ifelse(tips$day=="Fri", 1,
                      ifelse(tips$day=="Sat", 3,
                      ifelse(tips$day=="Sun", 4, 5)))))
# Convert categorical features using One Hot Encoding
# dummy <- dummyVars(" ~ .", data = tips)
# new_tips <- data.frame(predict(dummy, newdata = tips))
# head(new_tips)
# head(tips)

# Convert categorical features using Ordinal Encoding
new_tips <- data.frame(tips)
str(new_tips)
new_tips$day <- unclass(tips$day)
new_tips$time <- unclass(tips$time)
new_tips$sex <- unclass(tips$sex)
new_tips$smoker <- unclass(tips$smoker)
head(new_tips)
str(new_tips)

# Building a Generalised Model
model <- lm(tip ~ ., data = new_tips)
summary(model)
# Residuals:
# Min      1Q      Median  3Q      Max 
# -2.8918 -0.5753 -0.0608  0.4999  4.0673 
# 
# Coefficients:
#              Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)  0.805882   0.641380   1.256   0.2102    
# total_bill   0.094076   0.009524   9.877   <2e-16 ***
# sex         -0.029216   0.141060  -0.207   0.8361    
# smoker      -0.081041   0.143742  -0.564   0.5734    
# day          0.007834   0.100381   0.078   0.9379    
# time         0.005721   0.203697   0.028   0.9776    
# size         0.179367   0.089111   2.013   0.0453 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.021 on 237 degrees of freedom
# Multiple R-squared:  0.4688,	Adjusted R-squared:  0.4553 
# F-statistic: 34.86 on 6 and 237 DF,  p-value: < 2.2e-16

# 1. Checking of linear relationship between study and explanatory variables
# Correlation check
corr <- cor(tips_num)
corrplot(corr, method = "color")

# Observations :-
# tip has high correlation with total_bill and then with size. and negligible correlation with others.
# total_bill has high correlation with size and negligible correlation with others.
# day has high correlation with time and negligible correlation with others.

# 2. Checking for Influence Points
inflm.model <-  influence.measures(model)
which(apply(inflm.model$is.inf, 1, any))
# Which observations 'are' influential
sum_inflm <- summary(inflm.model)

n = dim(sum_inflm)[1]
k = dim(sum_inflm)[2]
# n = 20, k = 11

# Limits of DFBETAS and DFFITS
DFbetalim = 2 / sqrt(n)
DFfitslim = 2 * sqrt(k/n)
# DFBETA > 0.4472136, DFFITS > 1.48324
list_inflm = list()

# A point is termed as an influence point and needs further observation, 
# if COOK's D-statistic(D) > 1 
# if DFBETAS > 2/sqrt(n) = 2/sqrt(20)) = 0.4472136, i.e DFBETAS > 0.4472136
# if DFFITS > 2 * sqrt(k/n)  = 2*sqrt(11/20) = 1.48324, i.e DFFITS > 1.48324
# Observation : No influential points found.

for (i in 1 : n) {
  # Checking Cook's D-statistic
  if (sum_inflm[i, "cook.d"] > 1) {
    append(list_inflm, c(i))
  }
  # Checking DFBETAS
  for (j in 1 : (k - 4)) {
    if (sum_inflm[i, j] > DFbetalim) {
      append(list_inflm, c(i))
    }
  }
  # Checking DFFITS
  if (sum_inflm[i, "dffit"] > DFfitslim) {
    append(list_inflm, c(i))
  }
}

# Get the list of influential points
list_inflm <- unique(list_inflm)
print(length(list_inflm))
# Length is 0

model_num <- lm(tip ~ ., data = tips_num)
summary(model_num)

# 3. Multicolinearity Check (for numeric only)
vif(model_num)
# total_bill       size 
# 1.557586     1.557586 
# Observation : No such multicolinearity as VIF < 4 for all variables.

# 4. Check the normality of the residuals
# Calculate standardised residuals
res <- resid(model)
mean(res)
# The assumption of the zero mean holds. Mean of residuals = -5.688101e-17 ~ 0

plot(fitted(model), res)
# Add a horizontal line at 0
abline(0, 0)
# The residuals vs the fitted values seems to form a funnel, failing the assumption of Homoskadasticity

df_res <- as.data.frame(res)

# 4.1. Distribution Curves
ggplot(data = df_res, aes(x = res)) +
  geom_histogram(bins = 25, aes(y =..density..), fill = "orange") +
  geom_density()
# The residuals can be said to be approximated to normal distribution,
# but still, we will try doing other tests and proceed with necessary measures.

# 4.2. QQ-Plot
qqPlot(df_res$res)
# The data seems to fit into a straight line. Hence, the target variable can be assumed to be normal.

# 4.3. Shapiro-Wilks Test
# It is based on the correlation between the data and the corresponding normal scores.
# H0 : Part of Normal distribution
# H1 : Not part of normal distribution
shapiro.test(df_res$res)
# W = 0.96453, p-value = 9.519e-06
# Observation : The p-value of the SWT is less than 0.05, therefore, we reject it as a normal distribution.

# From Shapiro-Wilk test, we get that the distribution of the residuals are not normal.
# We should check the existence of outliers in both dependent and independent variables now.

# But the model without outliers, is poorer than the normal, and therefore this code is discarded.

# # Outlier Detection
# meltData <- melt(tips_num)
# p <- ggplot(meltData, aes(factor(variable), value))
# p + geom_boxplot() + facet_wrap(~variable, scale="free")
# 
# # Removing outliers by using IQR
# # tip
# Q1 <- quantile(new_tips$tip, .25)
# Q3 <- quantile(new_tips$tip, .75)
# IQR <- IQR(new_tips$tip)
# new_tips <- subset(new_tips, new_tips$tip > (Q1 - 1.5*IQR) & new_tips$tip < (Q3 + 1.5*IQR))
# dim(new_tips)
# # total_bill
# Q1 <- quantile(new_tips$total_bill, .25)
# Q3 <- quantile(new_tips$total_bill, .75)
# IQR <- IQR(new_tips$total_bill)
# new_tips <- subset(new_tips, new_tips$total_bill > (Q1 - 1.5*IQR) & new_tips$total_bill < (Q3 + 1.5*IQR))
# dim(new_tips)
# # size
# Q1 <- quantile(new_tips$size, .25)
# Q3 <- quantile(new_tips$size, .75)
# IQR <- IQR(new_tips$size)
# new_tips <- subset(new_tips, new_tips$size > (Q1 - 1.5*IQR) & new_tips$size < (Q3 + 1.5*IQR))
# dim(new_tips)
# # By this technique, we have managed to reduce the observations from 244 to 221.
# 
# # Building a Generalised Model
# model2 <- lm(tip ~ ., data = new_tips)
# summary(model2)
# 
# ors <- rstandard(model2)
# df_res <- as.data.frame(ors)

# Now, we would have to plot the distribution curve and the QQ plot to verify
# if our data has become Normal.
# ggplot(data = df_res, aes(ors)) +
#   geom_histogram(bins= 25, aes(y =..density..), fill = "orange") +
#   geom_density()
# # The residuals can be said to be approximated to normal distribution, 
# # but we will confirm it with QQ-plot and SWT.
# 
# # QQ-Plot
# qqPlot(df_res$ors)
# # The data seems to fit into a straight line still. Hence, further treatment is required.
# 
# # Shapiro-Wilks Test
# shapiro.test(df_res$ors)
# # W = 0.98182, p-value = 0.006065
# # The p-value of the SWT is less than 0.05, therefore, we reject it, yet again, as a normal distribution.

# Box cox transformation
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
# 0.1414141
new_model <- lm((tip^lambda - 1)/lambda ~ ., data = new_tips)
summary(new_model)
# Residual standard error: 0.3802 on 237 degrees of freedom
# Multiple R-squared:  0.4473,	Adjusted R-squared:  0.4333 
# F-statistic: 31.96 on 6 and 237 DF,  p-value: < 2.2e-16

res <- resid(new_model)
mean(res)
# The assumption of the zero mean holds. Mean of residuals = 1.4399e-17 ~ 0

plot(fitted(new_model), res)
# Add a horizontal line at 0
abline(0, 0)
abline(1, 0)
abline(-1, 0)
# The residuals vs the fitted values seems to form a horizontal band around 0, validating the assumption of Homoskadasticity.

df_res <- as.data.frame(res)
# Distribution Curves
ggplot(data = df_res, aes(x = res)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()
# The residuals can be said to be approximated to normal distribution,
# but still, we will try doing other tests and proceed with necessary measures.

# Shapiro-Wilks Test
shapiro.test(df_res$res)
# W = 0.9962, p-value = 0.8203
# P value greater than 0.05

# QQ-Plot
qqPlot(df_res$res)
# The data seems to fit into a straight line. Hence, the target variable can be assumed to be normal. 
# There are two observations that seems to be the outliers and hence we would remove those.
dim(new_tips)
new_tips <- new_tips[-c(173, 238), ]
dim(new_tips)

# Transforming Y variable using boxcox
for(i in 1 : dim(new_tips)[1])
  new_tips[i, "tip"] = ((new_tips[i, "tip"]^lambda - 1) / lambda)
head(new_tips)

# 5. Feature Selection
# Forward Selection
FSR = regsubsets(tip ~ ., data = new_tips, method = "forward")
summary(FSR)
Modelsummary.1 = cbind(summary(FSR)$which, R2=summary(FSR)$rsq, SSres=summary(FSR)$rss, 
                     AdjR2=summary(FSR)$adjr2, Cp=summary(FSR)$cp, BIC=summary(FSR)$bic)
Modelsummary.1
# Recommendation : total_bill, size

# Backward elimination
BER = regsubsets(tip ~ ., data = new_tips, method = "backward")
Modelsummary.2 = cbind(summary(BER)$which, R2=summary(BER)$rsq, SSres=summary(BER)$rss,
                       AdjR2=summary(BER)$adjr2, Cp=summary(BER)$cp, BIC=summary(BER)$bic)
Modelsummary.2
# Recommendation : total_bill, size

# Stepwise Regression
SWR = regsubsets(tip ~ ., data = new_tips, method = "seqrep")
Modelsummary.3 = cbind(summary(SWR)$which, R2=summary(SWR)$rsq, SSres=summary(SWR)$rss,
                       AdjR2=summary(SWR)$adjr2, Cp=summary(SWR)$cp, BIC=summary(SWR)$bic)
Modelsummary.3
# Recommendation : total_bill, size

### Final Model
set.seed(108)
# Splitting the dataset into train and test 8:2
trainPartitionRows <- createDataPartition(new_tips$tip, p = .80, list = FALSE)
nrow(new_tips)
nrow(trainPartitionRows)
head(trainPartitionRows)
trainDataset <- new_tips[ trainPartitionRows, ]
testDataset  <- new_tips[-trainPartitionRows, ]
head(trainDataset)
head(testDataset)

# Repeated Cross Validation
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
)

tips_model <- train(tip ~ total_bill + size,
                    data = new_tips,
                    method = "lm",
                    trControl = ctrl,
)

tips_model
# RMSE       Rsquared   MAE      
# 0.3598576  0.4966135  0.2927397

summary(tips_model)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.0035 -0.2415  0.0214  0.2485  0.8444 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.24166    0.06911   3.497 0.000561 ***
#   total_bill   0.03520    0.00329  10.698  < 2e-16 ***
#   size         0.06034    0.03058   1.973 0.049597 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3606 on 239 degrees of freedom
# Multiple R-squared:  0.4868,	Adjusted R-squared:  0.4825 
# F-statistic: 113.4 on 2 and 239 DF,  p-value: < 2.2e-16

### Save the model in a RDS file
# Save a single object to a file
saveRDS(tips_model, "./Model_tips.rds")

# Prediction data.frame(total_bill = 17 , size = 4)
str(testDataset)
test <- testDataset %>% dplyr::select(-c(tip))
pred <- predict(tips_model, test)

vals <- data.frame(predicted = pred, actual = testDataset$tip)

test_RMSE <- sqrt(mean((vals$predicted - vals$actual)^2))
test_RMSE

# Test RMSE = 0.3297749
# Train RMSE 0.3598576 > Test RMSE 0.3297749, therefore the model has genralised well.