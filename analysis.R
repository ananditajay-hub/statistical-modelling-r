# Statistical Modelling of Behavioural Data
# Author: Anandita Das
# Methods: EDA, Linear Regression, Logistic Regression
# Objective: To analyse factors influencing tipping behaviour

# Load dataset
tips <- read.csv("https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv")

# Convert categorical variables
tips$sex <- as.factor(tips$sex)
tips$smoker <- as.factor(tips$smoker)
tips$day <- as.factor(tips$day)
tips$time <- as.factor(tips$time)

# Exploratory Data Analysis
plot(tips$total_bill, tips$tip,
     xlab = "Total Bill",
     ylab = "Tip",
     main = "Tip vs Total Bill")

# Linear regression model
model1 <- lm(tip ~ total_bill + size + sex + smoker, data = tips)
summary(model1)

# Create binary outcome
tips$high_tip <- ifelse(tips$tip > median(tips$tip), 1, 0)

# Logistic regression model
model2 <- glm(high_tip ~ total_bill + size + smoker,
              data = tips,
              family = "binomial")
summary(model2)
# Correlation
cor(tips$total_bill, tips$tip)

# Boxplot
boxplot(tip ~ smoker, data = tips,
        main = "Tip Distribution by Smoking Status")

# Model diagnostics
plot(model1)
# Key Insight:
# Total bill is a strong predictor of tip amount (p < 0.001),
# while smoking status and gender are not statistically significant.
