###############################################################################

# Week 4 assignment: EDA using R

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
turtle_sales
View(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
View(turtle_sales2)

# View the descriptive statistics.
summary(turtle_sales2)

################################################################################

# 2. Review plots to determine insights into the data set.
## 2a) Scatterplots
# Create scatterplots.
qplot(Product, Platform, data=turtle_sales2)
qplot(NA_Sales, Platform, data=turtle_sales2)
qplot(EU_Sales, Platform, data=turtle_sales2)
qplot(Global_Sales, Platform, data=turtle_sales2)

## 2b) Histograms
# Create histograms.
qplot(Platform, data=turtle_sales2)
qplot(Product, bins=50, data=turtle_sales2)

## 2c) Boxplots
# Create boxplots.
qplot(Product, Platform, data=turtle_sales2, geom='boxplot')
qplot(NA_Sales, Platform, data=turtle_sales2, geom='boxplot')
qplot(EU_Sales, Platform, data=turtle_sales2, geom='boxplot')
qplot(Global_Sales, Platform, data=turtle_sales2, geom='boxplot')

# 3. Observations and insights

##

###############################################################################
###############################################################################

# Week 5 assignment: Cleaning and maniulating data using R

# 1. Load and explore the data

# View data frame created before.
View(turtle_sales2)

# Check output: Determine the min, max, and mean values for the sales data.
min(turtle_sales2$NA_Sales)
min(turtle_sales2$EU_Sales)
min(turtle_sales2$Global_Sales)

max(turtle_sales2$NA_Sales)
max(turtle_sales2$EU_Sales)
max(turtle_sales2$Global_Sales)

mean(turtle_sales2$NA_Sales)
mean(turtle_sales2$EU_Sales)
mean(turtle_sales2$Global_Sales)

# View the descriptive statistics.
summary(turtle_sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

library(dplyr)

df_ts2 = turtle_sales2 %>% group_by(Product)  %>%
  summarise(total_sales = sum(NA_Sales, EU_Sales, Global_Sales),
            .groups = 'drop') %>%
  as.data.frame()

# View the data frame.
View(df_ts2)

# Explore the data frame.
summary(df_ts2)
table(df_ts2$Product)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
library(ggplot2)
ggplot(turtle_sales2, aes(x=Product)) +
  geom_point(aes(y = NA_Sales, col = "NA")) + 
  geom_point(aes(y = EU_Sales, col = "EU")) +   
  geom_point(aes(y= Global_Sales, col = "GLOBAL")) +
  labs(title = "Product Sales by Region") +
  ylab("Sales") +
  xlab("Product")

# Create histograms.
ggplot(turtle_sales2, aes(NA_Sales, fill=Platform)) +
  geom_histogram()
ggplot(turtle_sales2, aes(EU_Sales, fill=Platform)) +
  geom_histogram()
ggplot(turtle_sales2, aes(Global_Sales, fill=Platform)) +
  geom_histogram()


# Create boxplots.
a <- ggplot(turtle_sales2, aes(x=Product, y=NA_Sales)) + 
  geom_boxplot()+ 
  theme_classic()
b <- ggplot(turtle_sales2, aes(x=Product, y=EU_Sales)) + 
  geom_boxplot()+ 
  theme_classic()
c <- ggplot(turtle_sales2, aes(x=Product, y=Global_Sales)) + 
  geom_boxplot()+ 
  theme_classic()
# Combine boxplots.
library("cowplot")
plot_grid(a, b, c  
          ,labels = c("NA", "EU", "Global"))

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(turtle_sales2$NA_Sales)
# Add a reference line:
qqline(turtle_sales2$NA_Sales, col='red')

qqnorm(turtle_sales2$EU_Sales)
qqline(turtle_sales2$EU_Sales, col='red')

qqnorm(turtle_sales2$Global_Sales)
qqline(turtle_sales2$Global_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((turtle_sales2$NA_Sales))
shapiro.test((turtle_sales2$EU_Sales))
shapiro.test((turtle_sales2$Global_Sales))

# Our p-value for all sales is very <0.05, and we can conclude that the data is  
# not normally distributed.

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(turtle_sales2$NA_Sales)
skewness(turtle_sales2$EU_Sales)
skewness(turtle_sales2$Global_Sales)
# All of the sales data suggests a positive skewness.

#Check for kurtosis.
kurtosis(turtle_sales2$NA_Sales)
kurtosis(turtle_sales2$EU_Sales)
kurtosis(turtle_sales2$Global_Sales)

# All sales data's kurtosis value is more than 3, indicates a leptokurtic
# distribution (i.e., a distribution with heavier tails and a higher peak).

## 3d) Determine correlation
# Determine correlation.
cor(turtle_sales2$NA_Sales, turtle_sales2$EU_Sales)
cor(turtle_sales2$NA_Sales, turtle_sales2$Global_Sales)
cor(turtle_sales2$EU_Sales, turtle_sales2$Global_Sales)
# Our correlation coefficient of 0.70, 0.93 and 0.87 suggests a strong
# positive correlation.

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
ggplot(turtle_sales, aes(y=Publisher, fill=Genre)) +
  geom_bar() 

ggplot(turtle_sales, aes(x=Genre, y=Year)) +
  geom_violin(fill='red')

ggplot(turtle_sales, aes(x=Year, y=Publisher)) +
  geom_boxplot(fill='purple')

ggplot(turtle_sales, aes(x=Global_Sales, y=Genre)) +
  geom_boxplot(fill='green',
               notch=TRUE,
               outlier.color='red')

ggplot(turtle_sales, aes(x=EU_Sales, y=Genre)) +
  geom_boxplot(fill='green',
               notch=TRUE,
               outlier.color='red')

ggplot(turtle_sales, aes(x=NA_Sales, y=Genre)) +
  geom_boxplot(fill='green',
               notch=TRUE,
               outlier.color='red')

ggplot(turtle_sales, aes(y=Publisher, fill=NA_Sales, color='red')) +
  geom_bar() +
  theme_classic()

ggplot(turtle_sales, aes(y=Publisher, fill=EU_Sales, color='red')) +
  geom_bar() +
  theme_classic()

ggplot(turtle_sales, aes(y=Publisher, fill=Global_Sales, color='red')) +
  geom_bar() +
  theme_classic()

###############################################################################

# 5. Observations and insights
#


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

# 1. Load and explore the data.
View(turtle_sales2)
summary(turtle_sales2)

# 2. Create a simple linear regression model.

# Determine the correlation between the sales columns.
cor(turtle_sales2$NA_Sales, turtle_sales2$EU_Sales, method = "pearson")
cor(turtle_sales2$NA_Sales, turtle_sales2$Global_Sales, method = "pearson")
cor(turtle_sales2$EU_Sales, turtle_sales2$Global_Sales, method = "pearson")

# Plot the relationship with base R graphics.
plot(turtle_sales2$NA_Sales, turtle_sales2$EU_Sales)
plot(turtle_sales2$NA_Sales, turtle_sales2$Global_Sales)
plot(turtle_sales2$EU_Sales, turtle_sales2$Global_Sales)

# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
library(dplyr)

# Select only numeric columns from df
numeric_cols <- select_if(turtle_sales, is.numeric)

##  - Determine the correlation between the sales columns.
cor(numeric_cols)
# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (numeric_cols) and set 
# character size (cex=2).
corPlot(numeric_cols, cex=2)

# Create a new object and 
# specify the lm function and the variables.
modela = lm(Global_Sales~NA_Sales+EU_Sales, data=numeric_cols)

# Print the summary statistics.
summary(modela)

# Create a visualisation to determine normality of data set.
qqnorm(residuals(modela))
qqline(residuals(modela), col='red')

# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
# Load the new data file (sales_test.csv) and view its structure.
salestest <- read.csv(file.choose(), header=TRUE)

# View the data.
str(salestest)

# Create a new object and specify the predict function.
predictTest = predict(modela, newdata=salestest,
                      interval='confidence')

# Print the object.
predictTest 

# 5. Include your insights and observations.

# To verify the accuracy, we compare the output of the predict() function 
# with the actual values we see in the turtle_sales data.
# These are relatively close numbers.

# The first predicted value is 71.46;
# the actual Price for the first data point is 67.85.

# The second predicted value is 6.85; 
# the actual Price for the second data point is 6.04.

# The third predicted value is 4.24; 
# the actual Price for the second data point is 4.32.

# The fourth predicted value is 4.13; 
# the actual Price for the second data point is 3.53.

# The fifth predicted value is 26.43; 
# the actual Price for the second data point is 23.21.

###############################################################################
