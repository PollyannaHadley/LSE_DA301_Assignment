## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)

# Import the data set.
sales <- read.csv(file.choose(), header=TRUE)


# Print the data frame.
view(sales)
head(sales)
glimpse(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)


# View the data frame.
View(sales_2)


# View the descriptive statistics.
summary(sales_2)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, EU_Sales, data=sales_2, geom=c('point', 'smooth'))
qplot(Global_Sales, EU_Sales, data=sales_2, geom=c('point', 'smooth'))
qplot(Global_Sales, NA_Sales, data=sales_2, geom=c('point', 'smooth'))


## 2b) Histograms
# Create histograms.
qplot(Global_Sales, bins= 10, data=sales_2)
qplot(NA_Sales, bins= 10, data=sales_2)
qplot(EU_Sales, bins= 10, data=sales_2)


## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, data=sales_2, geom='boxplot')
qplot(NA_Sales, data=sales_2, geom='boxplot')
qplot(EU_Sales, data=sales_2, geom='boxplot')

# Most informative graphs to provide insights at this stage are the hisotgrams
# and scatterplots


###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_by_product <- sales_2 %>% group_by(Product) %>%
  summarise(Total_Global_Sales=sum(Global_Sales),
            Total_NA_Sales=sum(NA_Sales),
            Total_EU_Sales=sum(EU_Sales),
            .groups='drop')

sales_by_product <- arrange(sales_by_product, desc(Total_Global_Sales))

# View the data frame.
View(sales_by_product)

# Explore the data frame.
summary(sales_by_product)


## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Total_NA_Sales, Total_EU_Sales, data=sales_by_product, geom=c('point', 'smooth'))
qplot(Total_Global_Sales, Total_EU_Sales, data=sales_by_product, geom=c('point', 'smooth'))
qplot(Total_Global_Sales, Total_NA_Sales, data=sales_by_product, geom=c('point', 'smooth'))

# Create histograms.
qplot(Total_Global_Sales, bins= 15, data=sales_by_product)
qplot(Total_NA_Sales, bins= 15, data=sales_by_product)
qplot(Total_EU_Sales, bins= 15, data=sales_by_product)

# Create boxplots.
qplot(Total_Global_Sales, data=sales_by_product, geom='boxplot')
qplot(Total_NA_Sales, data=sales_by_product, geom='boxplot')
qplot(Total_EU_Sales, data=sales_by_product, geom='boxplot')

###############################################################################

# 4. Observations and insights

## Your observations and insights:
# Scatterplots appear to show a positive correlation between Global Sales, NA Sales and EU Sales.
# All three histograms appear to show the data is positive, right skew with indicating extreme
# outliers of higher sales values
# When grouping by product, the product generating highest revenues (globally, NA and EU) is 107.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_by_product)
str(sales_by_product)


# Check output: Determine the min, max, and mean values.
# We will create a new dataframe containing only the numeric sales values only.
sales_only <- select(sales_by_product, -Product)
head(sales_only)

# Use the sapply() function to assess the min, max, and mean for these items
sapply(sales_only, min)
sapply(sales_only, max)
sapply(sales_only, mean) 


# View the descriptive statistics.
summary(sales_by_product)

# Can see the top sales by product have global sales of > 24.5m and therefore
# we can create a new column to categorise the product between Top 10 and Other
sales_by_product <- sales_by_product %>% 
  mutate(Top_10=if_else(Total_Global_Sales > 24.5, "Top 10", "Other") )

# Check data.
View(sales_by_product)

###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.

# Global Sales
qqnorm(sales_by_product$Total_Global_Sales, col='blue')
qqline(sales_by_product$Total_Global_Sales, col='red', lwd=2)

# North America Sales
qqnorm(sales_by_product$Total_NA_Sales, col='blue')
qqline(sales_by_product$Total_NA_Sales, col='red', lwd=2)

# European Sales
qqnorm(sales_by_product$Total_EU_Sales, col='blue')
qqline(sales_by_product$Total_EU_Sales, col='red', lwd=2)


## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_by_product$Total_Global_Sales)
shapiro.test(sales_by_product$Total_NA_Sales)
shapiro.test(sales_by_product$Total_EU_Sales)


## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Global Sales
skewness(sales_by_product$Total_Global_Sales) 
kurtosis(sales_by_product$Total_Global_Sales)

# North America Sales
skewness(sales_by_product$Total_NA_Sales) 
kurtosis(sales_by_product$Total_NA_Sales)

# European Sales
skewness(sales_by_product$Total_EU_Sales) 
kurtosis(sales_by_product$Total_EU_Sales)


## 2d) Determine correlation
# Determine correlation.
round(cor(sales_only), digits=2)


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.

# Relationship between Global Sales and NA Sales
ggplot(sales_by_product, aes(x=Total_Global_Sales, y=Total_NA_Sales)) +
  geom_point(color="green",
             alpha=0.5,
             size=3) + 
  geom_smooth(method='lm', se=FALSE) + 
  labs(title='Relationship between Global Sales and North America Sales',
       x='Global Sales per Product (£m)', y= 'North America Sales per Product (£m)') +
  theme_classic()

# Relationship between Global Sales and NA Sales - highlighting top 10
ggplot(sales_by_product, aes(x=Total_Global_Sales, y=Total_NA_Sales, col=Top_10)) +
  geom_point(alpha=0.5,
             size=3) + 
  scale_color_manual(values=c('green', 'blue')) +
  labs(title='Relationship between Global Sales and North America Sales',
       x='Global Sales per Product (£m)', y= 'North America Sales per Product (£m)',
       col='Ranking by Product Sales') +
  theme_classic()

# Relationship between Global Sales and EU Sales
ggplot(sales_by_product, aes(x=Total_Global_Sales, y=Total_EU_Sales)) +
  geom_point(color="red",
             alpha=0.5,
             size=3) + 
  geom_smooth(method='lm', se=FALSE) + 
  labs(title='Relationship between Global Sales and EU Sales',
       x='Global Sales per Product (£m)', y= 'EU Sales per Product (£m)') +
  theme_classic()

# Relationship between Global Sales and EU Sales - highlighting top 10
ggplot(sales_by_product, aes(x=Total_Global_Sales, y=Total_EU_Sales, col=Top_10)) +
  geom_point(alpha=0.5,
             size=3) + 
  scale_color_manual(values=c('red', 'blue')) + 
  labs(title='Relationship between Global Sales and EU Sales',
       x='Global Sales per Product (£m)', y= 'EU Sales per Product (£m)',
       col='Ranking by Product Sales') +
  theme_classic()

# Relationship between North America Sales and EU Sales
ggplot(sales_by_product, aes(x=Total_NA_Sales, y=Total_EU_Sales)) +
  geom_point(color="orange",
             alpha=0.5,
             size=3) + 
  geom_smooth(method='lm', se=FALSE) + 
  labs(title='Relationship between North America Sales and EU Sales',
       x='North America Sales per Product (£m)', y= 'EU Sales per Product (£m)') +
  theme_classic()

# Relationship between North America Sales and EU Sales - highlighting top 10
ggplot(sales_by_product, aes(x=Total_NA_Sales, y=Total_EU_Sales, col=Top_10)) +
  geom_point(alpha=0.5,
             size=3) + 
  scale_color_manual(values=c('orange', 'blue')) + 
  labs(title='Relationship between North America Sales and EU Sales',
       x='North America Sales per Product (£m)', y= 'EU Sales per Product (£m)',
       col='Ranking by Product Sales') +
  theme_classic()



###############################################################################

# 4. Observations and insights
# Your observations and insights:

# Null hypothesis is the data is normally distributed. In all cases the 
# Shapiro-Wilk p-value is less than 5% and therefore the null hypothesis is 
# rejected and there is evidence that the data tested is not normally distributed.

# Kurtosis greater than 3 in all cases indicating a leptokurtic distribution 
# with extreme outliers.
# Skewness statistics show the data is highly positive skewed. This outcome also
# supports the conclusions from visualising the histograms above

# Correlation statistics show the strongest positive correlation between Global
# sales and North America sales. There is also a strong correlation between Global
# sales and European sales. Finally, there is also a postive correlation between 
# North America and Europe, albeit not as strongly correlated as the relationship 
# between Global sales.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_by_product)

# Determine a summary of the data frame.
summary(sales_by_product)

###############################################################################

# 2. Create a multiple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model_1 <- lm(Total_Global_Sales ~ Total_NA_Sales + Total_EU_Sales, 
              data=sales_by_product)

# View the model outputs
model_1

# View the summary
summary(model_1)


###############################################################################

# 4. Predictions based on given values
# Create a data frame of the NA and EU sales figures to make the predictions.
predict_data <- data.frame(Total_NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                           Total_EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))
# Check new data frame.
str(predict_data)

# Generate predictions to mode.
predict_global = predict(model_1, newdata=predict_data,
                         interval='confidence')


# View predictions.
predict_global


###############################################################################

# 5. Observations and insights
# Your observations and insights:

# The adjusted R-squared figure of 0.97 means the 97% of movements in the 
# dependent variables are explained by movements in the independent variables 
# which means the regression model is reliable.

# When comparing the predicted sales to the observed values where available, 
# only 1 out of 3 preductions falls within the accepted range. The remaining 
# both fall below the expected lowest range values.This suggests the model is 
# overestimating the sales predictions and likely to be a result of the positive
# skew and lack of normality in the data set.


###############################################################################
###############################################################################




