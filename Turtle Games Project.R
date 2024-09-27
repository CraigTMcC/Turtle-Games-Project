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
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
sales <- read.csv(file.choose(), header=TRUE)

# Print the data frame.
head(sales)
view(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)


# View the data frame.
head(sales2)
view(sales2)

# View the descriptive statistics.
summary(sales2)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create various scatterplots. 
qplot(Product, NA_Sales, data=sales2, colour=Product)
qplot(Product, EU_Sales, data=sales2, colour=Product)
qplot(Platform, NA_Sales, data=sales2, colour=Platform)
qplot(Platform, EU_Sales, data=sales2, colour=Platform)
qplot(Platform, Global_Sales, data=sales2, colour=Platform)
qplot(NA_Sales, EU_Sales, colour= Product, data=sales2)
qplot(NA_Sales, EU_Sales, colour= Platform, data=sales2, geom=c('point', 'jitter'))

# The graph depicting the overall global sales trends provided the most 
# significant insight into product sales, as it closely resembled the regional 
# versions. Therefore, it was enhanced with titles and other elements for 
# inclusion in the report.
qplot(Product, Global_Sales, data=sales2, colour=Product) +
  ggtitle("Global Sales by Product Number") +
  ylab("Global Sales (million £)") + 
  xlab("Product Number") + 
  theme_minimal()

## 2b) Histograms
# Create histograms.
qplot(NA_Sales, data=sales2)
qplot(EU_Sales, data=sales2)
qplot(Global_Sales, data=sales2)
qplot(Product, data=sales2)
qplot(Platform, data=sales2)

# I found the histograms ineffective for analysing the data because the sales 
# figures were clustered around similar values, compounded by the varying number 
# of products and platforms.

## 2c) Boxplots
# Create boxplots.
qplot(Product, NA_Sales, data=sales2, geom='boxplot')
qplot(Product, EU_Sales, data=sales2, geom='boxplot')
qplot(Product, Global_Sales, data=sales2, geom='boxplot')
qplot(Product, Platform, data=sales2, geom='boxplot')
qplot(Product, NA_Sales, data=sales2, geom='boxplot', colour=Platform)

# The boxplots also didn't offer much visually for explaining data to 
# stakeholders, apart from indicating products that significantly outsell 
# others, a point already evident in the refined scatterplot mentioned earlier.

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# From the scatterplots created on a regional and global level, product sales are 
# larger for the products with smaller unique codes. There are slight variances 
# between North America and Europe, but by and large, products with codes under 
# 5000 are generating most of the sales. This is particularly evident when
# examining the global sales visualization. Different platforms show varying
# sales figures, with a few outliers depending on the region and product.

# The histograms illustrate that North American sales far outweigh sales in Europe.
# One product sells over 34 million copies, while most products sell between
# 0 and 10 million copies, compared to 0-5 million in Europe. However, Europe 
# does have the same biggest seller as North America, coming in at just under 
# 24 million copies. This clearly indicates that certain products outsell others
# significantly. Overall, X360, PS3, PC, and Wii were the best-performing platforms.

# The boxplots reinforce the aforementioned observations. The bulk of the products
# sell within a larger range in North America than in Europe, with significant
# outliers far outperforming those with much larger sales.

# With a vast number of products, it would be beneficial to delve into them at a
# granular level, especially those at either end of the sales spectrum. Importantly,
# products not selling should be reassessed for their value on the sales list,
# and the best sellers leveraged to create more sales. It would also be interesting
# to investigate if large regional differences in product sales can be realigned by
# discovering the reasons for the disparities and leveraging the positives from
# high sales in one region to increase sales in the other.

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
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales2)
view(sales2)

# Check output: Determine the min, max, and mean values.

# Select the columns you want to calculate min, max, and mean for
sales3 <- sales2 %>%
  select(NA_Sales, EU_Sales, Global_Sales)

# Apply the functions across the selected columns
results <- apply(sales3, 2, function(x) c(min = min(x), 
                                          max = max(x), mean = mean(x)))

# Convert the result to a dataframe
results2 <- as.data.frame(results)

# Rename the columns
rownames(results2) <- c("Min", "Max", "Mean")

# Add column names
colnames(results2) <- c("NA_Sales", "EU_Sales", "Global_Sales")

# Print the result
print(results2)

# View the descriptive statistics.
summary(sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Calculate the sum of sales for each region and globally
sales_sum <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales) ~ 1, 
                       data = sales2, FUN = sum)

# Print the result
print(sales_sum)

# Group data based on Product and determine the sum per Product.
product <- sales2 %>%
  select(Product, Global_Sales)

# View the data frame.
head(product)
view(product)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplot for Global Sales
ggplot(product, aes(x = Product, y = Global_Sales)) +  
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(x = "Product Number", y = "Sales (million £)", 
       title = "Global Product Sales") +
  theme_minimal() 

# Create scatterplot for EU Sales
ggplot(sales2, aes(x = Product, y = EU_Sales)) +  
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Product Number", y = "Sales (million £)", 
       title = "EU Product Sales") +
  theme_minimal() 

# Create scatterplot for NA Sales
ggplot(sales2, aes(x = Product, y = NA_Sales)) +  
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Product Number", y = "Sales (million £)", 
       title = "NA Product Sales") +
  theme_minimal()

# # Create scatterplot for all sales figures
ggplot(sales2, aes(x = Product)) +
  geom_point(aes(y = EU_Sales), color = "blue") +
  geom_point(aes(y = NA_Sales), color = "red") +
  geom_point(aes(y = Global_Sales), color = "green") +
  labs(x = "Product Number", y = "Sales (million £)",
       title = "EU, NA and Global Product Sales combined") +
  theme_minimal()

# Create scatterplot with EU Sales and NA Sales
ggplot(sales2, aes(x = Product)) +
  geom_point(aes(y = EU_Sales, color = "EU Sales")) +
  geom_point(aes(y = NA_Sales, color = "NA Sales")) +
  scale_color_manual(name = "Region",
                     values = c("EU Sales" = "blue", "NA Sales" = "red")) +
  labs(x = "Product Number", y = "Sales (million £)", 
       title = "EU & NA Product Sales") +
  theme_minimal()

# I thought this last scatterplot paired nicely with the global sales 
# scatterplot from above, emphasizing the trend in product sales while also 
# highlighting the sales in North America and Europe, particularly 
# the higher sales in North America.

# Create histograms.
ggplot(product, aes(x=Product)) + 
  geom_histogram(bins = 30)
# As with the histograms above, to many products means a diluted graphic.

# Create Boxplots
# Melt the dataframe to long format
sales_long <- tidyr::pivot_longer(sales2, cols = c("EU_Sales", "NA_Sales", 
      "Global_Sales"), names_to = "Region", values_to = "Sales")

# Create a boxplot
ggplot(sales_long, aes(x = Region, y = Sales, fill = Region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("EU_Sales" = "blue", "NA_Sales" = "red", 
      "Global_Sales" = "green")) +
  labs(x = "Region", y = "Sales", fill = "Region") +
  theme_minimal()
# Again not conveying anything not visible through the scatterplots.

################################################################################

# Create a barplot
# Calculate the sum of the specified columns
sales_sum <- colSums(sales2[c("NA_Sales", "EU_Sales", "Global_Sales")])

# Print output
print(sales_sum)

# Create a dataframe from the sums
sales_sum2 <- data.frame(
  Region = c("NA_Sales", "EU_Sales", "Global_Sales"),
  Sales = sales_sum)

# Create a bar plot
ggplot(sales_sum2, aes(x = Region, y = Sales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Total Sales (million £)", 
       title = "Total Sales by Region") +
  theme_minimal()

# I found this to be a great method of comparing sales for each region alongside 
# the global figures. However, it doesn't include a significant portion of 
# sales, both in this representation and also lacking in the dataset overall.


# Therefore, sales including unknown regions data was calculated. 

# Create a data frame
sales_data <- data.frame(
  Region = factor(c("NA Sales", "EU Sales", "Global Sales", "Other"), 
                  levels = c("NA Sales", "EU Sales", "Other", "Global Sales")),
  Sales = c(885.62, 578.61, 1877.81, NA))

# Calculate Other
other_sales <- sales_data$Sales[3] - (sales_data$Sales[1] + sales_data$Sales[2])

# Replace NA value with calculated Other value
sales_data$Sales[4] <- other_sales

# Create the Barplot
ggplot(sales_data, aes(x = fct_rev(Region), y = Sales, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Sales), vjust = -0.5, size = 3, color = "black") +  
  scale_fill_manual(values = c("NA Sales" = "red", "EU Sales" = "blue", 
                               "Global Sales" = "green", "Other" = "grey")) +
  labs(x = "Region", y = "Total Sales (million £)", 
       title = "Total Sales by Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# This barplot was created not only to showcase that NA Sales are the largest 
# contributor to global sales but also to highlight that there is a significant 
# portion of global sales that are undefined.

################################################################################
# 3. Determine the normality of the data set.
################################################################################

######## Global Sales ########

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify qqnorm function (draw a qqplot).
qqnorm(sales2$Global_Sales)

# Specify qqnorm function (draw a qqplot).
qqline(sales2$Global_Sales, col='green')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library (moments)

# Perform Shapiro-Wilk test.
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro <- shapiro.test(sales2$Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skew <- skewness(sales2$Global_Sales)
kurt <- kurtosis(sales2$Global_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales2$EU_Sales, sales2$Global_Sales)
cor(sales2$NA_Sales, sales2$Global_Sales)
cor(sales2$Product, sales2$Global_Sales)

# Create a data frame for the report
df1 <- data.frame(Test = c("Shapiro-Wilk", "Skewness", "Kurtosis"),
                      Statistic = c(shapiro$statistic, skew, kurt),
                      P_Value = c(shapiro$p.value, NA, NA))

# Print the table
print(df1)

######## EU Sales ########

# Specify qqnorm function (draw a qqplot).
qqnorm(sales2$EU_Sales)

# Specify qqnorm function (draw a qqplot).
qqline(sales2$EU_Sales, col='blue')

# Specify shapiro.test function (Shapiro-Wilk test).
shapiro2 <- shapiro.test(sales2$EU_Sales)

# Skewness and Kurtosis.
skew2 <- skewness(sales2$EU_Sales)
kurt2 <- kurtosis(sales2$EU_Sales)

# Determine correlation.
cor(sales2$EU_Sales, sales2$NA_Sales)
cor(sales2$EU_Sales, sales2$Global_Sales)
cor(sales2$EU_Sales, sales2$Product)

# Create a data frame for the report
df2 <- data.frame(Test = c("Shapiro-Wilk", "Skewness", "Kurtosis"),
                  Statistic = c(shapiro2$statistic, skew2, kurt2),
                  P_Value = c(shapiro2$p.value, NA, NA))

# Print the table
print(df2)


######## NA Sales ########

# Specify qqnorm function (draw a qqplot).
qqnorm(sales2$NA_Sales)

# Specify qqnorm function (draw a qqplot).
qqline(sales2$NA_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro3 <- shapiro.test(sales2$NA_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skew3 <- skewness(sales2$NA_Sales)
kurt3 <- kurtosis(sales2$NA_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales2$NA_Sales, sales2$EU_Sales)
cor(sales2$NA_Sales, sales2$Global_Sales)
cor(sales2$NA_Sales, sales2$Product)

# Create a data frame for the report
df3 <- data.frame(Test = c("Shapiro-Wilk", "Skewness", "Kurtosis"),
                  Statistic = c(shapiro3$statistic, skew3, kurt3),
                  P_Value = c(shapiro3$p.value, NA, NA))

# Print the table
print(df3)


###############################################################################


# 4. Plot the data
top_products <- sales2 %>%
  arrange(desc(EU_Sales + NA_Sales)) %>%
  head(10)

# Create a barplot
ggplot(top_products, aes(x = reorder(Product, -(EU_Sales + NA_Sales)), 
       y = EU_Sales + NA_Sales, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(x = "Product", y = "Total Sales", 
       title = "Top 10 Products with Highest Sales in EU and NA Regions") +
  theme_minimal()

# This barplot was created to illustrate the top-selling products in both the 
# EU and NA regions.

# Select the top 10 products with the highest sales in EU_Sales
top_eu_products <- sales2 %>%
  arrange(desc(EU_Sales)) %>%
  head(10)

# Select the top 10 products with the highest sales in NA_Sales
top_na_products <- sales2 %>%
  arrange(desc(NA_Sales)) %>%
  head(10)

# Combine the top products from EU_Sales and NA_Sales
top_products <- bind_rows(
  mutate(top_eu_products, Region = "EU_Sales"),
  mutate(top_na_products, Region = "NA_Sales"))

# Create a ggplot
ggplot(top_products, aes(x = reorder(Product, -(EU_Sales + NA_Sales)), 
       y = EU_Sales + NA_Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Product Number", y = "Total Sales (million £)", 
       title = "Top 10 Products with Highest Sales in EU and NA Regions") +
  scale_fill_manual(values = c("EU_Sales" = "blue", "NA_Sales" = "red"), 
  labels = c("EU Sales", "NA Sales")) +
  theme_minimal()

# I then sought to identify the best-selling products from each region, which 
#yielded some interesting results. It became evident that certain products, 
# like #107, performed well in both regions, while others, such as #123, were 
# predominantly sold in North America.

############ Bottom Products #############

# 4. Plot the data
bot_products <- sales2 %>%
  arrange(desc(EU_Sales + NA_Sales)) %>%
  tail(10)

# Create a barplot
ggplot(bot_products, aes(x = reorder(Product, -(EU_Sales + NA_Sales)), 
       y = EU_Sales + NA_Sales, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(x = "Product", y = "Total Sales", 
       title = "Bottom 10 Products with Lowest Sales in EU and NA Regions") +
  theme_minimal()

# Select the top 10 products with the highest sales in EU_Sales
bot_eu_products <- sales2 %>%
  arrange(desc(EU_Sales)) %>%
  tail(10)

# Select the top 10 products with the highest sales in NA_Sales
bot_na_products <- sales2 %>%
  arrange(desc(NA_Sales)) %>%
  tail(10)

# Combine the top products from EU_Sales and NA_Sales
bot_products <- bind_rows(
  mutate(bot_eu_products, Region = "EU_Sales"),
  mutate(bot_na_products, Region = "NA_Sales"))

# Create a ggplot
ggplot(bot_products, aes(x = reorder(Product, -(EU_Sales + NA_Sales)), 
       y = EU_Sales + NA_Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Product Number", y = "Total Sales (million £)", 
       title = "Bottom 10 Products with Lowest Sales in EU and NA Regions") +
  scale_fill_manual(values = c("EU_Sales" = "blue", "NA_Sales" = "red"), 
                    labels = c("EU Sales", "NA Sales")) +
  theme_minimal()

# The same was done for the bottom products in the EU and NA regions

# Create a dataframe showing the products with the greatest difference in sales 
# between the EU and NA regions.
eu_sales_grouped <- sales2 %>%
  group_by(Product) %>%
  summarise(
    EU_Sales = sum(EU_Sales),
    NA_Sales = sum(NA_Sales)
  ) %>%
  mutate(Difference = EU_Sales - NA_Sales) %>%
  top_n(10, Difference) %>%
  arrange(desc(Difference))

# Print the resulting dataframe
print(eu_sales_grouped)

# Create a barplot 
ggplot() +
  geom_bar(data = eu_sales_grouped, aes(x = reorder(Product, Difference), 
                y = Difference), stat = "identity", fill = "blue") +
  geom_text(data = eu_sales_grouped, aes(x = reorder(Product, Difference), 
                y = Difference, label = Difference), 
                vjust = -0.5, size = 3, colour = "black") +  
  labs(x = "Product Number", y = "Difference (million £)",
                title = "Top 10 Products: EU Sales greater than NA Sales", 
                x = "Product", y = "Difference") +
  theme_minimal()

# Create a dataframe showing the products with the greatest difference in sales 
# between the NA and EU regions.
na_sales_grouped <- sales2 %>%
  group_by(Product) %>%
  summarise(
    EU_Sales = sum(EU_Sales),
    NA_Sales = sum(NA_Sales)
  ) %>%
  mutate(Difference = NA_Sales - EU_Sales) %>%
  top_n(10, Difference) %>%
  arrange(desc(Difference))

# Print the resulting dataframe
print(na_sales_grouped)

# Create a barplot 
ggplot() +
  geom_bar(data = na_sales_grouped, aes(x = reorder(Product, Difference), 
                y = Difference), stat = "identity", fill = "red") +
  geom_text(data = na_sales_grouped, aes(x = reorder(Product, Difference), 
                y = Difference, label = Difference), 
                vjust = -0.5, size = 3, colour = "black") +  
  labs(x = "Product Number", y = "Difference (million £)",
                title = "Top 10 Products: NA Sales greater than EU Sales", 
                x = "Product", y = "Difference") +
  theme_minimal()

# The results from these two barplots provided valuable insights into which 
# products are significantly outselling others in each region compared to the 
# other. 


################################################################################


# 5. Observations and insights
# Your observations and insights here...
# When reviewing the sales data further, a consistent pattern emerged: 
# lower product numbers correlated with better sales figures, and North American 
# sales consistently surpassed those of the EU. Visualizing the data with 352 
# products proved challenging; histograms, boxplots, and bar plots proved 
# ineffectual as the data was difficult to interpret. While boxplots did reveal 
# a significant number of outliers, this was more clearly depicted in 
# scatterplots, providing a better understanding of sales trends.

# The Shapiro-Wilk tests conducted on EU_Sales, NA_Sales, and Global_Sales all 
# suggest non-normal distributions. This had already bee alluded to via the 
# Q-Q plots that had been conducted. Positive skewness values and high kurtosis 
# values further indicated right-skewed distributions and heavy tails, 
# respectively, for all three sales figures. As expected, there is a correlation 
# among sales figures for different regions and global sales, but not with the 
# products themselves.

# The top products from each region were also analysed to assess performance 
# across different areas. While only a small number of products performed poorly, 
# they warrant investigation to ensure cost-effectiveness in the market. The 
# best sellers should also be investigated for insight into their popularity.
# Additionally, it became evident that certain products outsold their counterparts 
# in either the EU or NA regions. Therefore, the data was compiled to identify 
# products with the greatest disparities in sales. These findings will be further 
# examined to understand the reasons behind such differences and explore how 
# strategies that contributed to high sales in one region can potentially boost 
# sales in the counterpart region.

# It should also be noted that sales in other global regions should be assessed 
# and categorized, as any improvement in market share would be beneficial. 
# Currently, global sales total 1877.81 million, while North America and the 
# EU together total 885.62 and 578.61 million, respectively, amount to a 
# collective total of 1464.23 million. This leaves 413.58 million sold outside 
# of these regions. By analyzing this data, we can identify which regions are 
# experiencing increasing sales and determine how to further enhance those markets. 
# Conversely, we can evaluate regions that are not showing improvement to 
# understand the underlying reasons and make changes to improve sales.


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
head(sales3)
view(sales3)

# Determine a summary of the data frame.
summary(sales3)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# Find a correlation.
cor(sales3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation for EU Sales to Global Sales.
plot(sales3$EU_Sales, sales3$Global_Sales)

# Create a linear regression model.
model1 <- lm(sales3$Global_Sales ~ sales3$EU_Sales)

# View the summary stats.
summary(model1)

# Basic visualisation for NA Sales to Global Sales.
plot(sales3$NA_Sales, sales3$Global_Sales)

# Create a linear regression model.
model2 <- lm(sales3$Global_Sales ~ sales3$NA_Sales)

# View the summary stats.
summary(model2)

# Basic visualisation for NA Sales to EU Sales.
plot(sales3$NA_Sales, sales3$EU_Sales)

# Create a linear regression model.
model3 <- lm(sales3$NA_Sales ~ sales3$EU_Sales)

# View the summary stats.
summary(model3)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
num_cols <- sapply(sales, is.numeric)

# View output
view(num_cols)

# Create a new dataframe
sales_num <- sales[, num_cols]

# View output
view(sales_num)

# Multiple linear regression model.
# Import the psych package.
library(psych)

# Determine correlation between variables.
cor(sales_num)

# Visulalise with the corPlot() function.
corPlot(sales_num, cex=1)

# Create a new object and 
# specify the lm function and the variables.
# Only using EU_Sales and NA_Sales as the X-variables
modela = lm(Global_Sales~EU_Sales+NA_Sales, data=sales_num)

# Print the summary statistics.
summary(modela)

# Add new variables.Adding all possible variables
modelb = lm(Global_Sales~EU_Sales+NA_Sales+Ranking+Product+Year, data=sales_num)

# Change the model name.
summary(modelb)

# Remove Ranking also from the model
modelc = lm(Global_Sales~EU_Sales+NA_Sales+Product+Year, data=sales_num)

# Change the model name.
summary(modelc)

# Remove Product from the model
modeld = lm(Global_Sales~EU_Sales+NA_Sales+Year, data=sales_num)

# Change the model name.
summary(modeld)

# Sales and Product in the model
modele = lm(Global_Sales~EU_Sales+NA_Sales+Product, data=sales_num)

# Change the model name.
summary(modele)


###############################################################################


# 4. Predictions based on given values
# Compare with observed values for a number of records.
sales_test <- data.frame(
  NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52)
)

# Predict global sales using the linear regression model
predicTest <- predict(modela, newdata = sales_test,
                      interval='confidence')

# Print the results.
print(predicTest)

# Define observed values
observed <- c(67.85, 6.04, 4.32, 3.53, 23.21)

# Create a data frame with predicted values
predicted <- c(71.468572, 6.856083, 4.248367, 4.134744, 26.431567)
lwr <- c(70.162421, 6.718420, 4.102094, 4.009122, 25.413344)
upr <- c(72.774723, 6.993745, 4.394639, 4.260365, 27.449791)

comparison <- data.frame(Observed = observed, Predicted = predicted, 
                         Lower = lwr, Upper = upr)

# Calculate differences between observed and predicted values
comparison$Percent_Dif <- ((comparison$Observed - 
                      comparison$Predicted) / comparison$Observed) * 100

# Round the entire comparison table to 2 decimal points
comparison2 <- round(comparison, 2)

# Print the comparison table
print(comparison2)

# Calculate Root Mean Square Error (RMSE):
residuals <- observed - predicted
RMSE <- sqrt(mean(residuals^2))

# Print results
print(RMSE)

# Calculate MAPE
MAPE <- mean(abs((observed - predicted) / observed)) * 100

# Print results
print(MAPE)

# Calculate the Coefficient of Determination 
SST <- sum((observed - mean(observed))^2)
SSR <- sum(residuals^2)
R_squared <- 1 - (SSR/SST)

# Print results
print(R_squared)

###############################################################################

# 5. Observations and insights
# North American sales exhibited a slightly stronger positive correlation with 
# global sales compared to EU sales, standing at 93% and 88% respectively. 
# Both correlations denote strong relationships. Additionally, both regions 
# demonstrated a good fit in a simple linear regression model, with coefficients 
# of determination (R^2) at 87% and 77% for North American and EU sales 
# respectively. These findings suggest that sales from both regions serve as 
# reliable predictors for global sales.

# In the context of multiple linear regression, Model A emerged as the chosen 
# model, explaining 97% of the variability. Subsequent models yielded similar 
# results, hovering around the 97% mark, with differences within 0.5% of each 
# other. Given the nature of sales observations and the substantial correlation 
# between sales variables, it may not be necessary to include all numeric columns.

# Following the generation of predictions from Model A, it becomes evident that 
# the model overestimates global sales for four of the observations, ranging 
# between 5% to 17%. While the model demonstrates a high degree of fit, 
# indicated by the R^2 value of 0.99, the Root Mean Square Error (RMSE) remains 
# relatively low at 2.21, reflecting good performance. However, the Mean 
# Absolute Percentage Error (MAPE) returns a slightly high result, just over 10%. 
# While not catastrophic, it suggests that the model may not achieve the desired 
# level of accuracy, as evidenced by the aforementioned results.


###############################################################################
###############################################################################




