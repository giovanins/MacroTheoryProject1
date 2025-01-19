options(scipen = 999)
data <- read.csv("Macro project 1 (use this).csv")
library(dplyr)
install.packages("stargazer")
library(stargazer)
library(ggplot2)
library(tidyr)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)

##Part 2

#seperating countries
uk_data <- data[data$Country == "United Kingdom", ]
korea_data <- data[data$Country == "Korea, Rep.", ]
madagascar_data <- data[data$Country == "Madagascar", ]
#making gdp growth rate per country
uk_data$Growth_Rate <- c(NA, diff(uk_data$GDP.per.capita)) / lag(uk_data$GDP.per.capita) * 100
korea_data$Growth_Rate <- c(NA, diff(korea_data$GDP.per.capita)) / lag(korea_data$GDP.per.capita) * 100
madagascar_data$Growth_Rate <- c(NA, diff(madagascar_data$GDP.per.capita)) / lag(madagascar_data$GDP.per.capita) * 100

#seperating out year and growth rates into seperate data table
table_data <- data.frame(
  Year = data$Year, 
  UK = uk_data$Growth_Rate, 
  Korea = korea_data$Growth_Rate, 
  Madagascar = madagascar_data$Growth_Rate
)
#previous data table repeated data all 3 times so only took first 50 line(1 iteration of the data)
table_data_50 <- head(table_data, 50)
table_data_50$Year <- as.numeric(gsub(",", "", table_data_50$Year))
#Created stargazer to easily compare all 3 countries growth per year
stargazer(table_data_50, 
          type = "text",  # You can also use "html" or "latex"
          title = "Annual GDP per capita Growth Rate",
          column.labels = c("Year", "UK", "Korea", "Madagascar"),
          summary = FALSE)

#Stacked data into 2 rows to easier plot later
long_data <- table_data_50 %>%
  gather(key = "Country", value = "Growth_Rate", -Year)
#plotted all 3 countires on 1 line plot to compare different growth rates per year
ggplot(long_data, aes(x = Year, y = Growth_Rate, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "GDP per Capita Growth Rate",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green"))


##Part 3
#using UK as single country, Subsetting data into different time groupings
uk_subset_pre_1990 <- uk_data %>%
  filter(Year <= 1990)

uk_subset_1991_2007 <- uk_data %>%
  filter(Year >= 1991 & Year <= 2007)

uk_subset_2008_future <- uk_data %>%
  filter(Year >= 2008)

#calculating average gdp growth for each period
Average_growth_pre_1990 <- mean(uk_subset_pre_1990$Growth_Rate, na.rm = TRUE)
print(Average_growth_pre_1990)

Average_growth_1991_2007 <- mean(uk_subset_1991_2007$Growth_Rate, na.rm = TRUE)
print(Average_growth_1991_2007)

Average_growth_2008_future <- mean(uk_subset_2008_future$Growth_Rate, na.rm = TRUE)
print(Average_growth_2008_future)

#replotted just UK for easier viewing
ggplot(uk_data, aes(x = Year, y = Growth_Rate, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "GDP per Capita Growth Rate",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal() +
  scale_color_manual(values = c("green"))

#running correlation matrix(filtered by years to only include 1991-2018 do to NA values, also
#excluding first 2 columns do to non-numeric)
filtered_data_uk <- uk_data %>%
  filter(Year >= 1991 & Year <= 2018)

uk_data_subset <- filtered_data_uk[, -c(1, 2)]
#then convert all to num so can run matrix
uk_data_subset$Net.trade.in.goods.and.services <- as.numeric(uk_data_subset$Net.trade.in.goods.and.services)
uk_data_subset$Gross.savings <- as.numeric(uk_data_subset$Gross.savings)
uk_data_subset$Adjusted.net.national.income <- as.numeric(uk_data_subset$Adjusted.net.national.income)
uk_data_subset$Adjusted.net.savings <- as.numeric(uk_data_subset$Adjusted.net.savings)
uk_data_subset$Total.Unemployment <- as.numeric(uk_data_subset$Total.Unemployment)

str(uk_data_subset)

correlation_matrix <- cor(uk_data_subset, use = "complete.obs")

# Print the correlation matrix
corrplot(correlation_matrix, method = "pie")

#scatterplot comparison
for (col_name in colnames(filtered_data_uk)) {
  if (col_name != "Growth_Rate") {
    # Create scatter plot for GDP Growth vs the current column
    plot <- ggplot(filtered_data_uk, aes_string(x = "Growth_Rate", y = col_name)) +
      geom_point() +
      labs(title = paste("Scatter Plot of GDP Growth vs", col_name),
           x = "GDP Growth Rate",
           y = col_name) +
      theme_minimal()
    
    # Print the plot
    print(plot)
  }
}


ggplot(data = uk_data_subset, aes(x = Growth_Rate, y = Gross.savings)) +
  geom_point() +
  labs(title = "Scatter Plot of GDP Growth vs Other Variables",
       x = "GDP Growth Rate",
       y = "Other Variable") +
  theme_minimal()


ggpairs(uk_data_subset, 
        title = "Scatter Plot Matrix: GDP Growth, GDP, and Inflation",
        cardinality_threshold = 30)

for (col_name in colnames(numeric_data)) {
  if (col_name != "Growth_Rate" && is.numeric(numeric_data[[col_name]])) {
    plot(numeric_data$Growth_Rate, numeric_data[[col_name]],
         main = paste("GDP Growth vs", col_name),
         xlab = "GDP Growth Rate",
         ylab = col_name,
         pch = 19)  # Use filled circles for points
  }
}