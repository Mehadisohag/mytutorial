# Import my data set and renamed the data as my_data

read.csv("1234.csv")
my_data <- read.csv("1234.csv")

# To make the summary of the bibliographic status (Top publishing years, less publishing years)

head(my_data)
tail(my_data)
View(my_data)
install.packages("tidyverse")
library(tidyverse)
require(tidyverse)
my_data[1,3]
my_data[ ,3]
my_data$Total.publication
library(tidyverse)
require(tidyverse)
my_data %>%
  select(Ranking, Year, Total.publication, Percentage) %>%
  filter(Ranking > 10 , Total.publication> 75 & Percentage> 10.41) %>%
  arrange(Total.publication)
my_data %>%
  select(Ranking, Year, Total.publication, Percentage) %>%
  filter(Ranking > 10 , Total.publication> 75 & Percentage> 14.1) %>%
  arrange(Total.publication)
my_data %>%
  select(Ranking, Year, Total.publication, Percentage) %>%
  filter(Ranking > 10 , Total.publication> 75 & Percentage> 14.3) %>%
  arrange(Total.publication)

summary(my_data)

view

# Visualizing the Histogram of the AMR publications Per Year (over the years)

library(dplyr)

# Filter data for years 2000 to 2023
filtered_data <- my_data %>%
  filter(Year >= 2000 & Year <= 2023)

# Plot
ggplot(filtered_data, aes(x = factor(Year), y = Total.publication, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total.publication), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Number of Publications per Year (2000-2023)", x = "Year", y = "Number of Publications") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot for the Trends of Publication over the years

library(ggplot2)
ggplot(my_data, aes(x = Year, y= Total.publication)) +
  geom_point() +
  ylim(0,150) +
  labs(title = "Trends of Publications in Antibiotic Resistance in Bangladesh", x = "Total Publication", y = "Year")

# Line Graph of the Trends of Publication over the years

library(ggplot2)
ggplot(my_data, aes(x = Year, y = Total.publication)) +
  geom_line(color = "blue") +
  labs(title = "Trend of Total Publications Over the Years", x = "Year", y = "Total Publications")


# Calculate the annual growth rate
annual_growth_rate <- function(data) {
  n <- length(data)
  initial_value <- data[1]
  final_value <- data[n]
  
  growth_rate <- ((final_value / initial_value)^(1/n) - 1) * 100
  
  return(growth_rate)
}

# Call the function with the data
growth_rate <- annual_growth_rate(publications)
print(paste("The annual growth rate of antimicrobial resistance research in Bangladesh from 2000 to 2023 is:", round(growth_rate, 2), "%"))

# Calculate the doubling time

doubling_time <- function(data) {
  n <- length(data)
  initial_value <- data[1]
  final_value <- data[n]
  
  doubling_time <- log(2) / (log(final_value / initial_value) / n)
  
  return(doubling_time)
}

# Call the function with the data
time <- doubling_time(publications)
print(paste("The doubling time of antimicrobial resistance research publications in Bangladesh from 2000 to 2023 is approximately:", round(time, 2), "years"))
