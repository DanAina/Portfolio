library(tidyverse)
library(dplyr)
library(rio)
library(lubridate)


# Aggregating Google Tends data

trends_files <- list.files( path = "/Users/danielaina/Desktop/OMSBA R/Lab3_Rawdata",
                            pattern = "^trends_up_to.*\\.csv$",
                            full.names = TRUE)
google_trends_data <- bind_rows(lapply(trends_files, read_csv))


# Extract first ten characters from monthorweek

google_trends_data <- google_trends_data %>%
  mutate(date_variable = ymd(str_sub(monthorweek, 1, 10)))

# Aggregate to months using floor_date
google_trends_data <- google_trends_data %>%
  mutate(monthly_date = floor_date(date_variable, unit = 'month'))


# Standardize the index variable within each school and keyword
google_trends_data <- google_trends_data %>%
  group_by(schname, keynum) %>%
  mutate(std_index = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()

# aggregate to the school-month level

aggregated_data <- google_trends_data %>%
  group_by(schname, monthly_date) %>%
  summarize(mean_std_index = mean(std_index, na.rm = TRUE))


# Print the first few rows of the aggregated data
head(aggregated_data)

# adding Score card data

scorecard_data <- read_csv("/Users/danielaina/Desktop/OMSBA R/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

# reading in id_name_link data

id_name_data <- import("/Users/danielaina/Desktop/OMSBA R/Lab3_Rawdata/id_name_link.csv")



schools <- id_name_data %>%
  group_by(schname) %>%
  mutate(n = n())

# Filter out schools that show up more than once
filtered_id_name_data <- schools %>%
  filter(n == 1) %>%
  select(schname, unitid, opeid)

final_data <- aggregated_data %>%
  inner_join(filtered_id_name_data, by = "schname") %>%
  inner_join(scorecard_data, by = c("unitid" = "UNITID", "opeid" = "OPEID")) %>%
  filter(PREDDEG == 3) %>% filter(monthly_date >= as.Date("2015-09-01")) %>%
  rename("median_earnings_" = "md_earn_wne_p10-REPORTED-EARNINGS")

# create the median income to set low and high value

threshold <- median(final_data$`median_earnings_`) # The median threshold income is 42,000





# Print the first few rows of the final data
head(final_data)



final_data$PostScorecard <- ifelse(final_data$monthly_date >= release_date, 1, 0)

# Regression Analysis  for dates before score card release vs after
library(fixest)

final_data$HighEarning <- ifelse(final_data$median_earnings_ >= threshold, 1, 0)

final_data$LowEarning <- ifelse(final_data$median_earnings_ <= threshold, 1, 0)


#create date value for date
release_date <- as.Date("2015-09-01")

#binary value for date


# regression 

# Binary regression analysis
reg_model <- feols(log(mean_std_index) ~ HighEarning +LowEarning + monthly_date, data = final_data)

 etable(reg_model)
# Print model summary
summary(reg_model)

# Make predictions
final_data$predicted_prob <- predict(reg_model, type = "response")

# Graph

ggplot(final_data, aes(x = median_earnings_ , y = mean_std_index, color = factor(HighEarning + LowEarning))) +
  geom_point() +  # scatter plot of observed data
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # fitted regression line
  labs(title = "Regression of mean_std_index on HighEarning",
       x = "Earnings",
       y = "mean_std_index")+
  scale_color_manual(values = c("red", "blue"),labels = c("Low Earnings", "High Earnings")) +
  theme_minimal()












