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
  mutate(std_index = (index - mean(index)) / sd(index)) %>%
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

final_data <- google_trends_data %>%
  inner_join(filtered_id_name_data, by = "schname") %>%
  inner_join(scorecard_data, by = c("unitid" = "UNITID", "opeid" = "OPEID"))

# Print the first few rows of the final data
head(final_data)

1+1

