# Code for exploratory analysis

library(tidyverse)
library(ggplot2)
library(here)
library(janitor)
library(patchwork)



# set data file path
file <- here("data", "healthcare_datasets.zip")

#display all files in healthcare_datasets.zip
unzip(file, list = TRUE)

# set data path in zip file
file1 = here("data", unzip(file, list = TRUE)[1,1])
file2 = here("data", unzip(file, list = TRUE)[2,1])

# read file healthcare-coverage
healthcare_coverage <- read_csv(file1, skip = 2, na = c("", "N/A"))
view(healthcare_coverage)

# read file healthcare-spending
healthcare_spending <- read_csv(file2, skip = 2, na = c("", "N/A"))
view(healthcare_spending)


# data preparation

# 1.clean missing data
## healthcare_coverage
healthcare_coverage %>%
  remove_missing() -> coverage_clean

view(coverage_clean)

## healthcare_spending
healthcare_spending %>%
  remove_missing() -> spending_clean

view(spending_clean)


# 2.join data
coverage_clean %>%
  left_join(spending_clean, by = "Location") -> data_join

view(data_join)


# 3.data transformation
data_join %>% 

  # filter "United States"
  filter(Location == "United States") %>%

  # convert wide to long format table
  pivot_longer(cols = -Location,
               names_to = c("year", ".value"),
               names_sep = "__",
               values_to = "coverage_spend") %>% 
  
  # arrange year
  arrange(year) %>%
  
  # clean column names
  clean_names() %>%

  # rename column
  rename(total_coverage = total) -> data_prep


# 4.data exploration
data_prep %>% 
  ggplot(aes(x = year, y = total_coverage)) +
  geom_col(fill = "blue") -> p1

data_prep %>% 
  ggplot(aes(x = year, y = total_health_spending)) +
  geom_col(fill = "blue") -> p2

data_prep %>% 
  ggplot(aes(x = total_coverage, y = total_health_spending)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") -> p3

(p1+p2)/p3





