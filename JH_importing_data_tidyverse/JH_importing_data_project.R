library(tidyverse)
library(readr)
library(readxl)
library(here)
library(RSQLite)
library(jsonlite)

# Q1,2
# Read excel file
excel_path <- here("JH_importing_data_tidyverse", "excel_data.xlsx")

data_excel1 <- read_excel(excel_path, sheet = 1)
view(data_excel1)

data_excel2 <- read_excel(excel_path, sheet = 2)
view(data_excel2)

  # find mean
data_excel2 %>% 
  select(X12) %>% 
  summarise(avg_X12 = mean(X12))
  
  # find correlation
cor(x = data_excel1$X5, y = data_excel2$X8)




# Q3
# Read db file
db_path <- here("JH_importing_data_tidyverse", "sqlite_data.db")
  
  # 1. open connection
con <- dbConnect(SQLite(),db_path)

  # 2. get data
dbListTables(con)
dbListFields(con, "table1")

query <- "SELECT * FROM table1
          WHERE ID = 8" 
data_query <- dbGetQuery(con, query)
view(data_query)

  # 3. find correlation
cor(x = data_query$S2, y = data_query$S3)





# Q4
# Read json file
json_path <- here("JH_importing_data_tidyverse", "table2.json")

  # option 1
data_json1 <- read_json(json_path, simplifyVector = TRUE)
view(data_json1)

  # option 2
data_json2 <- fromJSON(json_path)
view(data_json2)

  # join table
data_excel2 %>%
  inner_join(data_json1, by = "ID") %>%
  summarise(avg_J2 = mean(J2))

  # find correlation
data_excel2 %>%
  inner_join(data_json1, by = "ID") -> data_join

cor(x = data_join$X2, y = data_join$J4)







