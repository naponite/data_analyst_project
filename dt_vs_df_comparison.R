library(tidyverse)
library(data.table)

# data.table
dt <- fread("Sleep_health_and_lifestyle_dataset.csv")
class(dt)

# data.frame
df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
class(df)


# data subset comparison
dt_subset <- dt[Gender == "Male" & Age >= 30,]
View(dt_subset)

df_subset <- df[df$Gender == "Male" & df$Age >= 30,]
View(df_subset)




# select column

dt[ ,1] # not different result with df

df[ ,1]


dt %>%
  select(Gender, Age) # can apply basic data transformation

