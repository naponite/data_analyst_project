library(dplyr)
library(feather)
library(lubridate)

rd1 <- read_feather('diag.feather', columns = NULL)
df_diag <- data.frame(rd1)

rd2 <- read_feather('clinic.feather', columns = NULL)
df_clinic <- data.frame(rd2)

rd3 <- read_feather('demographic.feather', columns = NULL)
df_demo <- data.frame(rd3)

fil_i10 <- df_diag %>% 
  filter(grepl("I10",df_diag$icd10)) %>%
  filter(code_cl == "O13") %>% 
  filter(between(date_cl,ymd(20211001),ymd(20220930))) %>%
  left_join(df_demo,by = "id") %>%
  mutate(is_I10 = "YES") %>%
  select(id,is_I10) %>%
  unique()


fil_diag <- df_diag %>% 
  filter(grepl("E1[1-4]{1}.",df_diag$icd10)) %>%
  filter(code_cl == "O13") %>% 
  filter(between(date_cl,ymd(20211001),ymd(20220930))) %>%
  left_join(df_demo,by = "id") %>%
  left_join(fil_i10,by = "id")

df_final <- fil_diag %>%
  mutate(age = round(as.numeric(ymd(today())-ymd(fil_diag$bdate))/365 )) %>%
  filter(between(age,15,75)) %>%
  filter(is.na(is_I10))

