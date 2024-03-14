library(dplyr)
library(feather)
library(skimr)
library(patchwork)

df_cli <- read_feather("clinic.feather", column = NULL)
df_demo <- read_feather("demographic.feather", column = NULL)
df_diag <- read_feather("diag.feather", column = NULL)

#skim(df_cli)

#skim(df_demo_raw)

#skim(df_diag_raw)


## clean sex in df_demo
df_demo <- df_demo %>%
  filter(sex != "") %>%
  filter(bdate != "0000-00-00" 
         & bdate != "NULL" 
         & bdate < 2023-12-31
         & bdate > 1800-01-01) 
skim(df_demo)

## clean icd10 in df_diag
df_diag <- df_diag %>%
  filter(icd10 != "")
skim(df_diag)

## data preparation

df_final <- df_diag %>%
  left_join(df_cli,by = c("code_cl" = "c_unit")) %>%
  left_join(df_demo, by = "id") %>%
  filter(grepl("อายุร",dep_name)) %>%
  filter(grepl("(?:C3[0-9].?|J44.?|J84.?|J45.?)",icd10)) %>%
  filter(between(date_cl,ymd("2021-10-01"),ymd("2022-09-30"))) %>%
  mutate(age = round(as.numeric((today() - as.Date(bdate))/365))) %>%
  filter(between(age,15,85)) %>%
  mutate(sex = if_else(sex == "1", "male", "female"))

write_csv(df_final,"psu_test_data.csv")


# Q1
df_q1_1 <- df_final %>%
  mutate(year = year(date_cl),
         month = month(date_cl),
         year_month = paste(year,month, sep = "-")) %>%
  select(id, year_month, sex, age) %>%
  group_by(year_month) %>%
  count()

write_csv(df_q1_1,"summary_data.csv")
View(df_q1_1)


ggplot(df_q1_1, aes(x=year_month, y=n))+
  geom_col()+
  theme_minimal()


# Q2
df_q1_2 <- df_final %>%
  mutate(age_range = if_else(between(age,15,25), "15-25",
                    if_else(between(age,26,35), "26-35",
                    if_else(between(age,36,45), "36-45",
                    if_else(between(age,46,55), "46-55",
                    if_else(between(age,56,65), "56-65",
                    if_else(between(age,66,75), "66-75",
                    if_else(between(age,76,85), "76-85","")))))))) %>%
  select(id,age,age_range,sex) 


df_q1_22 <- df_q1_2 %>%
  group_by(age_range, sex) %>%
  count()


p1 <- ggplot(df_q1_2, aes(x=age, y=age_range))+
  geom_boxplot()+
  facet_wrap(~sex, ncol=1)+
  theme_minimal()

p1



