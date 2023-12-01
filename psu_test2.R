library(dplyr)
library(feather)
library(ggplot2)
library(skimr)
library(lubridate)

df_cli <- read_feather("clinic.feather", columns = NULL)
df_demo_raw <- read_feather("demographic.feather", columns = NULL)
df_diag <- read_feather("diag.feather", columns = NULL)


skim(df_diag)
glimpse(df_diag) %>% head()

skim(df_demo_raw)
glimpse(df_demo_raw) %>% head()

skim(df_cli)
glimpse(df_cli) %>% head()

## cleaned data
df_demo <- df_demo_raw %>%  
  filter(bdate != "0000-00-00" & bdate != "NULL") 

df <- df_diag %>%
  left_join(df_demo, by = "id") %>%
  left_join(df_cli, by = c("code_cl" = "c_unit")) %>%
  filter(grepl("^E1[1-4].",df_diag$icd10)) %>%
  filter(grepl("อายุร",dep_name)) %>%
  filter(between(date_cl,ymd("2021-10-01"),ymd("2022-09-30"))) %>%
  mutate(age = as.numeric(today()-ymd(bdate))/365) %>%
  filter(between(age,15,75)) %>%
  mutate(sex = if_else(sex == 1, "male", "female"))

df %>% ggplot(aes(sex,age)) +
  geom_boxplot()


         
df_i10 <- df_diag %>%
  mutate(i10 = if_else(icd10 == "I10", "yes","no")) %>%
  filter(i10 == "yes") %>%
  select(id,i10)

df_final <- df %>%
  left_join(df_i10, by = "id") %>%
  mutate(i10 = if_else(is.na(i10), "no", "yes")) %>%
  filter(i10 == "no") %>%
  select(-i10) %>%
  mutate(age = round(age))


#Question 1

summary_overall <- df_final %>%
  mutate(year = year(date_cl),
         month = month(date_cl)) %>%
  select(year, month, code_cl, sex, age) %>%
  group_by(year, month, code_cl, sex) %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    n = n()
  ) %>%
  arrange(year,month,code_cl)

write_csv(summary_overall,"psu_hospital_data.csv")


#Question 2

df_Q2 <- df_final %>%
  group_by(id) %>%
  mutate(first_iden_date = min(date_cl)) %>%
  filter(first_iden_date == date_cl) %>%
  group_by(code_cl, u_name) %>%
  summarize(n = n(),
            avg_age = round(mean(age)),
            min_age = min(age),
            max_age = max(age)) %>%
  arrange(desc(n))
  


## Patient count by year,month
summary_ym <- df_final %>%
  mutate(year = year(date_cl),
         month = month(date_cl)) %>%
  select(year, month) %>%
  group_by(year, month) %>%
  count(month) %>%
  arrange(year,month)


P1 <- df_final %>% 
  mutate(year = year(date_cl),
         month = month(date_cl)) %>%
  select(year, month) %>%
  ggplot(aes(month))+
  geom_bar(fill = "blue")+
  facet_wrap(~year)




