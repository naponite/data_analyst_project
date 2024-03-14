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
  select(year, month, code_cl,u_name, sex, age) %>%
  group_by(year, month, code_cl,u_name, sex) %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    n = n()
  ) %>%
  arrange(year,month,code_cl)

write_csv(summary_overall,"psu_hospital_data.csv")


# summary patient by Year-Month
summary_ym <- summary_overall %>%
  mutate(year_month = ym(paste(year,month, sep = "-"))) %>%
  group_by(year_month) %>%
  summarise( total_person = sum(n)) %>%
  select(year_month, total_person)


ggplot(summary_ym, aes(x = year_month, y = total_person))+
  geom_line(col="blue")+
  theme_minimal()+
  labs(title = "Patient count by Month between 2021-2022",
       x = "Month-Year",
       y = "Total(person)",
       )

# summary patient by clinic
summary_cli <- summary_overall %>%
  select(code_cl,u_name,n) %>%
  group_by(code_cl,u_name) %>%
  summarise(total_person = sum(n)) %>%
  arrange(desc(total_person)) %>%
  mutate(clinic_code = factor(code_cl, levels = unique(code_cl)))
  

ggplot(summary_cli, 
       aes(y = reorder(code_cl,total_person,decreasing = FALSE),
           x = total_person))+
  geom_col()+
  theme_minimal()+
  labs(title = "Patient count by clinic",
       x = "Clinic code",
       y = "Total (person)",
       caption = "O13 = อายุรกรรม")


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
  


