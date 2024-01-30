library(tidyverse) #ggplot2 included
library(feather)
library(skimr)
library(patchwork)

# Import data
df_cli <- read_feather("Original data/clinic.feather", column = NULL)
df_demo_raw <- read_feather("Original data/demographic.feather", column = NULL)
df_diag_raw <- read_feather("Original data/diag.feather", column = NULL)


# Data preview
skim(df_cli)

skim(df_demo_raw)

skim(df_diag_raw)


## Data cleaning

## clean sex in df_demo
df_demo <- df_demo_raw %>%
  filter(sex != "") %>%
  filter(bdate != "0000-00-00" 
         & bdate != "NULL" )

skim(df_demo)

## clean icd10 in df_diag
df_diag <- df_diag_raw %>%
  filter(icd10 != "")
skim(df_diag)

## data preparation
df_join <- df_diag %>%
  left_join(df_cli,by = c("code_cl" = "c_unit")) %>%
  left_join(df_demo, by = "id")

df_final <- df_join %>%
  mutate(age = round(as.numeric((today() - as.Date(bdate))/365))) %>%
  filter(between(age,15,85)) %>%
  filter(grepl("อายุร",dep_name)) %>%
  filter(grepl("(?:C34.?|J44.?|J84.?|J45.?)",icd10)) %>%
  filter(between(date_cl,ymd("2021-10-01"),ymd("2022-09-30"))) %>%
  mutate(sex = if_else(sex == "1", "male",
              if_else(sex == "2", "female", ""))) %>%
  mutate(group = if_else(grepl("C34.?",icd10),"lung_cancer",
                if_else(grepl("(?:J44.?|J84.?)",icd10),"COPD",
                if_else(grepl("J45.?",icd10),"asthma","other"))))

write_csv(df_final,"psu_test_data.csv")


# Q1
# filter lung cancer
df_q1 <- df_final %>%
  filter(group == "lung_cancer")
head(df_q1)

df_q1_1 <- df_q1 %>%
  mutate(year = year(date_cl),
         month = month(date_cl),
         year_month = paste(year,month, sep = "-")) %>%
  select(id, year_month, sex, age) %>%
  group_by(year_month) %>%
  count()

# export data
write_csv(df_q1_1,"lung_cancer_patient_statistic.csv")
View(df_q1_1)


ggplot(df_q1_1, aes(x=year_month, y=n))+
  geom_col(fill = "orange")+
  theme_minimal()+
  labs(title = "Lung cancer patient statistic between Oct-2021 to Sep-2022",
       x = "Year_Month",
       y = "patient (person)")+
  geom_text(aes(label = n), vjust = -0.5, hjust = 0.5)



# Q2
# age range grouping
df_q1_2 <- df_q1 %>%
  mutate(age_range = if_else(between(age,15,25), "15-25",
                    if_else(between(age,26,35), "26-35",
                    if_else(between(age,36,45), "36-45",
                    if_else(between(age,46,55), "46-55",
                    if_else(between(age,56,65), "56-65",
                    if_else(between(age,66,75), "66-75",
                    if_else(between(age,76,85), "76-85","")))))))) %>%
  select(id,age,age_range,sex) 



df_q1_22 <- df_q1_2 %>%
  group_by(age_range, sex) %>%    ## to add ratio
  count()

View(df_q1_22)

p1 <- ggplot(df_q1_2, aes(x=age, y=age_range))+
  geom_boxplot(col = "blue")+
  facet_wrap(~sex, ncol=1)+
  theme_minimal()+
  labs(title = "สถิติผู้ป่วยมะเร็งปอด จำแนกตามเพศ และช่วงอายุ",
       x = "ช่วงอายุ (Age range)",
       y = "Age")

p1



## Prepare data for Q3,4,5

df_p1 <- df_final %>%
  select(id, age,date_cl, code_cl, icd10, group) %>%
  group_by(id, group) %>%
  summarise(first_iden = min(date_cl))

write_csv(df_p1,"first_identify.csv")


head(df_p1)


df_p2 <- df_p1 %>%
  group_by(id) %>%
  summarise(asthma = sum(count_asth),
            copd = sum(count_copd),
            lung_cancer = sum(count_lung),
            first_asth = max(first_asth),
            first_copd = max(first_copd),
            first_lung = max(first_lung)) %>%
  
  mutate(first_asth = if_else(first_asth == 0, ymd(""),as.Date(first_asth)),
         first_copd = if_else(first_copd == 0, ymd(""),as.Date(first_asth)),
         first_lung = if_else(first_lung == 0, ymd(""),as.Date(first_asth)))


############### recheck wrong date ##########################

write_csv(df_p2,"data_prep_for_Q345.csv")

df_p3 <- df_p2 %>%
  filter((asthma + copd + lung_cancer) > 1) %>%
  filter(lung_cancer != 0)

View(df_p3)

## Q3     ############# back to improve ##############

df_q1_3 <- df_p2 %>%
  group_by(lung_cancer) %>%
  summarise(asthma = sum(asthma),
            copd = sum(copd)) %>%
  mutate(lung_cancer = if_else(lung_cancer == 0,"no","yes"),
         total = (asthma + copd))

  


## Q4

df_q1_4 <- df_p3 %>%
  mutate(asthma_vs_lc = if_else((asthma == 1 | )))

View(df_q1_4)

## Q6

df_q1_6 <-  df_final %>%
  select(id, age,date_cl, code_cl, u_name, icd10, group) %>%
  filter(group == "lung_cancer") %>%
  group_by(id, code_cl, u_name, age) %>%
  summarise(first_iden = min(date_cl)) %>%
  group_by(code_cl, u_name) %>%
  summarise(min_age = min(age),
            max_age = max(age),
            avg_age = round(mean(age)),
            n = n()) %>%
  arrange(desc(n))

write_csv(df_q1_6,"top_clinic_lung_cancer_iden.csv")
  
View(df_q1_6)


## Q7 need present all icd10 meaning ???????

df_q1_71 <- df_join %>%
  select(id, date_cl, icd10) %>%
  filter(grepl("C34.?",icd10)) %>%
  group_by(id) %>%
  count() %>%
  mutate(lung_cancer = "yes") %>%
  select(id, lung_cancer)

df_q1_72 <- df_join %>%
  mutate(age = round(as.numeric((today() - as.Date(bdate))/365))) %>%
  filter(between(age,15,85)) %>%
  filter(grepl("อายุร",dep_name)) %>%
  filter(between(date_cl,ymd("2021-10-01"),ymd("2022-09-30"))) %>%
  select(id, icd10) %>%
  left_join(df_q1_71, by = "id") %>%
  filter(!is.na(lung_cancer)) %>%
  filter(!grepl("C34.?",icd10)) %>%
  group_by(icd10) %>%
  count() %>%
  filter(grepl("[A-z].+",icd10)) %>%
  select(icd10) 

# export data
write_csv(df_q1_72,"other_comorbidity.csv")

View(df_q1_72)


## Q8

df_q1_8 <- df_final %>%
  select(date_cl, code_cl, u_name) %>%
  group_by(code_cl, u_name) %>%
  summarise(yearly = n(),
            monthly = round(n()/12),
            daily = round(n()/365)) %>%
  arrange(desc(yearly))

View(df_q1_8)

df_q1_8 %>% ggplot(aes(x = daily, y = reorder(code_cl,daily, decreasing = FALSE)))+
  geom_col()+
  theme_minimal()+
  labs(title = "สถิติผู้ป่วยที่เข้ารับบริการต่อวัน ในแต่ละคลินิก",
       x = "จำนวนผู้ใช้บริการต่อวัน (คน ต่อ วัน)",
       y = "Clinic code",
       caption = "O13(อายุรกรรม)")

write_csv(df_q1_8,"daily_patient_record.csv")






