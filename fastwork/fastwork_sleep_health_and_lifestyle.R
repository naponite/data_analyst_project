library(tidyverse)
library(ggplot2)
library(skimr)
library(stringr)
library(knitr)
library(ggpubr)
library(patchwork)
library(descr)


# 1. Data selection
df <- read_csv("Sleep_health_and_lifestyle_dataset.csv")

glimpse(df)
skim(df)


# 2. Data Preprocessing 

# Separate blood pressure columns > systolic , diastolic

df_prep <- df %>%
  mutate(bp_systolic = str_extract(`Blood Pressure`,"([1-9].+)/", group = TRUE),
         bp_diastolic = str_extract(`Blood Pressure`,"/([1-9].+)", group = TRUE)) %>%
  mutate(bp_systolic = as.numeric(bp_systolic),
         bp_diastolic = as.numeric(bp_diastolic)) %>%
  mutate(`BMI Category` = if_else(`BMI Category` == "Normal Weight","Normal",`BMI Category`)) %>%
  select(-`Blood Pressure`)

write_csv(df_prep,"preprocessed_data.csv")


# 3. Exploratory Data Analysis

summary(df_prep)

# Q1: How many person separate by gender and occupation (table)

df_p1 <- df_prep %>%
  select(`Person ID`, Gender, Age, Occupation, `Quality of Sleep`, 
         `Physical Activity Level`, `Stress Level`)

df_p1 %>%
  group_by(Gender, Occupation) %>%
  count() %>%
  kable(caption = "Person separated by gender and occupation")

  

# Q2: average sleep duration separate by 
# by occupation (bar chart)


df_p2 <- df_prep %>%
  select(`Person ID`, Age, Occupation, `Sleep Duration`)

p1 <- df_p2 %>%
  group_by(Occupation) %>%
  summarise(avg_sleep_duration = mean(`Sleep Duration`)) %>%
  ggplot(aes(x = avg_sleep_duration , y = reorder(Occupation, avg_sleep_duration)))+
  geom_col(fill = "orange")
  

p1

# by age (bar chart)
p2 <- df_p2 %>%
  mutate(age_range =  if_else(between(Age,27,30), "27-30",
                      if_else(between(Age,31,40), "31-40",
                      if_else(between(Age,41,50), "41-50",
                      if_else(between(Age,51,59), "51-59",""))))) %>%
  group_by(age_range) %>%
  summarise(avg_sleep_duration = mean(`Sleep Duration`)) %>%
  ggplot(aes(x = age_range, y = avg_sleep_duration))+
  geom_col(fill = "pink")+
  labs(title = "Average sleep duration separate by age",
       x = "Age range",
       y = "Sleep duration (hrs)")+
  geom_text(aes(label = round(avg_sleep_duration,1)), vjust = -0.5, hjust = 0.5)+
  theme_minimal()
  
  
p2

# Q3: relation between 
# -sleep duration and sleep quality (scatter)

p3 <- df_prep %>%
  select(`Sleep Duration`, `Quality of Sleep`) %>%
  ggplot(aes(x = `Sleep Duration`, y = `Quality of Sleep`))+
  geom_point()+
  geom_smooth(method = "lm",
              col = "red")+
  stat_cor(method = "pearson", label.x = 5, label.y = 10,
           col = "blue")+
  theme_minimal()

p3


# - sleep quality vs physical activity (scatter)

p4 <- df_prep %>%
  select(`Physical Activity Level`, `Quality of Sleep`) %>%
  ggplot(aes(x = `Physical Activity Level`, y = `Quality of Sleep`))+
  geom_point()+
  geom_smooth(method = "lm",
              col = "red")+
  stat_cor(method = "pearson",
           col = "blue")+
  theme_minimal()

p4

# - sleep quality vs stress level
p5 <- df_prep %>%
  select(`Stress Level`, `Quality of Sleep`) %>%
  ggplot(aes(x = `Stress Level`, y = `Quality of Sleep`))+
  geom_point()+
  geom_smooth(method = "lm",
              col = "red")+
  stat_cor(method = "pearson",
           col = "blue")+
  theme_minimal()

p5

# - physical activity vs stress level
p6 <- df_prep %>%
  select(`Stress Level`, `Physical Activity Level`) %>%
  ggplot(aes(x = `Physical Activity Level`, y = `Stress Level`))+
  geom_point()+
  geom_smooth(method = "lm",
              col = "red")+
  stat_cor(method = "pearson",
           col = "blue")+
  theme_minimal()

p6

# - physical activity vs step
p7 <- df_prep %>%
  select(`Daily Steps`, `Physical Activity Level`) %>%
  ggplot(aes(x = `Daily Steps`, y = `Physical Activity Level`))+
  geom_point()+
  geom_smooth(method = "lm",
              col = "red")+
  stat_cor(method = "pearson",
           col = "blue")+
  theme_minimal()

p7

(p3+p4)/(p5+p6)/p7

# Sleep quality by BMI category

p11 <- df_prep %>%
  select(`Quality of Sleep`, `BMI Category`) %>%
  ggplot(aes(x = `BMI Category`, y = `Quality of Sleep`))+
  geom_boxplot()+
  theme_minimal()

p11



# Q5: Sleep disorder vs 
# - sleep quality


df_p3 <- df_prep %>%
  select(`Quality of Sleep`, `Stress Level`, `Physical Activity Level`, `Sleep Disorder`)

p8 <- df_p3 %>%
  ggplot(aes(x = `Sleep Disorder`, y = `Quality of Sleep`))+
  geom_boxplot()+
  theme_minimal()

p8

# - stress
p9 <- df_p3 %>%
  ggplot(aes(x = `Sleep Disorder`, y = `Stress Level`))+
  geom_boxplot()+
  theme_minimal()

p9


# - physical activity

p10 <- df_p3 %>%
  ggplot(aes(x = `Sleep Disorder`, y = `Physical Activity Level`))+
  geom_boxplot()+
  theme_minimal()

p10

p8+p9+p10



### 4. Hypothesis Testing

#- Ho: BMI category has NO relationship with sleep disorder
#- Ha: BMI category has relationship with sleep disorder


df_h1 <- df_prep %>%
    select(`Person ID` ,`BMI Category`,`Sleep Disorder`) 


crosstab(df_h1$`BMI Category`,df_h1$`Sleep Disorder`,
         prop.c = TRUE,
         chisq = TRUE,
         plot = FALSE)




#- Ho: BMI category has NO relationship with daily step
#- Ha: BMI category has relationship with daily step

df_h2 <- df_prep %>%
  select(`Person ID` ,`BMI Category`,`Daily Steps`) %>%
  mutate(daily_steps = if_else(`Daily Steps` <= 4000, "0-4000",
                        if_else(between(`Daily Steps`,4001,6000),"4001-6000",
                        if_else(between(`Daily Steps`,6001,8000),"6001-8000",
                        if_else(between(`Daily Steps`,8001,10000),"8001-10000","other")))))


crosstab(df_h2$daily_steps,df_h2$`BMI Category`,
         prop.c = TRUE,
         chisq = TRUE,
         plot = FALSE)



## BMI category group_by occupation

df_i1 <- df_prep %>%
  select(`Person ID`, Occupation, `BMI Category`) %>%
  mutate(normal = if_else(df_prep$`BMI Category` == "Normal",1,0),
         obese = if_else(df_prep$`BMI Category` == "Obese",1,0),
         overweight = if_else(df_prep$`BMI Category` == "Overweight",1,0)) %>%
  group_by(Occupation) %>%
  summarise(persons = n(),
            total_normal = sum(normal),
            total_obese = sum(obese),
            total_overweight = sum(overweight)) %>%
  mutate(percent_overweight = round(total_overweight / persons *100)) %>%
  arrange(desc(percent_overweight))





