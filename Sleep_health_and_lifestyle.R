library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)

df <- read.csv('Sleep_health_and_lifestyle_dataset.csv', stringsAsFactors = FALSE)


# review data structure
glimpse(df)


# Overview health parameter group by sleep disorder

sleep_dis <- df %>%
  select(Sleep.Duration, Quality.of.Sleep, Physical.Activity.Level
         ,Stress.Level, Heart.Rate, Daily.Steps, Sleep.Disorder) %>%
  group_by(Sleep.Disorder) %>%
  summarise(n = n(),
            avg_sleep_duration = mean(Sleep.Duration),
            avg_sleep_quality = mean(Quality.of.Sleep),
            avg_act = mean(Physical.Activity.Level),
            avg_hstress = mean(Stress.Level),
            avg_hrate = mean(Heart.Rate),
            avg_step = mean(Daily.Steps)) %>%
  arrange(avg_sleep_duration)


df_normal <- df %>%
  select(Sleep.Duration, Quality.of.Sleep, Physical.Activity.Level
         ,Stress.Level, Heart.Rate, Daily.Steps, Sleep.Disorder) %>%
  filter(Sleep.Disorder == 'None')

df_apnea <- df %>%
  select(Sleep.Duration, Quality.of.Sleep, Physical.Activity.Level
         ,Stress.Level, Heart.Rate, Daily.Steps, Sleep.Disorder) %>%
  filter(Sleep.Disorder == 'Sleep Apnea')

df_insomnia <- df %>%
  select(Sleep.Duration, Quality.of.Sleep, Physical.Activity.Level
         ,Stress.Level, Heart.Rate, Daily.Steps, Sleep.Disorder) %>%
  filter(Sleep.Disorder == 'Insomnia')



# Find P-value for significant check

t.test(df_insomnia$Stress.Level, df_normal$Stress.Level,
       alternative = "two.sided",
       mu = 0,
       var.equal = TRUE)

t.test(df_insomnia$Stress.Level, df_apnea$Stress.Level,
       alternative = "two.sided",
       mu = 0,
       var.equal = TRUE)

t.test(df_apnea$Stress.Level, df_normal$Stress.Level,
       alternative = "two.sided",
       mu = 0,
       var.equal = TRUE)



## Train model  

prep_df <- df %>%
  select(-Person.ID, -Occupation, -Blood.Pressure)

# 1. split

set.seed(24)
n <- nrow(df)
id <- sample(n, size = 0.8*n)
train_df <- prep_df[id, ]
test_df <- prep_df[-id, ]



# 2. train

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

tree_model <- train(Sleep.Disorder ~. ,
                    data = train_df,
                    method = "rpart",
                    # complexity parameter
                    # high cp -> good generalization
                    tuneGrid = expand.grid(cp = c(0.02, 0.1, 0.25)),
                    trControl = ctrl)

rpart.plot(tree_model$finalModel)

# 3. score

p <- predict(tree_model, newdata = test_df)

# 4. evaluate model

confusionMatrix(p, factor(test_df$Sleep.Disorder),
                positive = "Insomnia",
                mode = "prec_recall")

## found Error: `data` and `reference` should be factors with the same levels.
## because p data is "factor" but test_df$Sleep.Disorder is "text"
## solved by "factor()"



## compare model with resamples

model1 <- train(Sleep.Disorder ~. ,
                data = train_df,
                method = "rpart",
                trControl = trainControl(method = "cv",
                                         number = 5,
                                         verboseIter = TRUE)
)
                
model2 <- train(Sleep.Disorder ~. ,
                data = train_df,
                method = "rf",
                trControl = trainControl(method = "cv",
                                         number = 5,
                                         verboseIter = TRUE)
)
  
  
list_model <- list(
  decisiontree = model1,
  randomforest = model2
  
)

result <- resamples(list_model)

summary(result)
