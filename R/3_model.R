source("R/utils.R")
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

read_data("data/generated/df_pairs_feature.csv")
read_data("data/generated/df_pairs.csv")

df_pairs_feature <- 
  df_pairs_feature %>% 
  mutate(match = match %>% factor(levels = c("unmatch", "match"))) 
  
set.seed(1)
train_indices <- 
  sample(1:nrow(df_pairs_feature), size = floor(nrow(df_pairs_feature)*0.85))

df_train <- 
  df_pairs_feature[train_indices,]

df_test <- 
  df_pairs_feature %>% 
  bind_cols(df_pairs) %>% 
  .[-train_indices,]

set.seed(2)
fold_indices <- createFolds(df_train$match)

train_control <- 
  trainControl(index = fold_indices,
               method = "cv", 
               number = 10,
               verboseIter = TRUE,
               savePredictions = TRUE,
               classProbs = TRUE)

set.seed(3)
(model_rf.grid_yancey.train_features.all <-
    train(match ~ .,
          df_train,
          # trControl = trainControl(method = "cv", number = 10),
          trControl = train_control,
          tuneGrid = expand.grid(.mtry = seq(3, 30, 3)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf.grid_yancey.train_features.all) %>% plot()
#write_rds(model_rf.grid_yancey.train_features.all, "data/model_rf.grid_yancey.train_features.all.rds")
#read_data("data/model_rf.grid_yancey.train_features.all.rds")



df_pairs_feature_imp <-
  df_pairs_feature %>% 
  select(matches(or("name_jw", "year", "age", "metric_race_code_same", 
                    "metric_gender_code", "freq_max", "freq_diff", 
                    "metric_distance_from_identical", "match")))

df_train_imp <- 
  df_pairs_feature_imp[train_indices, ]

set.seed(2)
(model_rf.mtry2_yancey.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          # tuneGrid = expand.grid(.mtry = 2),
          tuneGrid = expand.grid(.mtry = 2:10),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf.mtry2_yancey.train_features.imp) %>% plot()

#write_rds(model_rf.mtry2_yancey.train_features.imp, "data/model_rf.mtry2_yancey.train_features.imp.rds")
#read_data("data/model_rf.mtry2_yancey.train_features.imp.rds")

