# https://www.kaggle.com/datasets/tobyanderson/federalist-papers?resource=download

library(readr)
library(tidyverse)
library(tidymodels)
#library(agua)
library(discrim)
library(ggplot2)


data <- read_csv("archive/fedPapers85.csv") %>% 
  filter(!author %in%  c("HM", "Jay")) 

# disputed
test <- data %>% 
  filter(author == "dispt")

# split the rest
data <- data %>% 
  select(-filename) %>% 
  filter(author != "dispt")  %>% 
  mutate(author = as.factor(author)) 

set.seed(1234)
data_split <- data %>% 
  initial_split(0.8, strata = author)


data_train <- training(data_split)
data_valid <- testing(data_split)

# build model
nb_spec <-
  naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes") %>% 
  set_args(usepoisson = T)

nb_spec

# preprocessing
nb_rec <- recipe(author ~ ., data = data_valid) %>% 
  step_zv(all_predictors())

# workflow
nb_wflow <- workflow() %>% 
  add_model(nb_spec) %>% 
  #add_recipe(nb_rec)
  add_recipe(rec_pca)

nb_wflow

# fit model
model_fit <- nb_wflow %>% 
  fit(data_train)

model_fit %>% 
  extract_recipe(estimated = T)

# predict
pred <- predict(model_fit, data_train, type = "class")
tibble(predicted = pred, expected = data_test$author)

# vfolds
folds <- vfold_cv(data_train, strata = author, v = 10)

# fit_resamples
model_fit_resample <- nb_wflow %>% 
  fit_resamples(folds,
                control = control_resamples(save_pred = T))

# evaluate
collect_metrics(model_fit_resample)

pred <- collect_predictions(model_fit_resample)

# pred %>% 
#   group_by(id) %>% 
#   roc_curve(truth = author, .pred_Madison) %>% 
#   autoplot()

conf_mat_resampled(model_fit_resample, tidy = F) 

conf_mat_resampled(model_fit_resample, tidy = F) %>% 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "#eaeff6", high = "#233857") +
  theme(panel.grid.major = element_line(colour = "#233857"),
        axis.text = element_text(color = "#233857"),
        axis.title = element_text(color = "#233857"))
 
# estimate
pred %>% 
  group_by(id) %>% 
  accuracy(truth = author, estimate = .pred_class)

pred %>% 
  #group_by(id) %>% 
  f_meas(truth = author, estimate = .pred_class) # not so good

# disputed
predictions <- predict(model_fit, data_test, type = "class")

#  The modern consensus is that Madison wrote essays Nos. 49–58
tibble(text = dispt$filename, prediction = predictions$.pred_class)
