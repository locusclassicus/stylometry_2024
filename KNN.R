# https://uo-datasci-specialization.github.io/c4-ml-fall-2020/slides/w6p1-knn/w6p1.pdf
# https://stacks.tidymodels.org/articles/basics.html 


# build model
knn_spec <-
  nearest_neighbor() %>% 
  set_mode("classification") %>% 
  set_engine("kknn") %>% 
  set_args(neighbors = 3,
           weight_func = "cos")

knn_spec

# preprocessing
rec_norm <- recipe(author ~ ., data = data_train) %>% 
  step_normalize(all_predictors())

# workflow 
knn_wflow <- workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(rec_pca)

knn_wflow

# fit model 
model_fit <- knn_wflow %>% 
  fit(data_train)

extract_recipe(model_fit)

# use model results
extract_fit_engine(model_fit)  %>% 
  summary()

# predict
pred <- predict(model_fit, data_valid, type = "class")

# this is so much better!
tibble(predicted = pred, expected = data_test$author)

# fit_resamples
model_fit_resample <- knn_spec %>% 
  fit_resamples(author ~ ., folds,
                control = control_resamples(save_pred = T))

# evaluate
collect_metrics(model_fit_resample)

pred <- collect_predictions(model_fit_resample)

conf_mat_resampled(model_fit_resample, tidy = F) 

conf_mat_resampled(model_fit_resample, tidy = F) %>% 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "#eaeff6", high = "#233857") +
  theme(panel.grid.major = element_line(colour = "#233857"),
        axis.text = element_text(color = "#233857"),
        axis.title = element_text(color = "#233857"))

# dispt
predictions <- predict(model_fit, dispt, type = "class")

#  The modern consensus is that Madison wrote essays Nos. 49–58
tibble(text = dispt$filename, prediction = predictions$.pred_class)



