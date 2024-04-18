library(corrr)

null_spec <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("classification")

rec_nzv <-  recipe(author ~ ., data = data_valid) %>% 
  step_nzv(all_predictors())
rec_norm <- recipe(author ~ ., data = data_train) %>% 
  step_normalize(all_predictors())
rec_pca <- recipe(author ~ ., data = data_train) %>% 
  step_pca(all_predictors(), num_comp = 10) 

wflow_set <- workflow_set(preproc = list(rec_nzv, rec_norm, rec_pca),
                          models = list(nb_spec, knn_spec, null_spec))

summary(wflow_set)

keep_pred = control_resamples(save_pred = T, save_workflow = T)

models <- wflow_set %>% 
  workflow_map("fit_resamples", 
               seed = 123, verbose = T, 
               resamples = folds, control = keep_pred)


# evaluate models
est_sum <- collect_metrics(models, summarize = T) %>% 
  filter(.metric == "accuracy")

est_sum %>% 
  filter(str_detect(wflow_id, "Bay"))

# try it
model <- fit()

# evaluate correlation 
acc_indiv_estimates <- collect_metrics(models, summarize = F) %>% 
  filter(.metric == "accuracy")

acc_wider <- models_est %>% 
  select(wflow_id, .estimate, id) %>% 
  pivot_wider(id_cols = "id", names_from = "wflow_id", values_from = ".estimate")

corr_tbl <- corrr::correlate(acc_wider %>% select(-id), quiet = T)

# this shows within resamples correlation
acc_indiv_estimates %>%
  mutate(wflow_id = reorder(wflow_id, .estimate)) %>%
  ggplot(aes(x = wflow_id, y = .estimate, group = id, color = id, lty = id)) +
  geom_line(alpha = .8, lwd = 1.25) +
  theme(legend.position = "none")

# this evaluates the magnitude of correlations
acc_wider %>% 
  with( cor.test(recipe_1_naive_Bayes, recipe_2_nearest_neighbor) ) %>% 
  tidy()

