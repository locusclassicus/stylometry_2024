lasso_spec <- logistic_reg(penalty = 0.1, mixture = 1) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

lasso_spec

lasso_rec <- recipe(author ~ ., data = data_train)

lasso_wflow <- workflow() %>%
  add_recipe(lasso_rec) %>% 
  add_model(lasso_spec)

lasso_res <- fit_resamples(lasso_wflow, folds, 
                           control = control_resamples(save_pred = T))

lasso_est <- collect_metrics(lasso_res)
lasso_est

lasso_pred <- collect_predictions(lasso_res)
lasso_pred


lasso_pred %>% 
  group_by(id) %>% 
  roc_curve(truth = author, .pred_Madison) %>% 
  autoplot()

conf_mat_resampled(lasso_res, tidy = F) %>% 
  autoplot(type = "heatmap")

# tuning
tune_spec <- logistic_reg(penalty = tune(),
                          mixture = 1) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

tune_spec

lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

tune_wf <- workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso_spec)

set.seed(2020)
tune_res <- tune_grid(
  tune_wf, 
  folds, 
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)

tune_res

collect_metrics(tune_res)

autoplot(tune_res)

tune_res %>% 
  show_best(metric = "accuracy")

chosen_acc <- tune_res %>% 
  select_by_one_std_err(metric = "accuracy", -penalty)

final_lasso <- finalize_workflow(tune_wf, chosen_acc)
final_lasso

fitted_lasso <- fit(final_lasso, data_train)

fitted_lasso %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  arrange(estimate) # upon, up, at, there, to!

pred_valid <- predict(fitted_lasso, new_data = test)
tibble(text = test$filename, 
       prediction = pred_valid$.pred_class)
