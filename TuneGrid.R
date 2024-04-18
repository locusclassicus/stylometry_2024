knn1_spec <- knn_spec %>%
  set_args(neighbors = tune(),
           weight_func = tune(),
           dist_power = tune())

translate(knn1_spec)

knn1_res <- tune::tune_grid(
  knn1_spec,
  preprocessor = rec_norm,
  resamples = folds,
  control = tune::control_resamples(save_pred = TRUE))

res_tidy <- knn1_res %>%
  select(.predictions) %>%
  unnest()

preds <- knn1_res %>% 
  collect_predictions()

ests <- knn1_res %>%
  collect_metrics(summarize = T)

knn1_res %>%
  collect_metrics(summarize = FALSE) %>%
  distinct(neighbors, weight_func, dist_power)


knn1_res %>%
  show_best(metric = "accuracy")

knn1_res %>% 
  autoplot() +
  geom_line()

# дальше см. https://uo-datasci-specialization.github.io/c4-ml-fall-2020/slides/w6p1-knn/w6p1.pdf