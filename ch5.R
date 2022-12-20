# 5.3.1 tuneパッケージによるパラメータの探索 ------------------------------------------
# データの分割
# 学習データとテストデータの分割
set.seed(71)
split_ames_df <- initial_split(ames_raw,
                               strata = "Sale_Price")
ames_train <- training(split_ames_df)
ames_test <- testing(split_ames_df)

# 交差検証のため訓練データをさらに分割
ames_cv_splits <- vfold_cv(ames_train,
                           strata = "Sale_Price",
                           v = 10)

ames_rec <-
  recipes::recipe(Sale_Price ~ ., data = ames_train) %>%
  recipes::step_log(Sale_Price, base = 10) %>%
  recipes::step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  recipes::step_other(Neighborhood, threshold = .1)  %>%
  recipes::step_zv(recipes::all_predictors())

# ワークフローの設定
ames_rf_cv <-
  workflows::workflow() %>%
  workflows::add_recipe(ames_rec) %>%
  workflows::add_model(
    parsnip::rand_forest(
      # 探索したいパラメータにはtune()を設定
      mtry = tune::tune(),
      trees = tune::tune()
    ) %>%
      parsnip::set_engine("ranger",
                          num.threads = parallel::detectCores()) %>%
      parsnip::set_mode("regression"))

rf_params <-
  list(trees(),
       mtry() %>%
         dials::finalize(ames_rec %>% prep() %>% bake(new_data = NULL) %>% select(!Sale_Price))) %>%
  dials::parameters()
rf_params

rf_grid_range <-
  rf_params %>%
  dials::grid_random(size = 5)

rf_grid_range

# ランダムサーチの実行
ames_rf_grid <-
  ames_rf_cv %>%
  tune::tune_grid(resamples = ames_cv_splits,
                  grid = rf_grid_range,
                  control = tune::control_grid(save_pred = TRUE),
                  metrics = yardstick::metric_set(rmse))

autoplot(ames_rf_grid)

# 最適なパラメータを選択
ames_rf_grid_best <-
  ames_rf_grid %>%
  tune::show_best()

ames_rf_grid_best

# 選んだパラメータでモデル作成
ames_rf_model_best <-
  parsnip::rand_forest(
    # 最適なパラメータを選択
    # 1行目を選択
    trees = ames_rf_grid_best$trees[1],
    mtry = ames_rf_grid_best$mtry[1]
  ) %>%
  parsnip::set_engine("ranger",
                      seed = 71) %>%
  parsnip::set_mode("regression")

# ワークフローの更新
ames_rf_cv_last <-
  ames_rf_cv %>%
  workflows::update_model(ames_rf_model_best)

# 更新したワークフローで訓練データ全体にモデル適用
ames_rf_last_fit <-
  ames_rf_cv_last %>%
  tune::last_fit(split_ames_df)

# 最終的な精度を算出
last_rmse <- ames_rf_last_fit %>%
  tune::collect_metrics()

last_rmse

# 5.3.2 ベイズ最適化によるハイパーパラメータチューニング ------------------------------------------
ames_rf_bayes <-
  ames_rf_cv %>%
  tune::tune_bayes(
    resamples = ames_cv_splits,
    param_info = rf_params,
    initial = 5,
    iter = 5,
    metrics = yardstick::metric_set(rmse))

ames_rf_bayes_best <-
  ames_rf_bayes %>%
  tune::show_best()
ames_rf_bayes_best

# 5.3.2 ベイズ最適化によるハイパーパラメータチューニング(書籍に記載をしていない部分) ------------------------------------------
# 選んだパラメータでモデル作成
ames_rf_bayes_best <-
  parsnip::rand_forest(
    # 最適なパラメータを選択
    # 1行目を選択
    trees = ames_rf_bayes_best$trees[1],
    mtry = ames_rf_bayes_best$mtry[1]
  ) %>%
  parsnip::set_engine("ranger",
                      seed = 71) %>%
  parsnip::set_mode("regression")

# ワークフローの更新
ames_rf_cv_bayes_last <-
  ames_rf_cv %>%
  workflows::update_model(ames_rf_bayes_best)

# 更新したワークフローで訓練データ全体にモデル適用
ames_rf_bayes_last_fit <-
  ames_rf_cv_bayes_last %>%
  tune::last_fit(split_ames_df)

# 最終的な精度を算出
last_rmse_bayes <- ames_rf_bayes_last_fit %>%
  tune::collect_metrics()

last_rmse_bayes


