# 0.1 .2 tidymodelsパッケージ -------------------------------------------------------------------
install.packages("tidymodels")
library(tidymodels)

# 0.2.1 本書で主に使用するデータセット -------------------------------------------------------------------
# Ames Housigデータの呼び出し 
data(ames, package = "modeldata")
# データの一部を確認 
head(ames)

colnames(ames)

# 0.2.2 本書のゴール -------------------------------------------------------------------
library(tidymodels)
##################################
# 学習データとテストデータの分割 #
##################################
set.seed(71)
split_ames_df <- initial_split(ames,
                               strata = "Sale_Price")
ames_train <-training(split_ames_df)
ames_test <- testing(split_ames_df)
######################
# 前処理レシピの作成 #
######################
ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = .1)  %>%
  step_zv(recipes::all_predictors()) %>%
  prep()
# 前処理の適用
ames_train_baked <-
  ames_rec %>%
  bake(new_data = ames_train)
ames_test_baked <-
  ames_rec %>%
  bake(new_data = ames_test)
########################################
# クロスバリデーションとパラメータ探索 #
########################################
# クロスバリデーション用のデータ分割
ames_cv_splits <- vfold_cv(ames_train,
                           strata = "Sale_Price",
                           v = 10)
# ワークフローの設定
ames_rf_cv <-
  workflow() %>%
  add_model(
    rand_forest(
      mtry = tune::tune(),
      trees = tune::tune()
    ) %>%
      set_engine("ranger",
                 num.threads = parallel::detectCores()) %>%
      set_mode("regression")) %>%
  add_formula(Sale_Price ~ .)
# グリッドサーチ
rf_params <-
  list(trees(),
       mtry() %>%
         finalize(ames_train_baked %>%
                    select(-Sale_Price))) %>%
  parameters()
# パラメータの探索範囲を指定
set.seed(71)
rf_grid_range <-
  rf_params %>%
  grid_max_entropy(size = 10)
# グリッドサーチの実行
ames_rf_grid <-
  ames_rf_cv %>%
  tune_grid(ames_cv_splits,
            grid = rf_grid_range,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(yardstick:::rmse))
autoplot(ames_rf_grid)
# 最適なパラメータを選択
ames_rf_grid_best <-
  ames_rf_grid %>%
  show_best()
##############################
# 最適なパラメータで最終学習 #
##############################
# 選んだパラメータでモデル作成
ames_rf_model_best <-
  rand_forest(
    trees = ames_rf_grid_best$trees[1],
    mtry = ames_rf_grid_best$mtry[1]
  ) %>%
  set_engine("ranger") %>%
  set_mode("regression")
# ワークフローの更新
ames_rf_cv_last <-
  ames_rf_cv %>%
  update_model(ames_rf_model_best)
# 更新したワークフローで訓練データ全体にモデル適用
ames_rf_last_fit <-
  ames_rf_cv_last %>%
  last_fit(split_ames_df)
# 最終的なモデルの精度を算出
ames_rf_last_fit %>%
  collect_metrics()

# 0.5.3 パイプ演算子 -------------------------------------------------------------------
# パッケージがインストールされていれば、
# library(tidyverse)またはlibrary(tidymodels)でよい
library(magrittr)
# 1~10までの整数を足し上げる
sum(1:10)
# sum(1:10)と同じ結果になる
1:10 %>% sum()
