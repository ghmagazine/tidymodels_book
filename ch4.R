# setup -------------------------------------------------------------------
ggplot2::theme_set(ggplot2::theme_gray(base_size = 9))



# 4-1 workflowsパッケージによるレシピやモデル、データの管理 -------------------------------------
## workflowsパッケージを使わないモデル作成 -----------------------------------------------
# tidymodelsパッケージの読み込みにより
# recipes,parsnipパッケージなどを利用可能にする
library(tidymodels)


# データ分割の方法については1章で解説
data("ames", package = "modeldata")
set.seed(123)
# 売却価格による層化抽出
split_ames_df <-
  initial_split(ames, strata = "Sale_Price")
# training()関数およびtesting()関数で学習データと評価データに分割
ames_train <-
  training(split_ames_df)
ames_test <-
  testing(split_ames_df)


# 前処理のレシピを定義
ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  # 1. 対数変換
  step_log(all_outcomes(), base = 10, skip = TRUE) %>%
  # 2. Yeo-Johnson変換
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  # 3. 出現頻度の少ないカテゴリを変換
  step_other(Neighborhood, threshold = 0.1)  %>%
  # 4. 一意の値しか持たない変数の除外
  step_zv(all_predictors())


# モデルの定義
rf_model <-
  # ランダムフォレストを宣言
  rand_forest(
    trees = 50,
    mtry = 3) %>%
  # モデルのエンジンとしてrangerパッケージを指定
  set_engine("ranger", seed = 71) %>%
  # 回帰モデルの指定
  set_mode("regression")


# モデルに対してデータを適用
ames_rec_preped <-
  ames_rec %>%
  prep()
ames_rf_fit <-
  rf_model %>%
  fit(Sale_Price ~ .,
      data = bake(ames_rec_preped, new_data = NULL))


predict(ames_rf_fit,
        new_data = bake(ames_rec_preped, ames_test))


## workflowsパッケージを使ったモデル作成 ------------------------------------------------
# workflowsパッケージはtidymodelsパッケージに含まれるので
# 個別に読み込む必要はない
# library(workflows)

ames_wflow <-
  # ワークフローの宣言
  workflow() %>%
  # レシピの追加
  add_recipe(recipe = ames_rec) %>%
  # モデルの追加
  add_model(spec = rf_model)


# workflow()関数によって生成されるオブジェクトのクラスを確認
class(ames_wflow)


ames_wflow


# ワークフロー宣言時にレシピとモデル定義を指定しておく方法
# preprocessor引数にrecipeオブジェクト
# spec引数にmodel_specオブジェクトを与える
workflow(preprocessor = ames_rec, spec = rf_model)


# モデルへの適用も前処理を施す前のames_trainデータに対して行なう
# 自動的にワークフローに記述された前処理レシピがames_trainに適用される
rf_fit <-
  ames_wflow %>%
  fit(data = ames_train)


class(rf_fit)


rf_fit


# workflowオブジェクトに与えたrecipesパッケージによる前処理レシピの参照
rf_fit %>%
  extract_recipe()


# workflowオブジェクトに与えたparsnipパッケージでのモデル仕様の参照
rf_fit %>%
  extract_fit_parsnip()


# new_data引数に前処理を施す前の評価データを与える
predict(rf_fit, new_data = ames_test)


# 目的変数であるSale_Priceと
# モデルが予測した値.predが含まれるデータフレームを作成
df_ames_predict <-
  augment(rf_fit, new_data = ames_test)

# yardstickパッケージの評価関数rmse()によりRMSEを求める
df_ames_predict %>%
  # 評価データの目的変数の対数変換ここで行なう
  transmute(Sale_Price = log(Sale_Price, base = 10), .pred) %>%
  rmse(Sale_Price, .pred)


# augment()関数で得たデータフレームから目的変数の値を参照する
head(df_ames_predict$Sale_Price)


# Sale_Priceを対数変換せずに.predとの間のRMSEを求める
df_ames_predict %>%
  rmse(Sale_Price, .pred)


## モデルの改善に利用できる関数 ---------------------------------------------------------
# parsnipでmodel_specオブジェクトを生成する
lm_model <-
  linear_reg() %>%
  set_engine("lm")

lm_wflow <-
  ames_wflow %>%
  # ランダムフォレストを指定していたモデルの更新を行なう
  update_model(lm_model)

lm_wflow


lm_wflow %>%
  # ワークフローからレシピを削除する
  remove_recipe()


lm_wflow <-
  lm_wflow %>%
  remove_recipe() %>%
  add_variables(outcome = Sale_Price,
                predictors = c(Longitude, Latitude))

# Preprocessorの出力が変更されている点に注意
# PredictorsとしてLongitude, Latitudeの2変数だけが扱われている
lm_wflow


# recipeによって変数の関係が記述されているためにエラーとなる
lm_wflow %>%
  add_variables(outcome = Sale_Price,
                predictors = c(Longitude, Latitude))


# LongitudeとLatitudeだけを説明変数とする
# Sale_Priceを予測する線形回帰モデルの実行
lm_fit <-
  fit(lm_wflow, ames_train)

# 予測値の確認
predict(lm_fit, ames_test)

# 評価データames_testにモデルの予測結果の列を加える
augment(lm_fit, ames_test) %>%
  # 目的変数への対数変換等の処理を行なっていないので
  # RMSEの算出は目的変数の元の値と予測値から求める
  rmse(Sale_Price, .pred)


## リサンプリングデータへのworkflowの適用 ------------------------------------------------
# k分割交差検証によるリサンプリングデータの生成
set.seed(123)
folds <-
  ames_train %>%
  vfold_cv(v = 10, strata = Sale_Price)

folds


# リサンプリングデータに対するモデルの適用
keep_pred <-
  control_resamples(save_pred = TRUE, save_workflow = TRUE)
wf_fit_fold <-
  ames_wflow %>%
  fit_resamples(resamples = folds,
                control = keep_pred)

wf_fit_fold


collect_metrics(wf_fit_fold)


show_best(wf_fit_fold, metric = "rmse")



# 4-2 workflowsetsパッケージによる複数レシピやモデルの一元管理 ----------------------------------
## leave_var_out_formulas()関数による変数選択 --------------------------------------
# workflowsetsパッケージはtidymodelsに含まれるため
# 個別に読み込む必要はない
# library(workflowsets)


# penguinsデータの読み込み
data("penguins", package = "modeldata")

# データの分割
set.seed(123)
penguins_split <-
  # speciesを元にした層化抽出を行なう
  initial_split(penguins, strata = species)
penguins_train <-
  training(penguins_split)
penguins_test <-
  testing(penguins_split)

# k = 10のk分割交差検証
set.seed(123)
folds <-
  penguins_train %>%
  vfold_cv(v = 10, strata = species)


# full_model引数にTRUEを与えると
# すべての変数を含むモデル（フルモデル）を生成する
formulas <-
  leave_var_out_formulas(sex ~ ., data = penguins, full_model = TRUE)

formulas


# speciesが含まれないモデル式
formulas[["species"]]

# 宣言したすべての説明変数が含まれるモデル式
formulas[["everything"]]



## workflow_set()関数によるモデルと変数の組み合わせ ----------------------------------------
# 一般化線形モデルの仕様を定義する
lr_spec <-
  logistic_reg() %>%
  set_engine("glm")

# workflow_setオブジェクトの作成
penguins_workflows <-
   workflow_set(
     # モデル式を与える
     preproc = formulas,
     # 実行するモデルの種類をリスト形式で指定
     models = list(logistic = lr_spec),
     # モデル式とモデルの種類の組み合わせで実行するかのオプション
     cross = FALSE)


class(penguins_workflows)

# workflow_setオブジェクトはデータフレームの形式
penguins_workflows



## workflow_map()関数によるワークフローの適用 -------------------------------------------
# リサンプリングデータへのワークフローの適用
penguins_workflows_fit <-
  penguins_workflows %>%
   workflow_map(fn = "fit_resamples",
                resamples = folds,
                # 乱数固定のための引数
                seed = 123)

penguins_workflows_fit



## rank_results()関数による評価指標 ------------------------------------------------
penguins_workflows_fit %>%
  rank_results()


# 図4.1を出力するコード
penguins_workflows_fit %>%
  autoplot(
    # accuracyの値を比較する
    metric = "accuracy") +
  guides(color = "none", shape = "none")



## extract_workflow()関数によるモデル情報の参照 ----------------------------------------
# island_logisticのワークフローを参照
# 出力はworkflowオブジェクト
extract_workflow(penguins_workflows, id = "island_logistic")



## workflow_set()関数による複数のレシピ・モデルの適用 ---------------------------------------
# Chicagoデータの読み込み
data("Chicago", package = "modeldata")
# 先頭から365行を抽出
Chicago <-
  Chicago %>%
  slice_head(n = 365)


# 最初のレシピを作成
base_recipe <-
   recipe(ridership ~ ., data = Chicago) %>%
   # 1. 日付に関わる要素（年月日）を特徴量として扱う
   step_date(date) %>%
   # 2. 日付が祝日かどうかの特徴量を追加する
   step_holiday(date) %>%
   # 3. 日付を示すdate列をidとして処理（モデルから無視する）
   update_role(date, new_role = "id") %>%
   # 4. 因子型の変数に対するダミー変数化
   step_dummy(all_nominal()) %>%
   # 5. 単一の値からなる変数の削除
   step_zv(all_predictors()) %>%
   # 6. 平均0、標準偏差1となるような標準化
   step_normalize(all_predictors())


# 2つ目のレシピ。最初のレシピにstep_corr()関数を追加
filter_rec <-
   base_recipe %>%
   step_corr(all_of(stations), threshold = tune())

# 3つ目のレシピ。最初のレシピに対してPCAの処理を追加したもの
pca_rec <-
   base_recipe %>%
   step_pca(all_of(stations), num_comp = tune()) %>%
   step_normalize(all_predictors())


# 正則化
regularized_spec <-
   linear_reg(penalty = tune(), mixture = tune()) %>%
   set_engine("glmnet")

# 木モデル
cart_spec <-
   decision_tree(cost_complexity = tune(), min_n = tune()) %>%
   set_engine("rpart") %>%
   set_mode("regression")

# K近傍法
knn_spec <-
   nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
   set_engine("kknn") %>%
   set_mode("regression")


# 3つのレシピ、モデルからなる9つのワークフローを生成
chi_models <-
   workflow_set(
     # 名前付きのリストを与えるとworkflow_setオブジェクトのwflow_idの値に利用される
     preproc = list(simple = base_recipe,
                    filter = filter_rec,
                    pca = pca_rec),
     models = list(glmnet = regularized_spec,
                   cart = cart_spec,
                   knn = knn_spec),
     # 3つのレシピと3つのモデルの組み合わせを用意する
     cross = TRUE)

chi_models


## 時系列データのモデル選択 -----------------------------------------------------------
splits <-
  sliding_period(
    Chicago,
    # 時間インデックス: 対象となる日付・時間の変数
    index = date,
    # データが日毎に与えられており、日毎に処理することを宣言
    period = "day",
    # 各リサンプリングにおいて分析セットの件数を300+1件にする
    lookback = 300,
    # 各リサンプリングの評価セットには
    # 分析セットの最後の日付から7日分とする
    assess_stop = 7,
    # 各リサンプリングにおける分析セットの時間間隔を
    # 7（ここでは日数）とする
    # 評価セットの数だけずらしているため、
    # データの重複が発生しなくなる
    step = 7)
splits


set.seed(123)
chi_models <-
   chi_models %>%
   workflow_map(fn = "tune_grid",
                resamples = splits,
                grid = 10,
                metrics = metric_set(rmse),
                verbose = FALSE)

chi_models


# RMSEが最良となるグリッドのみを表示する
chi_models %>%
  rank_results(rank_metric = "rmse",
               select_best = TRUE) %>%
  select(rank, mean, model, wflow_id, .config)


# 図4.2を出力するコード
autoplot(chi_models, select_best = TRUE) +
  guides(shape = "none", color = "none") +
  facet_wrap(~ model, scales = "free_x")

