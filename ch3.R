# 機械学習モデルを作成するためのtidymodelsパッケージと
# データ操作のためのtidyverseパッケージの読み込み
library(tidyverse)
library(tidymodels)

# ロジスティック関数の可視化のためのデータ作成
x <- seq(-5, 5, length = 100)
f_x <- 1 / (1 + exp(-1 * x))
# 変換後の値を可視化
tibble(x = x, f_x = f_x) %>%
  ggplot(aes(x = x, y = f_x)) +
  geom_point()

# bivariate_trainデータの先頭数行を確認
bivariate_train %>%
  head()

# bivariateデータの説明変数と目的変数の関係の可視化
# 表示の都合上、各クラス10個のデータに絞っている
bivariate_train %>%
  group_by(Class) %>%
  slice(1:10) %>%
  ggplot(aes(x = A, y = B, shape = Class)) +
  geom_point(alpha = 0.6, size = 4)

# 分類モデルの例を示すためロジスティック回帰モデルを定義する
log_clf <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# fit()関数で学習を実行する
log_clf_fit <- log_clf %>%
  fit(Class ~ A + B , data = bivariate_train)

# 学習済みのモデルの確認
log_clf_fit

# 分類の判別境界を可視化するためのデータ作成
grid_A <-
  seq(max(bivariate_train$A), min(bivariate_train$A), length = 100)
grid_B <-
  seq(max(bivariate_train$B), min(bivariate_train$B), length = 100)
grid_AB <- expand_grid(A = grid_A, B = grid_B)

# 予測の実行と作図
predict(log_clf_fit, new_data = grid_AB) %>%
  bind_cols(grid_AB) %>%
  ggplot(aes(x = A, y = B)) +
  geom_tile(aes(fill = .pred_class))

# two_class_exampleの読み込み
data(two_class_example)

# 2クラスの結果に対する性能評価
two_class_example %>%
  accuracy(truth = truth, estimate = predicted)

# 多クラスの結果に対する性能評価
data(hpc_cv)
hpc_cv %>%
  accuracy(truth = obs, estimate = pred)

# 混同行列の確認
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)

# 混同行列の可視化方法、type引数に"mosaic"を指定した場合
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted) %>%
  autoplot(type = "mosaic")

# 混同行列の可視化方法、type引数に"heatmap"を指定した場合
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted) %>%
  autoplot(type = "heatmap")

# ROC曲線の計算と計算結果の先頭数行を確認する
two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve %>% head()

# ROC曲線の可視化
autoplot(two_class_curve)

# モデル1：rangerパッケージに木の数2000、無作為抽出する変数の数1でランダムフォレストを作成させ、
# モデルの予測結果を元のデータに結合させる
rand_1_res <- rand_forest(mtry = 1, trees = 20000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(Class ~ ., data = two_class_dat) %>%
  predict(two_class_dat, type = "prob") %>%
  bind_cols(two_class_dat) %>%
  roc_curve(truth = Class, estimate = .pred_Class1)

# モデル2：rangerパッケージに木の数5、無作為抽出する変数の数1でランダムフォレストを作成させ
# モデルの予測結果を元のデータに結合させる
rand_2_res <- rand_forest(mtry = 1, trees = 5) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(Class ~ ., data = two_class_dat) %>%
  predict(two_class_dat, type = "prob") %>%
  bind_cols(two_class_dat) %>%
  roc_curve(truth = Class, estimate = .pred_Class1)

# モデル3：rangerパッケージに木の数2、無作為抽出する変数の数1でランダムフォレストを作成させ
# モデルの予測結果を元のデータに結合させる
rand_3_res <- rand_forest(mtry = 1, trees = 2) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(Class ~ ., data = two_class_dat) %>%
  predict(two_class_dat, type = "prob") %>%
  bind_cols(two_class_dat) %>%
  roc_curve(truth = Class, estimate = .pred_Class1)

# モデル1のroc曲線をautoplot()関数で描画
autoplot(rand_1_res) +
  #モデル2のroc曲線をgeom_path()関数で描画
  geom_path(
    data = rand_2_res,
    linetype = "dashed",
    aes(x = 1 - specificity, y = sensitivity),
    col = "skyblue"
  ) +
  # モデル2のroc曲線をgeom_path()関数で描画
  geom_path(
    data = rand_3_res,
    linetype = "longdash",
    aes(x = 1 - specificity, y = sensitivity),
    col = "orange"
  )

# PR曲線の可視化
two_class_example %>%
  pr_curve(truth, Class1) %>%
  autoplot()

# PR AUCの計算
two_class_example %>%
  pr_auc(truth, Class1)

# 累積ゲイン曲線の計算
two_class_example %>%
  gain_curve(truth, Class1) %>%
  autoplot()

# リフト曲線の計算
lift_curve(two_class_example, truth, Class1) %>%
  autoplot()

# 2つ目のクラスを評価対象とする
two_class_example %>%
  f_meas(truth, predicted, event_level = "second")

# probablyパッケージの読み込み
library(probably)
# probablyパッケージのmake_two_class_pred()関数による閾値調整
two_class_example %>%
  mutate(change_threshold = make_two_class_pred(Class1, levels(truth), threshold = 0.7)) %>%
  head()
