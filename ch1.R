# setup -------------------------------------------------------------------
ggplot2::theme_set(ggplot2::theme_gray(base_size = 9))
library(dplyr)
library(ggplot2)
library(patchwork)


# 1-2 rsampleパッケージによるデータ分割のアプローチ ------------------------------------------
library(rsample)
data(ames, package = "modeldata")


# rnorm()関数を使った正規分布に従う乱数の発生では、
# 実行のたびに異なる値が出力される
rnorm(5, mean = 0, sd = 1)
rnorm(5, mean = 0, sd = 1)


# set.seed()関数で乱数の固定を行なうことができる
set.seed(123)
rnorm(5)

set.seed(123)
rnorm(5)



## initial_split()関数によるデータ分割 ---------------------------------------------
# 再現性のある結果を得るために乱数を制御する
set.seed(123)
ames_split <-
  initial_split(data = ames)


# initial_split()関数の結果はrsplitクラスになる
class(ames_split)
ames_split


# 学習データの取り出し
ames_train <-
  training(ames_split)
# 評価データの取り出し
ames_test <-
  testing(ames_split)

# initial_split()関数で学習データに分割されたデータの大きさの確認
# initial_split()関数を適用したオブジェクトを出力した際の学習データ件数 Training
# と一致し、amesデータセットと同じ列数であることがわかる
dim(ames_train)


set.seed(123)
# prop引数で学習用に用意するデータの比率を調整する
# propに与える数値が学習データの割合になる
ames_split <-
  initial_split(data = ames, prop = 0.80)
ames_split



## parsnipパッケージによるモデル作成 ----------------------------------------------------
library(parsnip)
# 線形回帰モデルの指定
lm_spec <-
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# Sale_Priceを予測するモデル学習結果のオブジェクト)の作成
lm_fit <-
  lm_spec %>%
  fit(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type, data = ames_train)


# 評価データames_testへのモデルの適用
# predict()関数では予測結果のれつからなるデータフレームが返却される
predict(lm_fit, new_data = ames_test)


# 評価データames_testへのモデルの適用
# 目的変数であるSale_Priceと
# モデルが予測した値.predが含まれるデータフレームを作成
augment(lm_fit, new_data = ames_test) %>%
  select(Sale_Price, .pred)


## yardstickパッケージによるモデル評価 --------------------------------------------------
# 学習データを使ってモデルの性能評価を行う
augment(lm_fit, new_data = ames_train)  %>%
  yardstick::rmse(truth = Sale_Price, estimate = .pred)


# 評価データを使ってモデルの性能評価を行う
augment(lm_fit, new_data = ames_test) %>%
  yardstick::rmse(truth = Sale_Price, estimate = .pred)



# 1-3 無作為抽出によるデータ分割が不適切なケースへの対応 -------------------------------------------
# 図1.2を出力するコード
ggplot() +
  geom_density(data = ames,
               aes(Sale_Price)) +
  geom_vline(data = quantile(ames$Sale_Price) %>%
               .[seq.int(2, 4)] %>% {
                 tibble::tibble(
                   pct = names(.),
                   sale_price = .)},
             aes(xintercept = sale_price),
             linetype = 2)


# amesデータにおけるSale_Priceの四分位数を算出する
q <-
  quantile(ames$Sale_Price)
q


# 分割後の学習データに含まれるSale_Priceの分布を四分位数をもとにカウント
training_sale_price <-
  training(ames_split)$Sale_Price

tibble::tibble(
  group = seq(length(q) - 1),
  n = c(table(cut(training_sale_price, q, include.lowest = TRUE)))
)


## initial_split()関数による層化抽出 -----------------------------------------------
set.seed(123)
# 引数strataにSale_Price変数を指定し、層を作る
ames_split <-
  initial_split(ames, prop = 0.80, strata = Sale_Price)

training_sale_price <-
  training(ames_split)$Sale_Price

tibble::tibble(
  group = seq(length(q) - 1),
  n = c(table(cut(training_sale_price, q, include.lowest = TRUE)))
)


## initial_time_split()関数による時系列データの分割 --------------------------------------
# drinksデータを利用可能にする
data(drinks, package = "modeldata")

head(drinks)


# initial_time_split()関数の実行
set.seed(123)
drinks_split_ts <-
  initial_time_split(drinks)
# 学習データ、評価データの参照方法はinitial_split()関数の結果と同様
train_ts_data <-
  training(drinks_split_ts)
test_ts_data <-
  testing(drinks_split_ts)


# 学習・評価データそれぞれに含まれる日付の範囲を表示する
range(train_ts_data$date)
range(test_ts_data$date)


# 図1.4を出力するコード
set.seed(123)
drinks_split <-
  initial_split(drinks)
train_data <-
  training(drinks_split)
test_data <-
  testing(drinks_split)

p1 <-
  bind_rows(
    train_data %>%
      mutate(source = "学習"),
    test_data %>%
      mutate(source = "評価")) %>%
  ggplot(aes(date, S4248SM144NCEN, shape = source)) +
  geom_point() +
  scale_shape_manual(values = c("学習" = 16, "評価" = 1)) +
  labs(title = "initial_split()関数によるデータ分割")

p2 <-
  bind_rows(
    train_ts_data %>%
      mutate(source = "学習"),
    test_ts_data %>%
      mutate(source = "評価")) %>%
  ggplot(aes(date, S4248SM144NCEN, shape = source)) +
  geom_point() +
  scale_shape_manual(values = c("学習" = 16, "評価" = 1)) +
  labs(title = "initial_time_split()関数によるデータ分割")

p1 + p2 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A")



# 1-4 リサンプリング法 ------------------------------------------------------------
## k分割交差検証法 ---------------------------------------------------------------
set.seed(123)
# k = 10のk分割交差検証を実行
# 学習データを引数dataに与える
folds <-
  vfold_cv(ames_train, v = 10)
folds



# vfold_cv()関数により生成されるsplits列のオブジェクトは
# rsplitクラスの他にvfol_splitクラスを持つ
class(folds$splits[[1]])

# splits列にはrsplitクラスのオブジェクトが格納される
folds$splits[[1]]


# 分析セットの参照
analysis(folds$splits[[1]])

# 検証セットの参照
assessment(folds$splits[[1]])


set.seed(123)
# v = 10、繰り返し回数 = 5の10*5のデータを作成する
ames_fold_rep5 <-
  vfold_cv(ames_train, v = 10, repeats = 5)

ames_fold_rep5


## ブートストラップ法 --------------------------------------------------------------
set.seed(123)
# 25個のブートストラップ標本を作成する
ames_boots <-
  bootstraps(ames_train, times = 25)

dim(ames_train)


# ブートストラップ標本に含まれる分析セットの数は元のデータ件数と一致する
# 評価セットの件数はブートストラップ標本ごとに異なる
ames_boots



# 時系列データのリサンプリング法 ---------------------------------------------------------
# drinksデータに年を表す変数を追加し、件数を3年間（36ヶ月）分に制限する
drinks_annual <-
  drinks %>%
  mutate(year = lubridate::year(date)) %>%
  filter(between(year, 1992, 1994))

# 時系列でのデータ分割により、1994年のデータが評価データに利用する
drinks_split_ts2 <-
  initial_time_split(drinks_annual, prop = 0.68)
train_ts_data2 <-
  training(drinks_split_ts2)

# 1992年と1993年の24ヶ月を学習データに扱う
train_ts_data2


# 時系列リサンプリングの実行
ts_fold <-
  sliding_window(train_ts_data2,
                 lookback = 11,
                 assess_start = 1,
                 assess_stop = 10)

ts_fold


# 最初の分析セットと検証セットの確認
analysis(ts_fold$splits[[1]])
assessment(ts_fold$splits[[1]])

# 2つ目の分析セットの確認
analysis(ts_fold$splits[[2]])
assessment(ts_fold$splits[[2]])



# 1-5 recipesパッケージによる前処理 --------------------------------------------------
# 図1.8を出力するコード
p_base <-
  ames %>%
  ggplot(aes(Sale_Price)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  scale_fill_identity()

p1 <- p_base +
  aes(fill = "#E69F00")
p2 <-
  p_base +
  aes(fill = "#0072B2") +
  scale_x_log10()

p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")


## recipe()関数による前処理の手順化 ---------------------------------------------------
library(recipes)


# レシピ（recipe）を表すオブジェクトであることがわかるようにrecとつける
ames_rec <-
  recipe(x = ames_train,
         formula = Sale_Price ~ .)


class(ames_rec)


ames_rec <-
  ames_rec %>%
  # 対数変換を行うstep_*()関数
  # デフォルトでは自然対数が使われるので常用対数の底10をbase引数に指定する
  step_log(Sale_Price, base = 10)



ames_rec


# prep()関数が実行される正常なコード
# step_*()関数で対象となる変数はいずれもrecipe(formula = )で定義されている
ames_rec <-
  prep(ames_rec)


# 問題となるコード
# モデル式に含まれない変数名salepriceを step_*()関数に与えてレシピを作った場合でも
# recipeオブジェクトは生成されるが、prep()関数の実行時に変数名がないためにエラーとなる
bad_rec <-
  recipe(ames_train,
         formula = Sale_Price ~ .) %>%
  # データ上に含まれない変数名を指定
  step_log(saleprice, base = 10)

bad_rec


bad_rec %>%
  prep()


# step_*()関数内でall_outcomes()関数を用いて、変数名の入力を省略する
recipe(x = ames_train,
       formula = Sale_Price ~ .) %>%
  step_log(all_outcomes(), base = 10)


# 2つの実行結果は同じ
all.equal(
  # recipe(ames_train, ...) で作成したレシピ
  prep(ames_rec),
  # prep()関数上でames_trainを指定
  prep(ames_rec, training = ames_train))



## bake()関数によるレシピの適用 ------------------------------------------------------
# 学習データに対するレシピの適用
ames_train_baked <-
  ames_rec %>%
  bake(new_data = NULL)

# 評価データに対するレシピの適用
ames_test_beked <-
  ames_rec %>%
  bake(new_data = ames_test)


# 対数変換を適用する前の学習データ内のSale_Priceの値を確認
ames_train %>%
  pull(Sale_Price) %>%
  head()


# レシピを適用（対数変換を実施）した後の学習データ内のSale_Priceの値を確認
ames_train_baked %>%
  pull(Sale_Price) %>%
  head()


# レシピの適用結果から目的変数とLot_Area変数のみを取り出す
ames_rec %>%
  bake(new_data = NULL, all_outcomes(), Lot_Area)


## skip引数による処理の省略 ---------------------------------------------------------
# 欠損値を含むデータフレームを作成
df_na_contains <-
  tibble(
    x = c(NA, 332L, 294L, 25L, 334L, 250L, NA, 175L, NA, 131L),
    y = c(NA, 50L, NA, 66L, NA, 7L, 13L, 30L, 44L, 85L))

set.seed(123)
na_split <-
  initial_split(df_na_contains)
# 学習データは7件、評価データには3件のデータが含まれる
na_split

# x列が欠損値となる行を除外するレシピを作成
na_rec <-
  recipe(y ~ x, data = training(na_split)) %>%
  # skip = FALSE を指定し、学習データ以外でもこの処理を実行させる
  step_naomit(x, skip = FALSE) %>%
  prep(training = training(na_split))

# 評価データにレシピを適用
# x列に欠損値を含む行はじょがいされるため、評価データの元の件数より少なくなる
na_rec %>%
  bake(new_data = testing(na_split))

na_rec_skip <-
  recipe(y ~ x, data = training(na_split)) %>%
  # skip = TRUE（初期値）を指定し、学習データ以外では欠損値を含む行を除外しない
  step_naomit(x, skip = TRUE) %>%
  prep(training = training(na_split))

# 評価データにレシピを適用
# 欠損値の除外処理は無視されるため評価データの件数は元のまま
na_rec_skip %>%
  bake(new_data = testing(na_split))

# skip = TRUEが問題となる場合
car_recipe <-
  recipe(mpg ~ ., data = mtcars) %>%
  # 対数変換を学習データに対してのみ行う
  step_log(disp, skip = TRUE) %>%
  # skipしたあとに同じ変数に処理を加える
  step_center(disp) %>%
  prep(training = mtcars)

# 学習データにのみすべてのstep_*()関数が適用される
bake(car_recipe, new_data = NULL) %>%
  pull(disp) %>%
  summary()
# 学習データ以外をbake(new_data = )に与えた場合、step_*(skip = TRUE)の処理は無視される
bake(car_recipe, new_data = mtcars) %>%
  pull(disp) %>%
  summary()



## 連続するstep_*()関数のレシピ化 ----------------------------------------------------
## # ここでの関数名はrecipesで使われているものではないので注意
## rec <-
##   recipes(y ~ x, data = df)
##
## rec <-
##   rec %>%
##   step_first(y)
##
## rec <-
##   rec %>%
##   step_second(x)


## # recipesパッケージのstep_*()関数はパイプ演算子(%>%)を使って数珠つなぎに記述できる
## rec <-
##   recipes(y ~ x, data = df) %>%
##   step_first(y) %>%
##   step_second(x)


# 図1.9を出力するコード
tibble::as_tibble(as.data.frame(table(ames_train$Neighborhood))) %>%
  purrr::set_names(c("Neighborhood", "Freq")) %>%
  ggplot(aes(forcats::fct_reorder(Neighborhood, Freq), Freq)) +
  geom_bar(stat = "identity") +
  xlab("Neighborhood") +
  gghighlight::gghighlight(Freq >= 220) +
  coord_flip()


ames_rec <-
  recipe(x = ames_train,
         formula = Sale_Price ~ .) %>%
  step_log(Sale_Price, base = 10) %>%
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.1)  %>%
  step_zv(all_predictors())


ames_rec



## step_*()関数内の変数の柔軟な指定とヘルパ関数 ---------------------------------------------
set.seed(123)
df <- data.frame(
  y = rnorm(10, mean = 100),
  x1 = rnorm(10, mean = 5),
  x2 = rnorm(10, mean = 3),
  x3 = letters[1:10])


## # step_*()関数に対象の変数名を直接指定する方法
## recipe(y ~ ., data = df) %>%
##   step_first(y) %>%
##   step_second(x1, x2) |>
##   step_third(x3) |>
##   step_fourth(x1, x2, x3)
##
## # 変数の指定にヘルパ関数を使う方法
## recipe(y ~ ., data = df) %>%
##   step_first(all_predictors()) %>%
##   step_second(all_numeric_predictors()) |>
##   step_third(all_nominal_predictors()) |>
##   step_fourth(num_range("x", 1:3))


recipe(y ~ ., data = df) %>%
  # 中心化の処理は数値型の変数でないと実行できないため、
  # 数値ではないx3を除外させる
  step_center(all_predictors(), -has_type("nominal")) %>%
  prep(training = df) %>%
  bake(new_data = NULL)



## recipesにおけるroleの役割 -----------------------------------------------------
summary(ames_rec)


# biomassデータでは学習データ、評価データを一つのデータにまとめてある
# 変数datasetをもとに学習データと評価データに区別される
data(biomass, package = "modeldata")
biomass_train <- biomass %>%
  filter(dataset == "Training")
biomass_test <- biomass %>%
  filter(dataset == "Testing")


rec <-
  recipe(HHV ~ ., data = biomass_train) %>%
  update_role(sample, new_role = "id variable") %>%
  update_role(dataset, new_role = "dataset") %>%
  step_center(all_predictors())

# 変更後のroleを確認する
summary(rec) %>%
  filter(variable %in% c("sample", "dataset"))


prep(rec, biomass_train) %>%
  bake(new_data = NULL)


# 文字列の説明変数に対して中央化を行おうとするためにエラーとなる例
rec <-
  recipe(HHV ~ ., data = biomass_train) %>%
  step_center(all_predictors())

prep(rec, biomass_train) %>%
  bake(new_data = NULL)


# roleを使わずに上記のエラーを回避する例
# 1. step_*()関数での処理対象の指定に適切な変数を指定する
recipe(HHV ~ ., data = biomass_train) %>%
  step_center(all_numeric_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)
# 2. あらかじめモデルに使わない変数をモデル式に含めない
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_train) %>%
  step_center(all_predictors()) %>%
  prep() %>%
  bake(new_data = NULL)


# カテゴリデータの前処理: エンコーディング ---------------------------------------------------
# amesデータの中のカテゴリ変数
# 40の変数がカテゴリデータとして扱われる
ames %>%
  select(tidyselect::where(is.factor)) %>%
  glimpse()


# Bldg_Typeに含まれる項目の確認
levels(ames$Bldg_Type)


# カテゴリ変数を含むモデル式を作成する
ames_cat_rec <-
  ames_train %>%
  recipe(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type)

# Bldg_Typeのダミー変数化を指定
ames_dummy_rec <-
  ames_cat_rec %>%
  step_dummy(Bldg_Type)

ames_dummy_rec


# 学習データへのレシピの適用
ames_dummy_rec %>%
  prep() %>%
  bake(new_data = NULL) %>%
  # 見やすさのためにデータの表示方法を変更
  glimpse()


ames_cat_rec %>%
  # one_hot = TRUE によりワンホットエンコーディングを指定する
  step_dummy(Bldg_Type, one_hot = TRUE) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  # 見やすさのためにデータの表示方法を変更
  glimpse()


## themisパッケージを使った不均衡データへの対応 ----------------------------------------------
library(themis)


# 計算機の実行速度を評価したデータ
data(hpc_data, package = "modeldata")

# 不要な列を除いておく
hpc_data_mod <-
  hpc_data %>%
  select(!c(protocol, day))

# 処理速度を評価したクラスの件数を確認
count(hpc_data_mod, class)


hpc_down_rec <-
  recipe(class ~ ., data = hpc_data_mod) %>%
  # classをもとにしたダウンサンプリングの指定
  # デフォルトでskip = TRUEが与えられている
  # そのため学習データ以外ではこの処理は無視される
  step_downsample(class, skip = TRUE) %>%
  prep()


hpc_down_rec %>%
  # データへのレシピの適用
  bake(new_data = NULL) %>%
  # class変数の件数を確認
  count(class)

recipe(class ~ ., data = hpc_data_mod) %>%
  # under_ratioの変更
  # 少数クラスのデータ件数が半分になり、他のクラス件数も調整される
  step_downsample(class, under_ratio = 0.5) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  count(class)


hpc_up_rec <-
  recipe(class ~ ., data = hpc_data_mod) %>%
  # classをもとにしたアップサンプリングの指定
  step_upsample(class, seed = 123, over_ratio = 1) %>%
  prep()

hpc_up_rec <-
  recipe(class ~ ., data = hpc_data_mod) %>%
  step_upsample(class, seed = 123) %>%
  prep()

hpc_up <-
  hpc_up_rec %>%
  bake(new_data = NULL)

hpc_up %>%
  count(class)


hpc_smote_rec <-
  recipe(class ~ ., data = hpc_data_mod) %>%
  # 特定の少数クラスのデータから近傍の5つのデータを探索、
  # そこからさらに1つの近傍データを無作為に選び、データの内挿を行なう
  step_smote(class, over_ratio = 0.25, neighbors = 5) %>%
  prep()

hpc_smote <-
  hpc_smote_rec %>%
  bake(new_data = NULL)

# SMOTEアルゴリズムによるデータ増加の結果を確認
# over_ratioの調整により、少数のクラスの件数が変化する
hpc_smote %>%
  count(class)

