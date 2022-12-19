## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.path = here::here("ch1_prepare/img/"), dev = "ragg_png")
rlang::is_installed("rsample", version = "1.0.0")
rlang::is_installed("recipes", version = "1.0.0") # 1.0.1
rlang::is_installed("workflows", version = "1.0.0")
rlang::is_installed("tidymodels", version = "1.0.0")
ggplot2::theme_set(ggplot2::theme_gray(base_size = 9))
library(dplyr)
library(ggplot2)
library(patchwork)


## ---------------------------------------------------------------------------------------------------------------
library(rsample)
data(ames, package = "modeldata")


## ---------------------------------------------------------------------------------------------------------------
# rnorm()関数を使った正規分布に従う乱数の発生では、
# 実行のたびに異なる値が出力される。
# <!--takaya長そうなので、改行しておく。コード内コメントはである調-->
rnorm(5, mean = 0, sd = 1)
rnorm(5, mean = 0, sd = 1)


## ---------------------------------------------------------------------------------------------------------------
# set.seed()関数で乱数の固定を行うことができる。
set.seed(123)
rnorm(5)

set.seed(123)
rnorm(5)


## ---------------------------------------------------------------------------------------------------------------
# 再現性のある結果を得るために乱数を制御する
set.seed(123)
ames_split <- 
  initial_split(data = ames)


## ---------------------------------------------------------------------------------------------------------------
# initial_split()関数の結果はrsplitクラスになる
class(ames_split)
ames_split


## ---------------------------------------------------------------------------------------------------------------
# 学習データの取り出し
ames_train <-
  training(ames_split)
# 評価データの取り出し
ames_test <-
  testing(ames_split)

dim(ames_train)


## ---------------------------------------------------------------------------------------------------------------
set.seed(123)
# prop引数で学習用に用意するデータの比率を調整する
# propに与える数値が学習データの割合になる
ames_split <- 
  initial_split(data = ames, prop = 0.80)
ames_split


## ---------------------------------------------------------------------------------------------------------------
library(parsnip)
# 線形回帰モデルの指定
lm_spec <- 
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# Sale_Priceを予測するモデルの作成
lm_fit <-
  lm_spec %>% 
  fit(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type, data = ames_train)


## ---------------------------------------------------------------------------------------------------------------
# 学習データ ames_test へのモデルの適用
# 目的変数である Sale_Priceとモデルが予測した値 .pred が含まれるデータフレームを作成
augment(lm_fit, new_data = ames_test) %>% 
  select(Sale_Price, .pred)


## ---------------------------------------------------------------------------------------------------------------
# 学習データを使ってモデルの性能評価を行う
augment(lm_fit, new_data = ames_train)  %>%
  yardstick::rmse(truth = Sale_Price, estimate = .pred)


## ---------------------------------------------------------------------------------------------------------------
# 評価データを使ってモデルの性能評価を行う
augment(lm_fit, new_data = ames_test) %>%
  yardstick::rmse(truth = Sale_Price, estimate = .pred)


## ----ch1_ames_sale_price_distribution, echo=FALSE, fig.cap="amesデータにおけるSale Priceの確率密度曲線。四分位数を波線で示しています。", fig.width=3, fig.height=2----
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


## ---------------------------------------------------------------------------------------------------------------
# amesデータにおけるSale_Priceの四分位数を算出する
q <- 
  quantile(ames$Sale_Price)
q
# 分割後の学習データに含まれるSale_Priceの分布を四分位数を元にカウント
tibble(
  Sale_Price = training(ames_split)$Sale_Price) %>% 
  mutate(group = case_when(
    between(Sale_Price, q[1], q[2]) ~ 1,
    between(Sale_Price, q[2]-1, q[3]) ~ 2,
    between(Sale_Price, q[3]-1, q[4]) ~ 3,
    between(Sale_Price, q[4]-1, q[5]) ~ 4
  )) %>% 
  count(group)


## ---------------------------------------------------------------------------------------------------------------
set.seed(123)
# 引数 strata にSale_Price変数を指定し、層を作る
ames_split <- 
  initial_split(ames, prop = 0.80, strata = Sale_Price)
tibble(
  Sale_Price = training(ames_split)$Sale_Price) %>% 
  mutate(group = case_when(
    between(Sale_Price, q[1], q[2]) ~ 1,
    between(Sale_Price, q[2]-1, q[3]) ~ 2,
    between(Sale_Price, q[3]-1, q[4]) ~ 3,
    between(Sale_Price, q[4]-1, q[5]) ~ 4
  )) %>% 
  count(group)


## ----load_drinks_data-------------------------------------------------------------------------------------------
# drinksデータを利用可能にする
data(drinks, package = "modeldata")


## ---- echo=FALSE------------------------------------------------------------------------------------------------
head(drinks) %>% 
  knitr::kable(caption = "drinksデータの内容を確認します。日付と販売量の変数が記録されています。")


## ----initial_time_split-----------------------------------------------------------------------------------------
# initial_time_split()関数の実行
set.seed(123)
drinks_split_ts <- 
  initial_time_split(drinks)
# 学習データ、評価データの参照方法はinitial_split()関数の結果と同様
train_ts_data <- 
  training(drinks_split_ts)
test_ts_data <- 
  testing(drinks_split_ts)


## ---------------------------------------------------------------------------------------------------------------
# 学習・評価データそれぞれに含まれる日付の範囲を表示する
range(train_ts_data$date)
range(test_ts_data$date)


## ----ch1_initial_time_split, echo=FALSE, fig.cap="時系列データに対するrsampleパッケージの分割関数の適用結果の違い。A) 時系列データにinitial_split()関数によるデータ分割を適用すると、評価データに割り当てられるデータが日付の並びを無視したものとなってしまう。B) initial_time_split()関数によるデータ分割を行った場合、評価データは最新のデータに割り当てられる。", fig.height=3----
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


## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------
## loo_cv(drinks)


## ----vfold_cv---------------------------------------------------------------------------------------------------
set.seed(123)
# k = 10のk分割交差検証を実行
# 学習データを引数dataに与える
folds <- 
  vfold_cv(ames_train, v = 10)
folds


## ---------------------------------------------------------------------------------------------------------------
# vfold_cv()関数により生成されるsplits列のオブジェクトはrsplitクラスの他にvfol_splitクラスをもつ
class(folds$splits[[1]])

# splits列にはrsplitクラスのオブジェクトが格納される
folds$splits[[1]]


## ---------------------------------------------------------------------------------------------------------------
# 分析セットの参照
analysis(folds$splits[[1]])

# 検証セットの参照
assessment(folds$splits[[1]])


## ----vfold_cv_repeats-------------------------------------------------------------------------------------------
set.seed(123)
# v = 10、繰り返し回数 = 5の10*5のデータを作成する
ames_fold_rep5 <- 
  vfold_cv(ames_train, v = 10, repeats = 5)

ames_fold_rep5


## ----bootstraps-------------------------------------------------------------------------------------------------
set.seed(123)
# 25個のブートストラップ標本を作成する
ames_boots <- 
  bootstraps(ames_train, times = 25)

dim(ames_train)
# ブートストラップ標本に含まれる分析セットの数は元のデータ件数と一致する
# 評価セットの件数はブートストラップ標本ごとに異なる
ames_boots


## ---------------------------------------------------------------------------------------------------------------
# drinksデータに年を表す変数を追加し、件数を3年間（36ヶ月）分に制限する
drinks_annual <- 
  drinks %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(between(year, 1992, 1994))

# 時系列でのデータ分割により、1994年のデータが評価データに利用されるようにする
drinks_split_ts2 <- 
  initial_time_split(drinks_annual, prop = 0.68)
train_ts_data2 <- 
  training(drinks_split_ts2)

# 1992年と1993年の24ヶ月が学習データに扱われる
train_ts_data2


## ----sliding_window---------------------------------------------------------------------------------------------
# 時系列リサンプリングの実行
ts_fold <-
  sliding_window(train_ts_data2,
                 lookback = 11, 
                 assess_start = 1, 
                 assess_stop = 10)

ts_fold


## ---------------------------------------------------------------------------------------------------------------
# 最初の分析セットと検証セットの確認
analysis(ts_fold$splits[[1]])
assessment(ts_fold$splits[[1]])

# 2つ目の分析セットの確認
analysis(ts_fold$splits[[2]])
assessment(ts_fold$splits[[2]])


## ----ch1_sale_price_histrogram, eval=TRUE, echo=FALSE, fig.cap="amesデータの売却価格のヒストグラム。Aは元の売却価格。Bは売却価格を対数変換した値を描画したもの。Aでは高額な価格のデータの存在により、分布の裾が右に伸びていますが、対数変換を施すことでBのように分布の歪みを軽減できます。", fig.height=3----
library(ggokabeito)
p_base <- 
  ames %>% 
  ggplot(aes(Sale_Price)) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  scale_fill_identity()

p1 <- p_base + 
  aes(fill = ggokabeito::palette_okabe_ito()[1])
p2 <- 
  p_base + 
  aes(fill = ggokabeito::palette_okabe_ito()[5]) +
  scale_x_log10()

p1 + p2 + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")


## ---- message=FALSE---------------------------------------------------------------------------------------------
library(recipes)


## ----ames_recipeの宣言------------------------------------------------------------------------------------------
# レシピ（recipe）を表すオブジェクトであることがわかるように rec とつける
ames_rec <-
   recipe(x = ames_train,
          formula = Sale_Price ~ .)


## ---------------------------------------------------------------------------------------------------------------
class(ames_rec)


## ---------------------------------------------------------------------------------------------------------------
ames_rec <-
  ames_rec %>%
  # 対数変換を行うstep_*()関数。
  # デフォルトでは自然対数が使われるので常用対数の底10をbase引数に指定する。
  step_log(Sale_Price, base = 10)


## ---------------------------------------------------------------------------------------------------------------
ames_rec


## ---------------------------------------------------------------------------------------------------------------
# prep()関数が実行される正常なコード。
# step_*()関数で対象となる変数はいずれもrecipe(formula = )で定義されている。
ames_rec <-
  prep(ames_rec)


## ---------------------------------------------------------------------------------------------------------------
# 問題となるコード
# モデル式に含まれない変数名 saleprice を step_*()関数に与えてレシピを作った場合でも
# recipeオブジェクトは生成されるが、prep()関数の実行時に変数名がないためにエラーとなる。
bad_rec <-
  recipe(ames_train,
       formula = Sale_Price ~ .) %>% 
  # データ上に含まれない変数名を指定
  step_log(saleprice, base = 10)

bad_rec


## ---------------------------------------------------------------------------------------------------------------
bad_rec %>%
  prep()


## ---------------------------------------------------------------------------------------------------------------
# step_*()関数内でall_outcomes()関数を用いて、変数名の入力を省略する
recipe(x = ames_train,
         formula = Sale_Price ~ .) %>%
  step_log(all_outcomes(), base = 10)


## ---------------------------------------------------------------------------------------------------------------
# 2つの実行結果は同じ
all.equal(
  # recipe(ames_train, ...) で作成したレシピ
  prep(ames_rec),
  # prep()関数上でames_trainを指定
  prep(ames_rec, training = ames_train))


## ---------------------------------------------------------------------------------------------------------------
# 学習データに対するレシピの適用
ames_train_baked <- 
  ames_rec %>% 
  bake(new_data = NULL)

# 評価データに対するレシピの適用
ames_test_beked <-
  ames_rec %>% 
  bake(new_data = ames_test)


## ---------------------------------------------------------------------------------------------------------------
ames_train %>% 
  pull(Sale_Price) %>% 
  head()

ames_train_baked %>% 
  pull(Sale_Price) %>% 
  head()


## ---------------------------------------------------------------------------------------------------------------
# レシピの適用結果から目的変数とLot_Area変数のみを取り出す
ames_rec %>% 
  bake(new_data = NULL, all_outcomes(), Lot_Area)


## ---------------------------------------------------------------------------------------------------------------
# 問題となる場合
car_recipe <- 
  recipe(mpg ~ ., data = mtcars) %>%
  step_log(disp, skip = TRUE) %>%
  step_center(all_numeric_predictors()) %>%
  prep(training = mtcars)

# 学習データにのみすべてのstep_*()関数が適用される
bake(car_recipe, new_data = NULL) %>% pull(disp) %>% summary()
# 学習データ以外をbake(new_data = )に与えた場合、step_*(skip = TRUE)の処理は無視される
bake(car_recipe, new_data = mtcars) %>% pull(disp) %>% summary()


## ---------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------
## # recipesパッケージのstep_*()関数はパイプ演算子(%>%)を使って数珠繋ぎに記述できる
## rec <-
##   recipes(y ~ x, data = df) %>%
##   step_first(y) %>%
##   step_second(x)


## ----ch1_ames_neighborhood_frequency, eval=TRUE, echo=FALSE, fig.cap="amesデータのNeighborhood変数に含まれるカテゴリの頻度", fig.height=3----
tibble::as_tibble(as.data.frame(table(ames_train$Neighborhood))) %>% 
  purrr::set_names(c("Neighborhood", "Freq")) %>% 
  ggplot(aes(forcats::fct_reorder(Neighborhood, Freq), Freq)) +
  geom_bar(stat = "identity") +
  xlab("Neighborhood") +
  gghighlight::gghighlight(Freq >= 220) +
  coord_flip()


## ---------------------------------------------------------------------------------------------------------------
ames_rec <-
  recipe(x = ames_train,
          formula = Sale_Price ~ .) %>% 
  step_log(Sale_Price, base = 10) %>% 
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.1)  %>%
  step_zv(all_predictors())


## ---------------------------------------------------------------------------------------------------------------
ames_rec


## ---------------------------------------------------------------------------------------------------------------
## tibble::tibble(
##   `関数名` = ls("package:recipes", pattern = "^step_")
## )


## ---------------------------------------------------------------------------------------------------------------
set.seed(123)
df <- data.frame(
  y = rnorm(10, mean = 100),
  x1 = rnorm(10, mean = 5),
  x2 = rnorm(10, mean = 3),
  x3 = letters[1:10])


## ---------------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------------
recipe(y ~ ., data = df) %>%
  # 中心化の処理は数値型の変数でないと実行できないため、
  # 数値ではないx3を除外させる
  step_center(all_predictors(), -has_type("nominal")) %>%
  prep(training = df) %>%
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
summary(ames_rec)


## ----update_role------------------------------------------------------------------------------------------------
# biomassデータでは学習データ、評価データを一つのデータにまとめてある。
# 変数 datasetをもとに学習データと評価データに区別される。
data(biomass, package = "modeldata")
biomass_train <- biomass %>% 
  filter(dataset == "Training")
biomass_test <- biomass %>% 
  filter(dataset == "Testing")


## ---------------------------------------------------------------------------------------------------------------
rec <- 
  recipe(HHV ~ ., data = biomass_train) %>%
  update_role(sample, new_role = "id variable") %>%
  update_role(dataset, new_role = "dataset") %>%
  step_center(all_predictors())

# 変更後のroleを確認する
summary(rec) %>% 
  filter(variable %in% c("sample", "dataset"))


## ---------------------------------------------------------------------------------------------------------------
prep(rec, biomass_train) %>% 
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
# 文字列の説明変数に対して中央化を行おうとするためにエラーとなる例
rec <- 
  recipe(HHV ~ ., data = biomass_train) %>%
  step_center(all_predictors())

prep(rec, biomass_train) %>% 
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
## # roleを使わずに上記のエラーを回避する例
## # 1. step_*()関数での処理対象の指定に適切な変数を指定する
## recipe(HHV ~ ., data = biomass_train) %>%
##   step_center(all_numeric_predictors()) %>%
##   prep() %>%
##   bake(new_data = NULL)
## # 2. あらかじめモデルに使わない変数をモデル式に含めない
## recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_train) %>%
##   step_center(all_predictors()) %>%
##   prep() %>%
##   bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
# amesデータの中のカテゴリ変数
ames %>% 
  select(tidyselect:::where(is.factor)) %>% 
  glimpse()


## ---------------------------------------------------------------------------------------------------------------
ames_cat_rec <- 
  ames_train %>% 
  recipe(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type) %>% 
  step_dummy(Bldg_Type)

ames_cat_rec


## ---------------------------------------------------------------------------------------------------------------
ames_cat_rec %>% 
  prep() %>% 
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
ames_train %>% 
  recipe(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type) %>% 
  step_dummy(Bldg_Type, one_hot = TRUE) %>% 
  prep() %>% 
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
ames_df <- 
  ames %>% 
  select(Sale_Price, Bsmt_Cond)
ames_df$Bsmt_Cond <- 
  factor(ames_df$Bsmt_Cond,
      levels = c("Excellent", "Good", "Typical", "Poor", "Fair", "No_Basement"),
      ordered = TRUE)

ames_df %>% 
  recipe(Sale_Price ~ Bsmt_Cond) %>% 
  step_ordinalscore(Bsmt_Cond) %>% 
  prep(train = ames_df) %>% 
  bake(new_data = NULL)

custom <- function(x) {
  new_values <- c(0, 3, 7, 10, 11, 13)
  new_values[as.numeric(x)]
}

ames_df %>% 
  recipe(Sale_Price ~ Bsmt_Cond) %>% 
  step_ordinalscore(Bsmt_Cond, convert = custom) %>% 
  prep() %>% 
  bake(new_data = NULL)


## ---------------------------------------------------------------------------------------------------------------
library(themis)


## ---------------------------------------------------------------------------------------------------------------
# 計算機の実行速度を評価したデータ
data(hpc_data, package = "modeldata")

# 不要な列を除いておく
hpc_data_mod <- 
  hpc_data %>%
  select(-protocol, -day)

# 処理速度を評価したクラスの件数を確認
count(hpc_data_mod, class)


## ----step_downsample--------------------------------------------------------------------------------------------
hpc_down_rec <- 
  recipe(class ~ ., data = hpc_data_mod) %>% 
  # デフォルトでskip = TRUEが与えられている。
  # そのため学習データ以外ではこの処理は無視される。
  step_downsample(class, skip = TRUE) %>% 
  prep()


## ---------------------------------------------------------------------------------------------------------------
hpc_down_rec %>% 
  bake(new_data = NULL) %>% 
  count(class)


## ----step_upsample----------------------------------------------------------------------------------------------
hpc_up_rec <- 
  recipe(class ~ ., data = hpc_data_mod) %>% 
  step_upsample(class, seed = 123) %>% 
  prep()

hpc_up <- 
  hpc_up_rec %>% 
  bake(new_data = NULL)

hpc_up %>% 
  count(class)


## ---------------------------------------------------------------------------------------------------------------
hpc_smote_rec <- 
  recipe(class ~ ., data = hpc_data_mod) %>%
  # step_impute_mean(all_predictors()) %>%
  step_smote(class, over_ratio = 0.25) %>%
  prep()

hpc_smote_rec %>% 
  bake(new_data = NULL) %>% 
  count(class)

