# 現在のR言語の環境を表示（実行結果は筆者の環境）
sessionInfo()

# lm()関数の詳細を調べるため、括弧を付けずに実行
lm

# 機械学習モデルを作成するためのtidymodelsパッケージと
# データ操作のためのtidyverseパッケージの読み込み
library(tidymodels)
library(tidyverse)

# amesデータの呼び出しとガレージ面積が0の物件を取り除く
data(ames)
ames_reg_data <- ames %>%
  filter(Garage_Area > 0)

# ガレージ面積と売却価格の間の関係性を可視化する
ames_reg_data %>%
  ggplot(aes(x = Garage_Area, y = Sale_Price)) +
  geom_point()

# statsパッケージで線形回帰モデルを作成する
lm_price_garage <-
  lm(formula = Sale_Price ~ Garage_Area, data = ames_reg_data)
lm_price_garage

# オブジェクトのクラスの確認
class(lm_price_garage)

# linear_reg()関数の詳細を確認する
linear_reg

# 機械学習を実行するための関数の引数として動作モードを指定する場合
linear_reg(mode = "regression")
# 動作モードを指定する関数を使用した場合
linear_reg() %>%
  set_mode(mode = "regression")

# linear_reg()関数が利用可能なパッケージ一覧の表示
show_engines("linear_reg")

# 機械学習を実行するための関数の引数として動作エンジンを指定する場合
linear_reg(mode = "regression", engine = "lm")

# 動作エンジンを指定する関数を使用した場合
linear_reg() %>%
  set_mode(mode = "regression") %>%
  set_engine(engine = "lm")

# parsnipパッケージによる線形回帰モデルの作成
lm_mod <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# 定義したモデルに、学習のために必要なデータや変数の情報を伝える
lm_fit <-
  lm_mod %>%
  fit(Sale_Price ~ Garage_Area, data = ames_reg_data)

# lm_fit()関数の詳細を確認する
lm_fit

# オブジェクトのクラスの確認
class(lm_fit)

# engineに指定したパッケージの引数を確認
lm_mod %>%
  translate()

# ハイパーパラメータを指定する方法
linear_reg() %>%
  set_args(mixture = 1, penalty = 1)

# ハイパーパラメータ指定のための専用関数を使わない方法
linear_reg(mixture = 1, penalty = 1)

# parsnipパッケージで使用できる機械学習モデルを探す
parsnip_addin()

# amesデータを学習データと評価データに分割する
ames_spl <- ames_reg_data %>% initial_split(prop = 0.7)
ames_train <- training(ames_spl)
ames_test <- testing(ames_spl)

# モデルの定義
lm_model <-
  linear_reg(mixture = 1, penalty = 1) %>%
  set_engine("lm") %>%
  set_mode("regression")

# formulaでデータを指定する場合
lm_formla_fit <-
  lm_model %>%
  fit(Sale_Price ~ Garage_Area, data = ames_train)

# 学習用のデータをxとyを分けて指定する場合
y <- ames_train %>% select(Sale_Price)
x <- ames_train %>% select(Garage_Area)
lm_xy_fit <-
  lm_model %>%
  fit_xy(x = x, y = y)

# 引数の名前を確認するために関数名のみを入力し実行する
glmnet::glmnet

# parsnipパッケージを介することでアルゴリズム間の引数の名前の違いを統一する例
# コードの中で異なるのはset_engineの中身のみ
rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("spark") %>%
  set_mode("regression")

rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

# エンジンに選択したパッケージが持つハイパーパラメータの指定方法の例
# parsnipパッケージのrand_forest()関数に存在せず、rangerパッケージに存在する引数の指定
rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

# parsnipパッケージのlogistic_reg()関数に存在せず、glmnetパッケージに存在する引数の指定
logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glmnet", nlambda = 10)

# parsnipパッケージのdecision_tree()関数に存在せず、rpartパッケージに存在する引数の指定
decision_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", parms = list(prior = c(.65, .35)))

# linear_reg()関数が存在しているかの確認
check_model_exists("linear_reg")

# my_lm()関数が存在しているかの確認
check_model_exists("my_lm")

# my_lm()関数の登録
set_new_model("my_lm")

# my_lm()関数の動作モードと動作エンジンの指定
set_model_mode(model = "my_lm", mode = "regression")

set_model_engine(model = "my_lm",
                 mode = "regression",
                 eng = "lm")

set_dependency("my_lm", eng = "lm", pkg = "stats")

# my_lm()関数の設定状況の確認
show_model_info("my_lm")

# 作成したい関数を作成
my_lm <-
  function(mode = "regression") {
    if (mode != "regression") {
      rlang::abort("this mode is not a known mode for my_lm()")
    }
    new_model_spec(
      "my_lm",
      args = NULL,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

# 学習の際の設定を入力
set_fit(
  model = "my_lm",
  eng = "lm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "stats", fun = "lm"),
    defaults = list()
  )
)

# 予測の際の設定を入力
set_pred(
  model = "my_lm",
  eng = "lm",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "response"
      )
  )
)

# my_lm()関数の設定状況の確認
show_model_info("my_lm")

# my_lm()がlm()関数と結びついているか確認
my_lm() %>%
  translate(engine = "lm")

# parsnipパッケージの開発者が作成した関数の結果を比較のために確認
linear_reg() %>%
  translate(engine = "lm")

# 自作の線形回帰モデルの定義
my_lm_mod <- my_lm() %>%
  set_engine("lm") %>%
  set_mode("regression")

# 学習の実行
fit_ed <- my_lm_mod %>%
  fit(Sepal.Length ~ Sepal.Width, data = iris)

# 学習済みモデルの確認
fit_ed

# statsパッケージで学習した場合の結果を表示して比較
lm(Sepal.Length ~ Sepal.Width, data = iris)

# ランダムフォレストモデルを定義し学習させる
ranger_rf <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger") %>%
  fit(Sale_Price ~ ., data = ames_train)

# 予測結果と元データを持ったデータを作成する
ames_test_res <-
  predict(ranger_rf, new_data = ames_test) %>%
  bind_cols(ames_test)

# データの先頭数行を確認
ames_test_res %>% head()

# rmseを計算する
ames_test_res %>%
  rmse(truth = Sale_Price, estimate = .pred)

# 予測と実測の値を散布図を使い確認する
ggplot() +
  geom_line(aes(x = Sale_Price, y = Sale_Price),
            data = ames_test_res,
            lty = 2) +
  geom_point(aes(x = Sale_Price, y = .pred), data = ames_test_res, alpha = 0.5) +
  labs(y = "Predicted Sale Price", x = "Sale Price") +
  coord_obs_pred()

# maeを計算する
ames_test_res %>%
  mae(truth = Sale_Price, estimate = .pred)

ames_test_res %>%
  metrics(truth = Sale_Price, estimate = .pred)

# 計算したい関数名を指定して
# 複数の評価指標を計算するためのames_metrics()関数を作成する
ames_metrics <- metric_set(rmse, rsq, mae, mpe, mape)

# ames_metrics()関数について知るためにクラスを確認する
ames_metrics %>% class()

# ames_metrics()関数の引数を確認するためにargs()関数を実行する
ames_metrics %>% args()

# データ内の予測値と実際の値を列名で指定し
# 複数の評価指標の計算結果を確認する
ames_test_res %>%
  ames_metrics(truth = Sale_Price, estimate = .pred)
