# 6.1.3 MeCab のダウンロードとインストール ------------------------------------------
# macOSの場合、以下はターミナルで実行
# MeCabのインストール
# tar zxfv mecab-0.996.tar.gz cd mecab-0.996
# ./configure
# make
# make check
# su
# make install
# 辞書のインストール
# tar zxfv mecab-ipadic-2.7.0-XXXX.tar.gz mecab-ipadic-2.7.0-XXXX
# ./configure
# make
# su
# make install

# 6.1.4 RMeCab パッケージのインストール ------------------------------------------
install.packages("RMeCab", repos = "http://rmecab.jp/R")

res <- RMeCab::RMeCabC("すもももももももものうち")

# 6.1.5 データをダウンロード ------------------------------------------
# コマンドプロンプトまたはターミナルで実行
# tar -xzf ldcc-20140209.tar.gz

# tidyverseパッケージの読み込み
library(tidyverse)
# ディレクトリのリストアップ
text_dir <- here::here("text")
livedoor_dirs <- list.dirs(file.path(text_dir),
                           recursive = FALSE)
# 除外するファイルを列挙
remove_file_regexp <- c(".*LICENSE.txt¦.*CHANGES.txt")
# ニュース記事ファイルのみに絞り込む
livedoor_files <- list.files(livedoor_dirs,
                             full.names = TRUE, recursive = FALSE) %>%
  # LICENSE.txtなどを除外
  stringr::str_subset(pattern = remove_file_regexp, negate = TRUE)
unlist (res)

# ファイル名からカテゴリを抽出する # カテゴリのリストアップ 
livedoor_categories <-
  c("dokujo-tsushin", "it-life-hack", "kaden-channel", "livedoor-homme", "movie-enter", "peachy",
  "smax",
  "sports-watch", ) "topic-news"
# 正規表現のOR条件で抽出できるよう、¦でつなぐ categories_regexp <-
stringr::str_c(livedoor_categories, collapse = "¦")
# 読み込み関数を作成
read_livedoor <- function(file) {
  # ファイルの読み込み
  lines <- readr::read_lines(file)
  # ディレクトリ名(カテゴリ名を抽出)
  dir_name <- stringr::str_match(file, categories_regexp)
  # データフレーム化
  df <- tibble::tibble(
    category = dir_name, source = lines[1], time_stamp = lines[2],
    source = lines[1],
    time_stamp = lines[2],
    body = paste(lines[3:length(lines)],
                 collapse = "\n\n")
  )
  return(df)
}

df_example <- read_livedoor(livedoor_files[1])

df_livedoor <- livedoor_files %>%
  # ニュース記事ファイルに関数を適用し、行方向に連結
  purrr::map_dfr(read_livedoor) %>%
  # 分析しやすいようにIDを振る
  dplyr::mutate(doc_id = dplyr::row_number())

# 6.2.1 rsample パッケージによるデータ分割 ------------------------------------------
# 乱数シードの指定
set.seed(71)
# 学習データ8割、評価データ2割に分割
livedoor_split <- rsample::initial_split(df_livedoor, prop = .8, strata = "category")
livedoor_train <- rsample::training(livedoor_split)
livedoor_test <- rsample::testing(livedoor_split)
# cv用にさらに分割
livedoor_cv_splits <- rsample::vfold_cv(livedoor_train,
                                        strata = "category", v = 3)

# 6.2.3 livedoor ニュースコーパスの前処理 ------------------------------------------
# MeCabでトークナイズする関数
mecab_tokenizer <- function(x) {
  # 残す品詞を指定
  keep_pos <- c("名詞", "形容詞", "動詞")
  res <- list()
  for (i in x) {
    # 形態素解析をして原型を返す
    wakati <- RMeCab::RMeCabC(i, mypref = 1) %>%
      unlist() %>%
      # 品詞を絞る
      ## unlist()でリスト化されたRMeCabの返り値は
      ## 名前付きベクトルになるので、names()が
      ## 指定した品詞に含まれる値のみを残したベクトルにする
      .[names(.) %in% keep_pos]
  }
  res <- c(res, list(wakati))
  return(res)
}

# 例としてデータフレームを作成
text_tibble <- tibble::tibble(
  text = c("吾輩は猫である", "今日は美味しい蕎麦を食べた", "明日は本を読みます")
  )

recipes::recipe(~text, data = text_tibble) %>%
  textrecipes::step_tokenize(text, custom_token = mecab_tokenizer) %>%
  textrecipes::show_tokens(text)

stopword_url <- "http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/ NLP/Filter/StopWord/word/Japanese.txt"
stopwords <- readr::read_lines(stopword_url) %>%
  # 空の文字列("")を削除
  .[stringr::str_detect(., "") == TRUE]

livedoor_rec <- recipes::recipe(category ̃~ body, data = livedoor_train) %>%
  # 文字列の正規化(数字英数字の半角化など)
  textrecipes::step_text_normalization() %>%
  # トークナイズ
  textrecipes::step_tokenize(body, custom_token = mecab_tokenizer) %>%
  # ストップワードの除去
  textrecipes::step_stopwords(body, custom_stopword_source = stopwords) %>%
  # 30回以上、200回以下の出現頻度の単語に絞る
  textrecipes::step_tokenfilter(body, min_times = 30, max_tokens = 200) %>%
  # Feature Hashingで特徴量に変換
  textrecipes::step_texthash(body, num_terms = 200)

# 6.2.4 XGBoost によるモデリング ------------------------------------------
livedoor_spec <- parsnip::boost_tree(
  sample_size = tune::tune(), loss_reduction = tune::tune(), tree_depth = tune::tune()
) %>%
  # アルゴリズムはXGBoostを使用
  parsnip::set_engine("xgboost") %>%
  # 分類モデル
  parsnip::set_mode("classification")

livedoor_workflow <- workflows::workflow() %>%
  workflows::add_model(livedoor_spec) %>%
  workflows::add_recipe(livedoor_rec)

# ハイパーパラメータチューニング
livedoor_tune_res <-
  livedoor_workflow %>% tune::tune_grid(
    resamples = livedoor_cv_splits,
    # ラテン超方格法を使用
    grid = dials::grid_latin_hypercube(
      dials::sample_prop(),
      dials::loss_reduction(),
      dials::tree_depth(),
      size = 10
    ),
    # 評価値にはF1値を使用
    metrics = yardstick::metric_set(yardstick::f_meas),
    control = tune::control_grid(save_pred = TRUE)
)

ggplot2::autoplot(livedoor_tune_res)

livedoor_tune_best <-
  livedoor_tune_res %>%
  tune::show_best()
livedoor_tune_best

# 選んだハイパーパラメータでモデル作成
livedoor_xgb_best <-
  parsnip::boost_tree(
    # 最適なハイパーパラメータを選択
    # 1行目を選択
    sample_size = livedoor_tune_best$sample_size[1],
    loss_reduction = livedoor_tune_best$loss_reduction[1],
    tree_depth = livedoor_tune_best$tree_depth[1]
  ) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode("classification")

# ワークフローの更新
livedoor_cv_last <-
  livedoor_workflow %>%
    workflows::update_model(livedoor_xgb_best)

# 更新したワークフローで学習データ全体にモデル適用
livedoor_last_fit <-
  livedoor_cv_last %>%
  tune::last_fit(livedoor_split,
                 # 評価指標はF1値
                 metrics = yardstick::metric_set(yardstick::f_meas))
# 最終的な精度を算出
last_f1 <- livedoor_last_fit %>%
  tune::collect_metrics()
last_f1

# COLUMN: broom パッケージと infer パッケージ ------------------------------------------
# broom パッケージによる前処理内容の tidy 化
car_recipe <-
  recipe(mpg ~ ., data = mtcars) %>%
  # 中心化
  step_center(disp, hp, drat, wt, qsec) %>%
  prep()

broom::tidy(car_recipe, 1)

# モデルの結果の tidy 化
# データの読み込み
data(ames, package = "modeldata")
# 線形回帰モデルの適用
lm_fit <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Bldg_Type, data = ames)

summary(lm_fit)

lm_fit_tidy <- broom::tidy(lm_fit)
lm_fit_tidy

lm_fit %>%
  broom::glance()

lm_fit %>% broom::augment() %>%
  head()

# infer パッケージによる R での統計的仮説検定
# データの読み込み
library(infer)
data(gss, package = "infer") # データの確認
head(gss)

# t検定を実行
ttest <- t.test(hours ~ college,
                data = gss)
# 結果の出力
ttest

ttest_tidy <- gss %>% t_test(hours ~ college,
                             # 差を計算するので、どちらからどちらを引くのかを指定
                             order = c("degree", "no degree"),
                             # 両側検定であることを指定
                             alternative = "two.sided")
ttest_tidy

# infer パッケージによる可視化
ttest_tidy2 <- gss %>%
  # 式を指定
  specify(hours ~ college) %>%
  # 差の検定(独立性の検定)であることを指定
  hypothesize(null = "independence") %>%
  # 分布等を指定
  calculate(stat = "t",
            order = c("degree", "no degree"),
            # 両側検定であることを指定
            alternative = "two.sided")

# t分布における帰無分布を描画
ttest_tidy2 %>%
  visualize(method = "theoretical")

ttest_tidy3 <- gss %>%
  # 式を指定
  specify(hours ~ college) %>%
  # 差の検定(独立性の検定)であることを指定
  hypothesize(null = "independence") %>%
  # サンプリング回数と方法を指定
  generate(reps = 200, type = "bootstrap") %>%
  # 分布等を指定
  calculate(stat = "t",
            order = c("degree", "no degree"),
            # 両側検定であることを指定
            alternative = "two.sided")

ttest_tidy3 %>%
  visualize(method = "both")