
# 12.8 バケットソート ------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# バケットソートの実装
bucket_sort <- function(vec, max_val = NA) {
  
  # 数列の要素数を取得
  N <- length(vec)
  
  # 最大値を設定
  if(is.na(max_val)) {
    max_val <- max(vec)
  }
  
  # 集計用の中間変数を初期化
  num_vec <- rep(x = 0, times = max_val)
  sum_vec <- rep(x = 0, times = max_val)
  val_vec <- rep(x = NA, times = N)
  
  # 要素ごとに処理
  for(i in 1:N) { # (前から)
    
    # 重複数をカウント
    num_vec[vec[i]] <- num_vec[vec[i]] + 1
  }
  
  # 値ごとに処理
  for(v in 1:max_val) {
    
    # 累積要素数をカウント
    if(v == 1) { # (初回)
      sum_vec[v] <- num_vec[v]
    } else {
      sum_vec[v] <- sum_vec[v-1] + num_vec[v]
    }
  }
  
  # 要素ごとに処理
  for(i in N:1) { # (後から)
    
    # 累積要素数番目に値を格納
    val_vec[sum_vec[vec[i]]] <- vec[i]
    
    # 累積要素数をカウントダウン
    sum_vec[vec[i]] <- sum_vec[vec[i]] - 1
  }
  
  # 数列を更新
  vec <- val_vec
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 50

# 最大値を指定
max_val <- 20

# 数列を生成
a <- sample(x = 1:max_val, size = N, replace = TRUE)
a; table(a)

# ソート
a <- bucket_sort(a)
sum(!(bucket_sort(a) == sort(a)))


# 可視化 ---------------------------------------------------------------------

# 数列を格納
seq_df <- tibble::tibble(
  index = 1:N, # 各試行のインデックス
  value = a    # 要素
)

# 数列を作図
ggplot() + 
  geom_bar(data = seq_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + 
  theme(panel.grid.minor.x = element_blank()) + # 図の体裁
  labs(title = "numerical sequence", 
       fill = "value", 
       x = "index", y = "value")


### ・一度に並べ替え -----

# 数列を格納
sequence_df <- tibble::tibble(
  iteration = 0, # フレーム番号
  id        = 1:N, # 要素ID
  # 数列用
  index = 1:N, 
  value = a, 
  # タイル用
  x        = index, 
  y        = 0.5 * value, 
  height   = value, 
  alpha    = 1, 
  linetype = "blank", 
  # ラベル用
  label_y = 0
) |> 
  dplyr::group_by(value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup()

# 値ごとの要素数をカウント
count_df <- tibble::tibble(
  iteration = 1, # フレーム番号
  id        = 1:N, # 要素ID
  # 数列用
  value = a, 
  # タイル用
  x       = value, 
  height  = 1, 
  alpha   = 0.1, 
  linetype = "dashed"
) |> 
  dplyr::group_by(value) |> # IDの割当・カウント用
  dplyr::mutate(
    count = dplyr::row_number(id), # 値ごとの累積要素数
    y     = count - 0.5, 
    # ラベル用
    label_y = y - 0.5, 
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup()

# 累積要素数をカウント
reorder_df <- tibble::tibble(
  iteration = 2, # フレーム番号
  id        = 1:N, # 要素ID
  # 数列用
  value = a, 
  # タイル用
  x        = value, 
  y        = 0.5, 
  height   = 1, 
  alpha    = 0.1, 
  linetype = "dashed", 
  # ラベル用
  label_y = y - 0.5
) |> 
  dplyr::arrange(value, id) |> # 累積カウント用
  dplyr::mutate(
    x = dplyr::row_number() # 累積要素数
  ) |> 
  dplyr::arrange(id) |> # IDの割当用
  dplyr::group_by(value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup()

# 数列をソート
swap_df <- tibble::tibble(
  iteration = 3, # フレーム番号
  id        = 1:N, # 要素ID
  # 数列用
  value = a, 
  # タイル用
  y        = 0.5 * value, 
  height   = value, 
  alpha    = 1, 
  linetype = "blank", 
  # ラベル用
  label_y = 0
) |> 
  dplyr::arrange(value, id) |> # 入替用
  dplyr::mutate(
    index = dplyr::row_number(), # 入替後のインデックス
    x     = index
  ) |> 
  dplyr::arrange(id) |> # IDの割当用
  dplyr::group_by(value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup()

# データを結合
trace_df <- dplyr::bind_rows(
  sequence_df, 
  count_df, 
  reorder_df, 
  swap_df
)


### ・1つずつ並べ替え -----

# インデックスの小さい順に要素を取り出す
sequence_df <- tidyr::expand_grid(
  iteration = 0:N, # カウント時の試行回数
  id        = 1:N  # 元のインデックス
) |> 
  dplyr::mutate(
    # 数列用
    index = id, 
    value = a[id], 
    # タイル用
    x         = index, 
    y         = 0.5 * value, 
    height    = value, 
    alpha     = 1, 
    linetype  = "solid", 
    # ラベル用
    label_y = 0
  ) |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(iteration < id) # 未カウントの要素を抽出

# インデックスの小さい順に要素を取り出してカウント
count_df <- tidyr::expand_grid(
  iteration = 0:N, # カウント時の試行回数
  id        = 1:N  # 元のインデックス
) |> 
  dplyr::mutate(
    # 数列用
    value = a[id], 
    # タイル用
    x         = value, 
    height    = 1, 
    alpha     = 0.1, 
    linetype  = "dashed"
  ) |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当・カウント用
  dplyr::mutate(
    # タイル用
    count = dplyr::row_number(id), # 値ごとの要素数
    y     = -count + 0.5, 
    # ラベル用
    label_y = y - 0.5, 
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(iteration >= id) # カウント済みの要素を抽出

# インデックスの小さい順に要素を入れ替えて戻す
swap_df <- tidyr::expand_grid(
  iteration = (N+1):(2*N+1), # 入替時の試行回数
  id        = 1:N # 元のインデックス
) |> 
  dplyr::mutate(
    # 数列用
    value = a[id], 
    # タイル用
    y         = 0.5 * value, 
    height    = value, 
    alpha     = 1, 
    linetype  = "solid", 
    # ラベル用
    label_y = 0
  ) |> 
  dplyr::arrange(iteration, value, id) |> # 入替用
  dplyr::group_by(iteration) |> # 入替用
  dplyr::mutate(
    # タイル用
    index = dplyr::row_number(), # 入替後のインデックス
    x     = index
  ) |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(iteration-(N+1) >= N+1-id) # 入替済みの要素を抽出

# インデックスの小さい順に要素を戻してカウント
discount_df <- tidyr::expand_grid(
  iteration = (N+1):(2*N+1), # 入替時の試行回数
  id        = 1:N # 元のインデックス
) |> 
  dplyr::mutate(
    # 数列用
    value = a[id], 
    # タイル用
    y         = -0.5, 
    height    = 1, 
    alpha     = 0.1, 
    linetype  = "dashed", 
    # ラベル用
    label_y = y - 0.5
  ) |> 
  dplyr::arrange(iteration, value, id) |> # 累積カウント用
  dplyr::group_by(iteration) |> # 累積カウント用
  dplyr::mutate(
    # タイル用
    x = dplyr::row_number() # 累積要素数
  ) |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    # 重複ラベル用
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(iteration-(N+1) < N+1-id) # 未入替の要素を抽出

# データを結合
trace_df <- dplyr::bind_rows(
  sequence_df, 
  count_df, 
  discount_df, 
  swap_df
)


### ・作図 -----

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_tile(data = trace_df, 
            mapping = aes(x = x, y = y, width = 1, height = height, 
                          fill = factor(value), color = factor(value), alpha = alpha, 
                          linetype = linetype, group = factor(id)), 
            linewidth = 1) + # 全要素
  geom_text(data = trace_df, 
            mapping = aes(x = x, y = label_y, label = as.character(value), group = factor(id)), 
            vjust = -1.5, size = 5) + # 要素ラベル
  geom_text(data = trace_df, 
            mapping = aes(x = x, y = label_y, label = dup_label, group = factor(id)), 
            vjust = -0.5, size = 4) + # 重複ラベル
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_linetype_identity() + 
  scale_linewidth_identity() + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~-., name = "count")) + # (一度に並べ替え用)
  coord_equal(ratio = 1) + # アスペクト比
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "bucket sort", 
       subtitle = "iteration: {next_state}", 
       fill = "value", 
       x = "index, value", y = "value")

# フレーム数を取得
frame_num <- trace_df[["iteration"]] |> 
  (\(val) {max(val) + 1})()

# 1試行当たりのフレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 30, 
  width = 1500, height = 1000, 
  renderer = gganimate::gifski_renderer()
)


