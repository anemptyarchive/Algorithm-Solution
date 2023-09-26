
# 12.6 ヒープソート -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# 部分木のヒープ化の実装
sub_heapify <- function(vec, i, N) {
  
  # 左側の子のインデックスを計算
  child_idx <- i * 2
  
  # 子がなければ終了
  if(child_idx > N) return(vec)
  
  # 値が大きい方の子のインデックスを設定
  if(child_idx+1 <= N & vec[child_idx+1] > vec[child_idx]) { # (&の左がFALSEだと右が処理されずNAにならないので動く)
    child_idx <- child_idx + 1
  }
  
  # 子が親以下なら終了
  if(vec[child_idx] <= vec[i]) return(vec)
  
  # 子と親を入替
  vec[1:N] <- replace(x = vec, list = c(child_idx, i), values = vec[c(i, child_idx)])
  
  # 子を根としてヒープ化
  vec[1:N] <- sub_heapify(vec, child_idx, N)
  
  # 数列を出力
  return(vec)
}

# ヒープ化の実装
heapify <- function(vec) {
  
  # 要素数を取得
  N <- length(vec)
  
  # 葉を除くノード番号の最大値を設定
  max_i <- N %/% 2
  
  # 全体をヒープ化
  for(i in max_i:1) {
    
    # i番目の頂点を根とする部分木をヒープ化
    vec[1:N] <- sub_heapify(vec, i, N)
  }
  
  # 数列を出力
  return(vec)
}

# ヒープソートの実装
heap_sort <- function(vec) {
  
  # 数列の要素数を取得
  N <- length(vec)
  
  # 葉を除くノード番号の最大値を設定
  max_i <- N %/% 2
  
  # 全体をヒープ化(1番目の最大値を探索)
  for(i in max_i:1) {
    
    # i番目の頂点を根とする部分木をヒープ化
    vec[1:N] <- sub_heapify(vec, i, N)
  }
  
  # 全体をソート
  for(i in N:2) {
    
    # 「前からi番目までの最大値」を「後ろからi番目の要素」と入替
    vec[1:N] <- replace(x = vec, list = c(1, i), values = vec[c(i, 1)])
    
    # 前からi-1番目までの要素(N-i+1個の最大値を除く要素)をヒープ化(N-i+2番目の最大値を探索)
    vec[1:(i-1)] <- sub_heapify(vec[1:(i-1)], 1, i-1)
  }
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 30

# 数列を作成
a <- sample(x = 1:N, size = N, replace = FALSE)
a <- rnorm(n = N, mean = 0, sd = 1) |> 
  round(digits = 1)
a

# ソート
heap_sort(a)
sum(!(heap_sort(a) == sort(a)))


### ・作図 ----

# ヒープ化
a <- heap_sort(a)

# 要素数を取得
N <- length(a)

# 数列を格納
tmp_df <- tibble::tibble(
  iteration = 0,   # 試行回数
  id        = 1:N, # 元のインデックス
  index     = 1:N, # 各試行のインデックス
  value     = a    # 要素
)

# 数列を作図
ggplot() + 
  geom_bar(data = tmp_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "numerical sequence", 
       subtitle = paste0("iteration: ", unique(tmp_df[["iteration"]])), 
       fill = "value", 
       x = "index", y = "value")


# ノードの座標を作成
d <- 0.6
vertex_df <- tibble::tibble(
  value = a, 
  index = 1:length(a), 
  depth = floor(log2(index)), # 縦方向のノード位置
  col_idx = index - 2^depth + 1, # 深さごとのノード番号
  coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向のノード位置
  label_offset = dplyr::if_else(
    condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
  ) # ラベル位置を左右にズラす
)

# エッジの座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  vertex_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, 
      node_type = "childe" # (確認用)
    ), 
  # 親ノードの座標
  vertex_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, 
      node_type = "parent", # (確認用)
      depth   = depth - 1, # 縦方向のノード位置
      index   = index %/% 2, 
      col_idx = index - 2^depth + 1, # 深さごとのノード番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向のノード位置
    )
) |> 
  dplyr::select(!label_offset) |> 
  dplyr::arrange(edge_id, depth)

# ツリーの高さを取得
max_h <- floor(log2(N))

# 二分木を作図
d <- 0.1
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # 辺
  geom_point(data = vertex_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = value), 
            size = 5) + # 値ラベル
  geom_text(data = vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -2, color = "green4") + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(max_h+d, -d)) + 
  labs(title = "binary heap", 
       subtitle = paste0("height = ", max_h), 
       y = "depth")


# 可視化：部分木ごとに操作 ---------------------------------------------------------------------

# 部分木のヒープ化の実装
trace_sub_heapify <- function(value, index, i, N) {
  
  # 値・インデックスを取得
  val_vec <- value
  idx_vec <- index
  
  # 左側の子のインデックスを計算
  child_idx <- i * 2
  
  # 子がなければ終了
  if(child_idx > N) {
    lt <- list(value = val_vec, index = idx_vec)
    return(lt)
  }
  
  # 値が大きい方の子のインデックスを設定
  if(child_idx+1 <= N & val_vec[child_idx+1] > val_vec[child_idx]) { # (&の左がFALSEだと右が処理されずNAにならないので動く)
    child_idx <- child_idx + 1
  }
  
  # 子が親以下なら終了
  if(val_vec[child_idx] <= val_vec[i]) {
    lt <- list(value = val_vec, index = idx_vec)
    return(lt)
  }
  
  # 子と親を入替
  val_vec[1:N] <- replace(x = val_vec, list = c(child_idx, i), values = val_vec[c(i, child_idx)])
  idx_vec[1:N] <- replace(x = idx_vec, list = c(child_idx, i), values = idx_vec[c(i, child_idx)])
  
  # 子を根としてヒープ化
  res_lt <- trace_sub_heapify(val_vec, idx_vec, child_idx, N)
  val_vec[1:N] <- res_lt[["value"]]
  idx_vec[1:N] <- res_lt[["index"]]
  
  # 数列を出力
  lt <- list(value = val_vec, index = idx_vec)
  return(lt)
}


### ・作図データの作成 -----

# 要素数を指定
N <- 20

# ヒープを作成
a <- rnorm(n = N, mean = 0, sd = 1) |> 
  round(digits = 1) |> 
  heapify()

# 数列を格納
tmp_df <- tibble::tibble(
  iteration = 0,   # 試行回数
  id        = 1:N, # 元のインデックス
  index     = id,  # 各試行のインデックス
  value     = a,   # 要素
  heap_num  = N,   # ヒープの要素数
  target_flag = FALSE, # 入替対象
  sorted_flag = FALSE  # ソート済み要素
)

# 作図用のオブジェクトを初期化
id_vec   <- 1:N
trace_df <- tmp_df

# 全体をソート
iter <- 0
for(i in N:2) {
  
  ## 最大値の入替操作
  
  # 1番目(i番目までの最大値)とi番目の要素を入替
  a[1:N]      <- replace(x = a, list = c(1, i), values = a[c(i, 1)])
  id_vec[1:N] <- replace(x = id_vec, list = c(1, i), values = id_vec[c(i, 1)])
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    iteration = iter + 1, 
    id        = id_vec, 
    index     = 1:N, 
    value     = a, 
    heap_num  = i, # (作図用にi番目のヒープとしてカウントしておく)
    target_flag = index %in% c(1, i), # 入替要素:(最大値と最後尾)
    sorted_flag = index >= i
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- iteration: ", iter+1, " ---"))
  print(a)
  
  ## ヒープ化操作(最大値の探索)
  
  # 要素が1つならヒープ化不要
  if(i == 2) break
  
  # i-1番目までの要素をヒープ化(N-i+2個目の最大値を探索)
  res_lt <- trace_sub_heapify(a[1:(i-1)], id_vec[1:(i-1)], i = 1, N = i-1)
  a[1:(i-1)]      <- res_lt[["value"]]
  id_vec[1:(i-1)] <- res_lt[["index"]]
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    iteration = iter + 2, 
    id        = id_vec, 
    index     = 1:N, 
    value     = a, 
    heap_num  = i - 1, 
    target_flag = FALSE, 
    sorted_flag = index >= i
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 試行回数をカウント
  iter <- iter + 2
}


### ・バイナリツリー -----

# 頂点の座標を作成
trace_vertex_df <- trace_df |> 
  dplyr::mutate(
    depth   = floor(log2(index)), # 縦方向のノード位置
    col_idx = index - 2^depth + 1, 
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向のノード位置
  ) |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    dup_id    = dplyr::row_number(id), # 重複IDを割当
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, index)

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  trace_vertex_df |> 
    dplyr::filter(iteration == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親ノードの座標
  trace_vertex_df |> 
    dplyr::filter(iteration == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, 
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, 
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2)
    )
) |> 
  dplyr::select(!iteration) |> # フレーム遷移の影響から外す
  dplyr::arrange(edge_id, depth)

# インデックスラベルの座標を作成
d <- 0.6
index_df <- trace_vertex_df |> 
  dplyr::filter(iteration == 0) |> # 1試行分のデータを抽出
  dplyr::mutate(
    offset = dplyr::if_else(
      condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
    ) # ラベル位置を左右にズラす
  ) |> 
  dplyr::select(!iteration) # フレーム遷移の影響から外す

# 入替対象の頂点の座標を作成
target_vertex_df <- dplyr::bind_rows(
  # 最大値との入替要素の座標
  trace_vertex_df |> 
    dplyr::filter(target_flag) |> # 入替要素を抽出
    dplyr::mutate(
      vertex_type = "max"
    ), 
  # (ヒープの最大値・最後尾を除く)ヒープ化要素の座標
  trace_vertex_df |> 
    dplyr::filter(!sorted_flag, !target_flag) |> # ヒープ要素を抽出
    dplyr::mutate(
      vertex_type = "heap"
    ), 
  # ソート済み要素の座標
  trace_vertex_df |> 
    dplyr::filter(sorted_flag, !target_flag) |> # ソート済み要素を抽出
    dplyr::mutate(
      vertex_type = "sort"
    ), 
  # フレーム調整用に最終値を複製
  trace_vertex_df |> 
    dplyr::filter(iteration == max(iteration)) |> # 最終結果を抽出
    dplyr::mutate(
      vertex_type = "sort", 
      heap_num    = 0, 
      target_flag = FALSE, 
      sort_flag   = TRUE, 
      iteration   = iteration + 1 # フレーム調整用
    )
) |> 
  dplyr::filter(iteration > 0) |> # フレーム調整用
  dplyr::mutate(
    vertex_label = paste0("iter: ", iteration, ", idx: ", index), 
    iteration = iteration - 1 # 表示フレームを調整
  ) |> 
  dplyr::arrange(iteration, index)

# 最大値との入替対象の辺の座標を計算
swap_edge_df <- target_vertex_df |> 
  dplyr::filter(target_flag) |> # 入替要素を抽出
  dplyr::arrange(iteration, index)

# ヒープ対象の辺の座標を計算
heap_edge_df <- dplyr::bind_rows(
  # 親の座標
  target_vertex_df |> 
    dplyr::filter(index <= heap_num%/%2) |> # 葉とソート済み要素を除去
    tidyr::uncount(weights = 2, .id = "childe_id") |> # 子の数に複製
    dplyr::mutate(
      vertex_type = "heap", 
      childe_id = childe_id - 1, # 行番号を子IDに変換
      edge_id   = index*2+childe_id # 子インデックス
    ) |> 
  dplyr::filter(edge_id <= heap_num), # 子がなければ除去
  # 子の座標
  target_vertex_df |> 
    dplyr::filter(index > 1, index <= heap_num) |> # 根とソート済み要素を除去
    dplyr::mutate(
      vertex_type = "heap", 
      edge_id = index
    ), 
) |> 
  dplyr::mutate(
    edge_label = paste0("iter: ", iteration+1, ", idx: ", edge_id)
  ) |> 
  dplyr::arrange(iteration, edge_id, index)


# ヒープソートのアニメーションを作図
max_h <- floor(log2(N))
graph <- ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # 辺
  geom_path(data = heap_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_label),
            color = "blue", linewidth = 1, linetype = "dashed") + # ヒープ対象の辺
  geom_path(data = swap_edge_df,
            mapping = aes(x = coord_x, y = depth, group = iteration),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_vertex_df,
             mapping = aes(x = coord_x, y = depth, color = vertex_type, group = vertex_label),
             size = 15, alpha = 0.6) + # 入替対象の頂点
  geom_point(data = trace_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = id),
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df,
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id),
            size = 4.5) + # 値ラベル
  geom_text(data = trace_vertex_df,
            mapping = aes(x = coord_x, y = depth, label = dup_label, group = id),
            size = 4, vjust = 2.5) + # 重複ラベル
  geom_text(data = index_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset), 
            size = 4, vjust = -1.9, color = "green4") + # インデックスラベル
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  scale_color_manual(breaks = c("max", "heap", "sort"),
                     values = c("red", "blue", "orange"),
                     labels = c("swap", "heap", "sorted"),
                     name = "vertex") + # (凡例表示用)
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heap sort", 
       subtitle = "iteration: {next_state}", 
       x = "", y = "depth")

# フレーム数を取得
frame_num <- max(trace_df[["iteration"]]) + 1

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 600, 
  renderer = gganimate::gifski_renderer()
)


### ・バーチャート -----

# 重複ラベルを作成
dup_label_df <- trace_df |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    dup_id    = dplyr::row_number(id), # 重複IDを割当
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, true = paste0("(", dup_id, ")"), false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, index)

# 入替対象の座標を作成
range_swap_df <- trace_df |> 
  dplyr::filter(iteration%%2 == 0) |> # 入替操作前のデータを抽出
  dplyr::group_by(iteration) |> # 入替対象の抽出用
  dplyr::filter(index %in% c(1, N-0.5*iteration)) |> # 入替対象の抽出
  dplyr::ungroup() |> 
  dplyr::mutate(
    bar_label = paste0("iter: ", iteration, ", idx: ", index)
  )

# ヒープの最大値を作成
upper_df <- trace_df |> 
  dplyr::filter(target_flag) |> # 入替操作時のデータを抽出
  dplyr::group_by(iteration) |> # 最大値の抽出用
  dplyr::filter(index != 1) |> # 最大値を抽出
  dplyr::ungroup() |> 
  dplyr::mutate(
    iteration = iteration - 1 # 表示フレームを調整
  )

# ヒープ範囲の座標を作成
d <- 0.2
range_heap_df <- tibble::tibble(
  iteration = 0:(2*N-3), 
  left  = 1, 
  right = c(N, rep((N-1):2, each = 2), 0), 
  x     = 0.5 * (left + right), 
  y     = 0.5 * (max(a) + min(c(0, a))), 
  w     = right - left + 1, 
  h     = max(a) - min(c(0, a)) + d
)

# ソート済み範囲の座標を作成
range_sort_df <- tibble::tibble(
  iteration = 0:(2*N-3), 
  left  = c(N+1, rep(N:3, each = 2), 1), 
  right = N, 
  x     = 0.5 * (left + right), 
  y     = 0.5 * (max(a) + min(c(0, a))), 
  w     = right - left + 1, 
  h     = max(a) - min(c(0, a)) + d
)


# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_tile(data = range_heap_df,
            mapping = aes(x = x, y = y, width = w, height = h, color = "heap"), 
            fill = "blue", alpha = 0.1, linewidth = 1, linetype = "dashed") + # ヒープ範囲
  geom_tile(data = range_sort_df,
            mapping = aes(x = x, y = y, width = w, height = h, color = "sort"), 
            fill = "orange", alpha = 0.1, linewidth = 1, linetype = "dashed") + # ソート済み範囲
  geom_bar(data = range_swap_df,
           mapping = aes(x = index, y = value, color = "max", group = bar_label), 
           stat = "identity", 
           fill = "red", alpha = 0.1, linewidth = 1, linetype = "dashed") + # 入替対象
  geom_bar(data = trace_df,
           mapping = aes(x = index, y = value, fill = factor(value), group = id),
           stat = "identity", show.legend = FALSE) + # 全ての要素
  geom_text(data = trace_df,
            mapping = aes(x = index, y = 0, label = as.character(value), group = id),
            vjust = -0.5, size = 4) + # 要素ラベル
  geom_text(data = dup_label_df,
            mapping = aes(x = index, y = 0, label = dup_label, group = id),
            vjust = 1, size = 3) + # 重複ラベル
  geom_hline(data = upper_df, 
             mapping = aes(yintercept = value), 
             color = "red", linewidth = 1, linetype = "dotted") + # ヒープの最大値
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_color_manual(breaks = c("max", "heap", "sort"), 
                     values = c("red", "blue", "orange"), 
                     labels = c("swap", "heap", "sorted"), 
                     name = "bar") + # (凡例表示用)
  guides(color = guide_legend(override.aes = list(fill = c("red", "blue", "orange")))) + # 凡例の体裁
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "heap sort", 
       subtitle = "iteration: {next_state}", 
       x = "index", y = "value")

# フレーム数を設定
frame_num <- max(trace_df[["iteration"]]) + 1

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 900, 
  renderer = gganimate::gifski_renderer()
)


# 可視化：要素ごとに操作 ---------------------------------------------------------------------


