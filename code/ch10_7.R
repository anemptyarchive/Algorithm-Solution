
# 10.7 ヒープ ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 数列のヒープ化 --------------------------------------------------------------------

### ・実装 -----

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
  
  # ヒープ化
  for(i in max_i:1) {
    
    # i番目の頂点を根としてヒープ化
    vec[1:N] <- sub_heapify(vec, i, N)
  }
  
  # 数列を出力
  return(vec)
}


### ・作図 -----

# 要素数を指定
N <- 30

# 数列を作成
a <- sample(x = 1:N, size = N, replace = FALSE)

# ヒープ化
a <- heapify(a)

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
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # 辺
  geom_point(data = vertex_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 10, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = value), 
            size = 5) + # 値ラベル
  geom_text(data = vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -1.5, color = "blue") + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heep", 
       subtitle = paste0("height = ", max_h), 
       y = "depth")


### ・可視化 -----

# 親子ノードの入替の実装
swap <- function(value, index, i, N) {
  
  # 数列の値・インデックスを取得
  val_vec <- value
  idx_vec <- index
  
  # 左側の子のインデックスを計算
  child_idx <- i * 2
  
  # 子がなければ終了
  if(child_idx > N) {
    lt <- list(value = val_vec, index = idx_vec, target = i)
    return(lt)
  }
  
  # 値が大きい方の子のインデックスを設定
  if(child_idx+1 <= N & val_vec[child_idx+1] > val_vec[child_idx]) {
    child_idx <- child_idx + 1
  }
  
  # 子が親以下なら終了
  if(val_vec[child_idx] <= val_vec[i]) {
    lt <- list(value = val_vec, index = idx_vec, target = i)
    return(lt)
  }
  
  # 子と親を入替
  val_vec[1:N] <- replace(x = val_vec, list = c(child_idx, i), values = val_vec[c(i, child_idx)])
  idx_vec[1:N] <- replace(x = idx_vec, list = c(child_idx, i), values = idx_vec[c(i, child_idx)])
  
  # 数列を出力
  if(child_idx <= N%/%2) {
    
    # 親子の入替があれば継続
    lt <- list(value = val_vec, index = idx_vec, target = child_idx)
    
  } else {
    
    # 子が葉なら終了
    lt <- list(value = val_vec, index = idx_vec, target = i)
  }
  return(lt)
}


# 要素数を指定
N <- 60

# 数列を作成
a <- sample(x = 1:N, size = N, replace = FALSE)
a

# 数列を格納
tmp_df <- tibble::tibble(
  step  = 0,   # 試行回数
  id    = 1:N, # 元のインデックス
  index = 1:N, # 各試行のインデックス
  value = a    # 要素
)

# 作図用のオブジェクトを初期化
id_vec   <- 1:N
trace_df <- tmp_df
trace_target_vec <- NULL

# ヒープ化
max_i <- N %/% 2
iter  <- 0
for(i in max_i:1) {
  
  # i番目の頂点を設定
  target_idx <- i
  
  # i番目の頂点を根とする部分木をヒープ化
  swap_flag <- TRUE
  while(swap_flag) {
    
    # 試行回数をカウント
    iter <- iter + 1
    
    # 親子の値を比較・入替
    res_lt <- swap(a, id_vec, target_idx, N)
    a[1:N]      <- res_lt[["value"]]
    id_vec[1:N] <- res_lt[["index"]]
    next_target_idx <- res_lt[["target"]]
    
    # 数列を格納
    tmp_df <- tibble::tibble(
      step  = iter, 
      id    = id_vec, 
      index = 1:N, 
      value = a
    )
    
    # 結果を記録
    trace_df <- dplyr::bind_rows(trace_df, tmp_df)
    trace_target_vec <- c(trace_target_vec, target_idx)
    
    # 入替の有無を確認
    if(target_idx == next_target_idx) {
      
      # 入替がなければ終了
      swap_flag <- FALSE
      
    } else {
      
      # 入替があれば入替後の頂点を親として設定
      target_idx <- next_target_idx
    }
    
    # 途中経過を表示
    print(paste0("--- step: ", iter, " ---"))
    print(a)
  }
}


# 頂点の座標を作成
trace_vertex_df <- trace_df |> 
  dplyr::group_by(step) |> # 座標計算用
  dplyr::mutate(
    depth   = floor(log2(index)), # ノードごとの深さ
    col_idx = index - 2^depth + 1, # 深さごとのノード番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向のノード位置
  ) |> 
  dplyr::ungroup()

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # ノードごとの深さ
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとのノード番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向のノード位置
    )
) |> 
  dplyr::select(!step) |> # フレーム遷移の影響から外す
  dplyr::arrange(edge_id, depth)

# インデックスラベルの座標を作成
d <- 0.6
index_df <- trace_vertex_df |> 
  dplyr::filter(step == 0) |> # 1試行分のデータを抽出
  dplyr::mutate(
    offset = dplyr::if_else(
      condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
    ) # ラベル位置を左右にズラす
  ) |> 
  dplyr::select(!step) # フレーム遷移の影響から外す

# ステップ数を取得
max_step <- length(trace_target_vec)

# 入替対象の頂点の座標を作成
target_vertex_df <- dplyr::bind_rows(
  # 親ノードの座標
  tibble::tibble(
    step    = 1:max_step, 
    index   = trace_target_vec, # 入替元のインデックス
    depth   = floor(log2(index)), 
    col_idx = index - 2^depth + 1, 
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
    parent_flag = TRUE, 
    childe_id   = NA
  ), 
  # 子ノードの座標
  tidyr::expand_grid(
    step      = 1:max_step, 
    childe_id = 0:1
  ) |> 
    dplyr::mutate(
      index   = trace_target_vec[step] * 2 + childe_id, # 入替元の子インデックス
      depth   = floor(log2(index)), 
      col_idx = index - 2^depth + 1, 
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
      parent_flag = FALSE
    ) |> 
    dplyr::filter(index <= N)
) |> 
  dplyr::mutate(
    step = step - 1 # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を計算
target_edge_df <- dplyr::bind_rows(
  # 親ノードの座標
  target_vertex_df |> 
    dplyr::filter(parent_flag) |> # 親ノードを抽出
    tidyr::uncount(weights = 2, .id = "childe_id") |> # 子ノード数に複製
    dplyr::mutate(
      childe_id = childe_id - 1, 
      edge_id   = index * 2 + childe_id # 子インデックス
    ) |> 
    dplyr::filter(edge_id <= N), # 子ノードがなければ除去
  # 子ノードの座標
  target_vertex_df |> 
    dplyr::filter(!parent_flag) |> # 子ノードを抽出
    dplyr::mutate(
      edge_id = index
    )
) |> 
  dplyr::arrange(step, index)


# ヒープ化のアニメーションを作図
graph <- ggplot() + 
  geom_text(data = index_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset), 
            size = 4, vjust = -1.7, color = "blue") + # インデックスラベル
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = step),
             size = 14, color = "red", alpha = 0.5) + # 入替対象の頂点
  geom_point(data = trace_vertex_df, 
             mapping = aes(x = coord_x, y = depth, group = id), 
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id), 
            size = 5) + # 値ラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max(trace_vertex_df[["depth"]], na.rm = TRUE), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heapify", 
       subtitle = "step: {next_state}", 
       x = "", y = "depth")

# フレーム数を取得
frame_num <- max(trace_df[["step"]]) + 1

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 600, 
  renderer = gganimate::gifski_renderer()
)


# 値ラベルの座標を作成
init_df <- trace_df |> 
  dplyr::filter(step == 0) # 初期値を抽出
last_df <- trace_df |> 
  dplyr::filter(step == max(step)) # 最終結果を抽出

# 入替推移を作図
ggplot() + 
  geom_line(data = trace_df, 
            mapping = aes(x = step, y = index, color = factor(value), group = id), 
            linewidth = 1) + 
  geom_text(data = init_df, 
            mapping = aes(x = step, y = index, label = value), 
            hjust = 1) + 
  geom_text(data = last_df, 
            mapping = aes(x = step, y = index, label = value), 
            hjust = 0) + 
  theme(legend.position = "none") + 
  labs(title = "heapify", 
       x = "step", y = "index")


# 値の挿入 --------------------------------------------------------------------

### ・実装 -----

# 値の挿入の実装
push <- function(vec, val) {
  
  # 挿入要素(子)のインデックスを設定
  i <- length(vec) + 1
  
  # ヒープ化
  while(i > 1) { # (根に達するまで)
    
    # 親のインデックスを計算
    parent_idx <- i %/% 2
    
    # 親が子以上なら終了
    if(vec[parent_idx] >= val) break
    
    # 子と親を入替
    vec[i] <- vec[parent_idx] # 親の値を子の位置に移動
    i      <- parent_idx      # 挿入位置を親の位置に更新
  }
  
  # 値を挿入
  vec[i] <- val
  
  # 数列を出力
  return(vec)
}


# 挿入値を指定
x <- 50

# 値を挿入してヒープ化
a <- push(a, x)
a; length(a)


### ・可視化 -----

# 要素数を指定
initial_N <- 50

# ヒープを作成
initial_a <- sample(x = 1:initial_N, size = initial_N, replace = FALSE) |> 
  heapify()

# 挿入する値を指定
x <- 49.5

# 値を挿入
a <- c(initial_a, x)

# 挿入後の要素数を取得
N <- length(a)

# 数列を格納
trace_df <- dplyr::bind_rows(
  # 初期値
  tibble::tibble(
    step  = 0, 
    id    = 1:initial_N, 
    index = id, 
    value = initial_a
  ), 
  # 挿入後
  tibble::tibble(
    step  = 1, 
    id    = 1:N, 
    index = id, 
    value = a
  )
)

# 挿入要素のインデックスを設定
i <- N

# 作図用のオブジェクトを初期化
id_vec <- 1:N
trace_target_vec <- i

# ヒープ化
iter <- 1
while(i > 1) {
  
  # 試行回数を更新
  iter <- iter+ 1
  
  # 親のインデックスを計算
  parent_idx <- i %/% 2
  
  # 親が子以上なら終了
  if(a[parent_idx] >= a[i]) {
    
    # 値を記録
    trace_target_vec <- c(trace_target_vec, parent_idx)
    
    break
  }
  
  # 子と親を入替
  a[1:N]      <- replace(x = a, list = c(i, parent_idx), values = a[c(parent_idx, i)])
  id_vec[1:N] <- replace(x = id_vec, list = c(i, parent_idx), values = id_vec[c(parent_idx, i)])
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    step  = iter, 
    id    = id_vec, 
    index = 1:N, 
    value = a
  )
  
  # 値を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  trace_target_vec <- c(trace_target_vec, parent_idx)
  
  # 挿入位置を親の位置に更新
  i <- parent_idx
  
  # 途中経過を表示
  print(paste0("--- step: ", iter, " ---"))
  print(a)
}


# 頂点の座標を作成
trace_vertex_df <- trace_df |> 
  dplyr::group_by(step) |> # 座標計算用
  dplyr::mutate(
    depth   = floor(log2(index)), # ノードごとの深さ
    col_idx = index - 2^depth + 1, # 深さごとのノード番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向のノード位置
  ) |> 
  dplyr::ungroup()

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 1, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 1, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # ノードごとの深さ
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとのノード番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向のノード位置
    )
) |> 
  dplyr::select(!step) |> # フレーム遷移の影響から外す
  dplyr::arrange(edge_id, depth)

# インデックスラベルの座標を作成
d <- 0.6
index_df <- trace_vertex_df |> 
  dplyr::filter(step == 1) |> # 1試行分のデータを抽出
  dplyr::mutate(
    offset = dplyr::if_else(
      condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
    ) # ラベル位置を左右にズラす
  ) |> 
  dplyr::select(!step) # フレーム遷移の影響から外す

# ステップ数を取得
max_step <- length(trace_target_vec)

# 入替対象の頂点の座標を作成
target_df <- dplyr::bind_rows(
  # 親ノードの座標
  tibble::tibble(
    step  = 1:max_step, 
    index = trace_target_vec, # 入替元のインデックス
    edge_id = index
  ), 
  # 子ノードの座標
  tibble::tibble(
    step  = 2:max_step, 
    index = trace_target_vec[-max_step],#c(NA, trace_target_vec[-max_step]), # 入替元の子インデックス
    edge_id = trace_target_vec[-1]
  )
) |> 
  dplyr::mutate(
    step = step - 1, # 表示フレームを調整
    depth   = floor(log2(index)), 
    col_idx = index - 2^depth + 1, 
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を作成
target_edge_df <- dplyr::bind_rows(
  target_df, 
  target_df |> 
    dplyr::filter(step == 0)
) # (原因不明の警告文対策)


# ヒープ化のアニメーションを作図
graph <- ggplot() + 
  geom_text(data = index_df,
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset),
            size = 4, vjust = -1.7, color = "blue") + # インデックスラベル
  geom_path(data = edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_df,
             mapping = aes(x = coord_x, y = depth, group = index),
             size = 14, color = "red", alpha = 0.5) + # 入替対象の頂点
  geom_point(data = trace_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = id),
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df,
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id),
            size = 5) + # 値ラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max(trace_vertex_df[["depth"]], na.rm = TRUE), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heapify : push", 
       subtitle = "step: {next_state}", 
       x = "", y = "depth")

# フレーム数を取得
frame_num <- max(trace_df[["step"]]) + 1

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 600, 
  renderer = gganimate::gifski_renderer()
)


# 最大値の削除 ------------------------------------------------------------------

### ・実装 -----

# 最大値の削除の実装
pop <- function(vec) {
  
  # 削除後の要素数を取得
  N <- length(vec) - 1
  
  # 最後尾(挿入要素)の値を取得
  val <- vec[N+1]
  
  # 最後尾の値を削除
  vec <- vec[-(N+1)]
  
  # 根(親)のインデックスを設定
  i <- 1
  
  # ヒープ化
  while(i*2 <= N) { # (葉に達するまで)
    
    # 左側の子のインデックスを計算
    child_idx <- i * 2
    
    # 値が大きい方の子のインデックスを設定
    if(child_idx+1 <= N & vec[child_idx+1] > vec[child_idx]) { # (&の左がFALSEだと右が処理されずNAにならないので動く)
      child_idx <- child_idx + 1
    }
    
    # 子が親以下なら終了
    if(vec[child_idx] <= val) break
    
    # 親と子を入替
    vec[i] <- vec[child_idx] # 子の値を親の位置に移動
    i      <- child_idx      # 挿入位置を子の位置に更新
  }
  
  # 値を挿入
  vec[i] <- val
  
  # 数列を出力
  return(vec)
}


# 最大値を削除してヒープ
a <- pop(a)
a; length(a)


### ・可視化 -----

# 要素数を指定
initial_N <- 50

# ヒープを作成
initial_a <- sample(x = 1:initial_N, size = initial_N, replace = FALSE) |> 
  heapify()

# 最大値を削除して、最後尾を先頭に挿入
a <- c(initial_a[initial_N], initial_a[-c(1, initial_N)])

# 削除後の要素数を取得
N <- length(a)

# 元のインデックスを作成
id_vec <- c(initial_N, 2:N)

# 数列を格納
trace_df <- dplyr::bind_rows(
  # 初期値
  tibble::tibble(
    step  = 0, 
    id    = 1:initial_N, 
    index = id, 
    value = initial_a
  ), 
  # 挿入後
  tibble::tibble(
    step  = 1, 
    id    = id_vec, 
    index = 1:N, 
    value = a
  )
)

# 挿入要素のインデックスを設定
i <- 1

# 作図用のオブジェクトを初期化
trace_target_vec <- i

# ヒープ化
iter <- 1
while(i*2 <= N) {
  
  # 試行回数を更新
  iter <- iter+ 1
  
  # 値が大きい方の子のインデックスを設定
  child_idx <- i * 2 # 左側の子
  if(child_idx+1 <= N & a[child_idx+1] > a[child_idx]) {
    child_idx <- child_idx + 1
  }
  
  # 子が親以下なら終了
  if(a[child_idx] <= a[i]) {
    
    # 値を記録
    trace_target_vec <- c(trace_target_vec, child_idx)
    
    break
  }
  
  # 親と子を入替
  a[1:N]      <- replace(x = a, list = c(i, child_idx), values = a[c(child_idx, i)])
  id_vec[1:N] <- replace(x = id_vec, list = c(i, child_idx), values = id_vec[c(child_idx, i)])
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    step  = iter, 
    id    = id_vec, 
    index = 1:N, 
    value = a
  )
  
  # 値を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  trace_target_vec <- c(trace_target_vec, child_idx)
  
  # 挿入位置を子の位置に更新
  i <- child_idx
  
  # 途中経過を表示
  print(paste0("--- step: ", iter, " ---"))
  print(a)
}


# 頂点の座標を作成
trace_vertex_df <- trace_df |> 
  dplyr::group_by(step) |> # 座標計算用
  dplyr::mutate(
    depth   = floor(log2(index)), # ノードごとの深さ
    col_idx = index - 2^depth + 1, # 深さごとのノード番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向のノード位置
  ) |> 
  dplyr::ungroup()

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親ノードの座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # ノードごとの深さ
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとのノード番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向のノード位置
    )
) |> 
  dplyr::select(!step) |> # フレーム遷移の影響から外す
  dplyr::arrange(edge_id, depth)

# インデックスラベルの座標を作成
d <- 0.6
index_df <- trace_vertex_df |> 
  dplyr::filter(step == 0) |> # 1試行分のデータを抽出
  dplyr::mutate(
    offset = dplyr::if_else(
      condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
    ) # ラベル位置を左右にズラす
  ) |> 
  dplyr::select(!step) # フレーム遷移の影響から外す

# ステップ数を取得
max_step <- length(trace_target_vec)

# 入替対象の頂点の座標を作成
target_vertex_df <- dplyr::bind_rows(
  # 削除と挿入
  tibble::tibble(
    step  = 1, 
    index = c(1, initial_N), 
    parent_flag = NA
  ), 
  # 親ノードの座標
  tibble::tibble(
    step  = 2:max_step, 
    index = trace_target_vec[-max_step], # 入替元のインデックス
    parent_flag = TRUE
  ), 
  # 子ノードの座標
  tidyr::expand_grid(
    step      = 2:max_step, 
    childe_id = 0:1
  ) |> 
    dplyr::mutate(
      index = trace_target_vec[step-1] * 2 + childe_id, # 入替元の子インデックス
      parent_flag = FALSE
    ) |> 
    dplyr::filter(index <= N)
) |> 
  dplyr::mutate(
    step = step - 1, # 表示フレームを調整
    depth   = floor(log2(index)), 
    col_idx = index - 2^depth + 1, 
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2)
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を計算
target_edge_df <- dplyr::bind_rows(
  # 削除と挿入
  target_vertex_df |> 
    dplyr::filter(is.na(parent_flag)) |> # 先頭と最後尾を抽出
    dplyr::mutate(
      edge_id = 1 # 根インデックス
    ), 
  # 親ノードの座標
  target_vertex_df |> 
    dplyr::filter(parent_flag) |> # 親ノードを抽出
    tidyr::uncount(weights = 2, .id = "childe_id") |> # 子ノード数に複製
    dplyr::mutate(
      childe_id = childe_id - 1, 
      edge_id   = index * 2 + childe_id # 子インデックス
    ) |> 
    dplyr::filter(edge_id <= N), # 子ノードがなければ除去
  # 子ノードの座標
  target_vertex_df |> 
    dplyr::filter(!parent_flag) |> # 子ノードを抽出
    dplyr::mutate(
      edge_id = index
    )
) |> 
  dplyr::arrange(step, index)


# ヒープ化のアニメーションを作図
graph <- ggplot() + 
  geom_text(data = index_df,
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset),
            size = 4, vjust = -1.7, color = "blue") + # インデックスラベル
  geom_path(data = edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = index),
             size = 14, color = "red", alpha = 0.5) + # 入替対象の頂点
  geom_point(data = trace_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = id),
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df,
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id),
            size = 5) + # 値ラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max(trace_vertex_df[["depth"]], na.rm = TRUE), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heapify : pop", 
       subtitle = "step: {next_state}", 
       x = "", y = "depth")

# フレーム数を取得
frame_num <- max(trace_df[["step"]]) + 1

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 600, 
  renderer = gganimate::gifski_renderer()
)


