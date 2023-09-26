
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
  if(child_idx+1 <= N & vec[child_idx+1] > vec[child_idx]) { # (&の左がFALSEだと右が処理されずNAにならない)
    child_idx <- child_idx + 1
  }
  
  # 子が親以下なら終了
  if(vec[child_idx] <= vec[i]) return(vec)
  
  # 子と親を入替
  vec[1:N] <- replace(x = vec, list = c(child_idx, i), values = vec[c(i, child_idx)])
  
  # 子を部分木の根としてヒープ化
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


### ・作図 -----

# 要素数を指定
N <- 30

# (簡易的に)数列を作成
a <- 1:N

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

# 葉を除く頂点数を計算
max_i <- N %/% 2

# 数列を格納
trace_df <- tibble::tibble(
  step  = 0,   # 試行回数
  id    = 1:N, # 元のインデックス
  index = 1:N, # 各試行のインデックス
  value = a    # 要素
) |> 
  dplyr::mutate(
    target_flag = FALSE, # 入替対象の頂点
    heap_flag   = index > max_i # ヒープ化済みの部分木頂点
  )

# 作図用のオブジェクトを初期化
id_vec <- 1:N

# ヒープ化
iter <- 0
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
    ) |> 
      dplyr::mutate(
        target_flag = index %in% c(target_idx, target_idx*2, target_idx*2+1), # 入替対象の親子
        heap_flag   = index >= i & !target_flag # 入替対象を除く処理済み頂点
      )
    
    # 結果を記録
    trace_df <- dplyr::bind_rows(trace_df, tmp_df)
    
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
    depth   = floor(log2(index)), # 縦方向の頂点位置
    col_idx = index - 2^depth + 1, # 深さごとの頂点番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向の頂点位置
  ) |> 
  dplyr::ungroup()

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子の座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親の座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # 縦方向の頂点位置
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとの頂点番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向の頂点位置
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

# ヒープ化済み頂点の座標を作成
heap_df <- trace_vertex_df |> 
  dplyr::filter(heap_flag, step > 0) |> # ヒープ化済み頂点を抽出・初期値を除去:(表示フレームの調整用)
  dplyr::mutate(
    step = step - 1 # 表示フレームを調整
  ) |> 
  dplyr::bind_rows(
    trace_vertex_df |> 
      dplyr::filter(step == max(step))
  ) # 最終値を追加:(表示フレームの調整用)

# 入替対象の頂点の座標を作成
target_vertex_df <- trace_vertex_df |> 
  dplyr::filter(target_flag) |> # 入替対象の親を抽出
  dplyr::mutate(
    step = step - 1 # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を作成
target_edge_df <- dplyr::bind_rows(
  # 親の座標
  target_vertex_df |> 
    dplyr::group_by(step) |> 
    dplyr::filter(index == min(index)) |> # 親を抽出
    dplyr::ungroup() |> 
    tidyr::uncount(weights = 2, .id = "childe_id") |> # 子の数に複製
    dplyr::mutate(
      childe_id = childe_id - 1, # 行番号を子IDに変換
      edge_id   = index * 2 + childe_id # 子インデックス
    ) |> 
    dplyr::filter(edge_id <= N), # 子がなければ除去
  # 子の座標
  target_vertex_df |> 
    dplyr::group_by(step) |> 
    dplyr::filter(index != min(index)) |> # 子を抽出
    dplyr::ungroup() |> 
    dplyr::mutate(
      edge_id = index
    )
) |> 
  dplyr::arrange(step, index)

# ヒープ化のアニメーションを作図
max_h <- floor(log2(N))
graph <- ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = step),
             size = 14, color = "red", alpha = 0.5) + # 入替対象の頂点
  geom_point(data = heap_df,
             mapping = aes(x = coord_x, y = depth, group = step),
             size = 14, color = "blue", alpha = 0.5) + # ヒープ化済みの頂点
  geom_point(data = trace_vertex_df, 
             mapping = aes(x = coord_x, y = depth, group = id), 
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df, 
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id), 
            size = 5) + # 値ラベル
  geom_text(data = index_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset), 
            size = 4, vjust = -1.7, color = "green4") + # インデックスラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
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


# 要素数を指定
N <- 30

# (簡易的に)数列を作成
a <- 1:N

# 挿入値を指定
x <- 28.5

# ヒープ化
a <- heapify(a)

# 値を挿入してヒープ化
a <- push(a, x)
a; length(a)



### ・可視化 -----

# 要素数を指定
initial_N <- 40

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
    value = initial_a, 
    target_flag = FALSE
  ), 
  # 挿入後
  tibble::tibble(
    step  = 1, 
    id    = 1:N, 
    index = id, 
    value = a, 
    target_flag = index == N # 挿入対象
  )
)

# 挿入要素のインデックスを設定
i <- N

# 作図用のオブジェクトを初期化
id_vec <- 1:N

# ヒープ化
iter <- 1
break_flag <- FALSE
while(i > 1) {
  
  # 試行回数を更新
  iter <- iter + 1
  
  # 親のインデックスを計算
  parent_idx <- i %/% 2
  
  # 親のインデックスを計算
  parent_idx <- i %/% 2
  
  # 親が子未満なら親子を入替
  if(a[parent_idx] < a[i]) {
    a[1:N]      <- replace(x = a, list = c(i, parent_idx), values = a[c(parent_idx, i)])
    id_vec[1:N] <- replace(x = id_vec[1:n], list = c(i, parent_idx), values = id_vec[c(parent_idx, i)])
  } else {
    break_flag <- TRUE
  }
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    step  = iter, 
    id    = id_vec, 
    index = 1:N, 
    value = a, 
    target_flag = index %in% c(i, parent_idx) # 入替対象
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 親が子以上なら終了
  if(break_flag) break
  
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
    depth   = floor(log2(index)), # 縦方向の頂点位置
    col_idx = index - 2^depth + 1, # 深さごとの頂点番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向の頂点位置
  ) |> 
  dplyr::ungroup()

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子の座標
  trace_vertex_df |> 
    dplyr::filter(step == 1, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親の座標
  trace_vertex_df |> 
    dplyr::filter(step == 1, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # 縦方向の頂点位置
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとの頂点番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向の頂点位置
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

# 入替対象の頂点の座標を作成
target_vertex_df <- trace_vertex_df |> 
  dplyr::filter(target_flag) |> # 入替対象の親を抽出
  dplyr::mutate(
    step = step - 1 # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を作成
target_edge_df <- target_vertex_df |> 
  dplyr::bind_rows(
    target_vertex_df |> 
      dplyr::filter(step == 0)
  ) # (警告文回避用に挿入値を複製)

# ヒープ化のアニメーションを作図
max_h <- floor(log2(N))
graph <- ggplot() + 
  geom_path(data = edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_id),
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = step),
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
  geom_text(data = index_df,
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset),
            size = 4, vjust = -1.7, color = "green4") + # インデックスラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heapify : push (up-heap)", 
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


### ・ヒープ化の可視化 -----

# 要素数を指定
N <- 20

# 数列を作成
initial_a <- sample(x = 1:N, size = N, replace = FALSE)


# 1番目の要素を挿入
a <- initial_a[1]

# 数列を格納
trace_df <- tibble::tibble(
  step  = 0, # 試行番号
  id    = 1:N, # 元のインデックス
  index = c(1, rep(NA, times = N-1)), # 試行ごとのインデックス:(未挿入はNAとする)
  value = initial_a, # 要素:(未挿入要素も含める)
  target_flag = FALSE, # 挿入・入替対象:(初手FALSEの方が処理が楽)
  insert_flag = !is.na(index) # 挿入済み要素
)

# 作図用のオブジェクトを初期化
id_vec <- 1:N

# 値の挿入とヒープ化
iter <- 0
for(n in 2:N) {
  
  # 最後尾に要素を追加
  a <- c(a, initial_a[n])
  i <- n
  
  # 要素を追加した数列を格納
  tmp_df <- tibble::tibble(
    step  = iter + 0.5, # (挿入操作は0.5とする)
    id    = id_vec, 
    index = c(1:n, rep(NA, times = N-n)), 
    value = c(a, initial_a[(n+1):N])[1:N], 
    target_flag = index %in% i, # 挿入対象
    insert_flag = !is.na(index)
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # ヒープ化
  break_flag <- FALSE
  while(i > 1) {
    
    # 試行回数を更新
    iter <- iter + 1
    
    # 親のインデックスを計算
    parent_idx <- i %/% 2
    
    # 親が子未満なら親子を入替
    if(a[parent_idx] < a[i]) {
      a[1:n]      <- replace(x = a, list = c(i, parent_idx), values = a[c(parent_idx, i)])
      id_vec[1:n] <- replace(x = id_vec, list = c(i, parent_idx), values = id_vec[c(parent_idx, i)])
    } else {
      break_flag <- TRUE
    }
    
    # 要素を入れ替えた数列を格納
    tmp_df <- tibble::tibble(
      step  = iter, # (入替操作は1とする)
      id    = id_vec, 
      index = c(1:n, rep(NA, times = N-n)), 
      value = c(a, initial_a[(n+1):N])[1:N], 
      target_flag = index %in% c(i, parent_idx), # 入替対象
      insert_flag = !is.na(index)
    )
    
    # 数列を記録
    trace_df <- dplyr::bind_rows(trace_df, tmp_df)
    
    # 親が子以上なら終了
    if(break_flag) break
    
    # 挿入位置を親の位置に更新
    i <- parent_idx
    
    # 途中経過を表示
    print(paste0("--- step: ", iter, " ---"))
    print(a)
  }
}

# 未挿入要素のプロット位置を設定
max_y <- floor(log2(N)) + 1

# 頂点の座標を作成
trace_vertex_df <- dplyr::bind_rows(
  # 挿入済み要素の座標
  trace_df |> 
    dplyr::filter(insert_flag) |> # 挿入済み要素を抽出
    dplyr::group_by(step) |> # 座標計算用
    dplyr::mutate(
      depth   = floor(log2(index)), # 縦方向の頂点位置
      col_idx = index - 2^depth + 1, # 深さごとの頂点番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向の頂点位置
    ) |> 
    dplyr::ungroup(), 
  # 未挿入要素の座標
  trace_df |> 
    dplyr::filter(!insert_flag) |> # 未挿入要素を抽出
    dplyr::mutate(
      depth = max_y, # 縦方向の頂点位置
      coord_x = id / (N + 1) # 横方向の頂点位置
    )
) |> 
  dplyr::arrange(step, index)

# 辺の座標を作成
trace_edge_df <- dplyr::bind_rows(
  # 子の座標
  trace_vertex_df |> 
    dplyr::filter(insert_flag, step > 0, depth > 0) |> # 挿入済み要素を抽出、初期値・根を除去
    dplyr::mutate(
      edge_id    = index, 
      edge_label = paste0("step: ", step, ", edge: ", edge_id) # フレームごとに描画用
    ), 
  # 親の座標
  trace_vertex_df |> 
    dplyr::filter(insert_flag, step > 0, depth > 0) |> # 挿入済み要素を抽出、初期値・根を除去
    dplyr::mutate(
      edge_id    = index, # 子インデックス
      edge_label = paste0("step: ", step, ", edge: ", edge_id), # フレームごとに描画用
      depth   = depth - 1, # 縦方向の頂点位置
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとの頂点番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向の頂点位置
    ), 
  tibble::tibble(
    step = 0
  ) # (フレーム順の謎バグ回避用)
) |> 
  dplyr::arrange(step, edge_id, index)

# インデックスラベルの座標を作成
d <- 0.6
index_df <- trace_vertex_df |> 
  dplyr::filter(insert_flag, step == max(step)) |> # 挿入済み要素・1試行分のデータを抽出
  dplyr::mutate(
    offset = dplyr::if_else(
      condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
    ) # ラベル位置を左右にズラす
  ) |> 
  dplyr::select(!step) # フレーム遷移の影響から外す

# 試行番号を取得
step_vec <- unique(trace_df[["step"]])

# 入替対象の頂点の座標を作成
target_vertex_df <- trace_vertex_df |> 
  dplyr::filter(target_flag) |> # 挿入・入替対象を抽出
  dplyr::mutate(
    frame_id = dplyr::dense_rank(step), # 試行回数の抽出用
    step     = step_vec[frame_id] # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を作成
target_edge_df <- trace_vertex_df |> 
  dplyr::filter(target_flag) |> # 挿入・入替対象を抽出
  dplyr::mutate(
    frame_id = dplyr::dense_rank(step), # 試行回数の抽出用
  ) |> 
  dplyr::filter(step%%1 == 0) |> # 入替対象を抽出(挿入対象を除去)
  dplyr::group_by(step) |> # 辺IDの作成用
  dplyr::mutate(
    edge_label = paste0("step: ", step, ", id: ", min(index)) # 線分間の干渉回避用
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    step = step_vec[frame_id] # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)


# ヒープ化のアニメーションを作図
graph <- ggplot() + 
  geom_rect(mapping = aes(xmin = 0, xmax = 1, ymin = max_y-0.25, ymax = max_y+0.2), 
            fill = "orange", color = "orange", alpha = 0.1, linetype ="dashed") + # 数列枠
  geom_path(data = trace_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_label),
            linewidth = 1) + # 辺
  geom_path(data = target_edge_df,
            mapping = aes(x = coord_x, y = depth, group = edge_label),
            color = "red", linewidth = 1, linetype = "dashed") + # 入替対象の辺
  geom_point(data = target_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = step),
             size = 14, color = "red", alpha = 0.5) + # 挿入・入替対象の頂点
  geom_point(data = trace_vertex_df,
             mapping = aes(x = coord_x, y = depth, group = id),
             size = 12, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = trace_vertex_df,
            mapping = aes(x = coord_x, y = depth, label = as.character(value), group = id),
            size = 5) + # 値ラベル
  geom_text(data = index_df,
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset),
            size = 4, vjust = -1.7, color = "green4") + # インデックスラベル:(ツリー用)
  geom_text(data = index_df,
            mapping = aes(x = index/(N+1), y = max_y, label = index),
            size = 4, hjust = 1.1, vjust = -1.7, color = "green4") + # インデックスラベル:(数列用)
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max_y, labels = c(as.character(0:(max_y-1)), ""), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(max_y, 0)) + 
  labs(title = "heapify", 
       subtitle = "step: {next_state}", 
       x = "", y = "depth")

# フレーム数を取得
frame_num <- trace_df[["step"]] |> 
  unique() |> 
  length()

# 遷移フレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 800, 
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
    if(child_idx+1 <= N & vec[child_idx+1] > vec[child_idx]) { # (&の左がFALSEだと右が処理されずNAにならない)
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


# 要素数を指定
N <- 30

# (簡易的に)数列を作成
a <- 1:N

# ヒープ化
a <- heapify(a)

# 最大値を削除してヒープ
a <- pop(a)
a; length(a)


### ・可視化 -----

# 要素数を指定
initial_N <- 20

# ヒープを作成
initial_a <- sample(x = 1:initial_N, size = initial_N, replace = FALSE) |> 
  heapify()

# 最大値を削除して、最後尾を先頭に挿入
a <- c(initial_a[initial_N], initial_a[-c(1, initial_N)])

# 削除後の要素数を取得
N <- length(a)

# 数列を格納
trace_df <- dplyr::bind_rows(
  # 初期値
  tibble::tibble(
    step  = 0, 
    id    = 1:initial_N, 
    index = id, 
    value = initial_a, 
    target_flag = FALSE, 
    heap_flag   = TRUE
  ), 
  # 削除後
  tibble::tibble(
    step  = 1, 
    id    = c(initial_N, 2:N, 1), 
    index = 1:initial_N, 
    value = c(a, NA), 
    target_flag = index %in% c(1, initial_N), # 挿入・削除対象
    heap_flag   = c(rep(TRUE, times = N), FALSE) # ヒープ要素
  )
)

# 挿入要素のインデックスを設定
i <- 1

# 作図用のオブジェクトを初期化
id_vec <- c(initial_N, 2:N)

# ヒープ化
iter <- 1
break_flag <- FALSE
while(i*2 <= N) {
  
  # 試行回数を更新
  iter <- iter + 1
  
  # 値が大きい方の子のインデックスを設定
  child_idx <- i * 2 # 左側の子
  if(child_idx+1 <= N & a[child_idx+1] > a[child_idx]) {
    child_idx <- child_idx + 1
  }
  
  # 子が親より大きいなら入替
  if(a[child_idx] > a[i]) {
    a[1:N]      <- replace(x = a, list = c(i, child_idx), values = a[c(child_idx, i)])
    id_vec[1:N] <- replace(x = id_vec, list = c(i, child_idx), values = id_vec[c(child_idx, i)])
  } else {
    break_flag <- TRUE
  }
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    step  = iter, 
    id    = id_vec, 
    index = 1:N, 
    value = a, 
    target_flag = index %in% c(i, i*2, i*2+1), # 入替対象
    heap_flag   = TRUE
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 挿入位置を子の位置に更新
  i <- child_idx
  
  # 途中経過を表示
  print(paste0("--- step: ", iter, " ---"))
  print(a)
}


# 全ての頂点の座標を作成
tmp_vertex_df <- trace_df |> 
  dplyr::group_by(step) |> # 座標計算用
  dplyr::mutate(
    depth   = floor(log2(index)), # 縦方向の頂点位置
    col_idx = index - 2^depth + 1, # 深さごとの頂点番号
    coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), # 横方向の頂点位置
  ) |> 
  dplyr::ungroup()

# ヒープの頂点の座標を作成
trace_vertex_df <- tmp_vertex_df |> 
  dplyr::filter(heap_flag) # ヒープ要素を抽出(最大値を除去)

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子の座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index
    ), 
  # 親の座標
  trace_vertex_df |> 
    dplyr::filter(step == 0, depth > 0) |> # 1試行分のデータを抽出・根を除去
    dplyr::mutate(
      edge_id = index, # 子インデックス
      depth   = depth - 1, # 縦方向の頂点位置
      index   = index %/% 2, # 親インデックス
      col_idx = index - 2^depth + 1, # 深さごとの頂点番号
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2) # 横方向の頂点位置
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

# 入替対象の頂点の座標を作成
target_vertex_df <- tmp_vertex_df |> 
  dplyr::filter(target_flag) |> # 入替対象の親を抽出
  dplyr::mutate(
    step = step - 1 # 表示フレームを調整
  ) |> 
  dplyr::arrange(step, index)

# 入替対象の辺の座標を作成
target_edge_df <- dplyr::bind_rows(
  # 親の座標
  target_vertex_df |> 
    dplyr::group_by(step) |> 
    dplyr::filter(index == min(index)) |> # 親を抽出
    dplyr::ungroup() |> 
    dplyr::mutate(
      n = dplyr::if_else(
        condition = step == 0, true = 1, false = 2
      )
    ) |> 
    tidyr::uncount(weights = n, .id = "childe_id") |> # 子の数に複製
    dplyr::mutate(
      childe_id = childe_id - 1, # 行番号を子IDに変換
      edge_id   = dplyr::if_else(
        condition = step == 0, true = initial_N, false = index * 2 + childe_id # 子インデックス
      )
    ) |> 
    dplyr::filter(edge_id <= initial_N), # 子がなければ除去
  # 子の座標
  target_vertex_df |> 
    dplyr::group_by(step) |> 
    dplyr::filter(index != min(index)) |> # 子を抽出
    dplyr::ungroup() |> 
    dplyr::mutate(
      edge_id = index
    )
) |> 
  dplyr::arrange(step, index)


# ヒープ化のアニメーションを作図
max_h <- floor(log2(N))
graph <- ggplot() + 
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
  geom_text(data = index_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = offset), 
            size = 4, vjust = -1.7, color = "green4") + # インデックスラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_x_continuous(labels = NULL) + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "heapify : pop (down-heap)", 
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


