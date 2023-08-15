
# ch12.5 クイックソート ----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# クイックソートの実装
quick_sort <- function(vec) {
  
  # 要素数を取得
  l <- 1
  r <- length(vec)
  
  # 要素数が1なら再帰的に処理を終了
  if(r == 1) return(vec)
  
  # ピボットを設定
  p_idx <- (l + r) %/% 2 # (中点で固定)
  p_val <- vec[p_idx]
  
  # ピボットを最後尾と入替
  vec[p_idx] <- vec[r]
  vec[r]     <- p_val
  
  # ピボット未満の値を探索:(j < r)
  i <- l
  for(j in l:(r-1)) { # (ピボットを除く)
    if(vec[j] < p_val) {
      
      # i番目とj番目の値と入替
      vec[l:r] <- replace(x = vec, list = c(i, j), values = vec[c(j, i)])
      i <- i + 1
    }
  }
  
  # ピボットを境界と入替
  vec[r] <- vec[i]
  vec[i] <- p_val
  
  # ピボット前後で分割してソート
  vec[l:(i-1)] <- quick_sort(vec[l:(i-1)])
  if(i < r) { # (境界が最後尾なら不要)
    vec[(i+1):r] <- quick_sort(vec[(i+1):r])
  }
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 100

# 数列を生成
a <- sample(x = 1:(2*N), size = N, replace = TRUE)
a <- rnorm(n = N, mean = 0, sd = 10) |> 
  round(digits = 1)
a

# ソート
quick_sort(a)


# 可視化 ---------------------------------------------------------------------

# ピボットの設定の実装
pivot_split <- function(value, index, left, right) {
  
  # 全体の要素数を取得
  val_vec <- value
  idx_vec <- index
  N       <- length(val_vec)
  
  # 分割範囲を取得
  l <- left
  r <- right
  
  # 分割範囲の要素数が1なら終了
  if(r - l < 1) {
    
    # リストに格納
    lt <- list(value = val_vec, index = idx_vec, new_index = l, pivot_index = l)
    
    # 結果を出力
    return(lt)
  }
  
  # ピボットを設定
  p_idx <- (l + r) %/% 2 # (中点で固定)
  p_val <- val_vec[p_idx]
  
  # ピボットを最後尾と入替
  val_vec[1:N] <- replace(x = val_vec, list = c(p_idx, r), values = c(val_vec[r], p_val))
  idx_vec[1:N] <- replace(x = idx_vec, list = c(p_idx, r), values = idx_vec[c(r, p_idx)])
  
  # ピボット未満の値を探索:(j < r)
  i <- l
  for(j in l:(r-1)) { # (ピボットを除く)
    if(val_vec[j] < p_val) {
      
      # j番目を境界と入替
      val_vec[1:N] <- replace(x = val_vec, list = c(i, j), values = val_vec[c(j, i)])
      idx_vec[1:N] <- replace(x = idx_vec, list = c(i, j), values = idx_vec[c(j, i)])
      
      # 境界位置をカウント
      i <- i + 1
    }
  }
  
  # ピボットを境界と入替
  val_vec[1:N] <- replace(x = val_vec, list = c(r, i), values = c(val_vec[i], p_val))
  idx_vec[1:N] <- replace(x = idx_vec, list = c(r, i), values = idx_vec[c(i, r)])
  
  # リストに格納
  lt <- list(value = val_vec, index = idx_vec, new_index = i, old_index = p_idx)
  
  # 結果を出力
  return(lt)
}


# 要素数を指定
N <- 20

# 数列を生成
a <- sample(x = 1:(N*2), size = N, replace = TRUE)
a; table(a)

# 数列の初期値を格納
tmp_df <- tibble::tibble(
  iteration = 0,   # 試行回数
  id        = 1:N, # 元のインデックス
  index     = 1:N, # 各試行のインデックス
  value     = a,   # 数列
  pivot_flag       = FALSE, # 各試行のピボット
  trace_pivot_flag = FALSE  # 各試行までのピボット
)

# 数列を作図
ggplot() + 
  geom_bar(data = tmp_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "numerical sequence", 
       subtitle = paste0("i = ", unique(tmp_df[["iteration"]])), 
       fill = "value", 
       x = "index", y = "value")


# 作図用のオブジェクトを初期化
id_vec          <- 1:N
trace_pivot_vec <- rep(NA, times = N)
trace_df        <- tmp_df

# 試行回数を初期化
iter <- 0

# クイックソート
loop_flag <- TRUE
while(loop_flag) {
  
  # 試行回数をカウント
  iter <- iter + 1
  
  # 前回までのピボット位置を取得
  range_idx_vec <- trace_pivot_vec |> 
    (\(vec) {vec[!is.na(vec)]})() |> # 欠損値を除去
    (\(vec) {c(0, vec, N+1)})() # 範囲計算用の値を追加
  
  # 分割数(前回までのピボット数+1)を取得
  max_cnt <- length(range_idx_vec) - 1
  
  # 前回のピボット値を初期化
  pivot_val_vec <- rep(NA, times = N)
  
  # 分割範囲ごとに処理
  for(pivot_cnt in 1:max_cnt) {
    
    # 分割範囲を取得
    l <- range_idx_vec[pivot_cnt] + 1
    r <- range_idx_vec[pivot_cnt+1] - 1
    
    # 範囲がない(ピボットが隣り合う)なら次の範囲に移行
    if(l > r) next
    
    # ピボットを設定
    res_lt <- pivot_split(value = a, index = id_vec, left = l, right = r)
    a[1:N] <- res_lt[["value"]]     # ピボット前後に整理した数列
    id_vec <- res_lt[["index"]]     # 元のインデックス
    i      <- res_lt[["new_index"]] # 移動後のピボット位置
    p_idx  <- res_lt[["old_index"]] # 移動前のピボット位置
    
    # ピボットを格納
    pivot_val_vec[i]   <- a[i]
    trace_pivot_vec[i] <- i
  }
  
  # 全要素にピボットが割り当てられたら終了
  if(all(!is.na(trace_pivot_vec))) {
    loop_flag <- FALSE
  }
  
  # 数列とピボットを格納
  tmp_df <- tibble::tibble(
    iteration = iter, 
    id        = id_vec, 
    index     = 1:N, 
    value     = a, 
    trace_pivot_flag = !is.na(trace_pivot_vec) # 今回までのピボット
  ) |> 
    dplyr::mutate(
      pivot_flag = !is.na(pivot_val_vec) # 今回のピボット
    )
  
  # 数列とピボットを記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- iteration:", iter, " ---"))
  print(a)
  print(trace_pivot_vec)
}


# ピボットの推移を作成
target_df <- trace_df |> 
  dplyr::filter(pivot_flag) # 各試行のピボットデータを抽出

# 全試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + # 全ての要素
  geom_vline(data = target_df, 
             mapping = aes(xintercept = index, color = factor(value)), 
             linewidth = 0.6, linetype = "dashed", show.legend = FALSE)  + # ピボット
  geom_hline(data = target_df, 
             mapping = aes(yintercept = value, color = factor(value)), 
             linewidth = 0.6, linetype = "dotted", show.legend = FALSE)  + # ピボットまでの上限値
  facet_wrap(iteration ~ ., scales = "free_x", labeller = label_both) + # 試行ごとに分割
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "quick sort", 
       fill = "value", 
       x = "index", y = "value")


# ピボットの推移を作成
target_pivot_df <- dplyr::bind_rows(
  trace_df |> 
    dplyr::arrange(iteration, index) |> # フレーム調整用
    dplyr::group_by(id) |> # フレーム調整用
    dplyr::mutate(
      pivot_flag = dplyr::lead(pivot_flag, n = 1)
    ) |> # (表示フレームの調整用に)ピボット割り当て試行を1つ前に変更
    dplyr::ungroup() |> 
    dplyr::filter(pivot_flag) |> # 各試行のピボットデータを抽出
    dplyr::mutate(
      type = "dashed"
    ), 
  trace_df |> 
    dplyr::filter(trace_pivot_flag) |> # 過去試行のピボットデータを抽出
    dplyr::mutate(
      type = "dotted"
    )
) |> 
  dplyr::arrange(iteration, index)

# 分割範囲の推移を作成
target_range_df <- trace_df |> 
  # 範囲計算用の値を追加
  tibble::add_row(
    tidyr::expand_grid(
      iteration = 0:max(trace_df[["iteration"]]), 
      index     = c(0, N+1)
    ) |> # 全ての組み合わせを作成
      dplyr::mutate(
        pivot_flag       = FALSE, 
        trace_pivot_flag = TRUE
      )
  ) |> 
  dplyr::filter(trace_pivot_flag) |> 
  dplyr::arrange(iteration, index) |> # 値の計算用
  dplyr::group_by(iteration) |> # 値の計算用
  # 作図用の値を計算
  dplyr::mutate(
    iteration = iteration, 
    left   = index + 1, 
    right  = dplyr::lead(index, n = 1) - 1, 
    x      = 0.5 * (left + right), 
    y      = 0.5 * max(a), 
    width  = right - left + 1, 
    height = max(a) + 1
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(width >= 1) |> 
  dplyr::select(iteration, left, right, x, y, width, height) |> 
  # ピボットデータを追加
  tibble::add_column(
    target_df |> 
      dplyr::select(id, index, value)
  )

# 重複ラベルを作成
dup_label_df <- trace_df |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    dup_id = dplyr::row_number(id), # 重複要素にIDを割り当て
    dup_num = max(dup_id), # 重複の判定用
    dup_label = dplyr::if_else(
      condition = dup_num > 1, 
      true = paste0("(", dup_id, ")"), 
      false = ""
    ) # 重複要素のみラベルを作成
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, index)

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = index, y = value, 
                         fill = factor(value), group = factor(id)), 
           stat = "identity") + # 全ての要素
  geom_vline(data = target_pivot_df, 
             mapping = aes(xintercept = index, 
                           color = factor(value), linetype = type, group = factor(id)), 
             linewidth = 0.6, show.legend = FALSE)  + # ピボット
  geom_tile(data = target_range_df,
            mapping = aes(x = x, y = y, width = width, height = height, 
                          color = factor(value), group = factor(id)),
            alpha = 0, linewidth = 0.6, linetype = "dashed", show.legend = FALSE) + # 入替範囲
  geom_text(data = trace_df, 
            mapping = aes(x = index, y = 0, label = as.character(value), group = factor(id)), 
            vjust = -0.5, size = 5) + # 要素ラベル
  geom_text(data = dup_label_df, 
            mapping = aes(x = index, y = 1.5, label = dup_label, group = factor(id)), 
            vjust = -0.5, size = 5) + # 重複ラベル
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  scale_linetype_identity() + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "quick sort", 
       subtitle = "iteration : {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# フレーム数を設定
frame_num <- max(trace_df[["iteration"]]) + 1

# 1試行当たりのフレーム数を指定
s <- 10

# 一時停止フレーム数を指定
p <- 10

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (frame_num + 2*p)*s, start_pause = p, end_pause = p, fps = 40, 
  width = 800, height = 600, 
  renderer = gganimate::gifski_renderer()
)


