
# 12.4 マージソート -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# マージソートの実装
merge_sort <- function(vec) {
  
  # 要素数を取得
  l <- 1
  r <- length(vec)
  
  # 要素数が1なら終了
  if(r == 1) return(vec)
  
  # 分割位置mを計算:(l ≤ m ≤ r)
  m <- l + (r - l) %/% 2
  
  # 分割してソート
  buf <- c(
    merge_sort(vec[l:m]),
    merge_sort(vec[(m+1):r]) |>
      rev()
  )
  
  # 要素ごとに処理
  idx_l <- 1
  idx_r <- 0
  for(i in 1:r) {
    
    # 小さい方を取り出して併合
    if(buf[idx_l] <= buf[r-idx_r]) {
      vec[i] <- buf[idx_l]
      idx_l <- idx_l + 1
    } else {
      vec[i] <- buf[r-idx_r]
      idx_r <- idx_r + 1
    }
    
  }
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 10

# 数列を生成
a <- sample(x = 1:N, size = N, replace = FALSE)
a

# ソート
merge_sort(a)


# 可視化 ---------------------------------------------------------------------

# 分割インデックスの実装
trace_idx <- function(mat = NA, iter = 0, left, right) {
  
  # 範囲を取得
  i <- iter
  l <- left
  r <- right
  
  # 試行回数をカウント
  if(i == 0) {
    i <- 1
    mat <- matrix(data = NA, nrow = 1, ncol = r)
  } else if(l == 1) {
    i <- i + 1
    mat <- rbind(
      mat, 
      matrix(data = NA, nrow = 1, ncol = ncol(mat))
    )
  } else {
    i <- i + 1
  }
  
  # 分割位置mを計算:(l ≤ m ≤ r)
  m <- l + (r - l) %/% 2
  
  # インデックスを格納
  mat[i, l:r] <- 1:(r-l+1)
  
  # 範囲が1なら終了
  if(l == r) return(mat)
  
  # インデックスを取得
  mat <- trace_idx(mat, iter = i, left = l, right = m)
  mat <- trace_idx(mat, iter = i, left = (m+1), right = r)
  
  # インデックスを出力
  return(mat)
}


# 数列を指定
a <- c(12, 9, 15, 3, 8, 17, 6, 1)

# 要素数を取得
N <- length(a)

# 数列を格納
tmp_df <- tibble::tibble(
  i   = 0,   # 試行回数
  idx = 1:N, # インデックス
  a   = a    # 要素
)

# 数列を作図
ggplot() + 
  geom_bar(data = tmp_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), 
           stat = "identity") + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       subtitle = paste0("i = ", unique(tmp_df[["i"]])), 
       fill = "value", 
       x = "index", y = "value")


# 分割インデックスを取得
trace_idx_mat <- trace_idx(mat = NA, iter = 0, left = 1, right = N)
trace_idx_mat[is.na(trace_idx_mat)] <- 1
trace_idx_mat

# 試行回数を設定
max_iter <- nrow(trace_idx_mat)

# 数列を記録
trace_df <- tmp_df

# マージソート
for(i in (max_iter-1):1) {
  
  # 分割インデックスを取得
  l_idx_vec <- which(trace_idx_mat[i, ] == 1)
  r_idx_vec <- c(l_idx_vec[-1]-1, N)
  m_idx_vec <- l_idx_vec + (r_idx_vec - l_idx_vec) %/% 2
  
  for(j in seq_along(l_idx_vec)) {
    
    # 範囲を取得
    l <- l_idx_vec[j]
    r <- r_idx_vec[j]
    m <- m_idx_vec[j]
    
    # 数列を分割
    if(l == r) {
      buf <- a[l]
    } else {
      buf <- c(a[l:m], rev(a[(m+1):r]))
    }
    
    # 要素ごとに処理
    idx_l <- 1
    idx_r <- 0
    max_r <- length(buf)
    for(j in l:r) {
      
      # 小さい方を取り出して併合
      if(buf[idx_l] <= buf[max_r-idx_r]) {
        a[j] <- buf[idx_l]
        idx_l <- idx_l + 1
      } else {
        a[j] <- buf[max_r-idx_r]
        idx_r <- idx_r + 1
      }
    }
  }
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    i   = max_iter - i, 
    idx = 1:N, 
    a   = a
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- i:", max_iter-i, " ---"))
  print(a)
}


# 入替範囲を作成
target_df <- tibble::tibble()
for(i in max_iter:1) { 
  
  # 分割インデックスを取得
  l_idx_vec <- which(trace_idx_mat[i, ] == 1)
  r_idx_vec <- c(l_idx_vec[-1]-1, N)
  
  # 座標を格納
  tmp_df <- tibble::tibble(
    i = max_iter - i, 
    x = cbind(l_idx_vec, r_idx_vec) |> 
      apply(MARGIN = 1, FUN = median), 
    y = 0.5 * max(a), 
    w = r_idx_vec - l_idx_vec + 1, 
    h = max(a)
  )
  
  # 座標を記録
  target_df <- dplyr::bind_rows(target_df, tmp_df)
}
target_df <- target_df |> 
  dplyr::mutate(
    g = dplyr::row_number() |> 
      factor()
  )

# 全ての試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), stat = "identity") + 
  geom_tile(data = target_df, 
            mapping = aes(x = x, y = y, width = w, height = h, group = g), 
            color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # 入替範囲
  facet_wrap(i ~ ., scales = "free_x", labeller = label_both) + # 試行ごとに分割
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "merge sort", 
       fill = "value", 
       x = "index", y = "value")


# 入替範囲を作成
target_df <- tibble::tibble()
for(i in (max_iter-1):1) { 
  
  # 分割インデックスを取得
  l_idx_vec <- which(trace_idx_mat[i, ] == 1)
  r_idx_vec <- c(l_idx_vec[-1]-1, N)
  
  # 座標を格納
  tmp_df <- tibble::tibble(
    i = max_iter - i - 1, 
    x = cbind(l_idx_vec, r_idx_vec) |> 
      apply(MARGIN = 1, FUN = median), 
    y = 0.5 * max(a), 
    w = r_idx_vec - l_idx_vec + 1, 
    h = max(a)
  )
  
  # 座標を記録
  target_df <- dplyr::bind_rows(target_df, tmp_df)
}
target_df <- target_df |> 
  dplyr::mutate(
    g = dplyr::row_number() |> 
      factor()
  )

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), stat = "identity") + # 全ての要素
  geom_tile(data = target_df, 
            mapping = aes(x = x, y = y, width = w, height = h, group = g), 
            color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # 入替範囲
  geom_text(data = trace_df, 
            mapping = aes(x = idx, y = 0, label = as.character(a), group = factor(a)), 
            vjust = -0.5, size = 5) + # 要素ラベル
  gganimate::transition_states(i, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "merge sort", 
       subtitle = "i = {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# 一時停止フレーム数を指定
p <- 10

# 1試行当たりのフレーム数を指定
s <- 5

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1 + 2*p)*s, start_pause = p, end_pause = p, fps = 50, 
  width = 1000, height = 800, 
  renderer = gganimate::gifski_renderer()
)


