
# 12.4 マージソート -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# マージソートの実装
merge_sort <- function(vec) {
  
  # 入替範囲の要素数を取得
  l <- 1
  r <- length(vec)
  
  # 要素数が1なら再帰処理を終了
  if(r == 1) return(vec)
  
  # 分割位置mを計算:(l ≤ m ≤ r)
  m <- l + (r - l) %/% 2
  
  # 分割してそれぞれソート
  buf <- c(
    merge_sort(vec[l:m]),
    merge_sort(vec[(m+1):r]) |>
      rev()
  )
  
  # 入替範囲内のインデックスを初期化
  idx_l <- 1
  idx_r <- 0
  
  # 要素ごとに処理
  for(i in 1:r) {
    
    # 小さい方の要素を取り出して併合
    if(buf[idx_l] <= buf[r-idx_r]) { # 左側が小さい場合
      
      # 左側の要素をj番目に格納
      vec[i] <- buf[idx_l]
      
      # インデックスを更新
      idx_l  <- idx_l + 1
      
    } else { # 右側が小さい場合
      
      # 右側の要素をj番目に格納
      vec[i] <- buf[r-idx_r]
      
      # インデックスを更新
      idx_r  <- idx_r + 1
    }
  }
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 50

# 数列を生成
a <- sample(x = 1:(2*N), size = N, replace = TRUE)
a <- rnorm(n = N, mean = 0, sd = 1) |> 
  round(digits = 1)
a; table(a)

# ソート
merge_sort(a)
sum(!(merge_sort(a) == sort(a)))


# 可視化 ---------------------------------------------------------------------

# インデックスの分割の実装
trace_idx <- function(mat = NA, iter = 0, left = 1, right = 1) {
  
  # 分割範囲を取得
  i <- iter
  l <- left
  r <- right
  
  # 試行回数をカウント
  if(i == 0) {
    
    # 初回ならマトリクスを初期化
    mat <- matrix(data = NA, nrow = 1, ncol = r)
    i <- 1
    
  } else if(l == 1) {
    
    # 左端の範囲なら行を追加
    mat <- rbind(
      mat, 
      matrix(data = NA, nrow = 1, ncol = ncol(mat))
    )
    i <- i + 1
    
  } else {
    
    i <- i + 1
  }
  
  # 分割位置mを計算:(l ≤ m ≤ r)
  m <- l + (r - l) %/% 2
  
  # 分割範囲のインデックスを格納
  mat[i, l:r] <- 1:(r-l+1)
  
  # 範囲が1なら再帰処理を終了
  if(l == r) return(mat)
  
  # インデックスを取得
  mat <- trace_idx(mat, iter = i, left = l, right = m)
  mat <- trace_idx(mat, iter = i, left = (m+1), right = r)
  
  # インデックスを出力
  return(mat)
}


# 数列を指定
#a <- c(12, 9, 15, 3, 8, 17, 6, 1)

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
  theme(panel.grid.minor.x = element_blank()) + # 図の体裁
  labs(title = "numerical sequence", 
       subtitle = paste0("iteration: ", unique(tmp_df[["iteration"]])), 
       fill = "value", 
       x = "index", y = "value")


# 分割インデックスを作成
trace_idx_mat <- trace_idx(mat = NA, iter = 0, left = 1, right = N)
trace_idx_mat[is.na(trace_idx_mat)] <- 1 # 欠損値を補完
trace_idx_mat

# 試行回数を取得:(初期値は除く)
max_iter <- nrow(trace_idx_mat) - 1

# 作図用のオブジェクトを初期化
id_vec   <- 1:N
trace_df <- tmp_df

# マージソート
for(i in 1:max_iter) {
  
  # 分割範囲のインデックスを作成
  l_idx_vec <- which(trace_idx_mat[max_iter-i+1, ] == 1)
  r_idx_vec <- c(l_idx_vec[-1]-1, N)
  m_idx_vec <- l_idx_vec + (r_idx_vec - l_idx_vec) %/% 2
  
  # 分割数を取得
  max_k <- length(l_idx_vec)
  
  # 分割範囲ごとに処理
  for(k in 1:max_k) {
    
    # 分割範囲を取得
    l <- l_idx_vec[k]
    r <- r_idx_vec[k]
    m <- m_idx_vec[k]
    
    # 前回の要素IDを保存
    old_id_vec <- id_vec
    
    # 数列を分割
    if(l == r) { # 要素数が1の場合
      buf <- a[l]
    } else {
      buf <- c(a[l:m], rev(a[(m+1):r]))
    }
    
    # 分割範囲内のインデックスを初期化
    idx_l <- 1
    idx_r <- 0
    max_r <- length(buf)
    
    # 要素ごとに処理
    for(j in l:r) {
      
      # 小さい方の要素を取り出して併合
      if(buf[idx_l] <= buf[max_r-idx_r]) {
        
        # 左側の要素をj番目に格納
        a[j] <- buf[idx_l]
        id_vec[1:N] <- replace(x = id_vec, list = j, values = old_id_vec[l+idx_l-1])
        
        # インデックスを更新
        idx_l <- idx_l + 1
        
      } else {
        
        # 右側の要素をj番目に格納
        a[j] <- buf[max_r-idx_r]
        id_vec[1:N] <- replace(x = id_vec, list = j, values = old_id_vec[m+idx_r+1])
        
        # インデックスを更新
        idx_r <- idx_r + 1
      }
    }
  }
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    iteration = i, 
    id        = id_vec, 
    index     = 1:N, 
    value     = a
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)

  # 途中経過を表示
  print(paste0("--- iteration: ", i, " ---"))
  print(a)
}


# 入替範囲を作成
range_df <- trace_idx_mat[(max_iter+1):1, ] |> # (逆順に並べ替え)
  tibble::as_tibble(.name_repair = NULL) |> 
  tibble::add_column(iteration = 0:max_iter) |> # 試行回数列を追加
  tidyr::pivot_longer(
    cols = !iteration, 
    names_to = "index", 
    names_prefix = "V", 
    names_transform = list(index = as.numeric), 
    values_to = "split_flag"
  ) |> # 分割インデックス列をまとめる
  dplyr::filter(split_flag == 1) |> # 分割位置を抽出
  dplyr::arrange(iteration, index) |> # 分割範囲の作成用
  dplyr::group_by(iteration) |> # 分割範囲の作成用
  dplyr::mutate(
    # 分割範囲を作成
    left  = index, 
    right = dplyr::lead(index, n = 1, default = N+1), 
    # タイル用
    x      = 0.5 * (right + left - 1), 
    y      = 0.5 * (max(a) + min(c(0, a))), 
    width  = right - left, 
    height = max(a) - min(c(0, a)) + 1
  ) |> 
  dplyr::ungroup()

# 全試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + # 全ての要素
  geom_tile(data = range_df,
            mapping = aes(x = x, y = y, width = width, height = height),
            color = "red", alpha = 0, linewidth = 0.6, linetype = "dashed") + # 入替範囲
  facet_wrap(iteration ~ ., scales = "free_x", labeller = label_both) + # 試行ごとに分割
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "merge sort", 
       fill = "value", 
       x = "index", y = "value")


# 入替範囲を作成
range_df <- trace_idx_mat[max_iter:1, ] |> # (最後を除き逆順に並べ替え)
  tibble::as_tibble(.name_repair = NULL) |> 
  tibble::add_column(iteration = 0:(max_iter-1)) |> # 試行回数列を追加
  tidyr::pivot_longer(
    cols = !iteration, 
    names_to = "index", 
    names_prefix = "V", 
    names_transform = list(index = as.numeric), 
    values_to = "split_flag"
  ) |> # 分割インデックス列をまとめる
  dplyr::filter(split_flag == 1) |> # 分割位置を抽出
  dplyr::arrange(iteration, index) |> # 分割範囲の作成用
  dplyr::group_by(iteration) |> # 分割範囲の作成用
  dplyr::mutate(
    # 分割範囲を作成
    left  = index, 
    right = dplyr::lead(index, n = 1, default = N+1), 
    # タイル用
    x      = 0.5 * (right + left - 1), 
    y      = 0.5 * (max(a) + min(c(0, a))), 
    width  = right - left, 
    height = max(a) - min(c(0, a)) + 1
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    id = dplyr::row_number() # 通し番号
  )

# 重複ラベルを作成
dup_label_df <- trace_df |> 
  dplyr::arrange(iteration, id) |> # IDの割当用
  dplyr::group_by(iteration, value) |> # IDの割当用
  dplyr::mutate(
    dup_id    = dplyr::row_number(id), # 重複IDを割り当て
    dup_num   = max(dup_id), # 重複の判定用
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
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + # 全ての要素
  geom_tile(data = range_df,
            mapping = aes(x = x, y = y, width = width, height = height, group = factor(id)),
            color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # 入替範囲
  geom_text(data = trace_df, 
            mapping = aes(x = index, y = 0, label = as.character(value), group = factor(id)), 
            vjust = -0.5, size = 5) + # 要素ラベル
  geom_text(data = dup_label_df, 
            mapping = aes(x = index, y = 0, label = dup_label, group = factor(id)), 
            vjust = 1, size = 4) + # 重複ラベル
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "merge sort", 
       subtitle = "iteration: {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# 1試行当たりのフレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (max_iter+1 + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 1200, height = 900, 
  renderer = gganimate::gifski_renderer()
)


