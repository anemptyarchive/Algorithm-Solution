
# 12.3 挿入ソート --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 実装 ----------------------------------------------------------------------

# 挿入ソートの実装
insertion_sort <- function(vec) {
  
  # 要素数を取得
  N <- length(vec)
  
  # 要素ごとに処理
  for(i in 2:N) { # (初回は不要)
    
    # i番目の値を取得
    val <- vec[i]
    
    # 挿入位置jの探索:(j < i)
    for(j in i:1) { # (逆順に処理)
      if(j > 1) { # (1番目は不要)
        
        if(vec[j-1] > val) {
          # j番目の値を入替
          vec[j] <- vec[j-1]
        } else {
          # 挿入位置が定まれば終了
          break
        }
        
      }
    }
    
    # i番目の値をj番目に挿入
    vec[j] <- val
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
insertion_sort(a)


# 可視化 ---------------------------------------------------------------------

# 数列を指定
a <- c(4, 1, 3, 5, 2)

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


# 数列を記録
trace_df <- tmp_df

# 挿入ソート
for(i in 1:N) {
  
  # i番目の値を取得
  v <- a[i]
  
  # 挿入位置jの探索:(j < i)
  for(j in i:1) {
    if(j > 1) {
      
      if(a[j-1] > v) {
        # j番目の値を入替
        a[j] <- a[j-1]
      } else {
        # 挿入位置が定まれば終了
        break
      }
      
    }
  }
  
  # i番目の値をj番目に挿入
  a[j] <- v
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    i   = i, 
    idx = 1:N, 
    a   = a
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- i:", i, " ---"))
  print(a)
}


# 挿入データを抽出
target_df <- trace_df |> 
  dplyr::filter(i == idx) |> 
  dplyr::mutate(
    i = i - 1, 
    y = max(a)
  )

# 全ての試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), stat = "identity") + 
  geom_bar(data = target_df,
           mapping = aes(x = idx, y = y, group = factor(a)), stat = "identity",
           color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # i番目の要素
  facet_wrap(i ~ ., scales = "free_x", labeller = label_both) + # 試行ごとに分割
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       fill = "value", 
       x = "index", y = "value")


# 挿入データを抽出
target_df <- trace_df |> 
    dplyr::filter((i+1) == idx)

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), stat = "identity") + # 全ての要素
  geom_bar(data = target_df,
           mapping = aes(x = idx, y = a, group = factor(a)), stat = "identity",
           color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # i番目の要素
  geom_text(data = trace_df, 
            mapping = aes(x = idx, y = 0, label = as.character(a), group = factor(a)), 
            vjust = -0.5, size = 5) + # 要素ラベル
  gganimate::transition_states(i, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       subtitle = "i = {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# 一時停止フレーム数を指定
p <- 10

# 1試行当たりのフレーム数を指定
s <- 10

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1 + 2*p)*s, start_pause = p, end_pause = p, fps = 50, 
  width = 800, height = 600, 
  renderer = gganimate::gifski_renderer()
)


