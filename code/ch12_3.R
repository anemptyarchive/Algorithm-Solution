
# 12.3 挿入ソート --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 数列を指定
a <- c(4, 1, 3, 5, 2)

# 要素数を取得
N <- length(a)

# 数列を格納
tmp_df <- tibble::tibble(
  i = 0, 
  idx = 1:N, 
  a = a
)

# 数列を作図
ggplot() + 
  geom_bar(data = tmp_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), 
           stat = "identity") + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       subtitle = "i = 0", 
       fill = "value", 
       x = "index", y = "value")


a <- c(4, 1, 3, 5, 2)

# 数列を記録
trace_df <- dplyr::bind_rows(
  tmp_df, # 初期値
  tibble::tibble(
    i = 1, 
    idx = 1:N, 
    a = a
  ) # 初回
)

# 挿入ソート
for(i in 2:N) {
  
  # i番目の値を取得
  v <- a[i]
  
  # j番目の値を入れ替え:(j < i)
  for(j in i:1) {
    if(j > 1) {
      if(a[j-1] > v) {
        a[j] <- a[j-1]
      } else {
        break
      }
    }
  }
  
  # i番目の値を挿入
  a[j] <- v
  
  # 数列を格納
  tmp_df <- tibble::tibble(
    i = i, 
    idx = 1:N, 
    a = a
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- ", i, " ---"))
  print(a)
}


# 全ての試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), 
           stat = "identity") + 
  facet_wrap(i ~ ., scales = "free_x", labeller = label_both) + # グラフを分割
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       fill = "value", 
       x = "index", y = "value")


# i番目のデータを抽出
target_df <- trace_df |> 
    dplyr::filter((i+1) == idx)
target_df

# 数列のアニメーションを作図
graph <- ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = idx, y = a, fill = factor(a)), stat = "identity") + # 全ての要素
  geom_text(data = trace_df, 
            mapping = aes(x = idx, y = 0, label = as.character(a), group = factor(a)), 
            hjust = 0) + # 要素ラベル
  geom_bar(data = target_df,
           mapping = aes(x = idx, y = a, group = factor(a)), stat = "identity",
           color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # i番目の要素
  gganimate::transition_states(i, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # フレーム遷移の緩急
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       subtitle = "i = {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# 一時停止フレーム数を指定
p <- 10

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1+2*p)*5, start_pause = p, end_pause = p, fps = 50, 
  renderer = gganimate::gifski_renderer()
)


