
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
  for(i in 2:N) { # (1番目は不要)
    
    # i番目の値を取得
    val <- vec[i]
    
    # i番目から逆順に挿入位置jを探索:(j ≤ i)
    for(j in (i-1):0) { # (i番目は不要・0番目は挿入処理との兼ね合い用)
      
      # j番目とi番目の値を大小比較
      if(j <= 0) {
        
        # 全要素を比較したらループを終了
        break
        
      } else if(vec[j] > val) {
        
        # j番目の値を1つ後に移動
        vec[j+1] <- vec[j]
        
      } else {
        
        # i番目の値以下ならループを終了
        break
      }
    }
    
    # i番目の値をj番目に挿入
    vec[j+1] <- val
  }
  
  # 数列を出力
  return(vec)
}


# 要素数を指定
N <- 25

# 数列を生成
a <- sample(x = 1:(2*N), size = N, replace = TRUE)
a <- rnorm(n = N, mean = 0, sd = 1) |> 
  round(digits = 1)
a; table(a)

# ソート
insertion_sort(a)
sum(!(insertion_sort(a) == sort(a)))


# 可視化 ---------------------------------------------------------------------

# 数列を指定
#a <- c(4, 1, 3, 5, 2)

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
  labs(title = "insertion sort", 
       subtitle = paste0("i = ", unique(tmp_df[["i"]])), 
       fill = "value", 
       x = "index", y = "value")


# 作図用のオブジェクトを初期化
id_vec   <- 1:N
trace_df <- tmp_df

# 挿入ソート
for(i in 1:N) {
  if(i > 1) { # (初回は不要)
    
    # i番目の値を取得
    v <- a[i]
    
    # i番目から逆順に挿入位置jを探索:(j ≤ i)
    for(j in (i-1):0) { # (i番目は不要・0番目は挿入処理との兼ね合い用)
      
      # j番目とi番目の値を大小比較
      if(j <= 0) {
        
        # 全要素を比較したらループを終了
        break
        
      } else if(a[j] > v) {
        
        # j番目の値を1つ後に移動
        a[j+1] <- a[j]
        id_vec[1:N] <- replace(x = id_vec, list = j+1, values = id_vec[j])
        
      } else {
        
        # i番目の値以下ならループを終了
        break
      }
    }
    
    # i番目の値をj番目に挿入
    a[j+1] <- v
    id_vec[1:N] <- replace(x = id_vec, list = j+1, values = i)
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
  print(paste0("--- iteration:", i, " ---"))
  print(a)
}


# 挿入データを作成
target_df <- trace_df |> 
  dplyr::filter(iteration == id) # i番目の要素を抽出

# 全試行の数列を作図
ggplot() + 
  geom_bar(data = trace_df, 
           mapping = aes(x = index, y = value, fill = factor(value)), stat = "identity") + # 全ての要素
  geom_bar(data = target_df,
           mapping = aes(x = id, y = value), stat = "identity",
           color = "red", alpha = 0, linewidth = 0.6, linetype = "dashed") + # 挿入前のi番目の要素
  geom_bar(data = insert_df,
           mapping = aes(x = index, y = value), stat = "identity",
           color = "red", alpha = 0, linewidth = 0.6, linetype = "dotted") + # 挿入後のi番目の要素
  geom_segment(data = target_df, 
               mapping = aes(x = id, y = value, xend = -Inf, yend = value, color = factor(value)), 
               linewidth = 0.6, linetype = "dashed", show.legend = FALSE) + # 挿入データまでの上限値
  facet_wrap(iteration ~ ., scales = "free_x", labeller = label_both) + # 試行ごとに分割
  theme(panel.grid.minor.x = element_blank()) + 
  labs(title = "insertion sort", 
       fill = "value", 
       x = "index", y = "value")


# 挿入データを作成
target_df <- trace_df |> 
    dplyr::filter((iteration + 1) == index) # i番目の要素を抽出

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
           mapping = aes(x = index, y = value, fill = factor(value), group = factor(id)), stat = "identity") + # 全要素
  geom_bar(data = target_df,
           mapping = aes(x = index, y = value, group = factor(id)), stat = "identity",
           color = "red", alpha = 0, linewidth = 1, linetype = "dashed") + # 挿入データ
  geom_segment(data = target_df,
               mapping = aes(x = index, y = value, xend = -2, yend = value,
                             color = factor(value), group = factor(id)),
               linewidth = 1, linetype = "dashed", show.legend = FALSE) + # 挿入データまでの上限値
  geom_text(data = trace_df, 
            mapping = aes(x = index, y = 0, label = as.character(value), group = factor(id)), 
            vjust = -0.5, size = 5) + # 要素ラベル
  geom_text(data = dup_label_df, 
            mapping = aes(x = index, y = 0, label = dup_label, group = factor(id)), 
            vjust = 1, size = 4) + # 重複ラベル
  gganimate::transition_states(states = iteration, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  coord_cartesian(xlim = c(0, N+1)) + # (上限値の線用)
  theme(panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  labs(title = "insertion sort", 
       subtitle = "iteration : {next_state}", 
       fill = "value", 
       x = "index", y = "value")

# 1試行当たりのフレーム数を指定
s <- 20

# gif画像を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1 + 2)*s, start_pause = s, end_pause = s, fps = 20, 
  width = 800, height = 600, 
  renderer = gganimate::gifski_renderer()
)


