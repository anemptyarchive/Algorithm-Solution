
# 挿入ソート -------------------------------------------------------------------

## chapter 12.3
## 実装と可視化


# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# ソートの実装 -----------------------------------------------------------------

### 実装 -----

# 挿入ソートの実装
insertion_sort <- function(vec) {
  
  # 数列の要素数を取得
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


### 確認 -----

# 要素数を指定
N <- 10

# 最大値を指定
max_val <- 20

# 乱数を生成
random_vals <- sample(x = 1:max_val, size = N, replace = TRUE)

# ソート
sorted_vals <- insertion_sort(random_vals)


# 結果を確認
sum(!(sort(random_vals) == sorted_vals))
random_vals; sorted_vals; table(random_vals)


# ソートアルゴリズムの可視化 ---------------------------------------------------

### 乱数の設定 -----

# 要素数を指定
N <- 10

# 数列を生成
a <- sample(x = 0:(2*N), size = N, replace = TRUE) # 一様乱数
a <- rnorm(n = N, mean = 0, sd = 1) |> # 正規乱数
  round(digits = 1)
table(a)


### 操作ごとの集計 -----

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
    step  = i, 
    id    = id_vec, 
    index = 1:N, 
    value = a
  )
  
  # 数列を記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste0("--- step: ", i, " ---"))
  print(a)
}


### 装飾用のデータの作成 -----

# 挿入データを作成
target_df <- trace_df |> 
    dplyr::filter((step + 1) == index) # i番目の要素を抽出

# 重複ラベルを作成
dup_label_df <- trace_df |> 
  dplyr::arrange(step, id) |> # IDの割当用
  dplyr::group_by(step, value) |> # IDの割当用
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
  dplyr::arrange(step, index)


### アニメーションの作成 -----

# 遷移フレーム数を指定
s <- 20

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = trace_df, 
    mapping = aes(x = index, y = value, fill = factor(value), group = factor(id)), 
    stat = "identity"
  ) + # 数列
  geom_bar(
    data    = target_df, 
    mapping = aes(x = index, y = value, group = factor(id)), 
    stat = "identity",
    color = "red", alpha = 0, linewidth = 1, linetype = "dashed"
  ) + # 挿入対象
  geom_segment(
    data = target_df, 
    mapping = aes(
      x = index, y = value, xend = -2, yend = value, 
      color = factor(value), group = factor(id)
    ), 
    linewidth = 1, linetype = "dashed", show.legend = FALSE
  ) + # 挿入対象の上限値
  geom_text(
    data    = trace_df, 
    mapping = aes(x = index, y = 0, label = as.character(value), group = factor(id)), 
    vjust = -0.5, size = 4
  ) + # 要素ラベル
  geom_text(
    data    = dup_label_df, 
    mapping = aes(x = index, y = 0, label = dup_label, group = factor(id)), 
    vjust = 1, size = 4
  ) + # 重複ラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes("cubic-in-out") + # 遷移の緩急
  coord_cartesian(xlim = c(0, N+1)) + # (上限値の線用)
  theme(legend.position = "none") + 
  labs(
    title = "insertion sort", 
    subtitle = "step: {next_state}", 
    fill = "value", 
    x = "index", y = "value"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1 + 2)*s, start_pause = s, end_pause = s, fps = 20,
  width = 1000, height = 750, units = "px", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/insertion_sort.mp4")
)


