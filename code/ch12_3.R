
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
  
  # 要素数を取得
  N <- length(vec)
  
  # インデックス順に処理:(i = 1, ..., N)
  for(i in 2:N) { # (1番目は不要)
    
    # 挿入対象を取出
    val <- vec[i]
    
    # 対象位置iから逆順に挿入位置jを探索:(j = 1, ..., i)
    for(j in (i-1):0) { # (i番目は不要)
      
      # j番目とi番目を大小比較
      if(j == 0) { # i番目が最小の場合
        
        # ループを終了
        break
        
      } else if(vec[j] > val) { # j番目が大きい場合
        
        # j番目を1つ後に移動
        vec[j+1] <- vec[j]
        
      } else { # i番目が大きい場合
        
        # ループを終了
        break
      }
    }
    
    # 挿入位置jに対象を挿入
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

# インデックスを初期化
id_vec <- 1:N

# 初期値を格納
trace_df <- tibble::tibble(
  step  = 0,      # 入替回数
  id    = id_vec, # 元のインデックス
  index = id_vec, # 入替後のインデックス
  value = a       # 入替後の数列
)

# 挿入ソート
for(i in 1:N) {
  if(i > 1) { # (初回は不要)
    
    # 挿入対象を取出
    v <- a[i]
    
    # 対象位置iから逆順に挿入位置jを探索:(j = 1, ..., i)
    for(j in (i-1):0) { # (i番目は不要)
      
      # j番目とi番目を大小比較
      if(j <= 0) { # i番目が最小の場合
        
        # ループを終了
        break
        
      } else if(a[j] > v) { # j番目が大きい場合
        
        # j番目を1つ後に移動
        a[j+1]      <- a[j]
        id_vec[1:N] <- replace(x = id_vec, list = j+1, values = id_vec[j])
        
      } else { # i番目が大きい場合
        
        # ループを終了
        break
      }
    }
    
    # 挿入位置jに対象を挿入
    a[j+1]      <- v
    id_vec[1:N] <- replace(x = id_vec, list = j+1, values = i)
  }
  
  # 更新値を格納
  tmp_df <- tibble::tibble(
    step  = i, 
    id    = id_vec, 
    index = 1:N, 
    value = a
  )
  
  # 作図用データを記録
  trace_df <- dplyr::bind_rows(trace_df, tmp_df)
  
  # 途中経過を表示
  print(paste("-----", "step:", i, "-----"))
  print(a)
}


### 装飾用のデータの作成 -----

# グラフサイズを設定
axis_val_max <- a |> 
  abs() |> 
  max() |> 
  ceiling()
axis_val_min <- -axis_val_max

# 挿入データを作成
target_df <- trace_df |> 
  dplyr::filter((step+1) == index) # i番目の要素を抽出

# 重複ラベルを作成
dup_label_df <- trace_df |> 
  dplyr::arrange(step, id) |> # IDの割当用
  dplyr::mutate(
    dup_id    = dplyr::row_number(id), # 重複IDを割当
    dup_num   = max(dup_id), # 重複の判定用
    dup_label = (dup_num > 1) |> # 重複を判定
      dplyr::if_else(
        true  = paste0("(", dup_id, ")"), 
        false = ""
      ), # 重複ラベル
    .by = c(step, value)
  ) |> 
  dplyr::arrange(step, index)

# ソート済み範囲を作成
range_sorted_df <- tibble::tibble(
  step   = 0:N, 
  left   = 1 - 0.5, 
  right  = (1-1):N + 0.5, 
  bottom = axis_val_min, 
  top    = axis_val_max, 
  x      = 0.5 * (left + right), 
  y      = 0.5 * (bottom + top), 
  w      = right - left, 
  h      = top - bottom
)

# 未ソート範囲を作成
range_random_df <- tibble::tibble(
  step   = 0:N, 
  left   = 1:(N+1) - 0.5, 
  right  = N + 0.5, 
  bottom = axis_val_min, 
  top    = axis_val_max, 
  x      = 0.5 * (left + right), 
  y      = 0.5 * (bottom + top), 
  w      = right - left, 
  h      = top - bottom
)

### アニメーションの作成 -----

# 遷移フレーム数を指定
s <- 20

# グラフサイズを設定
axis_idx_min <- 0 # (対象値の線用)

# ソートのアニメーションを作図
graph <- ggplot() + 
  geom_tile(
    data    = range_random_df,
    mapping = aes(x = x, y = y, width = w, height = h, color = "random"), 
    fill = "orange", alpha = 0.1, linewidth = 1, linetype = "dashed"
  ) + # 未ソート範囲
  geom_tile(
    data    = range_sorted_df,
    mapping = aes(x = x, y = y, width = w, height = h, color = "sorted"), 
    fill = "green4", alpha = 0.1, linewidth = 1, linetype = "dashed"
  ) + # 既ソート範囲
  geom_bar(
    data    = trace_df, 
    mapping = aes(x = index, y = value, fill = factor(value), group = factor(id)), 
    stat = "identity", show.legend = FALSE
  ) + # 数列
  geom_bar(
    data    = target_df, 
    mapping = aes(x = index, y = value, group = factor(id)), 
    stat = "identity",
    color = "red", fill = NA, linewidth = 1, linetype = "dashed"
  ) + # 挿入対象
  geom_segment(
    data = target_df, 
    mapping = aes(x = index, y = value, xend = axis_idx_min, yend = value, group = factor(id)), # (-Infだと遷移しない?)
    color = "red", linewidth = 1, linetype = "dotted"
  ) + # 対象値
  geom_text(
    data    = trace_df, 
    mapping = aes(x = index, y = 0, label = value, group = factor(id)), 
    vjust = -0.5, size = 4
  ) + # 要素ラベル
  geom_text(
    data    = dup_label_df, 
    mapping = aes(x = index, y = 0, label = dup_label, group = factor(id)), 
    vjust = 1, size = 4
  ) + # 重複ラベル
  gganimate::transition_states(states = step, transition_length = 9, state_length = 1, wrap = FALSE) + # フレーム遷移
  gganimate::ease_aes(default = "cubic-in-out") + # 遷移の緩急
  scale_color_manual(
    breaks = c("sorted", "random"), 
    values = c("green4", "orange")
  ) + # 凡例の表示用
  coord_cartesian(xlim = c(1-0.5, N+0.5)) + # 対象値の線用
  labs(
    title = "insertion sort", 
    subtitle = "step: {next_state}", 
    color = "elements", fill = "value", 
    x = "index", y = "value"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = (N+1 + 2)*s, start_pause = s, end_pause = s, fps = 20,
  width = 1000, height = 750, units = "px", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/insertion_sort.mp4")
)


