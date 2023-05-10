# ベイズ因子を計算する関数
bayes.factor <- function(h_top, h_bottom, hit, outside){
  ((h_top)^hit*(1-h_top)^outside)/((h_bottom)^hit*(1-h_bottom)^outside)
}

# 棄却サンプリング関数
sampling <- function(x,k,h_bottom=0.5, hit=24, outside=76){
  while(TRUE){
    z = runif(1, 0, 1)
    # 事前分布
    priors = ifelse(z >= 0.2 & z <= 0.3, 1/1000, 1)
    u = k*runif(1, 0, 1)
    if(bayes.factor(z, h_bottom, hit, outside)*priors > u){
      return(z)
    }
  }
}
# 1回の試行
sampling(1,k)
# パラメータ設定
n <- 1000000
x <- c(1:n)
dx <- 0.001
hypotheses <- seq(0,1,by=dx)
bfs.24 <- bayes.factor(hypotheses,0.5, 24, 76)
# k <- max(bfs)
# 事前分布
priors <- ifelse(hypotheses >= 0.2 & hypotheses <= 0.3, 1/1000, 1)
posteriors.24 <- priors * bfs.24
k.24 <- max(posteriors.24)
# 初期化
samples.24 <- c()
# 棄却サンプリングによる乱数抽出
samples.24 <- sapply(x, sampling, k=k.24, h_bottom=0.5, hit=24, outside=76)

# 確率密度分布
dens.24 <- density(samples.24)
probs=c(0.00001,0.025,0.05,0.5,0.975,0.99999)
quan.24 <- quantile(samples.24, probs = probs)
data.24 <- tibble(X=dens.24$x, Y=dens.24$y)
# 信頼区間描画用
data_B.24 <- tibble(X2=dens.24$x[which(dens.24$x>quan.24["2.5%"] & dens.24$x<quan.24["97.5%"])],
                    Y2=dens.24$y[which(dens.24$x>quan.24["2.5%"] & dens.24$x<quan.24["97.5%"])])
yend.24.025 <- min(dens.24$y[which(dens.24$x>quan.24["2.5%"] & dens.24$x<quan.24["50%"])])
yend.24.975 <- max(dens.24$y[which(dens.24$x>quan.24["97.5%"] & dens.24$x<quan.24["99.999%"])])

# 取得乱数
df.24 <- tibble(value = samples.24)
# ヒストグラム
g.24 <-
  ggplot() +
  geom_histogram(df.24, mapping = aes( x = value, y = after_stat(density)), 
                 position = "identity", bins = 100, alpha = 0.5 , color = "green", fill = "white") +
  geom_density(df.24, mapping = aes( x = value, y = after_stat(density)), alpha = 0.5 , color = "red", linewidth = 1.0) +
  geom_ribbon(data_B.24, mapping = aes(x=X2, ymin=0, ymax=Y2), alpha=0.3, fill="yellow") +
  geom_segment(mapping = aes(x=quan.24["2.5%"], y=0, xend=quan.24["2.5%"], yend=yend.24.025), linewidth = 1.0, color = "red") +
  geom_segment(mapping = aes(x=quan.24["97.5%"], y=0, xend=quan.24["97.5%"], yend=yend.24.975), linewidth = 1.0, color = "red") +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 12),
        plot.title    = element_text(color = "black", size = 12),
        plot.subtitle = element_text(color = "orange", size = 12)) +
  labs(y="density", x="当たり確率", 
       title = "アヒルゲームでの当たり確率",
       subtitle = "当たりが100回中24回場合（事前確率：0.2～0.3は1/1000）") +
  annotate("text", x=quan.24["2.5%"], y=-0.5, label=as.character(round(quan.24["2.5%"],digits=2)),
           adj="centor", color="darkgreen", size = 4) +
  annotate("text", x=quan.24["97.5%"], y=-0.5, label=as.character(round(quan.24["97.5%"],digits=2)),
           adj="centor", color="darkgreen", size = 4) +
  annotate("text", x=quan.24["50%"]*1.05, y=max(data_B.24$Y2)*0.2, label="95%信頼区間",
           adj="center", color="darkred", family = "Japan1GothicBBB", size = 4)
# グラフ保存
graph_path="./PDF/アヒルゲーム当たり24回ヒストグラム.pdf"  
ggsave(graph_path, plot = g.24, device = cairo_pdf, dpi=300, width=8, height=6)

#### 34回当たりの場合　------------------------------
# パラメータ設定
# n <- 1000000
# x <- c(1:n)
# dx <- 0.001
# hypotheses <- seq(0,1,by=dx)
bfs.34 <- bayes.factor(hypotheses,0.5, 34, 66)
# k <- max(bfs)
# 事前分布
priors <- ifelse(hypotheses >= 0.2 & hypotheses <= 0.3, 1/1000, 1)
posteriors.34 <- priors * bfs.34
k.34 <- max(posteriors.34)
# 初期化
samples.34 <- c()
# 棄却サンプリングによる乱数抽出
samples.34 <- sapply(x, sampling, k=k.34, h_bottom=0.5, hit=34, outside=66)

# 確率密度分布
dens.34 <- density(samples.34)
probs=c(0.00001,0.025,0.05,0.5,0.975,0.99999)
quan.34 <- quantile(samples.34, probs = probs)
data.34 <- tibble(X=dens.34$x, Y=dens.34$y)
# 信頼区間描画用
data_B.34 <- tibble(X2=dens.34$x[which(dens.34$x>quan.34["2.5%"] & dens.34$x<quan.34["97.5%"])],
                    Y2=dens.34$y[which(dens.34$x>quan.34["2.5%"] & dens.34$x<quan.34["97.5%"])])
yend.34.025 <- min(dens.34$y[which(dens.34$x>quan.34["2.5%"] & dens.34$x<quan.34["50%"])])
yend.34.975 <- max(dens.34$y[which(dens.34$x>quan.34["97.5%"] & dens.34$x<quan.34["99.999%"])])

# 取得乱数
df.34 <- tibble(value = samples.34)
# ヒストグラム
g.34 <-
  ggplot() +
  geom_histogram(df.34, mapping = aes( x = value, y = after_stat(density)), 
                 position = "identity", bins = 100, alpha = 0.5 , color = "green", fill = "white") +
  geom_density(df.34, mapping = aes( x = value, y = after_stat(density)), alpha = 0.5 , color = "red", linewidth = 1.0) +
  geom_ribbon(data_B.34, mapping = aes(x=X2, ymin=0, ymax=Y2), alpha=0.3, fill="yellow") +
  geom_segment(mapping = aes(x=quan.34["2.5%"], y=0, xend=quan.34["2.5%"], yend=yend.34.025), linewidth = 1.0, color = "red") +
  geom_segment(mapping = aes(x=quan.34["97.5%"], y=0, xend=quan.34["97.5%"], yend=yend.34.975), linewidth = 1.0, color = "red") +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 12),
        plot.title    = element_text(color = "black", size = 12),
        plot.subtitle = element_text(color = "orange", size = 12)) +
  labs(y="density", x="当たり確率", 
       title = "アヒルゲームでの当たり確率",
       subtitle = "当たりが100回中34回場合（事前確率：0.2～0.3は1/1000）") +
  annotate("text", x=quan.34["2.5%"], y=-0.5, label=as.character(round(quan.34["2.5%"],digits=2)),
           adj="centor", color="darkgreen", size = 4) +
  annotate("text", x=quan.34["97.5%"], y=-0.5, label=as.character(round(quan.34["97.5%"],digits=2)),
           adj="centor", color="darkgreen", size = 4) +
  annotate("text", x=quan.34["50%"]*1.05, y=max(data_B.34$Y2)*0.2, label="95%信頼区間",
           adj="center", color="darkred", family = "Japan1GothicBBB", size = 4)
# グラフ保存
graph_path="./PDF/アヒルゲーム当たり34回ヒストグラム.pdf"  
ggsave(graph_path, plot = g.34, device = cairo_pdf, dpi=300, width=8, height=6)

#### 比較　--------------------------------
# データ作成
max_sample <- 
  max(samples.34/samples.24) 
min_sample <- 
  min(samples.34/samples.24) 
df1 <- tibble(
  value = samples.34/samples.24
)
value = samples.34/samples.24
# 確率密度分布
dens <- density(value)
probs=c(0.00001,0.025,0.05,0.5,0.975,0.99999)
quan <- quantile(value, probs = probs)
# グラフ用にデータフレームを作成
data_A <- tibble(X=dens$x,
                 Y=dens$y)
# 信頼区間描画用
data_B <- tibble(X2=dens$x[which(dens$x>quan["5%"] & dens$x<quan["99.999%"])],
                 Y2=dens$y[which(dens$x>quan["5%"] & dens$x<quan["99.999%"])])
yend <- min(dens$y[which(dens$x>quan["5%"] & dens$x<1)])
# ヒストグラム
g_3424 <-
  ggplot() +
  geom_histogram(df1, mapping = aes( x = value, y = after_stat(density)), 
                 position = "identity", binwidth = 0.05, alpha = 0.5 , color = "green", fill = "white") +
  geom_density(df1, mapping = aes( x = value, y = after_stat(density)), alpha = 0.5 , color = "red", linewidth = 1.0) +
  geom_ribbon(data_B, mapping = aes(x=X2, ymin=0, ymax=Y2), alpha=0.3, fill="yellow") +
  geom_vline(xintercept = 1, linetype="dotted", linewidth = 1.0) +
  geom_segment(mapping = aes(x=quan["5%"], y=0, xend=quan["5%"], yend=yend), linewidth = 1.0, color = "red") +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 12),
        plot.title    = element_text(color = "black", size = 12),
        plot.subtitle = element_text(color = "orange", size = 12)) +
  labs(y="density", x="向上率", 
       title = "アヒルゲームでの当たり向上率",
       subtitle = "当たりが100回中24回から34回へ増えた場合") +
  # annotate("text", x=1, y=-0.2, label="100%", adj="center", color="darkgreen", size = 4) +
  annotate("text", x=min_sample, y=-0.1, 
           label=as.character(round(min_sample,digits = 2)), adj="center", color="darkgreen", size = 4) +
  annotate("text", x=max_sample, y=-0.1, 
           label=as.character(round(max_sample,digits = 2)), adj="center", color="darkgreen", size = 4) +
  annotate("text", x=quan["5%"], y=-0.1, label=as.character(round(quan["5%"],digits=2)),
           adj="centor", color="darkgreen", size = 4) +
  annotate("text", x=quan["50%"]*1.05, y=max(data_B$Y2)*0.2, label="95%信頼区間",
           adj="center", color="darkred", family = "Japan1GothicBBB", size = 4)
# annotate("text", x=quan["97.5%"], y=-max(data_B$Y2)/40, label=as.character(round(quan["97.5%"],digits=2)),
#          adj="center", color="darkgreen", size = 4)
# scale_x_continuous(breaks=seq(min_sample,max_sample,0.5),limits = c(min_sample,max_sample))

plot(g_3424)
# グラフ保存
graph_path="./PDF/アヒルゲーム当たり向上率ヒストグラム.pdf"  
ggsave(graph_path, plot = g_3424, device = cairo_pdf, dpi=300, width=8, height=6)

# 累積確率
ecdf.fun <- ecdf(samples.34/samples.24)
ecdf.fun(c(1.0))
ecdf.y <- sapply(seq(min_sample,max_sample,0.01),ecdf.fun)

df2 <- tibble(
  X = seq(min_sample,max_sample,0.01),
  Y = sapply(seq(min_sample,max_sample,0.01),ecdf.fun)*100
)
df3 <- tibble(
  X = seq(min_sample,1,0.01),
  Y = sapply(seq(min_sample,1,0.01),ecdf.fun)*100
)
# 累積確率グラフ
g_ecdf_3424 <- ggplot() +
  geom_path(df2, mapping = aes(x=X,y=Y), color="green", linewidth=1.0) +
  # geom_ribbon(df3, mapping = aes(x=X, ymin=0, ymax=Y), alpha=0.3, fill="lightgreen") +
  geom_vline(xintercept=1.0, linetype="dotted") +
  geom_hline(yintercept=ecdf.fun(c(1.0))*100, linetype="dotted") +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 12),
        plot.title    = element_text(color = "black", size = 12),
        plot.subtitle = element_text(color = "orange", size = 12)) +
  labs(y="累積確率(%)", x="向上率", 
       title = "アヒルゲームでの当たり向上率の累積確率",
       subtitle = "当たりが100回中24回から34回へ増えた場合の向上率の累積確率") +
  # scale_x_continuous(breaks=seq(0,max_sample,0.5),limits = c(0,max_sample)) +
  annotate("text", x=min_sample, y=(ecdf.fun(c(1.0))+0.03)*100, 
           label=paste0(as.character(round(ecdf.fun(c(1.0))*100,digits=2)),"%"),
           adj="left", color="darkgreen", size = 4) +
  annotate("text", x=1, y=-5, label="100%", adj="center", color="darkgreen", size = 4)
plot(g_ecdf_3424)
# グラフ保存
graph_path="./PDF/アヒルゲーム当たり向上率累積確率分布.pdf"  
ggsave(graph_path, plot = g_ecdf_3424, device = cairo_pdf, dpi=300, width=8, height=6)

g_3424 + g_ecdf_3424

# 合成グラフ保存
graph_path="./PDF/アヒルゲームのヒストグラム+累積確率分布.pdf"  
ggsave(graph_path, plot = g_3424 + g_ecdf_3424, device = cairo_pdf, dpi=300, width=12, height=6)
