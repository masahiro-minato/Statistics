####エクセルファイルの読み込み####
blood.pressure <- 
  read_excel("./Excel/血圧ﾃﾞｰﾀ.xlsx",
             sheet = 1,
             skip = 1) # A tibble: 107,735 × 104
blood.pressure <- blood.pressure %>% 
                  select(-1,-3) %>% 
                  rename(date = 日付, L = Ｌ, pulse = 脈)

blood.pressure$date <- 
  blood.pressure$date %>% 
  as.POSIXct()

blood.pressure <- 
  blood.pressure %>% 
  mutate(
    pulse = if_else(pulse < 50, NA, pulse),
    H = replace(H, is.na(pulse), NA),
    L = replace(L, is.na(pulse), NA)
  )

blood.pressure.pivot <- blood.pressure %>% 
  pivot_longer(c(H,L,pulse))

min(blood.pressure.pivot$value, na.rm = TRUE)

# グラフ描画
g <- ggplot(blood.pressure.pivot, aes(x = date, y = value)) +
  geom_point(aes(colour=name)) +
  theme_bw() + 
  labs(title = "血圧時系列推移") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("value") +
  geom_hline(yintercept = 140, lwd = 1.1, color = "red") +
  geom_hline(yintercept = 130, lwd = 1.2, color = "yellow") +
  geom_hline(yintercept = 90, lwd = 1.1, color = "red") +
  geom_hline(yintercept = 85, lwd = 1.2, color = "yellow") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
        axis.text.y = element_text(size = 16)) +
  scale_y_continuous(breaks=seq(50,150,length=11),limits=c(50,150))+
  scale_x_datetime(date_breaks = "2 month", date_labels = "%Y/%m") # 表示期間の設定
plot(g)

ggsave("./PDF/血圧時系列推移.pdf",
       plot = g, device = cairo_pdf, dpi=300, width=10, height=5)

omit_na <- na.omit(blood.pressure$H)
blood.pressure.na <- blood.pressure %>% 
  mutate_all(~replace(., is.na(.), 0))

# データリストの作成
data_list <- list(
  T = nrow(blood.pressure),
  len_obs = length(na.omit(blood.pressure$H)),
  obs_no = which(!is.na(blood.pressure$H)),
  H = na.omit(blood.pressure$H),
  L = na.omit(blood.pressure$L)
  # P = na.omit(blood.pressure$pulse)/max(na.omit(blood.pressure$pulse)) # 正規化
)
# コンパイル
mod <- cmdstan_model("./stan/bloodpressure2.stan")
# マルチコア対応
options(mc.cores = parallel::detectCores())
# path
title <- str_c("血圧データ")
output_dir <- str_c("./csv")
output_basename <-str_c(title,".",format(Sys.time(), "%H-%M-%S"))
object.path <- str_c("./Cmdstan_files/",title,".rds")
# csv保存ディレクトリーの作成
if(!dir.exists(output_dir)){
  dir.create(output_dir)}
# MCMCサンプリング
exTime <- system.time(
  fit <- mod$sample(data = data_list,
                    seed = 1234,
                    chains = 6,
                    parallel_chains = getOption("mc.cores", 24),
                    # threads_per_chain = 2,
                    iter_warmup = 2000, #10000,
                    iter_sampling = 2000, #5000,
                    thin = 4,
                    # adapt_delta = 0.90,
                    max_treedepth = 12,
                    refresh = 1,
                    init = 0, # エラー回避
                    output_dir = output_dir, # csvデータ保存ディレクトリ
                    output_basename = output_basename, # csvデータ保存名称
                    show_messages = FALSE
  )
)


# 実行時間
exTimeTable <- data.frame(user.self = exTime["user.self"], 
                          sys.self = exTime["sys.self"],
                          elapsed = exTime["elapsed"],
                          title = title,
                          row.names = "time")
exTimeTable
# サンプリング結果表示
fit$print(c("s_w_H","s_v_H","mu_H[1]", "lp__"), max_rows=10)
# fit$print(c("b_H","b_L","s_w_H","s_v_H","s_w_L","s_v_L", "mu_H", "mu_L", "lp__"), max_rows=50)
# rhatヒストグラム
fit %>% bayesplot::rhat() %>% hist(main=str_c("Histogram of ",title))

hist_data <- as_tibble(fit %>% bayesplot::rhat())

g.hist <- 
  ggplot(hist_data, aes(x = value)) +
  geom_histogram(alpha = 0.8, fill = "lightgreen", bins = 20) +
  theme_bw() + 
  labs(title = "Histogram of rhat") +
  theme(plot.title = element_text(size = 18, hjust = 0.01),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16)) 

plot(g.hist)

# 表示パラメータの設定
parm1 <- "mu_H"
parm2 <- "mu_L"
# 表示パラメータの抽出
parm1.tib <- 
  as_tibble(fit$draws(parm1) %>% as_draws_df)
parm2.tib <- 
  as_tibble(fit$draws(parm2) %>% as_draws_df)
# 分位数の算出
parm1.quan <- 
  apply(parm1.tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975), na.rm=TRUE)})
parm2.quan <- 
  apply(parm2.tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975), na.rm=TRUE)})
parm1.quan <- 
  as_tibble(parm1.quan) %>% 
  # 不要列の削除
  select(-.chain, -.iteration, -.draw )
parm2.quan <- 
  as_tibble(parm2.quan) %>% 
  # 不要列の削除
  select(-.chain, -.iteration, -.draw )
# グラフ表示用データフレームの作成
result_df1 <- 
  as_tibble(t(parm1.quan),.name_repair = 'unique') %>% 
  # 列名設定
  magrittr::set_colnames(c("lwr1", "fit1", "upr1"))
result_df <- 
  as_tibble(t(parm2.quan),.name_repair = 'unique') %>% 
  # 列名設定
  magrittr::set_colnames(c("lwr2", "fit2", "upr2")) %>%
  cbind(result_df1) %>%
  cbind(blood.pressure)

# 図示
p <- ggplot(data = result_df, aes(x = date)) + 
  theme_bw() + 
  labs(title = "血圧データ 水準成分 μ") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("血圧値") +
  geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.8, fill="cyan") +
  geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.8, fill="green") +
  geom_point(aes(y = H), alpha = 0.8, size = 1.5, color = "lightblue") +
  geom_point(aes(y = L), alpha = 0.8, size = 1.5, color = "lightgreen") +
  # geom_point(aes(y = pulse), alpha = 0.8, size = 1.5, color = "lightpink", shape=17) +
  geom_line(aes(y = fit1), linewidth = 0.5, alpha = 0.8) +
  geom_line(aes(y = fit2), linewidth = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 140, lwd = 1.1, color = "red") +
  geom_hline(yintercept = 130, lwd = 1.2, color = "yellow") +
  geom_hline(yintercept = 90, lwd = 1.1, color = "red") +
  geom_hline(yintercept = 85, lwd = 1.2, color = "yellow") +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
        axis.text.y = element_text(size = 16)) +
  scale_y_continuous(breaks=seq(50,150,length=11),limits=c(50,150))+
  scale_x_datetime(date_breaks = "2 month", date_labels = "%Y/%m") # 表示期間の設定
plot(p)

ggsave("./PDF/血圧データ+脈拍 水準成分.pdf",
       plot = p, device = cairo_pdf, dpi=300, width=10, height=5)


