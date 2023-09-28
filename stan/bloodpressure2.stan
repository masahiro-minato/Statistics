data {
  int T;               // データ取得期間の長さ
  int len_obs;         // データ数
  array[len_obs] int obs_no; // 観測値が得られた時点
  // int obs_no[len_obs]; // 観測値が得られた時点
  array[len_obs] real H;     // 観測値 血圧H
  array[len_obs] real L;     // 観測値 血圧L
  // array[len_obs] real P;     // 観測値 脈拍P
}

parameters {
  vector[T] mu_H;       // 水準成分
  vector[T] mu_L;       // 水準成分
  // real b_H;             // 係数
  // real b_L;             // 係数
  real<lower=0> s_w_H;  // 過程誤差の標準偏差
  real<lower=0> s_v_H;  // 観測誤差の標準偏差
  real<lower=0> s_w_L;  // 過程誤差の標準偏差
  real<lower=0> s_v_L;  // 観測誤差の標準偏差
}
  
// transformed parameters {
//   vector[T] alpha_H;    // 状態推定値
//   vector[T] alpha_L;    // 状態推定値
//   for(i in 1:len_obs){
//     alpha_H[obs_no[i]] = mu_H[obs_no[i]] + b_H * P[i];
//     alpha_L[obs_no[i]] = mu_L[obs_no[i]] + b_L * P[i];
//   }
// 
// }

model {
  // 弱情報事前分布
  // s_w ~ student_t(3, 0, 10);
  
  // 状態方程式に従い状態が遷移する(スムースレベルモデル)
  for(i in 3:T) {
    // mu_H[i] ~ normal(mu_H[i-1], s_w_H);
    mu_H[i] ~ normal(2*mu_H[i-1] -mu_H[i-2], s_w_H);
    mu_L[i] ~ normal(2*mu_L[i-1] -mu_L[i-2], s_w_L);
  }

  // 観測方程式に従い観測値が得られる
  for(i in 1:len_obs){
    H[i] ~ normal(mu_H[obs_no[i]] ,s_v_H);
    L[i] ~ normal(mu_L[obs_no[i]] ,s_v_L);
  }
}
