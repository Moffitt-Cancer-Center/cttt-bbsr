
library(trtswitch)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


sim1 <- tssim(
  tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
  p_X_1 = 0.3, p_X_0 = 0.3, 
  rate_T = 0.002, beta1 = -0.5, beta2 = 0.3, 
  gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
  zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4, 
  alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4, 
  theta1_1 = -0.4, theta1_0 = -0.4, theta2 = 0.2,
  rate_C = 0.0000855, accrualIntensity = 20/30, 
  fixedFollowup = 0, plannedTime = 1350, days = 30,
  n = 500, NSim = 100, seed = 314159)
df<-sim1[[1]]

id_11_raw <- subset(df, df$id==11)


#MSM
fit1 <- msm(
  sim1[[1]], id = "id", tstart = "tstart", 
  tstop = "tstop", event = "event", treat = "trtrand", 
  swtrt = "xo", swtrt_time = "xotime", 
  base_cov = "bprog", numerator = "bprog", 
  denominator = c("bprog", "L"), 
  ns_df = 0, swtrt_control_only = TRUE, boot = FALSE)


id_11_v1 <- subset(fit1$data_outcome, fit1$data_outcome$id==11)


# denominator switching model fit
fit1$fit_switch[[1]]$fit_den$parest[, c("param", "beta", "sebeta", "z")]

# numerator switching model fit
fit1$fit_switch[[1]]$fit_num$parest[, c("param", "beta", "sebeta", "z")]


# unstabilized weights
ggplot(fit1$data_outcome %>% filter(trtrand == 0), 
       aes(x = unstabilized_weight)) + 
  geom_histogram(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("unstabilized weights")

ggplot(fit1$data_outcome %>% filter(trtrand == 0), 
       aes(x = stabilized_weight)) + 
  geom_histogram(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("stabilized weights")


fit1$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]

exp(fit1$fit_outcome$parest[1, c("beta", "lower", "upper")])


#Ipcw

fit2 <- ipcw(
  sim1[[1]], id = "id", tstart = "tstart", 
  tstop = "tstop", event = "event", treat = "trtrand", 
  swtrt = "xo", swtrt_time = "xotime", 
  base_cov = "bprog", numerator = "bprog", 
  denominator = c("bprog", "L"),
  logistic_switching_model = TRUE, ns_df = 0,
  swtrt_control_only = TRUE, boot = FALSE)


id_11_v2 <- subset(fit2$data_outcome, fit2$data_outcome$id==11)

fit2$fit_switch[[1]]$fit_den$parest[, c("param", "beta", "sebeta", "z")]

fit2$fit_switch[[1]]$fit_num$parest[, c("param", "beta", "sebeta", "z")]


ggplot(fit2$data_outcome %>% filter(trtrand == 0), 
       aes(x = unstabilized_weight)) + 
  geom_histogram(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("unstabilized weights")


ggplot(fit2$data_outcome %>% filter(trtrand == 0), 
       aes(x = stabilized_weight)) + 
  geom_histogram(fill="#77bd89", color="#1f6e34", alpha=0.8) +
  scale_x_continuous("stabilized weights")


fit2$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]
exp(fit2$fit_outcome$parest[1, c("beta", "lower", "upper")])