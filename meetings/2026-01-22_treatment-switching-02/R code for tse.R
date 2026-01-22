library(trtswitch)
library(dplyr)

head(shilong)
# modify pd and dpd based on co and dco
shilong <- shilong %>%
  mutate(dpd = ifelse(co & !pd, dco, dpd),
         pd = ifelse(co & !pd, 1, pd)) %>%
  mutate(dpd = ifelse(pd & co & dco < dpd, dco, dpd))
head(shilong,10)

# the eventual survival time
shilong1 <- shilong %>%
  arrange(bras.f, id, tstop) %>%
  group_by(bras.f, id) %>%
  slice(n()) %>%
  select(-c("ps", "ttc", "tran"))


#bras.f The patient’s randomized arm, either MTA or CT
#ps The ECOGperformance status
#ttc The presence of concomitant treatments
#tran The use of platelet transfusions

# the last value of time-dependent covariates before pd
shilong2<- shilong %>%
  filter(pd == 0 | tstart <= dpd) %>%
  arrange(bras.f, id, tstop) %>%
  group_by(bras.f, id) %>%
  slice(n()) %>%
  select(bras.f, id, ps, ttc, tran)

# combine baseline and time-dependent covariates
shilong3 <- shilong1 %>%
  left_join(shilong2, by = c("bras.f", "id"))

subset(shilong, shilong$id %in% c(1,10))
subset(shilong3, shilong3$id %in% c(1,10))



fit1 <- tsesimp(
  data = shilong3, id = "id", time = "tstop", event = "event",
  treat = "bras.f", censor_time = "dcut", pd = "pd",
  pd_time = "dpd", swtrt = "co", swtrt_time = "dco",
  base_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
               "pathway.f"),
  base2_cov = c("agerand", "sex.f", "tt_Lnum", "rmh_alea.c",
                "pathway.f", "ps", "ttc", "tran"),
  aft_dist = "weibull", alpha = 0.05,
  recensor = TRUE, swtrt_control_only = FALSE, offset = 1,
  boot = FALSE)
# We can examine the Weibull AFT model fits and the corresponding value of ψ

df<-fit1$data_outcome
subset(shilong3, shilong3$id %in% c(1,10))
subset(fit1$data_outcome, fit1$data_outcome$id %in% c(1,10))


# control group
fit1$fit_aft[[1]]$fit$parest[, c("param", "beta", "sebeta", "z")]
fit1$psi
# experimental group
fit1$fit_aft[[2]]$fit$parest[, c("param", "beta", "sebeta", "z")]
fit1$psi_trt

#fit the outcome Cox model and compare the treatment hazard ratio estimate with the reported.
fit1$fit_outcome$parest[, c("param", "beta", "sebeta", "z")]


c(fit1$hr, fit1$hr_CI)


head(fit1$data_outcome,10)
