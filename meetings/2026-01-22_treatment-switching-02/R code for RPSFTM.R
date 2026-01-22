install.packages("rpsftm")
library(rpsftm)
library(data.table)
head(immdef)
#fwrite(immdef,"F:/Projects/Biwei/Treatment_swiching_ASA/RPSFTM/immdef.csv")
# ITT analysis
## Cox and KM
KM_noadj <- survfit(Surv(progyrs, prog) ~ imm, data=immdef)
ggsurvplot(KM_noadj,data = immdef,pval = T)
fit_cox = coxph(Surv(progyrs, prog) ~ imm, data=immdef)
summary(fit_cox)

## AFT model
library(eha)
itt_fit <- aftreg(Surv(progyrs, prog) ~ imm, data = immdef)
summary(itt_fit)

# RPSFTM
rx <- with(immdef, 1 - xoyrs / progyrs)
rpsftm_fit_lr <- rpsftm(formula = Surv(progyrs, prog) ~ rand(imm, rx), data = immdef, censor_time = censyrs)
summary(rpsftm_fit_lr)
rpsftm_fit_lr$psi
rpsftm_fit_lr$CI

library(ggplot2)
plot(rpsftm_fit_lr)
