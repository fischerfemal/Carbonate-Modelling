# Library
library(msm)
#### Graphing probability densitities
# Sites on same graphs
# Average chains

MCMC_upd <- ggs(bayes_fit_mcmc_upd)

ggs_crosscorrelation(MCMC_upd)
ggs_geweke(MCMC_upd)
gelman.diag(bayes_fit_mcmc_upd)
gelman.plot(bayes_fit_mcmc_upd)

ggmcmc(MCMC, file="model_gg_graphs.pdf", param_page=3)

mm <- as.matrix(bayes_fit_mcmc_upd, chains=T, iters=T)




#### Graphing parameters
pdf("160919_MCMC_Density.pdf")
### Site specific
## Bighorn Basin
# MAP
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAP_PETM_BB"], mm[, "MAP_Bkg_BB"], main = "MAP_BB", style="plain")
par(new=T)
MAP_Bkg_BB <- density(rnorm(100000, 1150, 200))
MAP_PETM_BB <- density(rnorm(100000, 1000, 301))
lines(MAP_Bkg_BB, col = "blue", lty = 2)
par(new=T)
lines(MAP_PETM_BB, col = "red", lty = 2)

# P_seas
denoverplot1(mm[, "P_seas_PETM_BB"], mm[, "P_seas_Bkg_BB"], main = "P_seas_BB", style="plain")
par(new=T)
P_seas <- density(rtnorm(100000, 0.15, 0.13, lower = 0, upper = 0.4))
lines(P_seas, lty = 2)

# ST
denoverplot1(mm[, "ST_PETM_BB"], mm[, "ST_Bkg_BB"], main = "ST_BB", style="plain")
par(new=T)
ST <- density(rtnorm(1000000, 0.5, 0.3, lower=0, upper=1))
lines(ST, lty=2)

# new plot
# MAT
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAT_Bkg_BB"], main = "MAT_BB", style="plain")
par(new=T)
MAT_Bkg_BB <- density(rnorm(100000, 15, 2))
lines(MAT_Bkg_BB, col = "blue", lty = 2)

# T_seas

denoverplot1(mm[, "T_seas_PETM_BB"], mm[, "T_seas_Bkg_BB"], main = "T_seas_BB", style="plain")
par(new=T)
T_seas_BB <- density(rnorm(100000, 15, 3.16))
lines(T_seas_BB, lty = 2)

# ch_ch_MAT

denoverplot1(mm[, "ch_ch_MAT_BB"], main = "ch_ch_MAT_BB", style="plain", col="black")
par(new=T)
ch_ch_MAT <- density(rtnorm(100000, mean=0, sd=0.6, lower=-2, upper=2))
lines(ch_ch_MAT, col = "black", lty = 2)

## AC

# MAP

layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAP_PETM_AC"], mm[, "MAP_Bkg_AC"], main = "MAP_AC", style="plain")
par(new=T)
MAP_AC <- density(rtnorm(100000, 600, 300, lower=100, upper=2000))
lines(MAP_Bkg_AC, col = "black", lty = 2)

# P_seas
denoverplot1(mm[, "P_seas_PETM_AC"], mm[, "P_seas_Bkg_AC"], main = "P_seas_AC", style="plain")
par(new=T)
lines(P_seas, lty = 2)

# ST
denoverplot1(mm[, "ST_PETM_AC"], mm[, "ST_Bkg_AC"], main = "ST_AC", style="plain")
par(new=T)
lines(ST, lty=2)

# new plot
# MAT
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAT_Bkg_AC"], main = "MAT_AC", style="plain", col="blue")
par(new=T)
MAT_Bkg_AC <- density(rnorm(100000, 16, 2))
lines(MAT_Bkg_AC, col = "blue", lty = 2)

# T_seas

denoverplot1(mm[, "T_seas_PETM_AC"], mm[, "T_seas_Bkg_AC"], main = "T_seas_AC", style="plain")
par(new=T)
T_seas_AC <- density(rnorm(100000, 15, 3.16))
lines(T_seas_AC, lty = 2)

# ch_ch_MAT

denoverplot1(mm[, "ch_ch_MAT_AC"], main = "ch_ch_MAT_AC", style="plain", col="black")
par(new=T)
lines(ch_ch_MAT, col = "black", lty = 2)

## ES

# MAP

layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAP_PETM_ES"], mm[, "MAP_Bkg_ES"], main = "MAP_ES", style="plain")
par(new=T)
MAP_ES <- density(rtnorm(100000, 1000, 400, lower=100, upper=2000))
lines(MAP_ES, col = "black", lty = 2)

# P_seas
denoverplot1(mm[, "P_seas_PETM_ES"], mm[, "P_seas_Bkg_ES"], main = "P_seas_ES", style="plain")
par(new=T)
lines(P_seas, lty = 2)

# ST
denoverplot1(mm[, "ST_PETM_ES"], mm[, "ST_Bkg_ES"], main = "ST_ES", style="plain")
par(new=T)
lines(ST, lty=2)

# new plot
# MAT
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAT_PETM_ES"], mm[, "MAT_Bkg_ES"], main = "MAT_ES", style="plain", xlim=c(10,40))
par(new=T)
MAT_Bkg_ES <- density(rnorm(100000, 20, 4))
lines(MAT_Bkg_ES, col = "blue", lty = 2)

# T_seas

denoverplot1(mm[, "T_seas_PETM_ES"], mm[, "T_seas_Bkg_ES"], main = "T_seas_ES", style="plain")
par(new=T)
T_seas_ES <- density(rnorm(100000, 10, 5))
lines(T_seas_ES, lty = 2)

# ch_ch_MAT

denoverplot1(mm[, "ch_ch_MAT_ES"], main = "ch_ch_MAT_ES", style="plain", col="black")
par(new=T)
lines(ch_ch_MAT, col = "black", lty = 2)


## CN

# MAP

layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAP_PETM_CN"], mm[, "MAP_Bkg_CN"], main = "MAP_CN", style="plain")
par(new=T)
MAP_CN <- density(rtnorm(100000, 1000, 400, lower=100, upper=2000))
lines(MAP_CN, col = "black", lty = 2)

# P_seas
denoverplot1(mm[, "P_seas_PETM_CN"], mm[, "P_seas_Bkg_CN"], main = "P_seas_CN", style="plain")
par(new=T)
lines(P_seas, lty = 2)

# ST
denoverplot1(mm[, "ST_PETM_CN"], mm[, "ST_Bkg_CN"], main = "ST_CN", style="plain")
par(new=T)
lines(ST, lty=2)

# new plot
# MAT
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
denoverplot1(mm[, "MAT_PETM_CN"], mm[, "MAT_Bkg_CN"], main = "MAT_CN", style="plain", xlim=c(10,40))
par(new=T)
MAT_Bkg_CN <- density(rnorm(100000, 23, 4))
lines(MAT_Bkg_CN, col = "blue", lty = 2)

# T_seas

denoverplot1(mm[, "T_seas_PETM_CN"], mm[, "T_seas_Bkg_CN"], main = "T_seas_CN", style="plain")
par(new=T)
T_seas_CN <- density(rnorm(100000, 10, 5))
lines(T_seas_CN, lty = 2)

# ch_ch_MAT

denoverplot1(mm[, "ch_ch_MAT_CN"], main = "ch_ch_MAT_CN", style="plain", col="black")
par(new=T)
lines(ch_ch_MAT, col = "black", lty = 2)


### Globals

layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
# pCO2
denoverplot1(mm[, "pCO2_Bkg"], main = "pCO2", style="plain")
par(new=T)
pCO2_Bkg <- density(rtnorm(100000, 600, 200, lower=200, upper=2000))
lines(pCO2_Bkg, col = "blue", lty = 2)

# pCO2_doubling
denoverplot1(mm[, "pCO2_Doubling"], main = "pCO2_Doubling", style="plain", col="black")
pCO2_Doubling <- density(rtnorm(100000, 1, 0.2, lower=0.5, upper=2))
lines(pCO2_Doubling, col = "black", lty = 2)


# deltaA_Bkg
denoverplot1(mm[, "deltaA_Bkg"], main = "deltaA_Bkg", style="plain", col="black")
ch_MAT <- density(rtnorm(100000,-5, 0.2, upper=-4, lower=-6))
lines(ch_MAT, col="black", lty=2)

par(mfrow=c(2,1))
# ch_MAT
denoverplot1(mm[, "ch_MAT"], main = "ch_MAT", style="plain", col="black")
ch_MAT <- density(rtnorm(100000, 7, 0.75, upper=9, lower=5))
lines(ch_MAT, col="black", lty=2)

# dO_MAT_sl_sp
denoverplot1(mm[, "dO_MAT_sl_sp"], main = "dO_MAT_sl", style="plain", col="black", xlim=c(0.2, 0.7))
dO_MAT_sl_sp <- density(rtnorm(100000, 0.5, 0.1, upper=0.7, lower=0.2))
lines(dO_MAT_sl_sp, col="black", lty=2)

# dO_MAT_sl_tm
denoverplot1(mm[, "dO_MAT_sl_tm"], main = "dO_MAT_sl", style="plain", col="black", xlim=c(0.2, 0.7))
dO_MAT_sl_tm <- density(rtnorm(100000, 0.3, 0.1, upper=0.5, lower=0.1))
lines(dO_MAT_sl_tm, col="black", lty=2)

dev.off()



