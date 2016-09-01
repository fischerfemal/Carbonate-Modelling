library(R2OpenBUGS)
library(coda)
library(lattice)
library(rjags)
library(R2jags)
library(ggmcmc)
library(mcmcplots)
set.seed(12345)


# data for runs are the dO and dC carbonate values for PETM and Background at each site
# Use raw data themselves - c()? or uniform distribution of range runif()?
# Taking mean and st dev. and putting it into a normal distribution - picking 1000 values for each

dC_Carb_Bkg_BB <- c(-9.27,	-9.10,	-9.17,	-8.79,	-8.83,	-8.02,	-9.35,	-9.01,	-8.34,	-8.80,	-8.76,	-8.66,	-8.86,	-9.13,	-12.39,	-9.08,	-8.53,	-9.40,	-10.93,	-9.82,	-9.45,	-9.89,	-10.58,	-10.21)
dO_Carb_P_Bkg_BB <- c(-8.63,	-9.00,	-8.37,	-9.35,	-8.88,	-9.05,	-8.24,	-8.31,	-9.02,	-9.07,	-8.77,	-8.37,	-8.39,	-9.67,	-9.14,	-8.84,	-8.97,	-9.04,	-9.79,	-9.10,	-8.42,	-9.62,	-8.04,	-8.66)
dC_Carb_PETM_BB <- c(-14.16, -12.87,	-13.86,	-14.39,	-15.88,	-14.28,	-15.47,	-14.23,	-13.88,	-14.37,	-14.26,	-14.52,	-13.95,	-14.88,	-14.76,	-15.18,	-14.90,	-13.18,	-13.38,	-14.43,	-13.97,	-14.04,	-14.85,	-14.28,	-13.90,	-13.55,	-13.68,	-14.20,	-14.38,	-13.87,	-13.59,	-13.47,	-13.81,	-13.43,	-13.77,	-13.87,	-13.80,	-13.47,	-13.22,	-13.02,	-13.27)
dO_Carb_P_PETM_BB <- c(-8.54,	-8.52,	-8.94,	-7.84,	-8.49,	-8.25,	-9.18,	-8.72,	-7.63,	-8.00,	-8.19,	-8.86,	-8.32,	-8.98,	-8.51,	-8.52,	-8.06,	-8.79,	-8.69,	-8.73,	-8.72,	-7.87,	-9.23,	-8.61,	-8.46,	-8.01,	-8.02,	-7.93,	-8.18,	-8.77,	-8.78,	-8.69,	-8.70,	-8.95,	-8.84,	-8.11,	-8.02,	-9.18,	-8.12,	-8.98,	-9.73)

dC_Carb_Bkg_AC <- c(-5.44,-5.54,-6.48,-6.20,-6.78, -5.84,-6.30,-5.42,-5.89,-5.29, -5.81,-4.47,-5.65,-5.71, -5.38, -6.22, -4.53,-4.74,-4.92,-4.85,
                    -6.80, -6.44, -5.20,-7.05, -6.23,-5.86,-7.30,-7.41, -5.80,-6.36,-5.02,-5.45,-5.89,-5.94,
                    -5.89,-6.17,-7.47,-5.93,-6.18,-6.24, -5.75,-5.93, -6.77,-5.92,-5.44)
dO_Carb_P_Bkg_AC <- c(-9.78,-8.98,-8.97,-9.01,-8.29,-7.42,-10.10,-8.68,-9.85,-7.21,-7.35, -9.09,-8.36,-8.12, -7.39,
                      -8.36,-8.19,-8.17, -8.76,-9.46, -11.19,-8.31,-9.32,-8.15,-9.61, -7.67, -7.63,-7.78,-8.78,
                      -7.77,-10.17, -8.36,-7.58,-7.15,-7.48, -8.39,-9.49,-8.77,-9.47, -9.34,-9.55,-9.90,-10.65,-9.22,
                      -7.61)
dC_Carb_PETM_AC <- c(-10.38,-9.46,-9.22,-9.11,-9.07)
dO_Carb_P_PETM_AC <- c(-7.61, -6.03,-6.91,-8.91,-6.45)


dC_Carb_Bkg_ES <- c(-9.70,-9.62,	-9.23,	-8.77,	-8.22,	-7.78,	-7.69,	-7.54,	-7.54,	-7.50,	-7.50,	-7.47,	-7.42,	-7.11,	-6.97,	-6.86,	-6.85,	-6.69,	-6.55,	-6.44,	-6.27,	-6.06,	-5.98)
dO_Carb_P_Bkg_ES <- c(-6.30,-5.50,	-5.80,	-5.80,	-5.90,	-5.70,	-5.80,	-5.30,	-6.20,	-4.70,	-4.80,	-5.20,	-5.40,	-5.00,	-5.00,	-4.70,	-4.60,	-4.50,	-5.10,	-4.60,	-4.60,	-4.50,	-4.50)
dC_Carb_PETM_ES <- c(-14.00,-13.56,	-13.51,	-13.28,	-13.27,	-13.27,	-13.22,	-13.17,	-13.09,	-13.04,	-13.00,	-12.70,	-12.65,	-12.65,	-12.50,	-12.29,	-12.14,	-11.22,	-10.61)
dO_Carb_P_PETM_ES <- c(-6.50,-6.10,	-6.30,	-5.90,	-5.90,	-6.30,	-5.80,	-6.10,	-5.50,	-6.10,	-6.20,	-5.90,	-6.10,	-6.10,	-6.10,	-6.00,	-5.90,	-6.30,	-6.10)

dC_Carb_Bkg_CN <- c(-7.54,-7.19,	-7.10,	-6.87,	-7.24,	-7.15,	-7.13,	-7.00,	-8.04,	-7.98,	-7.60,	-7.59,	-8.08,	-7.81,	-7.69,	-7.61,	-7.44,	-7.20,	-7.20,	-7.47,	-7.08,	-7.00,	-6.91,	-8.05,	-7.47,	-7.44)
dO_Carb_P_Bkg_CN <- c(-6.64,-7.10,	-7.03,	-5.55,	-8.23,	-7.96,	-8.39,	-7.89,	-7.86,	-7.72,	-7.30,	-7.55,	-6.69,	-6.74,	-6.74,	-7.25,	-8.84,	-8.08,	-7.73,	-8.56,	-8.39,	-6.87,	-7.67,	-7.86,	-9.89,	-8.76)
dC_Carb_PETM_CN <- c(-13.87,-12.95,	-12.92,	-12.65,	-12.65,	-12.63,	-12.62,	-12.62,	-12.59,	-12.48,	-12.47,	-12.46,	-12.13,	-12.12,	-12.11,	-12.02,	-11.62,	-11.38,	-11.26,	-11.24,	-11.21,	-10.82,	-10.78,	-10.64,	-10.54,	-10.51)
dO_Carb_P_PETM_CN <- c(-6.56,-6.85,	-6.55,	-7.72,	-6.77,	-6.35,	-7.69,	-6.55,	-6.46,	-6.31,	-6.76,	-7.44,	-7.14,	-6.59,	-6.30,	-6.62,	-6.04,	-5.56,	-6.64,	-7.00,	-5.87,	-6.34,	-6.60,	-6.31,	-7.03,	-6.94)

BB_Bkg <- as.numeric(length(dC_Carb_Bkg_BB))
BB_PETM <- as.numeric(length(dC_Carb_PETM_BB))
AC_Bkg <- as.numeric(length(dC_Carb_Bkg_AC))
AC_PETM <- as.numeric(length(dC_Carb_PETM_AC))
ES_Bkg <- as.numeric(length(dC_Carb_Bkg_ES))
ES_PETM <- as.numeric(length(dC_Carb_PETM_ES))
CN_Bkg <- as.numeric(length(dC_Carb_Bkg_CN))
CN_PETM <- as.numeric(length(dC_Carb_PETM_CN))


carbonate_data <- list(BB_Bkg=BB_Bkg, BB_PETM=BB_PETM, AC_Bkg=AC_Bkg, AC_PETM=AC_PETM, ES_Bkg=ES_Bkg, ES_PETM=ES_PETM, CN_Bkg=CN_Bkg, CN_PETM=CN_PETM, dC_Carb_Bkg_BB=dC_Carb_Bkg_BB, dO_Carb_P_Bkg_BB=dO_Carb_P_Bkg_BB, dC_Carb_PETM_BB=dC_Carb_PETM_BB, dO_Carb_P_PETM_BB=dO_Carb_P_PETM_BB, 
                       dC_Carb_Bkg_AC=dC_Carb_Bkg_AC, dO_Carb_P_Bkg_AC=dO_Carb_P_Bkg_AC, dC_Carb_PETM_AC=dC_Carb_PETM_AC, dO_Carb_P_PETM_AC=dO_Carb_P_PETM_AC,
                       dC_Carb_Bkg_ES=dC_Carb_Bkg_ES, dO_Carb_P_Bkg_ES=dO_Carb_P_Bkg_ES, dC_Carb_PETM_ES=dC_Carb_PETM_ES, dO_Carb_P_PETM_ES=dO_Carb_P_PETM_ES,
                       dC_Carb_Bkg_CN=dC_Carb_Bkg_CN, dO_Carb_P_Bkg_CN=dO_Carb_P_Bkg_CN, dC_Carb_PETM_CN=dC_Carb_PETM_CN, dO_Carb_P_PETM_CN=dO_Carb_P_PETM_CN)


# Define parameters that we want posterior distributions of 

parameters <- c("pCO2_Bkg","deltaA_Bkg"
                ,"MAP_Bkg_BB"
                ,"MAP_Bkg_AC"
                ,"MAP_Bkg_ES"
                ,"MAP_Bkg_CN"
                ,"P_seas_Bkg_BB"
                ,"P_seas_Bkg_AC"
                ,"P_seas_Bkg_ES"
                ,"P_seas_Bkg_CN"
                ,"MAT_Bkg_BB"
                ,"MAT_Bkg_AC"
                ,"MAT_Bkg_ES"
                ,"MAT_Bkg_CN"
                ,"T_seas_Bkg_BB"
                ,"T_seas_Bkg_AC"
                ,"T_seas_Bkg_ES"
                ,"T_seas_Bkg_CN"
                ,"ST_Bkg_BB"
                ,"ST_Bkg_AC"
                ,"ST_Bkg_ES"
                ,"ST_Bkg_CN"
                ,"ch_MAT"
                ,"Sens"
                ,"pCO2_PETM"
                ,"MAP_PETM_BB"
                ,"MAP_PETM_AC"
                ,"MAP_PETM_ES"
                ,"MAP_PETM_CN"
                ,"P_seas_PETM_BB"
                ,"P_seas_PETM_AC"
                ,"P_seas_PETM_ES"
                ,"P_seas_PETM_CN"
                ,"MAT_PETM_BB"
                ,"MAT_PETM_AC"
                ,"MAT_PETM_ES"
                ,"MAT_PETM_CN"
                ,"T_seas_PETM_BB"
                ,"T_seas_PETM_AC"
                ,"T_seas_PETM_ES"
                ,"T_seas_PETM_CN"
                ,"ST_PETM_BB"
                ,"ST_PETM_AC"
                ,"ST_PETM_ES"
                ,"ST_PETM_CN")

# Create starting values for each parameter for each chain
inits <- function () { 
  list("pCO2_Bkg" = runif(1, 300, 1000) ,"deltaA_Bkg" = runif(1, -5.5, -4.5)
       ,"MAP_Bkg_BB"= runif(1, 200, 3000)
       ,"MAP_Bkg_AC"= runif(1, 200, 3000)
       ,"MAP_Bkg_ES"= runif(1, 200, 3000)
       ,"MAP_Bkg_CN"= runif(1, 200, 3000)
       ,"P_seas_Bkg_BB"= runif(1, 0.001, 0.4)
       ,"P_seas_Bkg_AC"= runif(1, 0.001, 0.4)
       ,"P_seas_Bkg_ES"= runif(1, 0.001, 0.4)
       ,"P_seas_Bkg_CN"= runif(1, 0.001, 0.4)
       ,"MAT_Bkg_BB"= runif(1, 5, 40)
       ,"MAT_Bkg_AC"= runif(1, 5, 40)
       ,"MAT_Bkg_ES"= runif(1, 5, 40)
       ,"MAT_Bkg_CN"= runif(1, 5, 40)
       ,"T_seas_Bkg_BB"= runif(1, 5, 40)
       ,"T_seas_Bkg_AC"= runif(1, 5, 40)
       ,"T_seas_Bkg_ES"= runif(1, 5, 40)
       ,"T_seas_Bkg_CN"= runif(1, 5, 40)
       ,"ST_Bkg_BB"= runif(1,0,1)
       ,"ST_Bkg_AC"= runif(1,0,1)
       ,"ST_Bkg_ES"= runif(1,0,1)
       ,"ST_Bkg_CN"= runif(1,0,1)
       ,"ch_MAT"= runif(1, 5, 8)
       ,"Sens"= runif(1, 1 , 4)
       ,"MAP_PETM_BB"= runif(1, 200, 3000)
       ,"MAP_PETM_AC"= runif(1, 200, 3000)
       ,"MAP_PETM_ES"= runif(1, 200, 3000)
       ,"MAP_PETM_CN"= runif(1, 200, 3000)
       ,"P_seas_PETM_BB"= runif(1, 0.001, 0.4)
       ,"P_seas_PETM_AC"= runif(1, 0.001, 0.4)
       ,"P_seas_PETM_ES"= runif(1, 0.001, 0.4)
       ,"P_seas_PETM_CN"= runif(1, 0.001, 0.4)
       ,"T_seas_PETM_BB"= runif(1, 5, 40)
       ,"T_seas_PETM_AC"= runif(1, 5, 40)
       ,"T_seas_PETM_ES"= runif(1, 5, 40)
       ,"T_seas_PETM_CN"= runif(1, 5, 40)
       ,"ST_PETM_BB"= runif(1,0,1)
       ,"ST_PETM_AC"= runif(1,0,1)
       ,"ST_PETM_ES"= runif(1,0,1)
       ,"ST_PETM_CN"= runif(1,0,1)) }

bayes_model <- function() {
  
  for (i in 1:BB_Bkg) {
    
    EPSmax_Bkg_BB[i] <- 0.10 + ST_Bkg_BB * 0.40
    p_Bkg_BB[i] <- 0.4 + ST_Bkg_BB * 0.4
    CQP_Bkg_BB[i] <- MAP_Bkg_BB * P_seas_Bkg_BB
    CMP_cm_Bkg_BB[i] <- CQP_Bkg_BB[i] / 30
    CMP_mm_Bkg_BB[i] <- CQP_Bkg_BB[i] / 3
    CQT_Bkg_BB[i] <- MAT_Bkg_BB + T_seas_Bkg_BB
    CQT_K_Bkg_BB[i] <- CQT_Bkg_BB[i] + 273
    dO_P_Bkg_BB[i] <- -13.6 + 0.55 * MAT_Bkg_BB
    h_Bkg_BB[i] <- ifelse (0.1 + 0.9 * (CMP_mm_Bkg_BB[i] / 200) < 1, 0.1 + 0.9 * (CMP_mm_Bkg_BB[i] / 200), 1)
    A_atmP_Bkg_BB[i] <- 2.71828^((5.9702e6/CQT_K_Bkg_BB[i] ^ 2 - 3.2801e4/CQT_K_Bkg_BB[i] + 52.227)/1000)
    R_O_P_Bkg_BB[i] <- (dO_P_Bkg_BB[i]/1000 + 1)*0.0020052
    R_O_atm_Bkg_BB[i] <- R_O_P_Bkg_BB[i]/A_atmP_Bkg_BB[i]
    dO_atm_Bkg_BB[i] <- (R_O_atm_Bkg_BB[i]/0.0020052 - 1)*1000
    z_Bkg_BB[i] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_Bkg_BB))))/0.026
    z_m_Bkg_BB[i] <- z_Bkg_BB[i] * 0.01
    R_month_Bkg_BB[i] <- 1.25*exp(0.05452*CQT_Bkg_BB[i])*CMP_cm_Bkg_BB[i]/(4.259+CMP_cm_Bkg_BB[i])
    R_hourly_Bkg_BB[i] <- R_month_Bkg_BB[i] / 24 / 12.01
    A_Bkg_BB[i] <- 1/(2.71828^((-2.988e6 / CQT_K_Bkg_BB[i] ^ 2 + 7.6663e3 / CQT_K_Bkg_BB[i] - 2.4612)/1000))
    ETP_D_Bkg_BB[i] <- 0.0133*CQT_Bkg_BB[i]*(23.9*16.4+50)/(CQT_Bkg_BB[i]+15)
    ETP_M_Bkg_BB[i] <- ETP_D_Bkg_BB[i] * 30
    ETA_Bkg_BB[i] <- CMP_mm_Bkg_BB[i] / (sqrt(1 + (1 / (ETP_M_Bkg_BB[i]/CMP_mm_Bkg_BB[i]))^2))
    EPS_Bkg_BB[i] <- 0 + EPSmax_Bkg_BB[i] * (ETA_Bkg_BB[i]/CMP_mm_Bkg_BB[i])
    DIFC_Bkg_BB[i] <- EPS_Bkg_BB[i]*p_Bkg_BB[i]*0.14
    DIFC13_Bkg_BB[i] <- DIFC_Bkg_BB[i] * (1/1.004443)
    S_Bkg_BB[i] <- 0.21*28.26^2/(28.26*(pCO2_Bkg+25))^2
    W_Bkg_BB[i] <- (1.2*(MAP_Bkg_BB+975))/(27.2+0.04*(MAP_Bkg_BB+975))
    deltaP_Bkg_BB[i] <- deltaA_Bkg - (W_Bkg_BB[i] - S_Bkg_BB[i]*pCO2_Bkg)
    deltaP_hat_Bkg_BB[i] <- (deltaP_Bkg_BB[i]/1000+1)*0.011237/(1+0.011237*(deltaP_Bkg_BB[i]/1000+1))
    deltaA_hat_Bkg_BB[i] <- (deltaA_Bkg/1000+1)*0.011237/(1+0.011237*(deltaA_Bkg/1000+1))
    z_100_Bkg_BB[i] <- ifelse (z_Bkg_BB[i]<100, z_Bkg_BB[i], 100)
    dC_Soil_Bkg_BB[i] <- (((((R_hourly_Bkg_BB[i] / DIFC_Bkg_BB[i]) * deltaP_hat_Bkg_BB[i] * (100 * z_100_Bkg_BB[i] - z_100_Bkg_BB[i]^2 / 2) * (DIFC_Bkg_BB[i] / DIFC13_Bkg_BB[i])) + pCO2_Bkg * deltaA_hat_Bkg_BB[i]) / (R_hourly_Bkg_BB[i] * (100 * z_100_Bkg_BB[i]-z_100_Bkg_BB[i]^2 / 2) * (1 - DIFC_Bkg_BB[i] * deltaP_hat_Bkg_BB[i] / DIFC13_Bkg_BB[i]) / DIFC_Bkg_BB[i] + pCO2_Bkg * (1 - deltaA_hat_Bkg_BB[i]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_Bkg_BB[i] <- 2.71828 ^ (-2.988e3 / CQT_K_Bkg_BB[i] ^ 2 + 7.6663 / CQT_K_Bkg_BB[i] - 0.0024612)
    R_Soil_Bkg_BB[i] = (dC_Soil_Bkg_BB[i]/1000 + 1)*0.0020672
    R_Carb_Bkg_BB[i] <- R_Soil_Bkg_BB[i] / A_CO2_Carb_Bkg_BB[i]
    A_O_Bkg_BB[i] <- 1/2.71828^((2.78e6 / CQT_K_Bkg_BB[i] ^ 2 - 2.89)/1000)
    E_Bkg_BB[i] <- ETA_Bkg_BB[i]*0.3
    E_s_Bkg_BB[i] <- E_Bkg_BB[i] * (1/2.592e6) * 0.001
    DIFO_Bkg_BB[i] <- EPS_Bkg_BB[i] * p_Bkg_BB[i] * 2.3e-9
    z_i_Bkg_BB[i] <- DIFO_Bkg_BB[i]/E_s_Bkg_BB[i]
    DRF_Bkg_BB[i] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_Bkg_BB[i] <- ((1-h_Bkg_BB[i]) * DRF_Bkg_BB[i] * R_O_P_Bkg_BB[i] + h_Bkg_BB[i] * R_O_atm_Bkg_BB[i]) / (1/A_atmP_Bkg_BB[i])
    R_O_soil_Bkg_BB[i] <- ((R_O_surface_Bkg_BB[i] - R_O_P_Bkg_BB[i]) * 2.71828 ^ (-z_m_Bkg_BB[i]/z_i_Bkg_BB[i])) + R_O_P_Bkg_BB[i]
    dO_soil_Bkg_BB[i] <- (R_O_soil_Bkg_BB[i]/0.0020052 - 1)*1000
    dO_surface_Bkg_BB[i] <- (R_O_surface_Bkg_BB[i]/0.0020052 - 1)*1000
    R_O_Carb_Bkg_BB[i] <- R_O_soil_Bkg_BB[i] / A_O_Bkg_BB[i]
    
    dO_Carb_P_Bkg_BB_m[i] <- (R_O_Carb_Bkg_BB[i]/0.0020672 - 1) * 1000
    dC_Carb_Bkg_BB_m[i] = (R_Carb_Bkg_BB[i]/0.0020672 - 1) * 1000
    
    
    dO_Carb_P_Bkg_BB[i] ~ dnorm(dO_Carb_P_Bkg_BB_m[i], 400)
    dC_Carb_Bkg_BB[i] ~ dnorm(dC_Carb_Bkg_BB_m[i], 400) 
    
  }
  
  for (j in 1:BB_PETM) {
    
    EPSmax_PETM_BB[j] <- 0.10 + ST_PETM_BB * 0.40
    p_PETM_BB[j] <- 0.4 + ST_PETM_BB * 0.4
    CQP_PETM_BB[j] <- MAP_PETM_BB * P_seas_PETM_BB
    CMP_cm_PETM_BB[j] <- CQP_PETM_BB[j] / 30
    CMP_mm_PETM_BB[j] <- CQP_PETM_BB[j] / 3
    CQT_PETM_BB[j] <- MAT_PETM_BB + T_seas_PETM_BB
    CQT_K_PETM_BB[j] <- CQT_PETM_BB[j] + 273
    dO_P_PETM_BB[j] <- -13.6 + 0.55 * MAT_PETM_BB
    h_PETM_BB[j] <- ifelse (0.1 + 0.9 * (CMP_mm_PETM_BB[j] / 200) < 1, 0.1 + 0.9 * (CMP_mm_PETM_BB[j] / 200), 1)
    A_atmP_PETM_BB[j] <- 2.71828^((5.9702e6/CQT_K_PETM_BB[j] ^ 2 - 3.2801e4/CQT_K_PETM_BB[j] + 52.227)/1000)
    R_O_P_PETM_BB[j] <- (dO_P_PETM_BB[j]/1000 + 1)*0.0020052
    R_O_atm_PETM_BB[j] <- R_O_P_PETM_BB[j]/A_atmP_PETM_BB[j]
    dO_atm_PETM_BB[j] <- (R_O_atm_PETM_BB[j]/0.0020052 - 1)*1000
    z_PETM_BB[j] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_PETM_BB))))/0.026
    z_m_PETM_BB[j] <- z_PETM_BB[j] * 0.01
    R_month_PETM_BB[j] <- 1.25*exp(0.05452*CQT_PETM_BB[j])*CMP_cm_PETM_BB[j]/(4.259+CMP_cm_PETM_BB[j])
    R_hourly_PETM_BB[j] <- R_month_PETM_BB[j] / 24 / 12.01
    A_PETM_BB[j] <- 1/(2.71828^((-2.988e6 / CQT_K_PETM_BB[j] ^ 2 + 7.6663e3 / CQT_K_PETM_BB[j] - 2.4612)/1000))
    ETP_D_PETM_BB[j] <- 0.0133*CQT_PETM_BB[j]*(23.9*16.4+50)/(CQT_PETM_BB[j]+15)
    ETP_M_PETM_BB[j] <- ETP_D_PETM_BB[j] * 30
    ETA_PETM_BB[j] <- CMP_mm_PETM_BB[j] / (sqrt(1 + (1 / (ETP_M_PETM_BB[j]/CMP_mm_PETM_BB[j]))^2))
    EPS_PETM_BB[j] <- 0 + EPSmax_PETM_BB[j] * (ETA_PETM_BB[j]/CMP_mm_PETM_BB[j])
    DIFC_PETM_BB[j] <- EPS_PETM_BB[j]*p_PETM_BB[j]*0.14
    DIFC13_PETM_BB[j] <- DIFC_PETM_BB[j] * (1/1.004443)
    S_PETM_BB[j] <- 0.21*28.26^2/(28.26*(pCO2_PETM+25))^2
    W_PETM_BB[j] <- (1.2*(MAP_PETM_BB+975))/(27.2+0.04*(MAP_PETM_BB+975))
    deltaP_PETM_BB[j] <- deltaA_PETM - (W_PETM_BB[j] - S_PETM_BB[j]*pCO2_PETM)
    deltaP_hat_PETM_BB[j] <- (deltaP_PETM_BB[j]/1000+1)*0.011237/(1+0.011237*(deltaP_PETM_BB[j]/1000+1))
    deltaA_hat_PETM_BB[j] <- (deltaA_PETM/1000+1)*0.011237/(1+0.011237*(deltaA_PETM/1000+1))
    z_100_PETM_BB[j] <- ifelse (z_PETM_BB[j]<100, z_PETM_BB[j], 100)
    dC_Soil_PETM_BB[j] <- (((((R_hourly_PETM_BB[j] / DIFC_PETM_BB[j]) * deltaP_hat_PETM_BB[j] * (100 * z_100_PETM_BB[j] - z_100_PETM_BB[j]^2 / 2) * (DIFC_PETM_BB[j] / DIFC13_PETM_BB[j])) + pCO2_PETM * deltaA_hat_PETM_BB[j]) / (R_hourly_PETM_BB[j] * (100 * z_100_PETM_BB[j]-z_100_PETM_BB[j]^2 / 2) * (1 - DIFC_PETM_BB[j] * deltaP_hat_PETM_BB[j] / DIFC13_PETM_BB[j]) / DIFC_PETM_BB[j] + pCO2_PETM * (1 - deltaA_hat_PETM_BB[j]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_PETM_BB[j] <- 2.71828 ^ (-2.988e3 / CQT_K_PETM_BB[j] ^ 2 + 7.6663 / CQT_K_PETM_BB[j] - 0.0024612)
    R_Soil_PETM_BB[j] = (dC_Soil_PETM_BB[j]/1000 + 1)*0.0020672
    R_Carb_PETM_BB[j] <- R_Soil_PETM_BB[j] / A_CO2_Carb_PETM_BB[j]
    A_O_PETM_BB[j] <- 1/2.71828^((2.78e6 / CQT_K_PETM_BB[j] ^ 2 - 2.89)/1000)
    E_PETM_BB[j] <- ETA_PETM_BB[j]*0.3
    E_s_PETM_BB[j] <- E_PETM_BB[j] * (1/2.592e6) * 0.001
    DIFO_PETM_BB[j] <- EPS_PETM_BB[j] * p_PETM_BB[j] * 2.3e-9
    z_i_PETM_BB[j] <- DIFO_PETM_BB[j]/E_s_PETM_BB[j]
    DRF_PETM_BB[j] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_PETM_BB[j] <- ((1-h_PETM_BB[j]) * DRF_PETM_BB[j] * R_O_P_PETM_BB[j] + h_PETM_BB[j] * R_O_atm_PETM_BB[j]) / (1/A_atmP_PETM_BB[j])
    R_O_soil_PETM_BB[j] <- ((R_O_surface_PETM_BB[j] - R_O_P_PETM_BB[j]) * 2.71828 ^ (-z_m_PETM_BB[j]/z_i_PETM_BB[j])) + R_O_P_PETM_BB[j]
    dO_soil_PETM_BB[j] <- (R_O_soil_PETM_BB[j]/0.0020052 - 1)*1000
    dO_surface_PETM_BB[j] <- (R_O_surface_PETM_BB[j]/0.0020052 - 1)*1000
    R_O_Carb_PETM_BB[j] <- R_O_soil_PETM_BB[j] / A_O_PETM_BB[j]
    
    dO_Carb_P_PETM_BB_m[j] <- (R_O_Carb_PETM_BB[j]/0.0020672 - 1) * 1000
    dC_Carb_PETM_BB_m[j] = (R_Carb_PETM_BB[j]/0.0020672 - 1) * 1000
    
    dO_Carb_P_PETM_BB[j] ~ dnorm(dO_Carb_P_PETM_BB_m[j], 400)
    dC_Carb_PETM_BB[j] ~ dnorm(dC_Carb_PETM_BB_m[j], 400) 
    
  }
  
  for (k in 1:AC_Bkg) {
    
    EPSmax_Bkg_AC[k] <- 0.10 + ST_Bkg_AC * 0.40
    p_Bkg_AC[k] <- 0.4 + ST_Bkg_AC * 0.4
    CQP_Bkg_AC[k] <- MAP_Bkg_AC * P_seas_Bkg_AC
    CMP_cm_Bkg_AC[k] <- CQP_Bkg_AC[k] / 30
    CMP_mm_Bkg_AC[k] <- CQP_Bkg_AC[k] / 3
    CQT_Bkg_AC[k] <- MAT_Bkg_AC + T_seas_Bkg_AC
    CQT_K_Bkg_AC[k] <- CQT_Bkg_AC[k] + 273
    dO_P_Bkg_AC[k] <- -13.6 + 0.55 * MAT_Bkg_AC
    h_Bkg_AC[k] <- ifelse (0.1 + 0.9 * (CMP_mm_Bkg_AC[k] / 200) < 1, 0.1 + 0.9 * (CMP_mm_Bkg_AC[k] / 200), 1)
    A_atmP_Bkg_AC[k] <- 2.71828^((5.9702e6/CQT_K_Bkg_AC[k] ^ 2 - 3.2801e4/CQT_K_Bkg_AC[k] + 52.227)/1000)
    R_O_P_Bkg_AC[k] <- (dO_P_Bkg_AC[k]/1000 + 1)*0.0020052
    R_O_atm_Bkg_AC[k] <- R_O_P_Bkg_AC[k]/A_atmP_Bkg_AC[k]
    dO_atm_Bkg_AC[k] <- (R_O_atm_Bkg_AC[k]/0.0020052 - 1)*1000
    z_Bkg_AC[k] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_Bkg_AC))))/0.026
    z_m_Bkg_AC[k] <- z_Bkg_AC[k] * 0.01
    R_month_Bkg_AC[k] <- 1.25*exp(0.05452*CQT_Bkg_AC[k])*CMP_cm_Bkg_AC[k]/(4.259+CMP_cm_Bkg_AC[k])
    R_hourly_Bkg_AC[k] <- R_month_Bkg_AC[k] / 24 / 12.01
    A_Bkg_AC[k] <- 1/(2.71828^((-2.988e6 / CQT_K_Bkg_AC[k] ^ 2 + 7.6663e3 / CQT_K_Bkg_AC[k] - 2.4612)/1000))
    ETP_D_Bkg_AC[k] <- 0.0133*CQT_Bkg_AC[k]*(23.9*16.4+50)/(CQT_Bkg_AC[k]+15)
    ETP_M_Bkg_AC[k] <- ETP_D_Bkg_AC[k] * 30
    ETA_Bkg_AC[k] <- CMP_mm_Bkg_AC[k] / (sqrt(1 + (1 / (ETP_M_Bkg_AC[k]/CMP_mm_Bkg_AC[k]))^2))
    EPS_Bkg_AC[k] <- 0 + EPSmax_Bkg_AC[k] * (ETA_Bkg_AC[k]/CMP_mm_Bkg_AC[k])
    DIFC_Bkg_AC[k] <- EPS_Bkg_AC[k]*p_Bkg_AC[k]*0.14
    DIFC13_Bkg_AC[k] <- DIFC_Bkg_AC[k] * (1/1.004443)
    S_Bkg_AC[k] <- 0.21*28.26^2/(28.26*(pCO2_Bkg+25))^2
    W_Bkg_AC[k] <- (1.2*(MAP_Bkg_AC+975))/(27.2+0.04*(MAP_Bkg_AC+975))
    deltaP_Bkg_AC[k] <- deltaA_Bkg - (W_Bkg_AC[k] - S_Bkg_AC[k]*pCO2_Bkg)
    deltaP_hat_Bkg_AC[k] <- (deltaP_Bkg_AC[k]/1000+1)*0.011237/(1+0.011237*(deltaP_Bkg_AC[k]/1000+1))
    deltaA_hat_Bkg_AC[k] <- (deltaA_Bkg/1000+1)*0.011237/(1+0.011237*(deltaA_Bkg/1000+1))
    z_100_Bkg_AC[k] <- ifelse (z_Bkg_AC[k]<100, z_Bkg_AC[k], 100)
    dC_Soil_Bkg_AC[k] <- (((((R_hourly_Bkg_AC[k] / DIFC_Bkg_AC[k]) * deltaP_hat_Bkg_AC[k] * (100 * z_100_Bkg_AC[k] - z_100_Bkg_AC[k]^2 / 2) * (DIFC_Bkg_AC[k] / DIFC13_Bkg_AC[k])) + pCO2_Bkg * deltaA_hat_Bkg_AC[k]) / (R_hourly_Bkg_AC[k] * (100 * z_100_Bkg_AC[k]-z_100_Bkg_AC[k]^2 / 2) * (1 - DIFC_Bkg_AC[k] * deltaP_hat_Bkg_AC[k] / DIFC13_Bkg_AC[k]) / DIFC_Bkg_AC[k] + pCO2_Bkg * (1 - deltaA_hat_Bkg_AC[k]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_Bkg_AC[k] <- 2.71828 ^ (-2.988e3 / CQT_K_Bkg_AC[k] ^ 2 + 7.6663 / CQT_K_Bkg_AC[k] - 0.0024612)
    R_Soil_Bkg_AC[k] = (dC_Soil_Bkg_AC[k]/1000 + 1)*0.0020672
    R_Carb_Bkg_AC[k] <- R_Soil_Bkg_AC[k] / A_CO2_Carb_Bkg_AC[k]
    A_O_Bkg_AC[k] <- 1/2.71828^((2.78e6 / CQT_K_Bkg_AC[k] ^ 2 - 2.89)/1000)
    E_Bkg_AC[k] <- ETA_Bkg_AC[k]*0.3
    E_s_Bkg_AC[k] <- E_Bkg_AC[k] * (1/2.592e6) * 0.001
    DIFO_Bkg_AC[k] <- EPS_Bkg_AC[k] * p_Bkg_AC[k] * 2.3e-9
    z_i_Bkg_AC[k] <- DIFO_Bkg_AC[k]/E_s_Bkg_AC[k]
    DRF_Bkg_AC[k] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_Bkg_AC[k] <- ((1-h_Bkg_AC[k]) * DRF_Bkg_AC[k] * R_O_P_Bkg_AC[k] + h_Bkg_AC[k] * R_O_atm_Bkg_AC[k]) / (1/A_atmP_Bkg_AC[k])
    R_O_soil_Bkg_AC[k] <- ((R_O_surface_Bkg_AC[k] - R_O_P_Bkg_AC[k]) * 2.71828 ^ (-z_m_Bkg_AC[k]/z_i_Bkg_AC[k])) + R_O_P_Bkg_AC[k]
    dO_soil_Bkg_AC[k] <- (R_O_soil_Bkg_AC[k]/0.0020052 - 1)*1000
    dO_surface_Bkg_AC[k] <- (R_O_surface_Bkg_AC[k]/0.0020052 - 1)*1000
    R_O_Carb_Bkg_AC[k] <- R_O_soil_Bkg_AC[k] / A_O_Bkg_AC[k]
    
    dO_Carb_P_Bkg_AC_m[k] <- (R_O_Carb_Bkg_AC[k]/0.0020672 - 1) * 1000
    dC_Carb_Bkg_AC_m[k] = (R_Carb_Bkg_AC[k]/0.0020672 - 1) * 1000
    
    dO_Carb_P_Bkg_AC[k] ~ dnorm(dO_Carb_P_Bkg_AC_m[k], 400)
    dC_Carb_Bkg_AC[k] ~ dnorm(dC_Carb_Bkg_AC_m[k], 400) 
    
  }
  
  for (l in 1:AC_PETM) {
    
    EPSmax_PETM_AC[l] <- 0.10 + ST_PETM_AC * 0.40
    p_PETM_AC[l] <- 0.4 + ST_PETM_AC * 0.4
    CQP_PETM_AC[l] <- MAP_PETM_AC * P_seas_PETM_AC
    CMP_cm_PETM_AC[l] <- CQP_PETM_AC[l] / 30
    CMP_mm_PETM_AC[l] <- CQP_PETM_AC[l] / 3
    CQT_PETM_AC[l] <- MAT_PETM_AC + T_seas_PETM_AC
    CQT_K_PETM_AC[l] <- CQT_PETM_AC[l] + 273
    dO_P_PETM_AC[l] <- -13.6 + 0.55 * MAT_PETM_AC
    h_PETM_AC[l] <- ifelse (0.1 + 0.9 * (CMP_mm_PETM_AC[l] / 200) < 1, 0.1 + 0.9 * (CMP_mm_PETM_AC[l] / 200), 1)
    A_atmP_PETM_AC[l] <- 2.71828^((5.9702e6/CQT_K_PETM_AC[l] ^ 2 - 3.2801e4/CQT_K_PETM_AC[l] + 52.227)/1000)
    R_O_P_PETM_AC[l] <- (dO_P_PETM_AC[l]/1000 + 1)*0.0020052
    R_O_atm_PETM_AC[l] <- R_O_P_PETM_AC[l]/A_atmP_PETM_AC[l]
    dO_atm_PETM_AC[l] <- (R_O_atm_PETM_AC[l]/0.0020052 - 1)*1000
    z_PETM_AC[l] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_PETM_AC))))/0.026
    z_m_PETM_AC[l] <- z_PETM_AC[l] * 0.01
    R_month_PETM_AC[l] <- 1.25*exp(0.05452*CQT_PETM_AC[l])*CMP_cm_PETM_AC[l]/(4.259+CMP_cm_PETM_AC[l])
    R_hourly_PETM_AC[l] <- R_month_PETM_AC[l] / 24 / 12.01
    A_PETM_AC[l] <- 1/(2.71828^((-2.988e6 / CQT_K_PETM_AC[l] ^ 2 + 7.6663e3 / CQT_K_PETM_AC[l] - 2.4612)/1000))
    ETP_D_PETM_AC[l] <- 0.0133*CQT_PETM_AC[l]*(23.9*16.4+50)/(CQT_PETM_AC[l]+15)
    ETP_M_PETM_AC[l] <- ETP_D_PETM_AC[l] * 30
    ETA_PETM_AC[l] <- CMP_mm_PETM_AC[l] / (sqrt(1 + (1 / (ETP_M_PETM_AC[l]/CMP_mm_PETM_AC[l]))^2))
    EPS_PETM_AC[l] <- 0 + EPSmax_PETM_AC[l] * (ETA_PETM_AC[l]/CMP_mm_PETM_AC[l])
    DIFC_PETM_AC[l] <- EPS_PETM_AC[l]*p_PETM_AC[l]*0.14
    DIFC13_PETM_AC[l] <- DIFC_PETM_AC[l] * (1/1.004443)
    S_PETM_AC[l] <- 0.21*28.26^2/(28.26*(pCO2_PETM+25))^2
    W_PETM_AC[l] <- (1.2*(MAP_PETM_AC+975))/(27.2+0.04*(MAP_PETM_AC+975))
    deltaP_PETM_AC[l] <- deltaA_PETM - (W_PETM_AC[l] - S_PETM_AC[l]*pCO2_PETM)
    deltaP_hat_PETM_AC[l] <- (deltaP_PETM_AC[l]/1000+1)*0.011237/(1+0.011237*(deltaP_PETM_AC[l]/1000+1))
    deltaA_hat_PETM_AC[l] <- (deltaA_PETM/1000+1)*0.011237/(1+0.011237*(deltaA_PETM/1000+1))
    z_100_PETM_AC[l] <- ifelse (z_PETM_AC[l]<100, z_PETM_AC[l], 100)
    dC_Soil_PETM_AC[l] <- (((((R_hourly_PETM_AC[l] / DIFC_PETM_AC[l]) * deltaP_hat_PETM_AC[l] * (100 * z_100_PETM_AC[l] - z_100_PETM_AC[l]^2 / 2) * (DIFC_PETM_AC[l] / DIFC13_PETM_AC[l])) + pCO2_PETM * deltaA_hat_PETM_AC[l]) / (R_hourly_PETM_AC[l] * (100 * z_100_PETM_AC[l]-z_100_PETM_AC[l]^2 / 2) * (1 - DIFC_PETM_AC[l] * deltaP_hat_PETM_AC[l] / DIFC13_PETM_AC[l]) / DIFC_PETM_AC[l] + pCO2_PETM * (1 - deltaA_hat_PETM_AC[l]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_PETM_AC[l] <- 2.71828 ^ (-2.988e3 / CQT_K_PETM_AC[l] ^ 2 + 7.6663 / CQT_K_PETM_AC[l] - 0.0024612)
    R_Soil_PETM_AC[l] = (dC_Soil_PETM_AC[l]/1000 + 1)*0.0020672
    R_Carb_PETM_AC[l] <- R_Soil_PETM_AC[l] / A_CO2_Carb_PETM_AC[l]
    A_O_PETM_AC[l] <- 1/2.71828^((2.78e6 / CQT_K_PETM_AC[l] ^ 2 - 2.89)/1000)
    E_PETM_AC[l] <- ETA_PETM_AC[l]*0.3
    E_s_PETM_AC[l] <- E_PETM_AC[l] * (1/2.592e6) * 0.001
    DIFO_PETM_AC[l] <- EPS_PETM_AC[l] * p_PETM_AC[l] * 2.3e-9
    z_i_PETM_AC[l] <- DIFO_PETM_AC[l]/E_s_PETM_AC[l]
    DRF_PETM_AC[l] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_PETM_AC[l] <- ((1-h_PETM_AC[l]) * DRF_PETM_AC[l] * R_O_P_PETM_AC[l] + h_PETM_AC[l] * R_O_atm_PETM_AC[l]) / (1/A_atmP_PETM_AC[l])
    R_O_soil_PETM_AC[l] <- ((R_O_surface_PETM_AC[l] - R_O_P_PETM_AC[l]) * 2.71828 ^ (-z_m_PETM_AC[l]/z_i_PETM_AC[l])) + R_O_P_PETM_AC[l]
    dO_soil_PETM_AC[l] <- (R_O_soil_PETM_AC[l]/0.0020052 - 1)*1000
    dO_surface_PETM_AC[l] <- (R_O_surface_PETM_AC[l]/0.0020052 - 1)*1000
    R_O_Carb_PETM_AC[l] <- R_O_soil_PETM_AC[l] / A_O_PETM_AC[l]
    
    dO_Carb_P_PETM_AC_m[l] <- (R_O_Carb_PETM_AC[l]/0.0020672 - 1) * 1000
    dC_Carb_PETM_AC_m[l] = (R_Carb_PETM_AC[l]/0.0020672 - 1) * 1000
    
    dO_Carb_P_PETM_AC[l] ~ dnorm(dO_Carb_P_PETM_AC_m[l], 400)
    dC_Carb_PETM_AC[l] ~ dnorm(dC_Carb_PETM_AC_m[l], 400) 
    
  }
  
  
  for (e in 1:ES_Bkg) {
    
    EPSmax_Bkg_ES[e] <- 0.10 + ST_Bkg_ES * 0.40
    p_Bkg_ES[e] <- 0.4 + ST_Bkg_ES * 0.4
    CQP_Bkg_ES[e] <- MAP_Bkg_ES * P_seas_Bkg_ES
    CMP_cm_Bkg_ES[e] <- CQP_Bkg_ES[e] / 30
    CMP_mm_Bkg_ES[e] <- CQP_Bkg_ES[e] / 3
    CQT_Bkg_ES[e] <- MAT_Bkg_ES + T_seas_Bkg_ES
    CQT_K_Bkg_ES[e] <- CQT_Bkg_ES[e] + 273
    dO_P_Bkg_ES[e] <- -13.6 + 0.55 * MAT_Bkg_ES
    h_Bkg_ES[e] <- ifelse (0.1 + 0.9 * (CMP_mm_Bkg_ES[e] / 200) < 1, 0.1 + 0.9 * (CMP_mm_Bkg_ES[e] / 200), 1)
    A_atmP_Bkg_ES[e] <- 2.71828^((5.9702e6/CQT_K_Bkg_ES[e] ^ 2 - 3.2801e4/CQT_K_Bkg_ES[e] + 52.227)/1000)
    R_O_P_Bkg_ES[e] <- (dO_P_Bkg_ES[e]/1000 + 1)*0.0020052
    R_O_atm_Bkg_ES[e] <- R_O_P_Bkg_ES[e]/A_atmP_Bkg_ES[e]
    dO_atm_Bkg_ES[e] <- (R_O_atm_Bkg_ES[e]/0.0020052 - 1)*1000
    z_Bkg_ES[e] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_Bkg_ES))))/0.026
    z_m_Bkg_ES[e] <- z_Bkg_ES[e] * 0.01
    R_month_Bkg_ES[e] <- 1.25*exp(0.05452*CQT_Bkg_ES[e])*CMP_cm_Bkg_ES[e]/(4.259+CMP_cm_Bkg_ES[e])
    R_hourly_Bkg_ES[e] <- R_month_Bkg_ES[e] / 24 / 12.01
    A_Bkg_ES[e] <- 1/(2.71828^((-2.988e6 / CQT_K_Bkg_ES[e] ^ 2 + 7.6663e3 / CQT_K_Bkg_ES[e] - 2.4612)/1000))
    ETP_D_Bkg_ES[e] <- 0.0133*CQT_Bkg_ES[e]*(23.9*16.4+50)/(CQT_Bkg_ES[e]+15)
    ETP_M_Bkg_ES[e] <- ETP_D_Bkg_ES[e] * 30
    ETA_Bkg_ES[e] <- CMP_mm_Bkg_ES[e] / (sqrt(1 + (1 / (ETP_M_Bkg_ES[e]/CMP_mm_Bkg_ES[e]))^2))
    EPS_Bkg_ES[e] <- 0 + EPSmax_Bkg_ES[e] * (ETA_Bkg_ES[e]/CMP_mm_Bkg_ES[e])
    DIFC_Bkg_ES[e] <- EPS_Bkg_ES[e]*p_Bkg_ES[e]*0.14
    DIFC13_Bkg_ES[e] <- DIFC_Bkg_ES[e] * (1/1.004443)
    S_Bkg_ES[e] <- 0.21*28.26^2/(28.26*(pCO2_Bkg+25))^2
    W_Bkg_ES[e] <- (1.2*(MAP_Bkg_ES+975))/(27.2+0.04*(MAP_Bkg_ES+975))
    deltaP_Bkg_ES[e] <- deltaA_Bkg - (W_Bkg_ES[e] - S_Bkg_ES[e]*pCO2_Bkg)
    deltaP_hat_Bkg_ES[e] <- (deltaP_Bkg_ES[e]/1000+1)*0.011237/(1+0.011237*(deltaP_Bkg_ES[e]/1000+1))
    deltaA_hat_Bkg_ES[e] <- (deltaA_Bkg/1000+1)*0.011237/(1+0.011237*(deltaA_Bkg/1000+1))
    z_100_Bkg_ES[e] <- ifelse (z_Bkg_ES[e]<100, z_Bkg_ES[e], 100)
    dC_Soil_Bkg_ES[e] <- (((((R_hourly_Bkg_ES[e] / DIFC_Bkg_ES[e]) * deltaP_hat_Bkg_ES[e] * (100 * z_100_Bkg_ES[e] - z_100_Bkg_ES[e]^2 / 2) * (DIFC_Bkg_ES[e] / DIFC13_Bkg_ES[e])) + pCO2_Bkg * deltaA_hat_Bkg_ES[e]) / (R_hourly_Bkg_ES[e] * (100 * z_100_Bkg_ES[e]-z_100_Bkg_ES[e]^2 / 2) * (1 - DIFC_Bkg_ES[e] * deltaP_hat_Bkg_ES[e] / DIFC13_Bkg_ES[e]) / DIFC_Bkg_ES[e] + pCO2_Bkg * (1 - deltaA_hat_Bkg_ES[e]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_Bkg_ES[e] <- 2.71828 ^ (-2.988e3 / CQT_K_Bkg_ES[e] ^ 2 + 7.6663 / CQT_K_Bkg_ES[e] - 0.0024612)
    R_Soil_Bkg_ES[e] = (dC_Soil_Bkg_ES[e]/1000 + 1)*0.0020672
    R_Carb_Bkg_ES[e] <- R_Soil_Bkg_ES[e] / A_CO2_Carb_Bkg_ES[e]
    A_O_Bkg_ES[e] <- 1/2.71828^((2.78e6 / CQT_K_Bkg_ES[e] ^ 2 - 2.89)/1000)
    E_Bkg_ES[e] <- ETA_Bkg_ES[e]*0.3
    E_s_Bkg_ES[e] <- E_Bkg_ES[e] * (1/2.592e6) * 0.001
    DIFO_Bkg_ES[e] <- EPS_Bkg_ES[e] * p_Bkg_ES[e] * 2.3e-9
    z_i_Bkg_ES[e] <- DIFO_Bkg_ES[e]/E_s_Bkg_ES[e]
    DRF_Bkg_ES[e] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_Bkg_ES[e] <- ((1-h_Bkg_ES[e]) * DRF_Bkg_ES[e] * R_O_P_Bkg_ES[e] + h_Bkg_ES[e] * R_O_atm_Bkg_ES[e]) / (1/A_atmP_Bkg_ES[e])
    R_O_soil_Bkg_ES[e] <- ((R_O_surface_Bkg_ES[e] - R_O_P_Bkg_ES[e]) * 2.71828 ^ (-z_m_Bkg_ES[e]/z_i_Bkg_ES[e])) + R_O_P_Bkg_ES[e]
    dO_soil_Bkg_ES[e] <- (R_O_soil_Bkg_ES[e]/0.0020052 - 1)*1000
    dO_surface_Bkg_ES[e] <- (R_O_surface_Bkg_ES[e]/0.0020052 - 1)*1000
    R_O_Carb_Bkg_ES[e] <- R_O_soil_Bkg_ES[e] / A_O_Bkg_ES[e]
    
    dO_Carb_P_Bkg_ES_m[e] <- (R_O_Carb_Bkg_ES[e]/0.0020672 - 1) * 1000
    dC_Carb_Bkg_ES_m[e] = (R_Carb_Bkg_ES[e]/0.0020672 - 1) * 1000
    
    dO_Carb_P_Bkg_ES[e] ~ dnorm(dO_Carb_P_Bkg_ES_m[e], 400)
    dC_Carb_Bkg_ES[e] ~ dnorm(dC_Carb_Bkg_ES_m[e], 400) 
    
  }
  
  for (f in 1:ES_PETM) {
    
    EPSmax_PETM_ES[f] <- 0.10 + ST_PETM_ES * 0.40
    p_PETM_ES[f] <- 0.4 + ST_PETM_ES * 0.4
    CQP_PETM_ES[f] <- MAP_PETM_ES * P_seas_PETM_ES
    CMP_cm_PETM_ES[f] <- CQP_PETM_ES[f] / 30
    CMP_mm_PETM_ES[f] <- CQP_PETM_ES[f] / 3
    CQT_PETM_ES[f] <- MAT_PETM_ES + T_seas_PETM_ES
    CQT_K_PETM_ES[f] <- CQT_PETM_ES[f] + 273
    dO_P_PETM_ES[f] <- -13.6 + 0.55 * MAT_PETM_ES
    h_PETM_ES[f] <- ifelse (0.1 + 0.9 * (CMP_mm_PETM_ES[f] / 200) < 1, 0.1 + 0.9 * (CMP_mm_PETM_ES[f] / 200), 1)
    A_atmP_PETM_ES[f] <- 2.71828^((5.9702e6/CQT_K_PETM_ES[f] ^ 2 - 3.2801e4/CQT_K_PETM_ES[f] + 52.227)/1000)
    R_O_P_PETM_ES[f] <- (dO_P_PETM_ES[f]/1000 + 1)*0.0020052
    R_O_atm_PETM_ES[f] <- R_O_P_PETM_ES[f]/A_atmP_PETM_ES[f]
    dO_atm_PETM_ES[f] <- (R_O_atm_PETM_ES[f]/0.0020052 - 1)*1000
    z_PETM_ES[f] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_PETM_ES))))/0.026
    z_m_PETM_ES[f] <- z_PETM_ES[f] * 0.01
    R_month_PETM_ES[f] <- 1.25*exp(0.05452*CQT_PETM_ES[f])*CMP_cm_PETM_ES[f]/(4.259+CMP_cm_PETM_ES[f])
    R_hourly_PETM_ES[f] <- R_month_PETM_ES[f] / 24 / 12.01
    A_PETM_ES[f] <- 1/(2.71828^((-2.988e6 / CQT_K_PETM_ES[f] ^ 2 + 7.6663e3 / CQT_K_PETM_ES[f] - 2.4612)/1000))
    ETP_D_PETM_ES[f] <- 0.0133*CQT_PETM_ES[f]*(23.9*16.4+50)/(CQT_PETM_ES[f]+15)
    ETP_M_PETM_ES[f] <- ETP_D_PETM_ES[f] * 30
    ETA_PETM_ES[f] <- CMP_mm_PETM_ES[f] / (sqrt(1 + (1 / (ETP_M_PETM_ES[f]/CMP_mm_PETM_ES[f]))^2))
    EPS_PETM_ES[f] <- 0 + EPSmax_PETM_ES[f] * (ETA_PETM_ES[f]/CMP_mm_PETM_ES[f])
    DIFC_PETM_ES[f] <- EPS_PETM_ES[f]*p_PETM_ES[f]*0.14
    DIFC13_PETM_ES[f] <- DIFC_PETM_ES[f] * (1/1.004443)
    S_PETM_ES[f] <- 0.21*28.26^2/(28.26*(pCO2_PETM+25))^2
    W_PETM_ES[f] <- (1.2*(MAP_PETM_ES+975))/(27.2+0.04*(MAP_PETM_ES+975))
    deltaP_PETM_ES[f] <- deltaA_PETM - (W_PETM_ES[f] - S_PETM_ES[f]*pCO2_PETM)
    deltaP_hat_PETM_ES[f] <- (deltaP_PETM_ES[f]/1000+1)*0.011237/(1+0.011237*(deltaP_PETM_ES[f]/1000+1))
    deltaA_hat_PETM_ES[f] <- (deltaA_PETM/1000+1)*0.011237/(1+0.011237*(deltaA_PETM/1000+1))
    z_100_PETM_ES[f] <- ifelse (z_PETM_ES[f]<100, z_PETM_ES[f], 100)
    dC_Soil_PETM_ES[f] <- (((((R_hourly_PETM_ES[f] / DIFC_PETM_ES[f]) * deltaP_hat_PETM_ES[f] * (100 * z_100_PETM_ES[f] - z_100_PETM_ES[f]^2 / 2) * (DIFC_PETM_ES[f] / DIFC13_PETM_ES[f])) + pCO2_PETM * deltaA_hat_PETM_ES[f]) / (R_hourly_PETM_ES[f] * (100 * z_100_PETM_ES[f]-z_100_PETM_ES[f]^2 / 2) * (1 - DIFC_PETM_ES[f] * deltaP_hat_PETM_ES[f] / DIFC13_PETM_ES[f]) / DIFC_PETM_ES[f] + pCO2_PETM * (1 - deltaA_hat_PETM_ES[f]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_PETM_ES[f] <- 2.71828 ^ (-2.988e3 / CQT_K_PETM_ES[f] ^ 2 + 7.6663 / CQT_K_PETM_ES[f] - 0.0024612)
    R_Soil_PETM_ES[f] = (dC_Soil_PETM_ES[f]/1000 + 1)*0.0020672
    R_Carb_PETM_ES[f] <- R_Soil_PETM_ES[f] / A_CO2_Carb_PETM_ES[f]
    A_O_PETM_ES[f] <- 1/2.71828^((2.78e6 / CQT_K_PETM_ES[f] ^ 2 - 2.89)/1000)
    E_PETM_ES[f] <- ETA_PETM_ES[f]*0.3
    E_s_PETM_ES[f] <- E_PETM_ES[f] * (1/2.592e6) * 0.001
    DIFO_PETM_ES[f] <- EPS_PETM_ES[f] * p_PETM_ES[f] * 2.3e-9
    z_i_PETM_ES[f] <- DIFO_PETM_ES[f]/E_s_PETM_ES[f]
    DRF_PETM_ES[f] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_PETM_ES[f] <- ((1-h_PETM_ES[f]) * DRF_PETM_ES[f] * R_O_P_PETM_ES[f] + h_PETM_ES[f] * R_O_atm_PETM_ES[f]) / (1/A_atmP_PETM_ES[f])
    R_O_soil_PETM_ES[f] <- ((R_O_surface_PETM_ES[f] - R_O_P_PETM_ES[f]) * 2.71828 ^ (-z_m_PETM_ES[f]/z_i_PETM_ES[f])) + R_O_P_PETM_ES[f]
    dO_soil_PETM_ES[f] <- (R_O_soil_PETM_ES[f]/0.0020052 - 1)*1000
    dO_surface_PETM_ES[f] <- (R_O_surface_PETM_ES[f]/0.0020052 - 1)*1000
    R_O_Carb_PETM_ES[f] <- R_O_soil_PETM_ES[f] / A_O_PETM_ES[f]
    
    dO_Carb_P_PETM_ES_m[f] <- (R_O_Carb_PETM_ES[f]/0.0020672 - 1) * 1000
    dC_Carb_PETM_ES_m[f] = (R_Carb_PETM_ES[f]/0.0020672 - 1) * 1000
    
    dO_Carb_P_PETM_ES[f] ~ dnorm(dO_Carb_P_PETM_ES_m[f], 400)
    dC_Carb_PETM_ES[f] ~ dnorm(dC_Carb_PETM_ES_m[f], 400) 
    
  }
  
  for (g in 1:CN_Bkg) {
    
    EPSmax_Bkg_CN[g] <- 0.10 + ST_Bkg_CN * 0.40
    p_Bkg_CN[g] <- 0.4 + ST_Bkg_CN * 0.4
    CQP_Bkg_CN[g] <- MAP_Bkg_CN * P_seas_Bkg_CN
    CMP_cm_Bkg_CN[g] <- CQP_Bkg_CN[g] / 30
    CMP_mm_Bkg_CN[g] <- CQP_Bkg_CN[g] / 3
    CQT_Bkg_CN[g] <- MAT_Bkg_CN + T_seas_Bkg_CN
    CQT_K_Bkg_CN[g] <- CQT_Bkg_CN[g] + 273
    dO_P_Bkg_CN[g] <- -13.6 + 0.55 * MAT_Bkg_CN
    h_Bkg_CN[g] <- ifelse (0.1 + 0.9 * (CMP_mm_Bkg_CN[g] / 200) < 1, 0.1 + 0.9 * (CMP_mm_Bkg_CN[g] / 200), 1)
    A_atmP_Bkg_CN[g] <- 2.71828^((5.9702e6/CQT_K_Bkg_CN[g] ^ 2 - 3.2801e4/CQT_K_Bkg_CN[g] + 52.227)/1000)
    R_O_P_Bkg_CN[g] <- (dO_P_Bkg_CN[g]/1000 + 1)*0.0020052
    R_O_atm_Bkg_CN[g] <- R_O_P_Bkg_CN[g]/A_atmP_Bkg_CN[g]
    dO_atm_Bkg_CN[g] <- (R_O_atm_Bkg_CN[g]/0.0020052 - 1)*1000
    z_Bkg_CN[g] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_Bkg_CN))))/0.026
    z_m_Bkg_CN[g] <- z_Bkg_CN[g] * 0.01
    R_month_Bkg_CN[g] <- 1.25*exp(0.05452*CQT_Bkg_CN[g])*CMP_cm_Bkg_CN[g]/(4.259+CMP_cm_Bkg_CN[g])
    R_hourly_Bkg_CN[g] <- R_month_Bkg_CN[g] / 24 / 12.01
    A_Bkg_CN[g] <- 1/(2.71828^((-2.988e6 / CQT_K_Bkg_CN[g] ^ 2 + 7.6663e3 / CQT_K_Bkg_CN[g] - 2.4612)/1000))
    ETP_D_Bkg_CN[g] <- 0.0133*CQT_Bkg_CN[g]*(23.9*16.4+50)/(CQT_Bkg_CN[g]+15)
    ETP_M_Bkg_CN[g] <- ETP_D_Bkg_CN[g] * 30
    ETA_Bkg_CN[g] <- CMP_mm_Bkg_CN[g] / (sqrt(1 + (1 / (ETP_M_Bkg_CN[g]/CMP_mm_Bkg_CN[g]))^2))
    EPS_Bkg_CN[g] <- 0 + EPSmax_Bkg_CN[g] * (ETA_Bkg_CN[g]/CMP_mm_Bkg_CN[g])
    DIFC_Bkg_CN[g] <- EPS_Bkg_CN[g]*p_Bkg_CN[g]*0.14
    DIFC13_Bkg_CN[g] <- DIFC_Bkg_CN[g] * (1/1.004443)
    S_Bkg_CN[g] <- 0.21*28.26^2/(28.26*(pCO2_Bkg+25))^2
    W_Bkg_CN[g] <- (1.2*(MAP_Bkg_CN+975))/(27.2+0.04*(MAP_Bkg_CN+975))
    deltaP_Bkg_CN[g] <- deltaA_Bkg - (W_Bkg_CN[g] - S_Bkg_CN[g]*pCO2_Bkg)
    deltaP_hat_Bkg_CN[g] <- (deltaP_Bkg_CN[g]/1000+1)*0.011237/(1+0.011237*(deltaP_Bkg_CN[g]/1000+1))
    deltaA_hat_Bkg_CN[g] <- (deltaA_Bkg/1000+1)*0.011237/(1+0.011237*(deltaA_Bkg/1000+1))
    z_100_Bkg_CN[g] <- ifelse (z_Bkg_CN[g]<100, z_Bkg_CN[g], 100)
    dC_Soil_Bkg_CN[g] <- (((((R_hourly_Bkg_CN[g] / DIFC_Bkg_CN[g]) * deltaP_hat_Bkg_CN[g] * (100 * z_100_Bkg_CN[g] - z_100_Bkg_CN[g]^2 / 2) * (DIFC_Bkg_CN[g] / DIFC13_Bkg_CN[g])) + pCO2_Bkg * deltaA_hat_Bkg_CN[g]) / (R_hourly_Bkg_CN[g] * (100 * z_100_Bkg_CN[g]-z_100_Bkg_CN[g]^2 / 2) * (1 - DIFC_Bkg_CN[g] * deltaP_hat_Bkg_CN[g] / DIFC13_Bkg_CN[g]) / DIFC_Bkg_CN[g] + pCO2_Bkg * (1 - deltaA_hat_Bkg_CN[g]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_Bkg_CN[g] <- 2.71828 ^ (-2.988e3 / CQT_K_Bkg_CN[g] ^ 2 + 7.6663 / CQT_K_Bkg_CN[g] - 0.0024612)
    R_Soil_Bkg_CN[g] = (dC_Soil_Bkg_CN[g]/1000 + 1)*0.0020672
    R_Carb_Bkg_CN[g] <- R_Soil_Bkg_CN[g] / A_CO2_Carb_Bkg_CN[g]
    A_O_Bkg_CN[g] <- 1/2.71828^((2.78e6 / CQT_K_Bkg_CN[g] ^ 2 - 2.89)/1000)
    E_Bkg_CN[g] <- ETA_Bkg_CN[g]*0.3
    E_s_Bkg_CN[g] <- E_Bkg_CN[g] * (1/2.592e6) * 0.001
    DIFO_Bkg_CN[g] <- EPS_Bkg_CN[g] * p_Bkg_CN[g] * 2.3e-9
    z_i_Bkg_CN[g] <- DIFO_Bkg_CN[g]/E_s_Bkg_CN[g]
    DRF_Bkg_CN[g] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_Bkg_CN[g] <- ((1-h_Bkg_CN[g]) * DRF_Bkg_CN[g] * R_O_P_Bkg_CN[g] + h_Bkg_CN[g] * R_O_atm_Bkg_CN[g]) / (1/A_atmP_Bkg_CN[g])
    R_O_soil_Bkg_CN[g] <- ((R_O_surface_Bkg_CN[g] - R_O_P_Bkg_CN[g]) * 2.71828 ^ (-z_m_Bkg_CN[g]/z_i_Bkg_CN[g])) + R_O_P_Bkg_CN[g]
    dO_soil_Bkg_CN[g] <- (R_O_soil_Bkg_CN[g]/0.0020052 - 1)*1000
    dO_surface_Bkg_CN[g] <- (R_O_surface_Bkg_CN[g]/0.0020052 - 1)*1000
    R_O_Carb_Bkg_CN[g] <- R_O_soil_Bkg_CN[g] / A_O_Bkg_CN[g]
    
    dO_Carb_P_Bkg_CN_m[g] <- (R_O_Carb_Bkg_CN[g]/0.0020672 - 1) * 1000
    dC_Carb_Bkg_CN_m[g] = (R_Carb_Bkg_CN[g]/0.0020672 - 1) * 1000
    
    dO_Carb_P_Bkg_CN[g] ~ dnorm(dO_Carb_P_Bkg_CN_m[g], 400)
    dC_Carb_Bkg_CN[g] ~ dnorm(dC_Carb_Bkg_CN_m[g], 400) 
    
  }
  
  for (o in 1:CN_PETM) {
    
    EPSmax_PETM_CN[o] <- 0.10 + ST_PETM_CN * 0.40
    p_PETM_CN[o] <- 0.4 + ST_PETM_CN * 0.4
    CQP_PETM_CN[o] <- MAP_PETM_CN * P_seas_PETM_CN
    CMP_cm_PETM_CN[o] <- CQP_PETM_CN[o] / 30
    CMP_mm_PETM_CN[o] <- CQP_PETM_CN[o] / 3
    CQT_PETM_CN[o] <- MAT_PETM_CN + T_seas_PETM_CN
    CQT_K_PETM_CN[o] <- CQT_PETM_CN[o] + 273
    dO_P_PETM_CN[o] <- -13.6 + 0.55 * MAT_PETM_CN
    h_PETM_CN[o] <- ifelse (0.1 + 0.9 * (CMP_mm_PETM_CN[o] / 200) < 1, 0.1 + 0.9 * (CMP_mm_PETM_CN[o] / 200), 1)
    A_atmP_PETM_CN[o] <- 2.71828^((5.9702e6/CQT_K_PETM_CN[o] ^ 2 - 3.2801e4/CQT_K_PETM_CN[o] + 52.227)/1000)
    R_O_P_PETM_CN[o] <- (dO_P_PETM_CN[o]/1000 + 1)*0.0020052
    R_O_atm_PETM_CN[o] <- R_O_P_PETM_CN[o]/A_atmP_PETM_CN[o]
    dO_atm_PETM_CN[o] <- (R_O_atm_PETM_CN[o]/0.0020052 - 1)*1000
    z_PETM_CN[o] <- (-6.45+sqrt(41.6025-(0.052*(137.24-MAP_PETM_CN))))/0.026
    z_m_PETM_CN[o] <- z_PETM_CN[o] * 0.01
    R_month_PETM_CN[o] <- 1.25*exp(0.05452*CQT_PETM_CN[o])*CMP_cm_PETM_CN[o]/(4.259+CMP_cm_PETM_CN[o])
    R_hourly_PETM_CN[o] <- R_month_PETM_CN[o] / 24 / 12.01
    A_PETM_CN[o] <- 1/(2.71828^((-2.988e6 / CQT_K_PETM_CN[o] ^ 2 + 7.6663e3 / CQT_K_PETM_CN[o] - 2.4612)/1000))
    ETP_D_PETM_CN[o] <- 0.0133*CQT_PETM_CN[o]*(23.9*16.4+50)/(CQT_PETM_CN[o]+15)
    ETP_M_PETM_CN[o] <- ETP_D_PETM_CN[o] * 30
    ETA_PETM_CN[o] <- CMP_mm_PETM_CN[o] / (sqrt(1 + (1 / (ETP_M_PETM_CN[o]/CMP_mm_PETM_CN[o]))^2))
    EPS_PETM_CN[o] <- 0 + EPSmax_PETM_CN[o] * (ETA_PETM_CN[o]/CMP_mm_PETM_CN[o])
    DIFC_PETM_CN[o] <- EPS_PETM_CN[o]*p_PETM_CN[o]*0.14
    DIFC13_PETM_CN[o] <- DIFC_PETM_CN[o] * (1/1.004443)
    S_PETM_CN[o] <- 0.21*28.26^2/(28.26*(pCO2_PETM+25))^2
    W_PETM_CN[o] <- (1.2*(MAP_PETM_CN+975))/(27.2+0.04*(MAP_PETM_CN+975))
    deltaP_PETM_CN[o] <- deltaA_PETM - (W_PETM_CN[o] - S_PETM_CN[o]*pCO2_PETM)
    deltaP_hat_PETM_CN[o] <- (deltaP_PETM_CN[o]/1000+1)*0.011237/(1+0.011237*(deltaP_PETM_CN[o]/1000+1))
    deltaA_hat_PETM_CN[o] <- (deltaA_PETM/1000+1)*0.011237/(1+0.011237*(deltaA_PETM/1000+1))
    z_100_PETM_CN[o] <- ifelse (z_PETM_CN[o]<100, z_PETM_CN[o], 100)
    dC_Soil_PETM_CN[o] <- (((((R_hourly_PETM_CN[o] / DIFC_PETM_CN[o]) * deltaP_hat_PETM_CN[o] * (100 * z_100_PETM_CN[o] - z_100_PETM_CN[o]^2 / 2) * (DIFC_PETM_CN[o] / DIFC13_PETM_CN[o])) + pCO2_PETM * deltaA_hat_PETM_CN[o]) / (R_hourly_PETM_CN[o] * (100 * z_100_PETM_CN[o]-z_100_PETM_CN[o]^2 / 2) * (1 - DIFC_PETM_CN[o] * deltaP_hat_PETM_CN[o] / DIFC13_PETM_CN[o]) / DIFC_PETM_CN[o] + pCO2_PETM * (1 - deltaA_hat_PETM_CN[o]))) / 0.011237 - 1) * 1000
    A_CO2_Carb_PETM_CN[o] <- 2.71828 ^ (-2.988e3 / CQT_K_PETM_CN[o] ^ 2 + 7.6663 / CQT_K_PETM_CN[o] - 0.0024612)
    R_Soil_PETM_CN[o] = (dC_Soil_PETM_CN[o]/1000 + 1)*0.0020672
    R_Carb_PETM_CN[o] <- R_Soil_PETM_CN[o] / A_CO2_Carb_PETM_CN[o]
    A_O_PETM_CN[o] <- 1/2.71828^((2.78e6 / CQT_K_PETM_CN[o] ^ 2 - 2.89)/1000)
    E_PETM_CN[o] <- ETA_PETM_CN[o]*0.3
    E_s_PETM_CN[o] <- E_PETM_CN[o] * (1/2.592e6) * 0.001
    DIFO_PETM_CN[o] <- EPS_PETM_CN[o] * p_PETM_CN[o] * 2.3e-9
    z_i_PETM_CN[o] <- DIFO_PETM_CN[o]/E_s_PETM_CN[o]
    DRF_PETM_CN[o] <- 1 + 0.6*(1/0.9723 - 1)
    R_O_surface_PETM_CN[o] <- ((1-h_PETM_CN[o]) * DRF_PETM_CN[o] * R_O_P_PETM_CN[o] + h_PETM_CN[o] * R_O_atm_PETM_CN[o]) / (1/A_atmP_PETM_CN[o])
    R_O_soil_PETM_CN[o] <- ((R_O_surface_PETM_CN[o] - R_O_P_PETM_CN[o]) * 2.71828 ^ (-z_m_PETM_CN[o]/z_i_PETM_CN[o])) + R_O_P_PETM_CN[o]
    dO_soil_PETM_CN[o] <- (R_O_soil_PETM_CN[o]/0.0020052 - 1)*1000
    dO_surface_PETM_CN[o] <- (R_O_surface_PETM_CN[o]/0.0020052 - 1)*1000
    R_O_Carb_PETM_CN[o] <- R_O_soil_PETM_CN[o] / A_O_PETM_CN[o]
    
    dO_Carb_P_PETM_CN_m[o] <- (R_O_Carb_PETM_CN[o]/0.0020672 - 1) * 1000
    dC_Carb_PETM_CN_m[o] <- (R_Carb_PETM_CN[o]/0.0020672 - 1) * 1000
    
    dO_Carb_P_PETM_CN[o] ~ dnorm(dO_Carb_P_PETM_CN_m[o], 400)
    dC_Carb_PETM_CN[o] ~ dnorm(dC_Carb_PETM_CN_m[o], 400) 
    
  }
  
  
  MAT_PETM_BB <- MAT_Bkg_BB + ch_MAT
  MAT_PETM_AC <- MAT_Bkg_AC + ch_MAT
  MAT_PETM_ES <- MAT_Bkg_ES + ch_MAT
  MAT_PETM_CN <- MAT_Bkg_CN + ch_MAT
  
  pCO2_PETM <- pCO2_Bkg * ((ch_MAT / Sens) * 2)
  deltaA_PETM <- deltaA_Bkg - 4.6
  
  pCO2_Bkg ~ dunif(300, 1000)
  deltaA_Bkg ~ dunif(-5.5, -4.5)
  
  MAP_Bkg_BB ~ dunif(200, 3000)
  MAP_Bkg_AC ~ dunif(200, 3000)
  MAP_Bkg_ES ~ dunif(200, 3000)
  MAP_Bkg_CN ~ dunif(200, 3000)
  
  P_seas_Bkg_BB ~ dunif(0.001, 0.4)
  P_seas_Bkg_AC ~ dunif(0.001, 0.4)
  P_seas_Bkg_ES ~ dunif(0.001, 0.4)
  P_seas_Bkg_CN ~ dunif(0.001, 0.4)
  
  MAT_Bkg_BB ~ dunif(5, 40)
  MAT_Bkg_AC ~ dunif(5, 40)
  MAT_Bkg_ES ~ dunif(5, 40)
  MAT_Bkg_CN ~ dunif(5, 40)
  
  T_seas_Bkg_BB ~ dunif(5, 40)
  T_seas_Bkg_AC ~ dunif(5, 40)
  T_seas_Bkg_ES ~ dunif(5, 40)
  T_seas_Bkg_CN ~ dunif(5, 40)
  
  ST_Bkg_BB ~ dunif(0, 1)
  ST_Bkg_AC ~ dunif(0, 1)
  ST_Bkg_ES ~ dunif(0, 1)
  ST_Bkg_CN ~ dunif(0, 1)
  
  Sens ~ dunif(1, 4)
  ch_MAT ~ dunif(5, 8)
  
  MAP_PETM_BB ~ dunif(200, 3000)
  MAP_PETM_AC ~ dunif(200, 3000)
  MAP_PETM_ES ~ dunif(200, 3000)
  MAP_PETM_CN ~ dunif(200, 3000)
  
  P_seas_PETM_BB ~ dunif(0.001, 0.4)
  P_seas_PETM_AC ~ dunif(0.001, 0.4)
  P_seas_PETM_ES ~ dunif(0.001, 0.4)
  P_seas_PETM_CN ~ dunif(0.001, 0.4)
  
  T_seas_PETM_BB ~ dunif(5, 40)
  T_seas_PETM_AC ~ dunif(5, 40)
  T_seas_PETM_ES ~ dunif(5, 40)
  T_seas_PETM_CN ~ dunif(5, 40)
  
  ST_PETM_BB ~ dunif(0, 1)
  ST_PETM_AC ~ dunif(0, 1)
  ST_PETM_ES ~ dunif(0, 1)
  ST_PETM_CN ~ dunif(0, 1)
  
}



# Model Fitting

bayes_fit <- jags(model.file = bayes_model, parameters.to.save = parameters, data = carbonate_data, inits = inits, 
                  n.chains=4, n.iter = 10000000, n.burnin = 100000, n.thin = 10000
)

# update if there is little convergence

bayes_fit_upd <- autojags(bayes_fit, n.iter=1000000, n.thin=10000, n.update = 3)


print(bayes_fit)
print(bayes_fit_upd)

bayes_fit_mcmc <- as.mcmc(bayes_fit)

bayes_fit_mcmc_upd <- as.mcmc(bayes_fit_upd)

denplot(bayes_fit_mcmc_upd, "T_seas_Bkg_BB")
denplot(bayes_fit_mcmc_upd, "T_seas_PETM_BB")
denplot(bayes_fit_mcmc_upd, "MAP_Bkg_BB")
denplot(bayes_fit_mcmc_upd, "MAP_PETM_BB")
denplot(bayes_fit_mcmc_upd, "ST_Bkg_BB")
denplot(bayes_fit_mcmc_upd, "MAT_Bkg_BB")
denplot(bayes_fit_mcmc_upd, "pCO2_Bkg")
denplot(bayes_fit_mcmc_upd, "pCO2_PETM")
denplot(bayes_fit_mcmc_upd, "Sens")
denplot(bayes_fit_mcmc_upd, "MAT_Bkg_CN")
denplot(bayes_fit_mcmc_upd, "MAP_PETM_ES")

denplot(bayes_fit_mcmc, "T_seas_Bkg_BB")
denplot(bayes_fit_mcmc, "T_seas_PETM_BB")
denplot(bayes_fit_mcmc, "MAP_Bkg_BB")

