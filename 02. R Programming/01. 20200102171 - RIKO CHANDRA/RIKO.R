library(MASS)
library(car)
library(lmtest)
library(psych)
library(readxl)
library(nortest)
library(outliers)
library(tseries)

OLAH<- read_excel(path = "OLAH DATA/R/RIKO/OLAH.xlsx", col_names = TRUE)
DF <- OLAH[,!names(OLAH) %in% c("Kode Saham", "Tahun", "SQ_LEV","SQ_ROA","LN_SIZE",
                              "LN_CFO","LN_WCTT","LN_CH","LN_Q","SQ_CFO","SQ_WCTT","SQ_CH")]

sum(is.na(OLAH))

MODEL<- lm(Q~LEV+ROA+LN_SIZE+GROWTH+SQ_CFO+SQ_WCTT+LN_CH,OLAH)

describe(DF)

summary(MODEL)

shapiro.test(residuals(MODEL))

bptest(MODEL)

par(mfrow = c(2, 2))
plot(MODEL)

dwtest(MODEL, alternative = "two.sided")

vif(MODEL)