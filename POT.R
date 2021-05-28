library(readr)
#DATA <- read_csv("DATA.csv")
DATA <- read_csv("Desktop/PROG1/DATA.csv")
#View(DATA)
library(xtable)
library(fBasics)
library(fExtremes)
#data handling
library(zoo)

data1= read.zoo(DATA, format = "%d/%m/%Y", headers = TRUE, stringsAsFactors=FALSE)
#View(data1)
###########Extract each spread value for individual currencies##################
GBP_GPD  <- (data1$GBP)
EUR_GPD  <- (data1$EUR)
USD_GPD  <-(data1$USD)
ETB_GPD  <- (data1$ETB)
KES_GPD  <- (data1$KES)
TZS_GPD  <- (data1$TZS)

#############mrl Plot for Each Currency#################
pdf(file = "~/Desktop/PROG1/g1.pdf")
mrlPlot(GBP_GPD, ci = 0.95, umin = mean(GBP_GPD), umax = max(GBP_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

pdf(file = "~/Desktop/PROG1/g2.pdf")
mrlPlot(EUR_GPD, ci = 0.95, umin = mean(EUR_GPD), umax = max(EUR_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

pdf(file = "~/Desktop/PROG1/g3.pdf")
mrlPlot(USD_GPD, ci = 0.95, umin = mean(USD_GPD), umax = max(USD_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

pdf(file = "~/Desktop/PROG1/g4.pdf")
mrlPlot(ETB_GPD, ci = 0.95, umin = mean(ETB_GPD), umax = max(ETB_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

pdf(file = "~/Desktop/PROG1/g5.pdf")
mrlPlot(KES_GPD, ci = 0.95, umin = mean(KES_GPD), umax = max(KES_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

pdf(file = "~/Desktop/PROG1/g6.pdf")
mrlPlot(TZS_GPD, ci = 0.95, umin = mean(TZS_GPD), umax = max(TZS_GPD), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)
dev.off()

x <- fit.gpd(EUR_GPD, threshold, est = "mle")
###############GDP FIT for each Currency but this didn't work ############

fit_GBP <- gpdFit(GBP_GPD, u = min(GBP_GPD), type = "mle")
fit_EUR <- gpdFit(EUR_GPD, u = min(EUR_GPD), type = "mle")
fit_USD <- gpdFit(USD_GPD, u = min(USD_GPD), type = "mle")
fit_ETB <- gpdFit(ETB_GPD, u = min(ETB_GPD), type = "pwm")
fit_KES <- gpdFit(KES_GPD, u = min(KES_GPD), type = "pwm")
fit_TZS <- gpdFit(TZS_GPD, u = min(TZS_GPD), type = "pwm")

fit_GBP
fit_EUR
fit_USD
fit_ETB
fit_KES
fit_TZS

summary(fit_GBP)
summary(fit_EUR)
summary(fit_USD)
summary(fit_ETB)
summary(fit_KES)
summary(fit_TZS)


################## Risk Measure for each currency #############################
Risk1 <- gpdRiskMeasures(fit_GBP, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
Risk2 <- gpdRiskMeasures(fit_EUR, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
Risk3 <- gpdRiskMeasures(fit_USD, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
Risk4 <- gpdRiskMeasures(fit_ETB, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
Risk5 <- gpdRiskMeasures(fit_KES, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))
Risk6 <- gpdRiskMeasures(fit_TZS, prob = c(0.99, 0.995, 0.999, 0.9995, 0.9999))

############### Examine Risk measure ##########################################
Risk1
Risk2
Risk3
Risk4
Risk5
Risk6


