library(readr)
DATA <- read_csv("Desktop/PROG1/DATA.csv")
#View(DATA)
########################Libraries###############################################
library(zoo)
library(xts)

############################change data to time series format###################
data1= read.zoo(DATA, format = "%d/%m/%Y", headers = TRUE, stringsAsFactors=FALSE)
#View(data1)
##############Plot data ########################################################
#plot.zoo(data1,col = "green")
#class(data1)

###########Extract each spread value for individual currencies##################
GBP  <- (data1$GBP)
EUR  <- (data1$EUR)
USD  <-(data1$USD)
ETB  <- (data1$ETB)
KES  <- (data1$KES)
TZS  <- (data1$TZS)

##########log difference########################################################
logGBP <- diff(log(GBP))
logEUR  <- diff(log(EUR))
logUSD  <-diff(log(USD))
logETB  <- diff(log(ETB))
logKES  <- diff(log(KES))
logTZS  <- diff(log(TZS))

###box jerkins##################################################################
#par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
#acf(logGBP,na.action = na.pass,main = "GBP")
#pacf(logGBP,na.action = na.pass,main = "GBP")

############GARCH (1,1) IMPLEMENTATION ##########

############load rugarch library################################################
library(rugarch)

############GARCH(1,1) model for Individual Currencies

gGBP <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
gEUR <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
gUSD <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
gETB <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
gKES <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")
gTZS <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)),distribution.model = "std")

#fit model
garch11_GBP <- ugarchfit(gGBP,data = logGBP)
garch11_EUR <- ugarchfit(gEUR,data = logEUR)
garch11_USD <- ugarchfit(gUSD,data = logUSD)
garch11_ETB <- ugarchfit(gETB,data = logETB)
garch11_KES <- ugarchfit(gKES,data = logKES)
garch11_TZS <- ugarchfit(gTZS,data = logTZS)



################volatility######################################################
volGBP = ts(garch11_GBP@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)
volEUR = ts(garch11_EUR@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)
volUSD = ts(garch11_USD@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)
volETB = ts(garch11_ETB@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)
volKES = ts(garch11_KES@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)
volTZS = ts(garch11_TZS@fit$sigma^2,start= c(2012),end = c(2022),frequency = 12)

#volGBP
########plot volatility ########################################################
pdf(file = "~/Desktop/PROG1/v1.pdf")
plot(volGBP,main="",ylab="GBP",xlab="year",col="red")
dev.off()

pdf(file = "~/Desktop/PROG1/v2.pdf")
plot(volEUR,main="",ylab="EUR",xlab="year",col="blue")
dev.off()

pdf(file = "~/Desktop/PROG1/v3.pdf")
plot(volUSD,main="",ylab="USD",xlab="year",col="green")
dev.off()

pdf(file = "~/Desktop/PROG1/v4.pdf")
plot(volETB,main="",ylab="ETB",xlab="year",col="purple")
dev.off() 

pdf(file = "~/Desktop/PROG1/v5.pdf")
plot(volKES,main="",ylab="KES",xlab="year",col="grey")
dev.off()

pdf(file = "~/Desktop/PROG1/v6.pdf")
plot(volTZS,main="",ylab="TZS",xlab="year",col="orange")
dev.off()



#Display Parameter Estimates

garch11_GBP 
garch11_EUR 
garch11_USD 
garch11_ETB 
garch11_KES  
garch11_TZS 





#GET PARAMETER ESTIMATES IN A TABLE 
#examine coefficeints and model

fit <-garch11_TZS
library(texreg)
extract.rugarch <- function(fit, 
                            include.rsquared = TRUE, include.loglike = TRUE, include.aic = TRUE, include.bic = TRUE) {
  
  # extract coefficient table from fit:
  coefnames <- rownames(as.data.frame(fit@fit$coef))
  coefs <- fit@fit$coef
  se <- as.vector(fit@fit$matcoef[, c(2)])
  pvalues <-  as.vector(fit@fit$matcoef[, c(4)])       # numeric vector with p-values
  
  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    r2 <-  1 - (var(fit@fit$residuals) / var(y))
    gof <- c(gof, r2)
    gof.names <- c(gof.names, "R^2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglike == TRUE) {
    loglike <- fit@fit$LLH
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- infocriteria(fit)[c(1)]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  if (include.bic == TRUE) {
    bic <- infocriteria(fit)[c(2)]
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  # create texreg object:
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs,
    se = se,
    pvalues = pvalues, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}

#print table:
texreg(extract.rugarch(fit, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.

