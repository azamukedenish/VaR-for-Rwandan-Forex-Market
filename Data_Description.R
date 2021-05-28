library(readr)
DATA <- read_csv("DATA.csv")
#View(DATA)

#Libraries#
library(zoo)
library(xts)
library(xtable)

#########change data format to time series format
data1= read.zoo(DATA, format = "%d/%m/%Y", headers = TRUE, stringsAsFactors=FALSE)
#View(data1)

##########Summary of data

data_summary <- summary(DATA)
data_summary
##get table for latex
xtable(data_summary,digits = 7)

pdf(file = "image.pdf")
##########Plot data 
plot.zoo(data1,col = 6:1,main = "",xlab = "year", 
         ylab = c("USD","EUR","GBP","ETB","KES", "TZS"))
dev.off()

######Extract each spread value for individual currencies
GBP <-  (data1$GBP)
EUR <- (data1$EUR)
USD <-(data1$USD)
ETB <- (data1$ETB)
KES <- (data1$KES)
TZS <- (data1$TZS)

#############################log fifference

British_Pound <- na.omit(diff(log(GBP)))
European_Euro <- na.omit(diff(log(EUR)))
American_Dollar <- na.omit(diff(log(USD)))
Ethiopian_Birr <- na.omit(diff(log(ETB)))
Kenyan_Shilling <- na.omit(diff(log(KES)))
Tanzanian_Shilling <- na.omit(diff(log(TZS)))

###################Time series 
pdf("vola.pdf")
plot(cbind.zoo(British_Pound ,European_Euro,American_Dollar,Ethiopian_Birr ,Kenyan_Shilling ,Tanzanian_Shilling ),col=c(6:1),
     main="",xlab="Year",ylab = c("USD","EUR","GBP","ETB","KES", "TZS"))
dev.off()

