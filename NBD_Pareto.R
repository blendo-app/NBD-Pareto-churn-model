######################
#prerequisite: Load Data in variable 'data' as an event log of the form:
# <email_address> | <open action timestamp>
#------------------------------------------
# aaaaa@gg.com   | 2013-03-02
# bbbbb@hh.com  |  2011-02-04
#####################
library(reshape)
library(BTYD)
library(ggplot2)
library(plyr)
library(lubridate)
#assign ids to customers if they don't exist yet
data$id<-NA
data$id[1]<-1
j<-nrow(data)
id <- numeric(nrow(data))
id[1]<-1
for(i in 2:j){
  print(i)
  if (data$email_address[i]==data$email_address[i-1]){
    id[i]<-id[i-1]
  }else{
    id[i]<-1+id[i-1]
  }
}
data$id<-id
data$timestamp<-as.POSIXct(data$timestamp)


elog<-as.data.frame(cbind(cust=data$id, date=data$timestamp))
elog$date<- as.Date(as.character(trunc(as.POSIXct(elog$date, origin="1970-01-01"), "day")))



elog <- dc.MergeTransactionsOnSameDate(elog)
head(elog)
summary(elog)  # no NAs


# exploring data
purchaseFreq <- ddply(elog, .(cust), summarize, 
                      daysBetween = as.numeric(diff(date)))

ggplot(purchaseFreq,aes(x=daysBetween))+
  geom_histogram(fill="orange")+
  xlab("Time between opens (days)")+
  theme_minimal()

elog$date<-as.POSIXct(elog$date)
# splitting data
(end.of.cal.period <- min(elog$date)+as.numeric((max(elog$date)-min(elog$date))/2))
data <- dc.ElogToCbsCbt(elog, per="week", 
                        T.cal=end.of.cal.period,
                        statistic = "freq") 

cal2.cbs <- as.matrix(data[[1]][[1]])
str(cal2.cbs)

#Parameter estimation

(params2 <- pnbd.EstimateParameters(cal2.cbs))
(LL <- pnbd.cbs.LL(params2, cal2.cbs))
# it is a good idea to make a few more estimation to see if the converge
p.matrix <- c(params2, LL)
for (i in 1:5) {
  params2 <- pnbd.EstimateParameters(cal2.cbs, params2)
  LL <- pnbd.cbs.LL(params2, cal2.cbs)
  p.matrix.row <- c(params2, LL)
  p.matrix <- rbind(p.matrix, p.matrix.row)
}

p.matrix
(params2 <- p.matrix[dim(p.matrix)[1],1:4])

param.names <- c("r", "alpha", "s", "beta")

LL <- pnbd.cbs.LL(params2, cal2.cbs)
#controu plots
dc.PlotLogLikelihoodContours(pnbd.cbs.LL, params2, cal.cbs = cal2.cbs , n.divs = 5,
                             num.contour.lines = 7, zoom.percent = 0.3,
                             allow.neg.params = FALSE, param.names = param.names)
#heterogeneity of open
pnbd.PlotTransactionRateHeterogeneity(params2, lim = NULL)
#heterogeneity of drop out
pnbd.PlotDropoutRateHeterogeneity(params2)

#individual predicitions - 1 (or 52 weeks) year period - new customer
pnbd.Expectation(params2, t = 52)

#individual predictions - 1 year period - existing customer
cal2.cbs["16",]
x <- cal2.cbs["16", "x"]         
t.x <- cal2.cbs["16", "t.x"]     
T.cal <- cal2.cbs["16", "T.cal"]
pnbd.ConditionalExpectedTransactions(params2, T.star = 52, 
                                     x, t.x, T.cal)

#probabilities of customers being alive

x          
t.x        
#end of calibration
T.cal <- 59
pnbd.PAlive(params2, x, t.x, T.cal)
params3 <- pnbd.EstimateParameters(cal2.cbs)
p.alives <- pnbd.PAlive(params3, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])
ggplot(as.data.frame(p.alives),aes(x=p.alives))+
  geom_histogram(colour="grey",fill="orange")+
  ylab("Number of Customers")+
  xlab("Probability Customer is 'Live'")+
  theme_minimal()
pnbd.PlotFrequencyInCalibration(params2, cal2.cbs, 
                                censor=7, title="Model vs. Reality during Calibration")
#assess model in holdout period
x.star   <- data[[2]][[2]][,1]
cal2.cbs <- cbind(cal2.cbs, x.star)
str(cal2.cbs)

holdoutdates <- attributes(data[[2]][[1]])[[2]][[2]]
holdoutlength <- round(as.numeric(max(as.Date(holdoutdates))-
                                    min(as.Date(holdoutdates)))/7)

T.star <- holdoutlength
censor <- 7 
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params2, T.star,
                                                    cal2.cbs, x.star, censor)

