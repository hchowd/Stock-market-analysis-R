#install.packages("quantmod") used to download Quantmod package, which is used 
#to develop, test and deploy statistical trading models.
library(quantmod)
#downloading 10 years worth of stock market data. Using google finance.
getSymbols('SPY', src='google')
SPY
View(SPY)
head(SPY)
tail(SPY)
class(SPY)
summary(SPY)
basket_symbols<-c('BABA', 'AAPL', 'AMZN', 'TSLA', 'SPY')
getSymbols(basket_symbols, src='google')
tail(BABA)
tail(AMZN)
tail(TSLA)
combine<-data.frame(as.xts(merge(BABA, AAPL, AMZN, TSLA, SPY)))
class(combine)
head(combine,5) 
tail(combine,3)
chartSeries(combine, name ="Basket trends",type="matchsticks", theme="white", 
            TA=NULL, subset="2017::")
#installed TTR package in order to perform technical analysis on the datasets
library(TTR)

chartSeries(SPY, theme='white')
addSMA(n=20)
addROC(n=20)
chartSeries(SPY, theme="white", TA="addVo();addBBands();addCCI()", subset='2017')

#various types of charts can be created in R to visualise and analyse data
lineChart(SPY, line.type='h', theme="white")
barChart(SPY, bar.type='hlc', theme="black", TA=NULL)
candleChart(SPY, subset="2015")
chartSeries(SPY, type="candlesticks", subset="2017-06")
chartSeries(SPY, type="matchsticks", subset="2017-06")
chartSeries(SPY, theme=chartTheme('white'), up.col="blue",
            dn.col="black")
#the following block of code will work on SPY only if it is of class "xts","zoo"
#not if it's of class data.frame
SPY.EMA.20<- EMA(SPY$SPY.Close, n=20)
SPY.EMA.100<- EMA(SPY$SPY.Close, n=100)
addTA(SPY.EMA.20, on=1, col = "red")
addTA(SPY.EMA.100, on=1, col = "blue")
addTA(SPY.EMA.20 - SPY.EMA.100,col='blue', type='h',legend="20-100 MA")


#creating own moving averages instead of using preloaded ones from TTR package
#getSymbols(c('QQQ'), src='google')
plot(QQQ$QQQ.Close)
period <- 100
price_vector <- QQQ$QQQ.Close
moving_average_vector <- c()
for (ind in seq(period,(length(price_vector))) ){
  moving_average_vector <- c(moving_average_vector, mean(price_vector[(ind-period):ind]))
}

par(mfrow=c(2,1))
plot(QQQ$QQQ.Close)
plot(moving_average_vector, type='l', col='red', lwd=3, main = paste('SMA', period))

length(moving_average_vector)



#multiple moving averages
library(binhf)
chartSeries(SPY, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")
SPY.EMA.10 <- EMA(SPY$SPY.Close, n=10) 
SPY.EMA.50 <- EMA(SPY$SPY.Close, n=50) 
SPY.EMA.200 <- EMA(SPY$SPY.Close, n=200) 
Fast.Diff <- SPY.EMA.10 - SPY.EMA.50
Slow.Diff <- SPY.EMA.50 - SPY.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA")
EMA.Fast <- EMA(SPY$SPY.Close, n=10) 
EMA.Medium <- EMA(SPY$SPY.Close, n=50) 
EMA.Slow <- EMA(SPY$SPY.Close, n=200) 
Fast.Diff <- EMA.Fast - EMA.Medium
Slow.Diff <- EMA.Medium - EMA.Slow

# look for long entries
Long_Trades <- ifelse(
  Slow.Diff  > 0 &
    Fast.Diff  > 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") < 0, SPY$SPY.Close, NA)

# look for long exits (same thing but inverse signts)
Short_Trades <- ifelse(
  Slow.Diff  < 0 &
    Fast.Diff  < 0 &
    shift(v=as.numeric(Fast.Diff), places=1, dir="right") > 0, SPY$SPY.Close, NA)
plot(SPY$SPY.Close, col="black")
## Warning in plot.xts(SPY): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1.5, pch=18)
points(Short_Trades, col='red', cex=1.5, pch=18)



#tracking profit and loss
ProfitLoss_Calculator <- function(objDF) {
  # make column names generic so they can handle any symbol
  colnames(objDF) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Long_Trades", "Short_Trades")
  
  current_long <- 0
  current_short <- 0
  
  for (ind in seq(1,nrow(objDF))) {
    if (objDF$Long_Trades[ind] !=0) {
      # first trade should be an entry, last trade an exit
      if ((current_long==0 & objDF$Long_Trades[ind] > 0) | (current_long !=0)) {
        # next trade should be opposite sign of previous trade (entry -> exit)
        if (sign(objDF$Long_Trades[ind]) != sign(current_long)) {
          current_long <- as.numeric(objDF$Long_Trades[ind])
          print(paste('Long', current_long))
        }
      }
      if (current_long != as.numeric(objDF$Long_Trades[ind]))
        objDF$Long_Trades[ind] <- 0
    }
    if (objDF$Short_Trades[ind] !=0) {
      # first trade should be an entry
      if ((current_short==0 & objDF$Short_Trades[ind] > 0) | (current_short !=0)) {
        # next trade should be opposite sign of previous trade (entry -> exit)
        if (sign(objDF$Short_Trades[ind]) != sign(current_short)) {
          current_short <- as.numeric(objDF$Short_Trades[ind])
          print(paste('Short', current_short))
        }
      }
      if (current_short != as.numeric(objDF$Short_Trades[ind]))
        objDF$Short_Trades[ind] <- 0
    }
  }
  
  # trim to be even, if not add last close held in chart
  if ((!length(objDF$Long_Trades[objDF$Long_Trades != 0])%% 2) == 0)
    objDF$Long_Trades[length(objDF$Close)] <- -1 * objDF$Close[length(objDF$Close)]
  if ((!length(objDF$Short_Trades[objDF$Short_Trades != 0])%% 2) == 0)
    objDF$Short_Trades[length(objDF$Close)] <- -1 * objDF$Close[length(objDF$Close)]
  
  print(paste('Final Longs:',round(sum(objDF$Long_Trades * -1),2)))
  print(paste('Final Shorts:',round(sum(objDF$Short_Trades),2)))
  
  
  # plot trade entries and exits
  par(mfrow=c(2,1))
  plot(objDF$Close, main='Long Trades')
  points(ifelse(objDF$Long_Trades > 0, objDF$Long_Trades, NA), col='green', cex=1.5, pch=16)
  points(ifelse(objDF$Long_Trades < 0, objDF$Long_Trades * -1, NA), col='red', cex=1.5, pch=15)
  
  #plot(objDF$Close, main='Short Trades')
  #points(ifelse(objDF$Short_Trades > 0, objDF$Short_Trades, NA), col='green', cex=1.5, pch=16)
  #points(ifelse(objDF$Short_Trades < 0, objDF$Short_Trades * -1, NA), col='red', cex=1.5, pch=15)
}


#getSymbols(c('SPY'), src='google')
## [1] "SPY"
# remove any NAs 
SPY <- SPY[!(rowSums(is.na(SPY))),]

SMA.Fast <- SMA(SPY$SPY.Close, n=20)
SMA.Medium <- SMA(SPY$SPY.Close, n=100) 
SMA.Slow <- SMA(SPY$SPY.Close, n=200) 
fast_detrend_ma <- SMA.Fast - SMA.Medium
slow_detrend_ma <- SMA.Medium - SMA.Slow

# look for long entries
SPY$Long_Trades <- ifelse(
  slow_detrend_ma  > 0 &
    fast_detrend_ma  > 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") < 0, SPY$SPY.Close, NA)
# exits for longs
SPY$Long_Trades <- ifelse(fast_detrend_ma  < 0, -1 * SPY$SPY.Close, SPY$Long_Trades)
SPY$Long_Trades[is.na(SPY$Long_Trades)] <- 0

# look for short entries
SPY$Short_Trades <- ifelse(
  slow_detrend_ma  < 0 &
    fast_detrend_ma < 0 &
    shift(v=as.numeric(fast_detrend_ma), places=1, dir="right") > 0, SPY$SPY.Close, NA)
# exits for longs
SPY$Short_Trades <- ifelse(fast_detrend_ma  > 0, -1 * SPY$SPY.Close, SPY$Short_Trades)
SPY$Short_Trades[is.na(SPY$Short_Trades)] <- 0

names(SPY)
ProfitLoss_Calculator(SPY)



#counter trends

SPY <- SPY[!(rowSums(is.na(SPY))),]
chartSeries(SPY, theme="white", TA="addRSI(n=100);addCCI(n=100);addROC(n=100)")
chartSeries(SPY, theme="white", TA="addRSI(n=100);addChVol(n=100);")

#plotting slow EMA on S&P 500
SPY.EMA.50 <- EMA(SPY$SPY.Close, n=50) 
SPY.EMA.200 <- EMA(SPY$SPY.Close, n=200) 
Slow.Diff <- SPY.EMA.50 - SPY.EMA.200

RSI.Fast <- RSI(price=SPY$SPY.Close,n=10)
RSI.Slow <- RSI(price=SPY$SPY.Close,n=30)
RSI.Diff <- RSI.Fast-RSI.Slow

CV.IND <- chaikinVolatility(HL=SPY, n=100)

# look for long entries
Long_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  < 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") > 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") < 0  &
    Slow.Diff > 0, SPY$SPY.Close, NA)

# look for short entries
Short_Trades <- ifelse(
  CV.IND < -0.1 &
    RSI.Diff  > 0 &
    shift(v=as.numeric(RSI.Diff ), places=1, dir="right") < 0  &
    shift(v=as.numeric(RSI.Diff ), places=2, dir="right") > 0  &
    Slow.Diff < 0, SPY$SPY.Close, NA)

plot(SPY$SPY.Close, main='RSI')
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)



#common indicators ADX, volume based indicators
QQQ <- QQQ[!(rowSums(is.na(QQQ))),]
SPY <- SPY[!(rowSums(is.na(SPY))),]

library(TTR)
chartSeries(QQQ, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')

chartSeries(SPY, theme="white", TA="addSMA(50, col='black');addSMA(200, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)", subset='2013::')

VWAP.Slow <- VWAP(price=SPY$SPY.Close, volume=SPY$SPY.Volume, n=100)
VWAP.Fast <- VWAP(price=SPY$SPY.Close, volume=SPY$SPY.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(SPY, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');
            addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')")
VWAP.Slow <- VWAP(price=QQQ$QQQ.Close, volume=QQQ$QQQ.Volume, n=100)
VWAP.Fast <- VWAP(price=QQQ$QQQ.Close, volume=QQQ$QQQ.Volume, n=20)
VWAP.Diff <- VWAP.Fast- VWAP.Slow

chartSeries(QQQ, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');
            addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')")

chartSeries(QQQ, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');
           addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)")

ADX.20 <- ADX(QQQ,n=14)

# look for long entries
Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff> 0, QQQ$QQQ.Close, NA)

# look for long entries
Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff < 0, QQQ$QQQ.Close, NA)

plot(QQQ$QQQ.Close)
## Warning in plot.xts(QQQ): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)


chartSeries(SPY, theme="white", TA="addVo();addTA(VWAP.Slow, on=1, col='red');
      addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue');
            addADX(n = 14, maType='EMA', wilder=TRUE)")

ADX.20 <- ADX(SPY,n=14)

# look for long entries
Long_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff> 0, SPY$SPY.Close, NA)

# look for long entries
Short_Trades <- ifelse(
  ADX.20$ADX > 20 &
    VWAP.Diff < 0, SPY$SPY.Close, NA)

plot(SPY$SPY.Close)
## Warning in plot.xts(SPY): only the univariate series will be plotted
points(Long_Trades, col='blue', cex=1, pch=18)
points(Short_Trades, col='red', cex=1, pch=18)
