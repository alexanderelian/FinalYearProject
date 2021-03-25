source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/strat.R') 

numOfDays <- 1000
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#adxSeq <- seq(from=15,to=65,by=5)
adxSeq <- seq(from=10,to=50,by=5)
emvSeq  <- seq(from=0,to=1,by=0.2) 
sdSeq <- seq(from=0.5, to=2.5, by=0.5)
paramsList  <- list(adxSeq,sdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=(length(adxSeq)),ncol=length(emvSeq))
#colnames(resultsMatrix) <- c("0","0.1","0.2","0.3","0.4","0.5")
pfolioPnLList <- vector(mode="list",length=numberComb)
count <- 1
for (x in 1:length(adxSeq)) {
    print(x)
    for (i in 1:length(sdSeq)) {
    #for (i in 1:length(emvSeq)) {
        print(i)
        #params <- list(lookback=lb,sdParam=sdp,series=1:4,posSizes=rep(1,10))
        params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.4,emvShort=-0.4,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=5,series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
        
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        #resultsMatrix[count,] <- c(ad,em,pfolioPnL$fitAgg)
        if(pfolioPnL$fitAgg < 0) {
            resultsMatrix[x,i] <- -0.1
        } else {
        resultsMatrix[x,i] <- pfolioPnL$fitAgg
        }
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        #print(resultsMatrix[count,])
        print(resultsMatrix[x,])
        count <- count + 1
    }
}
print(resultsMatrix)
#print(length(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),]))

# print(test)
# 
# test[1,1] <- resultsMatrix[1,3]
# test[1,2] <- resultsMatrix[2,3]
# test[2,1] <- resultsMatrix[3,3]
# test[2,2] <- resultsMatrix[4,3]
# test[3,1] <- resultsMatrix[5,3]
# test[3,2] <- resultsMatrix[6,3]
# print(test)
# 
library(reshape2)
p <- ggplot(melt(resultsMatrix),aes(x=Var1, y=Var2, fill=value))
p <- p + scale_y_discrete(name="standard deviation", limits = c("0.5","1","1.5","2","2.5"))
p <- p + scale_x_discrete(name="adx", limits = c("10","15","20","25","30","35","40","45","50"))
p <- p + geom_tile()
plot(p)
print(resultsMatrix)
