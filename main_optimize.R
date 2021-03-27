source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/strat.R') 



tmp <<- matrix(ncol= 0, nrow = 22)
print("tmp matrix created")


currentMax = 0
bestHolding = 0

numOfDays <- 1000
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#adxSeq <- seq(from=15,to=65,by=5)
adxSeq <- seq(from=25,to=45,by=5)
emvSeq  <- seq(from=0,to=1,by=0.2) 
sdSeq <- seq(from=1, to=2, by=0.5)
holdingLong <- seq(from=1, to=21, by=1)
paramsList  <- list(adxSeq,sdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=(length(adxSeq)),ncol=length(sdSeq))
#colnames(resultsMatrix) <- c("0","0.1","0.2","0.3","0.4","0.5")
pfolioPnLList <- vector(mode="list",length=numberComb)
count <- 1
for (x in 1:length(adxSeq)) {   #loop through first param
    print(x)
    for (i in 1:length(sdSeq)) {    #loop through first param
            print(i)
        
            tmp <<- matrix(ncol= 0, nrow = 22)  #aggreagte ROI

            # #params <- list(lookback=lb,sdParam=sdp,series=1:4,posSizes=rep(1,10))
            params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(0,0,0,0,0,0,0,0,0,0),shortHoldPeriod=c(0,0,0,0,0,0,0,0,0,0),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            # #params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=holdingLong[days],shortHoldPeriod=c(0,0,0,0,0,0,0,0,0,0),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            print(c("param sent is" ,params$sdParam[i]))
            results <- backtest(dataList, getOrders, params, sMult)

            print(tmp)
            avg <- matrix(ncol= 1, nrow = 22)
            for (a in 1:22){
                avg[a,1]<-(mean(tmp[a,]))    #get avg
            }
            avgExitDay <- which(avg == max(avg), arr.ind = TRUE)
            # #print(avg)
            print(paste("Exit at ", max(avg)," Which is day ", avgExitDay[1,1]-1,sep = ""))     #min of avg for shorting, as looking for a price drop, not price increase
            #
            if(currentMax < max(avg))   {bestHolding = avgExitDay[1,1]-1}
            
        
            BH <- bestHolding

            params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(0,0,0,0,0,0,0,0,0,0),shortHoldPeriod=c(BH,BH,BH,BH,BH,BH,BH,BH,BH,BH),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            #params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=holdingLong[days],shortHoldPeriod=c(0,0,0,0,0,0,0,0,0,0),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            #source('main.R')
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
p <- p + scale_y_discrete(name="standard deviation", limits = c("1","1.5","2"))
p <- p + scale_x_discrete(name="adx", limits = c("20","25","30","35","40","45"))
p <- p + geom_tile()
plot(p)
print(resultsMatrix)
