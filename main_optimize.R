source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/strat.R') 



tmp <<- matrix(ncol= 0, nrow = 22)
print("tmp matrix created in main_optimize")


currentMax = 0
bestHolding = 0

numOfDays <- 1000
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

#adxSeq <- seq(from=15,to=65,by=5)
adxSeq <- seq(from=20,to=40,by=5)
emvSeq  <- seq(from=0,to=1,by=0.2) 
sdSeq <- seq(from=1, to=2, by=0.5)
holdingLong <- seq(from=1, to=21, by=1)
paramsList  <- list(adxSeq,sdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=(length(adxSeq)),ncol=length(sdSeq))
holdingDaysMatrix <- matrix(nrow=(length(adxSeq)),ncol=length(sdSeq))
positionTakenMatrix <- matrix(nrow=(length(adxSeq)),ncol=length(sdSeq))
ROIResults <- matrix(nrow=(length(adxSeq)),ncol=length(sdSeq))

#colnames(resultsMatrix) <- c("0","0.1","0.2","0.3","0.4","0.5")
pfolioPnLList <- vector(mode="list",length=numberComb)
count <- 1

for (x in 1:length(adxSeq)) {   #loop through first param
    for (i in 1:length(sdSeq)) {    #loop through first param
        
            tmp <<- matrix(ncol= 0, nrow = 22)  #aggreagte ROI for every holding day
                #Hold Periods do not matter, as we enter then plot the next 21 days...
            params <- list(lookback=50,lookbackShort=34,MOsdParam=sdSeq[i],sdParamShort=1,adx=14,MOadxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(0,0,0,0,0,0,0,0,0,0),shortHoldPeriod=c(0,0,0,0,0,0,0,0,0,0),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            results <- backtest(dataList, getOrders, params, sMult)
            
            avg <- matrix(ncol= 1, nrow = 22)
            for (a in 1:22){
                avg[a,1]<-(mean(tmp[a,]))    #get avg
            }
            avgExitDay <- which(avg == max(avg), arr.ind = TRUE)
 
            print(paste("Exit at ", max(avg)," Which is day ", avgExitDay[1,1]-1,sep = ""))  
            BH <- as.integer(avgExitDay[1,1]-1) #minus one as the day 1 is 0 days.
            
            print("plotting with params:")
            print(c("adx : ", adxSeq[x]))
            print(c("sd : ", sdSeq[i]))
            print(c("Holding : ", BH))
            
            #First for short, as holding for long is 0, and holding for every short is the optimal BH
            #Vise versa for the second line...
            #params <- list(lookback=50,lookbackShort=34,MOsdParam=sdSeq[i],sdParamShort=1,adx=14,MOadxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=0,shortHoldPeriod=c(BH,BH,BH,BH,BH,BH,BH,BH,BH,BH),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            params <- list(lookback=50,lookbackShort=34,MOsdParam=sdSeq[i],sdParamShort=1,adx=14,MOadxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(BH,BH,BH,BH,BH,BH,BH,BH,BH,BH),shortHoldPeriod=0,series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            #params <- list(lookback=50,lookbackShort=34,sdParam=sdSeq[i],sdParamShort=1,adx=14,adxSignal=adxSeq[x],emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=holdingLong[days],shortHoldPeriod=c(0,0,0,0,0,0,0,0,0,0),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
            results <- backtest(dataList, getOrders, params, sMult)
            
            #print(names(results))
            holdingDaysMatrix[x,i] <- BH
            positionTakenMatrix[x,i] <- results$k
            ROIResults[x,i] <- max(avg)
            
            
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
print("Optimal Holding Periods (ROI)")
print(holdingDaysMatrix)
print("Positions Taken")
print(positionTakenMatrix)



print("PD Matrix")
print(resultsMatrix)
library(reshape2)
p <- ggplot(melt(resultsMatrix),aes(x=Var1, y=Var2, fill=value))
p <- p + labs(title= "PD Fitness")
p <- p + scale_y_discrete(name="standard deviation", limits = c("1","1.5","2"))
p <- p + scale_x_discrete(name="adx", limits = c("20","25","30","35","40"))
p <- p + geom_tile()
#plot(p)


print("ROI Matrix")
print(ROIResults)
q <- ggplot(melt(ROIResults),aes(x=Var1, y=Var2, fill=value))
q <- q + labs(title= "ROI Fitness")
q <- q + scale_y_discrete(name="standard deviation", limits = c("1","1.5","2"))
q <- q + scale_x_discrete(name="adx", limits = c("20","25","30","35","40"))
q <- q + geom_tile()
#plot(q)

grid.arrange(p,q)

