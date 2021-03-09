#Ichi Package

#Create First Strategy using BBands
#library(IKTrading)
#require(devtools)
#install.packages("quantstrat", repos = "http://R-Forge.R-project.org")
#library(quantstrat) #Load package
#install_github("IlyaKipnis/IKTrading")

#require(IKTrading)


maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

smaDF <- data.frame() #SMA Data Frame
count <- 0
getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    
    #for (i in 1:length(params$series)) { #All Series
    for (i in 1:1) {  #One Series
      #Get Data
      cl <- newRowList[[params$series[i]]]$Close
      vol <- newRowList[[params$series[i]]]$Volume
      hi <- newRowList[[params$series[i]]]$High 
      low <- newRowList[[params$series[i]]]$Low 
      #print(is.xts(store$cl[startIndex:store$iter,i]))
      
      #Get from store Tester
      #high <- store$hi[startIndex:store$iter,i]
      #print(high)
      #low <- store$low[startIndex:store$iter,i]
      #print(low)
      #open <- store$op[startIndex:store$iter,i]
      #print(open)
      
      ##Data Frames
      #High/Low/Close Data Frame
      HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                       Low = store$low[startIndex:store$iter,i],
                       Close = store$cl[startIndex:store$iter])
      #High/Low Data Frame
      HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                          Low = store$low[startIndex:store$iter,i])
      
      
      
      #####Momentum Strategy Indicators#####
      
      ###ADX INDICATOR
      adx <- last(ADX(HLCdf,n=params$adx))
      #adxTester <- (ADX(HLCdf))
      #adx <- (ADX(HLCdf,n=params$adx))
      #print(adx)
      #print(adx[,4])
      
    #  Test Opportunity 
    #  if (adx[,4] > 20){
    #    print("Signal Created")
    #  }
      
      ##Stoch Oscil
      #stocOsc <- (stoch(HLCdf))
      stocOsc <- last(stoch(HLCdf),2) #Get the last 2 for crossover testing
      #print(stocOsc[1,])
      
    #  Test Opportunity 
    #  if ((stocOsc[1,2] < stocOsc[1,3]) && (stocOsc[2,2] > stocOsc[2,3])){ #FAST D CROSS ABOVE SLOW D
    #    print("Signal Created")
    #  }
      
      #SMI
      #smi <- SMI(HLCdf[,2]) #signal
      smi <- last(SMI(HLCdf))
      #print(smi[,2])
      
    #  Test Opportunity 
    #  if(smi[,1] > 40){    #value
    #   print("Signal Created")
    #  }
      
      #Ease of Movement
      #emv <- (EMV(HLdf,store$vol[startIndex:store$iter,i]))
      emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i])) #(OUTPUT is repeated and ammended to the end)
      #print(emv[,1]) ##[,1] emv
      #print(emv[,2]) ##[,2] maEMV
      
    #  Test Opportunity 
    #  if (emv[,1] > 0.2) {
    #     print("Signal Created")
    #  }
      
      
      ####ENTER THE MARKET 
      #  Test enter 
      #  if (emv[,1] > 0.2) {
      #     print("Entered Trade Opportunity!")
      #      pos[params$series[i]] <- params$posSizes[params$series[i]]
      #  }


      
      ##EXIT THE TRADE
      if (emv[,1] < 0.25) { #EMV shows that there is a slow in prices ability to move upwards
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
        #print("Exit (EMV) Trade Opportunity!")
      }
      if (adx[,4] < 25){    #ADX shows that the momentum is now weak/slowing down
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
        #print("Exit (ADX) Trade Opportunity!")
      }
      
  
      ####30 and 0.5 can be parameterised and then optimised....
      
      #if (emv[,1] > 1.5){ ###need to get the laat one, as its repeated every lookback period + 1
      #  print("Entry")
      #} else if(emv[,1] < -1.5){
      #  print("Exit")
      #}
      
      
      
      #####Mean Reversion Strategy Indicators#####
      #pdf(file = "C:\Users\alexe\OneDrive\Desktop\Plot.pdf",
      #    width = 4, # The width of the plot in inches
      #    height = 4) # The height of the plot in inches
      #plot(smaResult)
      #dev.off()
      
      ###BBands
      bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$sdParam))
      #print(bbands)
      
      ###TRADE USING BBANDS BELOW
      
      ###Averages
      smaShort <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort))
      #print(smaShort)
      
      sma <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookback))
      #sma <- (SMA(store$cl[startIndex:store$iter,i],n=params$lookback))
      #print(sma)
      
      ema <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
      #macd <- na.omit(ema)
      #print(ema)
      
      #VWAP
      vwap <- last(VWAP(store$cl[startIndex:store$iter,i], store$vol[startIndex:store$iter,i], 1))
      #print(vwap)
      
      ###MACD (Need to understand the output)
      macd <- MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE )
      #macd <- MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, maType="EMA" )
      #macd <- na.omit(macd)
      #print(macd)
      
      
      
      
      #####Market Making Strategy Indicators#####
      
      ###Ichimoku Cloud
      ####Need the package to access this function
      #ichi <- ichimoku(HLCdf, nFast = 9, nMed = 26, nSlow = 52)
      #print(ichi)
      
      ###OBV (Need volume Store)
      obv <- OBV(store$cl[startIndex:store$iter,i],store$vol[startIndex:store$iter,i])
      #print(obv)
      
      ###RSI (Understand the output)
      #########fixxxx and understand to applyu to the rest of it
      
      #startIndex <- store$iter - params$rsi - 1 # needs 2 extra periods
      #rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi)) 
      
      rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi))
      #rsi <- na.omit(rsi)
      #print(rsi)
      

      # readline(prompt="Press [enter] to continue")
      


      ##HOW TO ACT IS BELOW...
      #Mean Reversion#
      
      #lets trade on the bbands
      #if (cl < bbands[,"dn"]) {
      #  # if close is relatively low go long (i.e., Mean Reversion)
      #  pos[params$series[i]] <- params$posSizes[params$series[i]]
      #}
      #else if (cl > bbands[,"up"]) {
      #  # if close is relatively high go short (again, Mean Reversion)
      #  pos[params$series[i]] <- -params$posSizes[params$series[i]]
      #}
      
      ##lets trade on MACD...
      macdValCross <- 0.1
      smallwick <- 0.0
      #print(is.matrix(macd))
      #print(macd[,1])
      #print(mac)
      #if (macd[i,1] > macdValCross){
        #print("1")
      #if (macd > 0.2){
      #  if (store$op[startIndex:store$iter,i] > store$cl[startIndex:store$iter,i]) {
      #    print("2")
      #    if ((store$hi[startIndex:store$iter,i] - store$op[startIndex:store$iter,i]) > smallwick){
      #      print("Found a place to trade")
      #    }
      #  }
      #}
      
      #print(store$op[startIndex:store$iter,i])
      
      ##MEAN REVERSION USING RSI (TREND REVERSALS)
      #if (rsi[i] >= 70){
      #  if (macd[i,1] > macd[i,2]){
      #    pos[params$series[i]] <- params$posSizes[params$series[i]]
      #  }
      #} else if (rsi[i] <= 30){
      #  if (macd[i,1] < macd[i,2]){
      #    pos[params$series[i]] <- -params$posSizes[params$series[i]]
      #  }
      #}
      
    }
  }
      # This strategy only trades on certain series according to params$series.
      # The strategy will be long (short) whenever
      # the close is above (below) the upper (lower) Bollinger Band.
      # Thus this is trend-following type strategy.
      
      marketOrders <- marketOrders + pos
      
      return(list(store=store,marketOrders=marketOrders,
                  limitOrders1=allzero,limitPrices1=allzero,
                  limitOrders2=allzero,limitPrices2=allzero))
}



    initClStore  <- function(newRowList,series) {
      clStore <- matrix(0,nrow=maxRows,ncol=length(series))
      return(clStore)
    }
    updateClStore <- function(clStore, newRowList, series, iter) {
      for (i in 1:length(series))
        clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
      return(clStore)
    }
    initStore <- function(newRowList,series) {
      #return(list(iter=0,cl=initClStore(newRowList,series))) ##original
      return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series)
                  ,hi=initHiStore(newRowList,series),low=initLowStore(newRowList,series)
                  ,op=initLowStore(newRowList,series)))
      #return(list(iter=0,vol=initVolStore(newRowList,series))) #### edited line to add volume to store
    }
    updateStore <- function(store, newRowList, series) {
      store$iter <- store$iter + 1
      store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
      store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)#### edited line to add volume to store
      store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)#### edited line to add high to store
      store$low <- updateLowStore(store$low,newRowList,series,store$iter)#### edited line to add low to store
      store$op <- updateLowStore(store$op,newRowList,series,store$iter)#### edited line to add low to store
      return(store)
    }
    
    #Volume Store
    initVolStore  <- function(newRowList,series) {
      volStore <- matrix(0,nrow=maxRows,ncol=length(series))
      ##print(volStore)
      return(volStore)
    }
    updateVolStore <- function(volStore, newRowList, series, iter) {
      for (i in 1:length(series))
        volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
      return(volStore)
    }
    
    ##High Store
    initHiStore  <- function(newRowList,series) {
      hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
      ##print(volStore)
      return(hiStore)
    }
    updateHiStore <- function(hiStore, newRowList, series, iter) {
      for (i in 1:length(series))
        hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
      return(hiStore)
    }
    
    ##Low Store
    initLowStore  <- function(newRowList,series) {
      lowStore <- matrix(0,nrow=maxRows,ncol=length(series))
      ##print(volStore)
      return(lowStore)
    }
    updateLowStore <- function(lowStore, newRowList, series, iter) {
      for (i in 1:length(series))
        lowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
      return(lowStore)
    }
    
    ##Open Store
    initOpenStore  <- function(newRowList,series) {
      opStore <- matrix(0,nrow=maxRows,ncol=length(series))
      ##print(volStore)
      return(opStore)
    }
    updateOpenStore <- function(opStore, newRowList, series, iter) {
      for (i in 1:length(series))
        opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
      return(opStore)
    }
    
#moving average indicator
    
    


