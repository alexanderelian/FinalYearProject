maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  marketOrders <- -currentPos; pos <- allzero #Initialise Position for marketOrders
  
  #Trade in every series - Once the lookback period has passed
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    #Loop through every series in the data
    #for (i in 1:length(params$series)) { #All Series
    for (i in 1:1) {  #One Series
      
      #pos[params$series[i]] <- MomentumStrategy(store, startIndex, i, pos)
      
      MomentumPos <- MomentumStrategy(store, startIndex, i, pos)
      #print(MomentumPos)
      if(currentPos != 0){
        pos[params$series[i]] <- MomentumPos - currentPos
      }
      else{
        pos[params$series[i]] <- MomentumPos
      }

      
      #MeanRevPos <- MeanRevStrategy(store, startIndex, i, pos)
      #pos[params$series[i]] <- MeanRevPos
      
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


  MomentumStrategy <- function(store, startIndex, i, pos){ #params inside of the function.

    stratPos <- 0
    myPos <- params$posSizes[params$series[i]] 
    
    #High/Low/Close Data Frame
    HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                        Low = store$low[startIndex:store$iter,i],
                        Close = store$cl[startIndex:store$iter])
    #High/Low Data Frame
    HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                       Low = store$low[startIndex:store$iter,i])
    
    
    ##### Momentum Strategy Indicators #####
    ##### USING MARKET ORDERS #####
    adx <- last(ADX(HLCdf,n=params$adx))
    stocOsc <- last(stoch(HLCdf),2) #Last 2 to check if its a crossover
    #print(stocOsc)
    smi <- last(SMI(HLCdf)) 
    emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i])) 
    
    cl = store$cl[startIndex:store$iter]
    vol = store$vol[startIndex:store$iter,i]
    hi = store$hi[startIndex:store$iter,i]
    bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                          n=params$lookback,sd=params$sdParam))
    
    #volMA <- tail(SMA(store$vol[startIndex:store$iter,i], 20, 1 ))
    #volchange <- store$vol[startIndex:store$iter,i] / volMA
    
    vwap.20 <- last(VWAP(store$cl[startIndex:store$iter,i], store$vol[startIndex:store$iter,i], 1))
    #print(vwap.20)
     
    evwma.20 <- last(EVWMA(cl, vol, 20))
    #print(evwma.20)
      
    
    #Function Detect breakout
    #Lookout for break above the bband using vwap Mayeb some more indicators
    ema <-last(EMA(cl,n=13))
    macdlookback  = 7
    macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),macdlookback)
    
    histogramValue.old <- (macd[macdlookback-1,1] - macd[macdlookback-1,2])   
    histogramValue.new <- (macd[macdlookback,1] - macd[macdlookback,2])   #MOMENTUM IS INCREASING #Most recent   (IF NEW > OLD = Acceleration in momentum) + if crossing above signal line (- if crossing below)
    #print(histogramValue.new)
    
    rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi))
  
  #  if (cl > bbands[,"up"]) {   #Breakout Detected  ##or vwap
  #    if (rsi > 65 && rsi < 80){
  #      #if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
  #      if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
  #        if ((histogramValue.new > histogramValue.old) && (histogramValue.new > 0)) { #+perhaps some value, to check rate of growth has been sufficient (%)
  #          stratPos <- stratPos + myPos
  #        }
  #      }
  #    }
  #  }
    
    # https://www.dailyfx.com/education/bollinger-bands/
    #Look for divergence in the histograms of MACD
    if (histogramValue.new > histogramValue.old && histogramValue.new > 0){ #Positive Divergence
      #Look for entry on a break #if close price is higer than local high of past 10 days
      if (last(cl) > max(last(hi,20))){
        if (cl > bbands[,"up"]){
          stratPos <- stratPos + myPos
        }
      }
    }
    
    if (histogramValue.new < histogramValue.old && histogramValue.new < 0){ #Positive Divergence
      #Look for entry on a break #if close price is higer than local high of past x days
      if (last(cl) < min(last(hi,20))){
        if (cl > bbands[,"dn"]){
          stratPos <- stratPos - myPos
        }
      }
    }
    
    #Better PD
    
    #  if (cl > bbands[,"up"]) {   #Breakout Detected  ##or vwap
    #    if (rsi > 65 && rsi < 80){
    #      #if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
    #      if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
    #        if ((histogramValue.new > histogramValue.old) && (histogramValue.new > 0)) { #+perhaps some value, to check rate of growth has been sufficient (%)
    #          stratPos <- stratPos + myPos
    #        }
    #      }
    #    }
    #  }
    #if (cl < bbands[,"dn"]) {   #Breakout Detected  ##or vwap
    #  if (rsi < 35 && rsi > 20){
    #    if ((macd[1,1] > macd[1,2]) && (macd[2,1] < macd[2,2])){ ##CROSS BELOW SIGNAL LINE (BEARISH)
    #  #if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
    #    #if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
    #      if ((histogramValue.new < histogramValue.old) && (histogramValue.new < 0)) { #+perhaps some value, to check rate of growth has been sufficient (%) <0
    #        #if(histogramValue.new < -0.5){######rsi to confirm breakout
    #          #print(histogramValue.new)
    #          stratPos <- stratPos - myPos
    #        }
    #      #}
    #    #}
    #  }
    #  }
    #}
    
    
    #Function Avoid fakout/comfirm the breakout/detech strength
    
    #adx... 
    #emv if can happen (confirming)
    #check above local high (reisitence libnes) (TRUE BREAKOUT) //over 15
    
    if (stratPos == 1) {
      if (adx[,4] > 30) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT STRENGTH
        if (emv[,1] > 0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move
          #print("ping")
          #localMax = 
          #print((store$hi[startIndex:store$iter-1]))
        }
      }
    }
    
    if (stratPos == -1) {
      if (adx[,4] > 30) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT STRENGTH
        if (emv[,1] < -0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move
          #print("pong")
          #localMax = 
          #print((store$hi[startIndex:store$iter-1]))
        }
      }
    }
    
    
    
    
    
      
    
    #go long pos[params$series[i]] <- params$posSizes[params$series[i]] 
    
    
    
    
    ###SHORT IS THE OPPPOSITE
  

    
    #if (vwap.20 > bbands[,"up"]) {
    #  if (adx[,4] > 30) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
    #    if (emv[,1] > 0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)
    #    pos[params$series[i]] <- params$posSizes[params$series[i]] 
    #    }
    #  }
    #}
    
    
    
    
    
    
#    ##OPPORTUNITIES TO ENTER THE MARKET (VERY TIGHT STRATEGY - CAN REMOVE INDICATORS AND OPTIMISE AND DISTRIBUTE FUNDS LATER)
#    if ((stocOsc[1,2] < stocOsc[1,3]) && (stocOsc[2,2] > stocOsc[2,3])) {  #CrossOver (FastD CrossesAbove SlowD)
#      #if(smi[,1] < 40){   #VALUES (20/80 = Oversold/Overbought) (50 is flat) 
#        if (adx[,4] > 30) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
#          if (emv[,1] > 0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)  
#            #print("Entered Trade Opportunity!")
#            pos[params$series[i]] <- params$posSizes[params$series[i]]
#            #print(pos[params$series[i]])
#          }
#        }
#      #}
#    }
    
    
    
    #OPPORTUNITIES TO ENTER THE MARKET (VERY TIGHT STRATEGY - CAN REMOVE INDICATORS AND OPTIMISE AND DISTRIBUTE FUNDS LATER)
    #if ((stocOsc[1,2] > stocOsc[1,3]) && (stocOsc[2,2] < stocOsc[2,3])) {  #CrossOver (FastD Crosses below SlowD) - SHORT
    #  #if(smi[,1] < 40){   #VALUES (20/80 = Oversold/Overbought) (50 is flat) 
    #  if (adx[,4] > 30) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
    #    if (emv[,1] < -0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)  
    #      #print("Entered Trade Opportunity!")
    #      pos[params$series[i]] <- -params$posSizes[params$series[i]]
    #      #print(pos[params$series[i]])
    #    }
    #  }
    #  #}
    #}
    
    #print(stocOsc)
  
#    #OPPORTUNITIES TO EXIT THE MARKET
#    if (emv[,1] < 0) { #EMV shows that there is a slow in prices ability to move upwards
#      pos[params$series[i]] <- -params$posSizes[params$series[i]]
#      #print("Exit (EMV) Trade Opportunity!")
#    }
#    if (adx[,4] < 25){    #ADX shows that the momentum is now weak/slowing down
#      pos[params$series[i]] <- -params$posSizes[params$series[i]]
#      #print("Exit (ADX) Trade Opportunity!")
#    }
#    if(smi[,1] > 80){   #VALUES (80 = Overbought) Going to reverse.
#      pos[params$series[i]] <- -params$posSizes[params$series[i]]
#      #print("Exit (SMI) Trade Opportunity!")
#    }
#    
#    ##if there is a cross or stop loss if hits 200 day ma
#    
    #return(pos[params$series[i]])
    return(stratPos)
  }
  
  
  
  MeanRevStrategy <- function(store, startIndex, i, pos){ #params inside of the function.
    cl = store$cl[startIndex:store$iter]
    bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                          n=params$lookback,sd=params$sdParam))
    #print(bbands)
    
    ###Averages
    smaShort <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort))
    #print(smaShort)
    sma <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookback))
    #print(sma)
    ema <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
    #print(ema)
    vwap <- last(VWAP(store$cl[startIndex:store$iter,i], store$vol[startIndex:store$iter,i], 1))
    #print(vwap)
    
    macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),2)
    #print(macd[,1])
    
    ### Extra Indicators
    rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi))
  
    #ENTER THE MARKET
    if (cl[i] < bbands[,"dn"]) {
      # if close is relatively low go long (i.e., Mean Reversion)
      pos[params$series[i]] <- params$posSizes[params$series[i]] 
    }
    
    if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
      if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
    }
    
    if (sma < ema)  {
      pos[params$series[i]] <- params$posSizes[params$series[i]]
    }
    
    if (rsi <= 30){
      pos[params$series[i]] <- params$posSizes[params$series[i]]
    }
    
    if (vwap < bbands[,"dn"]) {
      pos[params$series[i]] <- params$posSizes[params$series[i]]
    }
    
    
    
    
    #EXIT THE MARKET
    if (cl[i] > bbands[,"up"]) {
      # if close is relatively high go short (again, Mean Reversion)
      pos[params$series[i]] <- -params$posSizes[params$series[i]]
    }
  
     
    if ((macd[1,1] > macd[1,2]) && (macd[2,1] < macd[2,2])){ ##CROSS BELOW
       pos[params$series[i]] <- -params$posSizes[params$series[i]]
    }

    #if (rsi >= 65){
    #  pos[params$series[i]] <- -params$posSizes[params$series[i]]
    #}
    
    
    #MORE SPECIFIC (COMBINE INDICATORS)
    #if (cl[i] < bbands[,"dn"]) {
    #  if (macd[2,1] < 0)  {
    #    if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
    #      pos[params$series[i]] <- params$posSizes[params$series[i]]
    #    }
    #  }
    #}
    
    return(pos[params$series[i]])
  }




  ### Store Functions ###
  #Per Element Of The Store
  initClStore  <- function(newRowList,series) {
    clStore <- matrix(0,nrow=maxRows,ncol=length(series))
    return(clStore)
  }
  updateClStore <- function(clStore, newRowList, series, iter) {
    for (i in 1:length(series))
      clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
    return(clStore)
  }
  
  initVolStore  <- function(newRowList,series) {
    volStore <- matrix(0,nrow=maxRows,ncol=length(series))
    return(volStore)
  }
  updateVolStore <- function(volStore, newRowList, series, iter) {
    for (i in 1:length(series))
      volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
    return(volStore)
  }
  
  initHiStore  <- function(newRowList,series) {
    hiStore <- matrix(0,nrow=maxRows,ncol=length(series))
    return(hiStore)
  }
  updateHiStore <- function(hiStore, newRowList, series, iter) {
    for (i in 1:length(series))
      hiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
    return(hiStore)
  }
  
  initLowStore  <- function(newRowList,series) {
    lowStore <- matrix(0,nrow=maxRows,ncol=length(series))
    return(lowStore)
  }
  updateLowStore <- function(lowStore, newRowList, series, iter) {
    for (i in 1:length(series))
      lowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
    return(lowStore)
  }
  
  initOpenStore  <- function(newRowList,series) {
    opStore <- matrix(0,nrow=maxRows,ncol=length(series))
    return(opStore)
  }
  updateOpenStore <- function(opStore, newRowList, series, iter) {
    for (i in 1:length(series))
      opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
    return(opStore)
  }
  
  #Store Functions
  initStore <- function(newRowList,series) {
    return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series)
                ,hi=initHiStore(newRowList,series),low=initLowStore(newRowList,series)
                ,op=initLowStore(newRowList,series)))
  }
  updateStore <- function(store, newRowList, series) {
    store$iter <- store$iter + 1
    store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
    store$vol <- updateVolStore(store$vol,newRowList,series,store$iter) #Add volume to store
    store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)    #Add high to store
    store$low <- updateLowStore(store$low,newRowList,series,store$iter) #Add low to store
    store$op <- updateLowStore(store$op,newRowList,series,store$iter)   #Add low to store
    return(store)
  }