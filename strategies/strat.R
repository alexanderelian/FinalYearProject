# "strat"=list(lookback=50,sdParam=2,adx=14,nfast=12,nSlow=26,nSig = 9,series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(50,10))

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    for (i in 1:length(params$series)) {

      x <- MomentumStrategy(store, newRowList, currentPos, info, params, i, startIndex)
      
      ##same for function mean reversion
      
      if (x != 0){
        pos[params$series[i]] <- x
      }
      
    }
  }
  marketOrders <- pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


MomentumStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  position <- 0
  
  #Set Up
  cl <- newRowList[[params$series[i]]]$Close
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter])
  HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                     Low = store$low[startIndex:store$iter,i])
  
  #Calculate Indicators
  adx <- last(ADX(HLCdf,n=params$adx))
  emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i]))
  bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                        n=params$lookback,sd=params$sdParam))
  
  macdlookback  = 7
  macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),macdlookback)
  #Generate MACD Histogram Values
  histogramValue.old <- (macd[macdlookback-2,1] - macd[macdlookback-2,2])   
  histogramValue.new <- (macd[macdlookback,1] - macd[macdlookback,2])   #MOMENTUM IS INCREASING #Most recent   (IF NEW > OLD = Acceleration in momentum) + if crossing above signal line (- if crossing below)
  
  #MAYBE CONSIDER Double Breakout?
  if (cl < bbands[,"dn"]) {
     if (histogramValue.new < histogramValue.old && histogramValue.new < 0){ #Negative Divergence
       if (adx[,4] > 25) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT Sufficient STRENGTH to the breakout        //if very strong maybe give more funds, increase position?
         if (emv[,1] < -0.3) {  #VALUES (Break Above 0 is price rise with ease (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move more and in a positive
           #go short (i.e., trend following)
           position <- -params$posSizes[params$series[i]] - currentPos[[i]]
         }
       }
     }
  }
  else if (cl > bbands[,"up"]) {
     if (histogramValue.new > histogramValue.old && histogramValue.new > 0){ #Positive Divergence
       if (adx[,4] > 25) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT Sufficient STRENGTH to the breakout
         if (emv[,1] > 0.3) {  #VALUES (Break Above 0 is price rise with ease (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move more and in a positive
           #go long (again, trend following)
           position <- params$posSizes[params$series[i]] - currentPos[[i]]
         }
       }
     }
  }
  return(position)
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