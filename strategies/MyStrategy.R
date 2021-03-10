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
      pos[params$series[i]] <- MomentumPos
      #if (tester != 0 && tester != -1) {
      #  print(tester)
      #}
      
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


  MomentumStrategy <- function(store, startIndex, i, pos){ #params inside of the function.

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
    smi <- last(SMI(HLCdf)) 
    emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i])) 
    #print(emv)
       
    #OPPORTUNITIES TO ENTER THE MARKET (VERY TIGHT STRATEGY - CAN REMOVE INDICATORS AND OPTIMISE AND DISTRIBUTE FUNDS LATER)
    if ((stocOsc[1,2] < stocOsc[1,3]) && (stocOsc[2,2] > stocOsc[2,3])){  #CrossOver (FastD CrossesAbove SlowD)
      if(smi[,1] > 60){   #VALUES (20/80 = Oversold/Overbought) (50 is flat) 
        if (adx[,4] > 30){  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
          if (emv[,1] > 0.5) {  #VALUES (Break Above 0 is price rise (below is price fall): Further from 0 equals stronger)  
            
            #print("Entered Trade Opportunity!")
            pos[params$series[i]] <- params$posSizes[params$series[i]]
            print(pos[params$series[i]])
            
          }
        }
      }
    }
  
    #OPPORTUNITIES TO EXIT THE MARKET
    if (emv[,1] < 0) { #EMV shows that there is a slow in prices ability to move upwards
      pos[params$series[i]] <- -params$posSizes[params$series[i]]
      #print("Exit (EMV) Trade Opportunity!")
    }
    if (adx[,4] < 25){    #ADX shows that the momentum is now weak/slowing down
      pos[params$series[i]] <- -params$posSizes[params$series[i]]
      #print("Exit (ADX) Trade Opportunity!")
    }
    if(smi[,1] > 80){   #VALUES (80 = Overbought) Going to reverse.
      pos[params$series[i]] <- -params$posSizes[params$series[i]]
      #print("Exit (SMI) Trade Opportunity!")
    }
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