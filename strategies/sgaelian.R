# Name: Alexander Elian
# Student ID: 201351963
# Email: sgaelian@liverpool.ac.uk

#This is the params passed in through example strategies.
#"sgaelian"=list(lookback=50,shortEma.F=c(4,4,3,11,11,11,11,11,11,3),shortEma.S=c(35,30,35,11,11,11,11,11,11,35),longEma.F=c(11,11,4,2,11,3,3,11,3,11),longEma.S=c(11,11,30,40,11,35,35,11,35,11),LongRiskStopPercentage=c(0,0.99,0.98,0.98,0,0.98,0.98,0.99,0.98,0.98),ShortRiskStopPercentage=c(1.02,1.02,1.01,0,0,0,0,0,0,1.01),MOsdParam=c(1.5,1.5,2,1.5,99,1.5,99,99,1.5,99),MOShortSdParams=c(1.5,99,2,99,99,99,99,99,99,1.5),MOadxSignal=c(25,25,20,25,0,35,0,0,35,0),MOShortadxSignal=c(25,0,25,0,0,0,0,0,0,30),emvLong=0.3,emvShort=-0.3,adx=14,nfast=12,nSlow=26,nSig = 9,holdPeriod=c(19,18,18,21,0,21,16,21,8,5),shortHoldPeriod=c(21,0,16,14,0,18,0,0,0,21),series=c(1,2,3,4,5,6,7,8,9,10))

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #Reset positions
  pos <- allzero
  stMomentumPosition <- allzero
  breakoutPosition <- allzero

  #Money Management:
  #For every series, see which im currently holding a short position
  totalshortvalue <- 0
  for (tmpcounter1 in 1:10){
    cl <- newRowList[[params$series[tmpcounter1]]]$Close
    if ((currentPos[tmpcounter1] < 0))  {
      #Accumulate the total size of my short positions
      totalshortvalue = totalshortvalue + (currentPos[tmpcounter1] * as.double(cl))
    }
  }
  
  #Calculate the money left in my account for distribution between my strategies
  #This is minus the amount of money i currently have invested in short trades
  #This aims to reduce the risk of me going bankrupt.
  moneyLeft <<- as.double(info$balance) - abs(totalshortvalue)

  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    for (i in 1:length(params$series)) {

      #breakout - Momentum Strategy
      breakout <- MomentumStrategy(store, newRowList, store$breakout, info, params, i, startIndex)
      #Retrieve the positions generated
      breakoutPosition[params$series[i]] <- breakout$position
      #Update the store
      store <- breakout$store
      
      #st (Short-Term) - Momentum Strategy
      stMomentum <- MomentumStrategyType2(store, newRowList, store$stMomentum, info, params, i, startIndex)
      #Retrieve the positions generated
      stMomentumPosition[params$series[i]] <- stMomentum$position
      #Update the store
      store <- stMomentum$store
      
    }
  }
  
  #Add my Strategy positions to the store
  store$breakout <- store$breakout + breakoutPosition
  store$stMomentum <- store$stMomentum + stMomentumPosition
  #Combine my positions, to generate my market orders.
  marketOrders <- breakoutPosition + stMomentumPosition

  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


# Momentum Strategy
# Breakout Strategy
MomentumStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  #Reset Position
  position <- 0
  
  #Retrieve the close data
  cl <- newRowList[[params$series[i]]]$Close
  #Generate High, Low, Close DataFrame for use in indicators
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter,i])
  #Generate High, Low DataFrame for use in indicators
  HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                     Low = store$low[startIndex:store$iter,i])
  
  
  #Money Management Strategy
  #Using the money remaining in the account, half is accessable to this strategy
  moneyForThisStrat <- moneyLeft/2
  #Position Sizes are generated by dividing the account by 8,
  #This is generated by the amount of risk taken per trade within this strategy
  positionSizes <- moneyForThisStrat/8
  #Position sizes are generated based off yesterdays close and rounded for ease
  posSize <- round(positionSizes / cl, digits=2)
  
  #ADX Indicator
  adx <- last(ADX(HLCdf,n=params$adx))
  #EMV Indicator
  emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i]))
  
  #BBands Indicator, Long and Short
  Longbbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$MOsdParam[i]))   
  Shortbbands <- last(BBands(store$cl[startIndex:store$iter,i],
                             n=params$lookback,sd=params$MOShortSdParams[i]))
  
  #MACD Indicator
  macdlookback  = 7 #Lookback period
  macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),macdlookback)
  
  #Generate MACD Histogram Values
  histogramValue.old <- (macd[macdlookback-2,1] - macd[macdlookback-2,2])   
  histogramValue.new <- (macd[macdlookback,1] - macd[macdlookback,2])   
  
  #Looking for an opportunity to take a short position
  #If the close price has broken below the lower bollinger band
  if (cl < Shortbbands[,"dn"]) {
    #If there has been a decrease in the size of the histograms over the previous x days (Displays Downwards and Growing Momentum)
    if (histogramValue.new < histogramValue.old && histogramValue.new < 0){
      #If the ADX value is larger than my defined value for this stock
        #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
      if (adx[,4] > params$MOShortadxSignal[i]) {
        #If EMV displays that there is the ability for prices to move upwards with ease (Above some region defined)
        if (emv[,1] < params$emvShort) {
          #Take a short position
          position <- -posSize - currentPos[[i]]
          #Set a risk stop above the current close price
          store$BreakoutRiskStop[i] <- cl*1.07

        }
      }
    }
  }
  
  #Looking for an opportunity to take a long position
  #If the close price has broken above the upper bollinger band
  else if (cl > Longbbands[,"up"]) {
    #If there has been an increase in the size of the histograms over the previous x days (Displays Upwards and Growing Momentum)
    if (histogramValue.new > histogramValue.old && histogramValue.new > 0){
      #If the ADX value is larger than my defined value for this stock
        #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)
      if (adx[,4] > params$MOadxSignal[i]) {
        #If EMV displays that there is the ability for prices to move upwards with ease (Above some region defined)
        if (emv[,1] > params$emvLong) { #Avoid Fakeout
          #Take a long position
          position <- posSize - currentPos[[i]] 
          #Set a risk stop below the current close price
          store$BreakoutRiskStop[i] <- cl*0.90
        }
      }
    }
  }
  
  
  
  #Exit Strategy Logic
  #If currently in a short trade
  if (currentPos[i] >= 1 || position >= 1) {
    #If previously i was in a long position
    if (store$count[i] < 0) {
      store$count[i] = 1 #Begin the count
    } 
    #If the defined holding period has been reached, or the close price has dropped below my risk stop position
    else if ((store$count[i] == params$holdPeriod[i]) || (store$BreakoutRiskStop[i] > cl)) {
      #Exit the short position
      position <- -currentPos[i]
      #Reset the couter to 0
      store$count[i] <- 0
    } else {
      store$count[i] <- store$count[i] + 1  #Else increment the count
    }
  }
  
  #If currently in a short trade
  else if (currentPos[i] <= -1 || position <= -1) {
    #If previously i was in a long position
    if (store$count[i] > 0) {
      store$count[i] = -1 #Begin the count
    } 
    #If the defined holding period has been reached, or the close price has risen above my risk stop position
    else if ((store$count[i] == -params$shortHoldPeriod[i]) || (store$BreakoutRiskStop[i] < cl)) {
      #Exit the short position
      position <- -currentPos[i]
      #Reset the couter to 0
      store$count[i] <- 0
    } else {
      store$count[i] <- store$count[i] - 1  #Else decrement the count
    }
  }
  else {
    store$count[i] <- 0 #Reset count to 0
  }
  return(list(position=position, store=store))
}


# Momentum Strategy Type Two
# Moving-Average Trend Following Strategy
MomentumStrategyType2 <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  #Reset Position
  position <- 0
  
  #Retrieve the Close, High and Low data.
  cl <- newRowList[[params$series[i]]]$Close
  hi <- newRowList[[params$series[i]]]$High
  lo <- newRowList[[params$series[i]]]$Low
  #Generate a HLC (High,Low,Close) DataFrame for use in functions.
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter])
  
  #Local Variables
  crossingLookback <- 5         #How many days prior to store
  crossedAboveToday <- FALSE    #Reset Boolen Values
  crossedBelowToday <- FALSE    #Reset Boolen Values
  crossedAbove <- FALSE         #Reset Boolen Values
  crossedBelow <- FALSE         #Reset Boolen Values
  
  
  
  #Money Management Strategy
  #Using the money remaining in the account, half is accessable to this strategy
  moneyForThisStrat <- moneyLeft/2
  #Position Sizes are generated by dividing the account by 3.5,
  #This is generated by the amount of risk taken per trade within this strategy
  positionSizes <- moneyForThisStrat/3.5
  #Position sizes are generated based off yesterdays close and rounded for ease
  posSize <- round(positionSizes / cl, digits=2)
  
  
  
  #Calculate my moving averages.
  #Using the params passed into the strategy, and store the last x number of days for future reference
  ShortEMA.Fast <- last(EMA(store$cl[startIndex:store$iter,i],n=params$shortEma.F[i]),crossingLookback)
  ShortEMA.Slow <- last(EMA(store$cl[startIndex:store$iter,i],n=params$shortEma.S[i]),crossingLookback)
  LongEMA.Fast <- last(EMA(store$cl[startIndex:store$iter,i],n=params$longEma.F[i]),crossingLookback)
  LongEMA.Slow <- last(EMA(store$cl[startIndex:store$iter,i],n=params$longEma.S[i]),crossingLookback)
  
  
  
  
  #SIGNAL GENERATION
  
  #Signal Generation for a possible Shorting Opportunity
  #Check for a Cross in todays Fast and Slow Moving Averages
  #NOTE: 11 is a out of bounds parameter used to signal i do not wish to check this series as i am not trading this series.
  if ((params$shortEma.S[i] != 11) && (params$shortEma.F[i] != 11)){
    #Check if the fast moving avergage crossed below the slow movig average today
    if((ShortEMA.Fast[crossingLookback-1] > ShortEMA.Slow[crossingLookback-1]) && ((ShortEMA.Fast[crossingLookback] < ShortEMA.Slow[crossingLookback])))  {
      #Signal a cross below with boolean to TRUE
      crossedBelowToday <-  TRUE
    }
    #Now we check if the Moving averages crossed in the past 3 days
    for (crossCounter in 2:4){
      if((ShortEMA.Fast[crossCounter] > ShortEMA.Slow[crossCounter]) && ((ShortEMA.Fast[crossCounter+1] < ShortEMA.Slow[crossCounter+1])))  {
        #Signal a recent cross above with boolean to TRUE
        crossedBelow <-  TRUE
      }
    }
  }
  
  #Signal Generation for a possible Longing Opportunity
  #Check for a Cross in todays Fast and Slow Moving Averages
  #NOTE: 11 is a out of bounds parameter used to signal i do not wish to check this series as i am not trading this series.
  if ((params$longEma.S[i] != 11) && (params$longEma.F[i] != 11)){
    #Check if the fast moving avergage crossed above the slow movig average today
    if((LongEMA.Fast[crossingLookback-1] < LongEMA.Slow[crossingLookback-1]) && ((LongEMA.Fast[crossingLookback] > LongEMA.Slow[crossingLookback])))  {
      #Signal a cross above with boolean to TRUE
      crossedAboveToday <-  TRUE
    } 
    #Now we check if the Moving averages crossed in the past 3 days
    for (crossCounter in 2:4){
      if((LongEMA.Fast[crossCounter] < LongEMA.Slow[crossCounter]) && ((LongEMA.Fast[crossCounter+1] > LongEMA.Slow[crossCounter+1])))  {
        #Signal a recent cross above with boolean to TRUE
        crossedAbove <-  TRUE
      } 
    }
  }
  
  #When Crossed, take todays hi/lo as my market entry point.
  #If the two moving averages have crossed today
  #If fast crossed above slow:
  if (crossedAboveToday) {
    #Store the market high price of today
    store$longEntryStop[i] <- hi
  } 
  #If fast crossed below slow:
  else if (crossedBelowToday)  {
    #Store the market low price of today
    store$shortEntryStop[i] <- lo
  }
  
  
  
  #ENTRY LOGIC
  
  #Long Entry Logic
    #If in the past x number of days, the fast MA has moved above the slow MA
      # &&
        #If currently the fast MA is still above the slow MA : Consider taking a long position
  if ((crossedAbove) && (LongEMA.Fast[crossingLookback] > LongEMA.Slow[crossingLookback])) {
    #In order to wait for the right time to enter, enter only if todays close price is more than the market high of the day that the MA's crossed
    if (cl > store$longEntryStop[i]) {
      #Take a short position
      position <- posSize - currentPos[[i]]
      #Set a risk stop
      store$RiskStop[i] <- cl*0.99
      #Set a trailing stop loss (Constantly updated at a later time)
      store$stopLoss[i] <- cl*0.99
    } 
  }
  
  #Short Entry Logic
    #If in the past x number of days, the fast MA has moved below the slow MA
      # &&
        #If currently the fast MA is still below the slow MA : Consider taking a short position
  if((crossedBelow) && (ShortEMA.Fast[crossingLookback] < ShortEMA.Slow[crossingLookback])) {
    #In order to wait for the right time to enter, enter only if todays close price is less than the market low of the day that the MA's crossed
    if (cl < store$shortEntryStop[i]) {
      #Take a short position
      position <- -posSize - currentPos[[i]]
      #Set a risk stop
      store$RiskStop[i] <- cl*1.02
      #Set a trailing stop loss (Constantly updated at a later time)
      store$stopLoss[i] <- cl*1.02
    } 
  }
  
  
  #Exit Strategy
  
  #Check if currently in a long position
  if (currentPos[i] >= 1 || position >= 1) {
    #If today's fast MA is below the slow MA: Exit the trade  
      #OR If todays's close price has fallen lower than the position of my risk stop: Exit the trade
        #OR If todays's close price has fallen lower than the position of my trailing stop loss: Exit the trade
    if ((LongEMA.Fast[crossingLookback] < LongEMA.Slow[crossingLookback]) || (cl < store$RiskStop[i]) || (cl < store$stopLoss[i])) {
      #Dont stay in a long position
      position <- -currentPos[i]
    }
    #If i do not wish to exit the trade today, update my trailing stop loss, using todays current price
    if (store$stopLoss[i] < cl*0.89) {
      store$stopLoss[i] <- cl*0.89
    }
  }
  
  #Check if currently in a short position
  else if (currentPos[i] <= -1 || position <= -1) { 
    #If today's fast MA is above the slow MA: Exit the trade  
      #OR If todays's close price has risen higher than the position of my risk stop: Exit the trade
        #OR If todays's close price has risen higher than the position of my trailing stop loss: Exit the trade
    if ((ShortEMA.Fast[crossingLookback] > ShortEMA.Slow[crossingLookback]) || (cl > store$RiskStop[i]) || (cl > store$stopLoss[i])) {#|| (cl > store$RiskStop[i])){
      #Dont stay short
      position <- -currentPos[i]
    }
    #If i do not wish to exit the trade today, update my trailing stop loss, using todays current price
    if (store$stopLoss[i] > cl*1.06) {
      store$stopLoss[i] <- cl*1.06
    }
  }
  
  return(list(position=position, store=store))
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
  #Strategy Handling Vectors
  breakout <- vector(mode="double",length=length(series))
  stMomentum <- vector(mode="double",length=length(series))
  
  #Risk Management Tools:
  #Trailing StopLoss for Moving Average Momentum Strategy
  stopLoss <- vector(mode="double",length=length(series))
  #Risk Stop for Moving Average Momentum Strategy
  RiskStop <- vector(mode="double",length=length(series))
  #Risk Stop for Breakout Strategy
  BreakoutRiskStop <- vector(mode="double",length=length(series))

  #Moving Average Strategy - Entry positions for both Long & Short
  longEntryStop <- vector(mode="double",length=length(series))
  shortEntryStop <- vector(mode="double",length=length(series))
  
  #Breakout Strategy - Counter - How many days holding the position: To time my exit
  count <- vector(mode="numeric",length=length(series))

  return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series),
              hi=initHiStore(newRowList,series),low=initLowStore(newRowList,series),
              op=initLowStore(newRowList,series),count=count,stopLoss=stopLoss,
              longEntryStop=longEntryStop,shortEntryStop=shortEntryStop, 
              RiskStop=RiskStop,BreakoutRiskStop=BreakoutRiskStop,
              breakout=breakout,stMomentum=stMomentum))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)    #Added Close data to the store
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter) #Added Volume data to the store
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter)    #Added High data to the store
  store$low <- updateLowStore(store$low,newRowList,series,store$iter) #Added Low data to the store
  store$op <- updateLowStore(store$op,newRowList,series,store$iter)   #Added Open data to store
  return(store)
}