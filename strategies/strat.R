#"strat"=list(lookback=50,lookbackShort=34,MOsdParam=c(1.5,1.5,2,1.5,99,1.5,1.5,1.5,1.5,1.5),MOShortSdParams=c(1.5,99,2,2,99.5,1.5,99,99,99,1.5),sdParamShort=1,adx=14,MOadxSignal=c(25,25,20,25,0,35,25,35,35,25),MOShortadxSignal=c(25,0,25,30,0,30,0,0,0,30),emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(19,18,18,21,0,21,16,21,8,5),shortHoldPeriod=c(21,0,16,14,0,18,0,0,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=c(24,20,142,6,13,96,89,1,42,306),spreadPercentage=0.001,inventoryLimits=rep(10,10))
maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #print(currentPos)
  pos <- allzero
  stMomentumPosition <- allzero
  breakoutPosition <- allzero
  MOLO1 <- allzero
  MOLP1 <- allzero
  #MMLO2 <- allzero
  #MMLP2 <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    #for (i in 1:1) {  #One Series
    for (i in 1:length(params$series)) {
      #MO - Momentum Strategy
      # x <- MomentumStrategy(store, newRowList, currentPos, info, params, i, startIndex)
      # position <- x$position
      # store <- x$store
      
      breakout <- MomentumStrategy(store, newRowList, store$breakout, info, params, i, startIndex)
      breakoutPosition[params$series[i]] <- breakout$position
      store <- breakout$store
      
      #MO - Momentum Strategy
      stMomentum <- MomentumStrategyType2(store, newRowList, store$stMomentum, info, params, i, startIndex)
      stMomentumPosition[params$series[i]] <- stMomentum$position
      store <- stMomentum$store
      
      #MOLO1[params$series[i]] <- x$limitOrders1[params$series[i]]
      #MOLP1[params$series[i]] <- x$limitPrices1[params$series[i]]
      
      #MR - Mean Reversion Strategy
      # x <- MeanRevStrategy(store, newRowList, currentPos, info, params, i, startIndex) #(2)
      # position <- x$position
      # store <- x$store

      #MM - Market Making Strategy
      # MM <- MarketMakingStrategy(store, newRowList, currentPos, info, params, i, startIndex)
      # #MM$MarketOrders[i]
      # #position <- 0
      # #print(MM$marketOrders)
      # position <- MM$marketOrders[i]
      # MMLO1[params$series[i]] <- MM$limitOrders1[i]
      # MMLP1[params$series[i]] <- MM$limitPrices1[i]
      # MMLO2[params$series[i]] <- MM$limitOrders2[i]
      # MMLP2[params$series[i]] <- MM$limitPrices2[i]
      
      # MMLO1 <- MM$limitOrders1      #LIST IS COMPLETED AND AMMENDED, SO AT THE END OF THE 10, THE FULL LIST IS RETRUNED WITH WHAT POSITIONS I WANT
      # MMLP1 <- MM$limitPrices1
      # MMLO2 <- MM$limitOrders2
      # MMLP2 <- MM$limitPrices2

      
      # if (position != 0){
      #   pos[params$series[i]] <- position
      # }
      # if (thisStMomentumPosition != 0){
      #   stMomentumPosition[params$series[i]] <- thisStMomentumPosition
      # }
      # if (thisBreakoutPosition != 0){
      #   breakoutPosition[params$series[i]] <- thisBreakoutPosition
      # }
      
    }
  }
  
  if (store$iter %% 50 == 0 ){
    print("--------------")
    print(currentPos)
    print(store$breakout)
    print(store$stMomentum)
  }
  
  store$breakout <- store$breakout + breakoutPosition
  store$stMomentum <- store$stMomentum + stMomentumPosition
  
  #For market Orders
  # marketOrders <- pos
  # store <- store
  # limitOrders1 <- allzero
  # limitPrices1 <- allzero
  # limitOrders2 <- allzero
  # limitPrices2 <- allzero

  #For Limit Orders (Market Making)
  marketOrders <- breakoutPosition + stMomentumPosition
  limitOrders1 <- MOLO1
  limitPrices1 <- MOLP1
  #limitOrders2 <- MMLO2
  #limitPrices2 <- MMLP2
  
  blankList <- c(0,0,0,0,0,0,0,0,0,0)
  #print("-----------")
  #print(limitOrders1)
  #print(limitPrices1)
  #print(limitOrders2)
  #print(limitPrices2)
  
  
  # if (!identical(blankList,limitOrders1)){
  #  print(limitOrders1)
  # }
  # if (!identical(blankList,limitPrices1)){
  #   print(limitPrices1)
  # }
  
  # 
  # if (!identical(blankList,MMLO2)){
  #   print(limitOrders2)
  # }
  # if (!identical(blankList,MMLP2)){
  #   print(limitOrders2)
  # }
  # 
  # 
  # if (!identical(blankList,limitPrices1)){
  #   print(limitOrders1)
  # }
  # if (!identical(blankList,limitPrices2)){
  #   print(limitOrders2)
  # }
  # 
  # if (!identical(blankList,limitOrders1)){
  #   print(limitOrders2)
  # }
  # if (!identical(blankList,limitOrders2)){
  #   print(limitOrders2)
  # }
  
  #print(limitPrices1)
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=allzero,limitPrices2=allzero))
  
   # return(list(store=store,marketOrders=allzero,
   #             limitOrders1=limitOrders1,limitPrices1=allzero,
   #             limitOrders2=allzero,limitPrices2=allzero))
}


MomentumStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  #Using Market Orders
  position <- 0

  #Set Up
  cl <- newRowList[[params$series[i]]]$Close
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter,i])
  HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                     Low = store$low[startIndex:store$iter,i])
  
  
  posSize <- 10000 / cl
  
  #Calculate Indicators
  adx <- last(ADX(HLCdf,n=params$adx))
  emv <- last(EMV(HLdf,store$vol[startIndex:store$iter,i]))
  # bbands <- last(BBands(store$cl[startIndex:store$iter,i],
  #                       n=params$lookback,sd=params$sdParam[i]))
  Longbbands <- last(BBands(store$cl[startIndex:store$iter,i],
                        n=params$lookback,sd=params$MOsdParam[i]))       #For main_optimisation only [i] and in adx for long and short trades
  Shortbbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$MOShortSdParams[i]))       #For main_optimisation only [i] and in adx for long and short trades
  
  # Error in if (cl < bbands[, "dn"]) { : 
  # missing value where TRUE/FALSE needed
  # Called from: MomentumStrategy(store, newRowList, currentPos, info, params, 
  #                               i, startIndex)
  
  #^caused by sd params haveing an [i] - need it for full run, as it changed in the list... for optimise on one series only one is passed through
  
  macdlookback  = 7
  macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),macdlookback)
  
  #print(macd)
  #Generate MACD Histogram Values
  histogramValue.old <- (macd[macdlookback-2,1] - macd[macdlookback-2,2])   
  histogramValue.new <- (macd[macdlookback,1] - macd[macdlookback,2])   #MOMENTUM IS INCREASING #Most recent   (IF NEW > OLD = Acceleration in momentum) + if crossing above signal line (- if crossing below)
  
  #Look for a short
  #print(bbands)
  #print(store$iter)
  
  #if(store$iter >= 968) {}else{     #if you there are less than 31 days of data left, dont enter any trades...
  
  #print(c("param recieced is",params))
    
  #Could also use adx (+/-DMI to confirm up/downtrends)
  if (cl < Shortbbands[,"dn"]) {
     if (histogramValue.new < histogramValue.old && histogramValue.new < 0){ #Downward Momentum & Increasing
       if (adx[,4] > params$MOShortadxSignal[i]) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT Sufficient STRENGTH to the breakout        //if very strong maybe give more funds, increase position?
         if (emv[,1] < params$emvShort) {  #VALUES (Break Above 0 is price rise with ease (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move more and in a positive
           #position <- -params$posSizes[params$series[i]] - currentPos[[i]] #Go short (trend following)
           
           position <- -posSize - currentPos[[i]]  #Go long (trend following)
           
           
           
           #store$BreakoutRiskStop[i] <- cl*1.05
           
           
           
           #print(index(cl))
           #print(bbands[,"dn"])
           #print(cl)
           
           
           
           # # Holding Period Calculation Code
           # BoughtAt <- as.double(cl)
           # date <- index(cl)  #Todays Date
           # dateLimits <- paste(as.Date(date),"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
           # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
           # 
           # d1 <- dataList[[i]]
           # return <- ((BoughtAt-d1$Close[dateLimits])/BoughtAt)*100
           # tmp <<- cbind(tmp,matrix(return))  #Calculate and Store ROI
           
           
           
           
          #  BoughtAt <- as.double(cl)
          #  #print(BoughtAt)
          #  
          #  date <- index(cl)  #Trades Date
          #  dateLimits <- paste(as.Date(date),"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
          #  dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
          # 
          #  #PARAMS (for title)
          #  adxParam = 25
          #  emvParam = 0.3
          #  PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
          # 
          #  #Generate and Plot Graph
          #  #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
          # #     width = 10, # The width of the plot in inches
          # #     height = 10) # The height of the plot in inches
          #  d1 <- dataList[[i]]
          #  graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
          #  print(c("plotted",graphTitle))
          # 
          #  #Plotting Function
          #  chartSeries((d1$Close/BoughtAt),theme='white',subset = dateLimits, name = graphTitle, plot = TRUE)
          #  #addTA(SMA(d1$Close,n=50),on=NA)
          #  abline(v = 7,col = 'blue')   #X-Axis line, date of enter
          #  abline(v = 14,col = 'blue')   #X-Axis line, date of enter



           #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
           #addTA(ADX(HLCdf2,n=14),on=1)

           #dev.off()
           
  # #Plot Generation Code
          #  date <- index(cl)  #Trades Date
          #  dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
          #  dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
          # 
          #  #PARAMS (for title)
          #  adxParam = 25
          #  emvParam = 0.3
          #  PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
          # 
          #  #Generate and Plot Graph
          # # pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
          # #     width = 10, # The width of the plot in inches
          # #     height = 10) # The height of the plot in inches
          #  d1 <- dataList[[i]]
          #  graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
          #  print(c("plotted",graphTitle))
          # 
          #  #Plotting Function
          #  chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=params$MOsdParam),addMACD()),subset = dateLimits, name = graphTitle, plot = TRUE)
          #  addTA(SMA(d1$Close,n=50),on=NA)
          #  abline(v = 8,col = 'blue')   #X-Axis line, date of enter



           #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
           #addTA(ADX(HLCdf2,n=14),on=1)

          # dev.off()
          }
       }
     }
  }
  #Look for a long
  else if (cl > Longbbands[,"up"]) {
     if (histogramValue.new > histogramValue.old && histogramValue.new > 0){ #Upwards Momentum & Growing
       if (adx[,4] > params$MOadxSignal[i]) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)  #needs to be [i] for full run: as there is multiple series to runthrough same for bbands sd
         if (emv[,1] > params$emvLong) {  #VALUES (Break Above 0 is price rise with ease (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move more and in a positive
           #position <- params$posSizes[params$series[i]] - currentPos[[i]]  #Go long (trend following)
           position <- posSize - currentPos[[i]]  #Go long (trend following)
           
           #store$BreakoutRiskStop[i] <- cl*0.92
           
           
           
           
           
           #print(index(cl))
           # # # # Holding Period Calculation Code        
           # BoughtAt <- as.double(cl)
           # date <- index(cl)  #Trades Date
           # dateLimits <- paste(as.Date(date),"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
           # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
           # d1 <- dataList[[i]]
           # 
           # return <- ((d1$Close[dateLimits]-BoughtAt)/BoughtAt)*100
           # 
           # #returnVa2 <- (d1$Close[dateLimits]/BoughtAt) * 100   #%return at this point
           # 
           # tmp <<- cbind(tmp,matrix(return))
           # #print(tmp)
           
           
           
#Plot Returns          
           #BoughtAt <- as.double(cl)
           # #print(BoughtAt)
           # 
           # date <- index(cl)  #Trades Date
           # dateLimits <- paste(as.Date(date),"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
           # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
           # 
           # #PARAMS (for title)
           # adxParam = 25
           # emvParam = 0.3
           # PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
           # 
           # #Generate and Plot Graph
           # pdf(file = (paste("Plots/Momentum/Returns/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
           #     width = 10, # The width of the plot in inches
           #     height = 10) # The height of the plot in inches
           # d1 <- dataList[[i]]
           # graphTitle <- paste("Long Opportunity Returns - Series", i,"-DATE:",index(cl))
           # print(c("plotted",graphTitle))
           # 
           # #Plotting Function
           # closePrice <- d1$Close[dateLimits]
           # returnVa <- (closePrice/BoughtAt)    #%return at this point
           # chartSeries(returnVa,theme='white',subset = dateLimits, name = graphTitle, plot = TRUE)#returnVa
           # 
           # #chartSeries((d1$Close/BoughtAt),theme='white',subset = dateLimits, name = graphTitle, plot = TRUE)#returnVa
           # #addTA(SMA(d1$Close,n=50),on=NA)
           # abline(v = 7,col = 'blue')   #X-Axis line, date of enter
           # abline(v = 14,col = 'blue')   #X-Axis line, date of enter
           # 
           # dev.off()

  # #Plot Generation Code
  #          date <- index(cl)  #Trades Date
  #          dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
  #          dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
  # 
  #          #PARAMS (for title)
  #          adxParam = 25
  #          emvParam = 0.3
  #          PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
  # 
  #          #Generate and Plot Graph
  #          pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
  #               width = 10, # The width of the plot in inches
  #               height = 10) # The height of the plot in inches
  #          d1 <- dataList[[i]]
  #          graphTitle <- paste("Long Opportunity Traded - Series", i,"-DATE:",index(cl))
  #          print(c("plotted",graphTitle))
  # 
  #          #Plotting Function
  #          chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addMACD()),subset = dateLimits, name = graphTitle, plot = TRUE)
  #          addTA(SMA(d1$Close,n=50),on=NA)
  #          abline(v = 8,col = 'blue')   #X-Axis line, date of enter
  # 
  # 
  # 
  #          #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
  #          #addTA(ADX(HLCdf2,n=14),on=1)
  # 
  #          dev.off()

  #Holding Period Code
           # check if we have been in trade too long
           # we maintain that pos[i] is an integer
           # if pos[i] == 0 we were flat last period
           # if pos[i] >  0 we have been long  for store$count[i] periods
           # if pos[i] <  0 we have been short for store$count[i] periods

           # if (position[params$series[i]] == 1) {# long signal today
           #   if (store$count[i] < 0) # last time we were short
           #     store$count[i] == position[params$series[i]] # == 1
           #   else if (store$count[i] == params$holdPeriod) { # reached holding period
           #     position[params$series[i]] <- 0 # don't stay long
           #     store$count[i] <- 0 # reset count to 0
           #   }
           #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
           #     store$count[i] <- store$count[i] + 1
           # }
           # 
           # else if (position[params$series[i]] == -1) {# short signal today
           # 
           #   if (store$count[i] > 0) # last time we were long
           #     store$count[i] == position[params$series[i]] # == -1
           #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
           #     position[params$series[i]] <- 0 # don't stay short
           #     store$count[i] <- 0 # reset count to 0
           #   }
           #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
           #     store$count[i] <- store$count[i] - 1
           # }
           # else
           #   store$count[i] <- 0 # reset count to 0
         }
       }
     }
  }
  #print(position)
  #print(store$count)
  #}
  
  #Holding Period Code
  # check if we have been in trade too long
  # we maintain that pos[i] is an integer
  # if pos[i] == 0 we were flat last period
  # if pos[i] >  0 we have been long  for store$count[i] periods
  # if pos[i] <  0 we have been short for store$count[i] periods
  
  
  
  # if (currentPos[i] >= 1 || position >= 1) {# long signal today
  #   #print("shouldnt see this")
  #   #print(store$count[i])
  #   if (store$count[i] < 0) {# last time we were short
  #     store$count[i] = 1 # == 1 (was position)
  #   } else if (store$count[i] == params$holdPeriod[i]) { # reached holding period
  #     position <- -currentPos[i] # don't stay long
  #     store$count[i] <- 0 # reset count to 0
  #     #print("FORCED ENDED LONG TRADE")
  #   }
  #   else {# 0 <= store$count[i] != (should be <) params$holdPeriod
  #     store$count[i] <- store$count[i] + 1
  #     #print(store$count[i])
  #   }
  # }
  # 
  # else if (currentPos[i] >= -1 || position >= -1) {# short signal today  #position size
  #   #print("")
  #   #print(store$count[i])
  #   if (store$count[i] > 0) {# last time we were long
  #     store$count[i] = -1 # == -1 (was position)
  #   } else if (store$count[i] == -params$shortHoldPeriod[i]) { # reached holding period
  #     position <- -currentPos[i] # don't stay short
  #     store$count[i] <- 0 # reset count to 0
  #     #print("FORCED ENDED SHORT TRADE")
  #   }
  #   else {# 0 >= store$count[i] != (should be >) -params$holdPeriod
  #     store$count[i] <- store$count[i] - 1
  #     #print(store$count[i])
  #   }
  # }
  # else {
  #   store$count[i] <- 0 # reset count to 0
  # }
  
  #if (position < 0 || currentPos[i] < 0){
  #  print(position)
  #  print(currentPos[i])
  #}
  #print(position)
  #print(currentPos[i])
  #print(store$count[i])
  #print(i)
  
  if (currentPos[i] >= 1 || position >= 1) {  #Check if i have taken a long position
    #print("You should not see this, if you have a long trade has occured")
    if (store$count[i] < 0) { #If last time i was short...
      store$count[i] = 1 # Begin the count
    } else if ((store$count[i] == params$holdPeriod[i])){#} || (store$BreakoutRiskStop[i] > cl)) { # If holding period is reached
      position <- -currentPos[i] # Don't stay long
      store$count[i] <- 0 # Reset count to 0
      #print("FORCED ENDED LONG TRADE")
    } else {
      store$count[i] <- store$count[i] + 1  #Add another day to count (One more day we stayed long)
      #print(store$count[i])
    }
  }

  else if (currentPos[i] <= -1 || position <= -1) { #Check if i have taken a short position
    #print("Entered Short Trade")
    if (store$count[i] > 0) {   #If last time i was long...
      store$count[i] = -1 #Begin the count
    } else if ((store$count[i] == -params$shortHoldPeriod[i])){# || (store$BreakoutRiskStop[i] < cl)) { # If holding period is reached
      #print(c("Left Short Trade after day ", store$count[i]))
      position <- -currentPos[i] # Don't stay short
      store$count[i] <- 0 # Reset count to 0
    } else {# 0 >= store$count[i] != (should be >) -params$holdPeriod
      store$count[i] <- store$count[i] - 1
      #print(store$count[i])
    }
  }
  else {
    store$count[i] <- 0 # reset count to 0
  }
  
  return(list(position=position, store=store))
}

MomentumStrategyType2 <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  #Using Market Orders
  position <- 0
  bidLimitOrderList <- c(0,0,0,0,0,0,0,0,0,0)
  bidLimitPriceList <- c(0,0,0,0,0,0,0,0,0,0)
  
  #Set Up
  cl <- newRowList[[params$series[i]]]$Close
  hi <- newRowList[[params$series[i]]]$High
  lo <- newRowList[[params$series[i]]]$Low
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter])
  
  posSize <- 10000 / cl
  
  #Check for a cross...
  crossingLookback <- 5
  crossedAboveToday <- FALSE
  crossedBelowToday <- FALSE
  crossedAbove <- FALSE
  crossedBelow <- FALSE
  
  
  #NEED THESE ACCESSABBLE FOR THE EXIT STRATEGY
  ShortEMA.Fast <- last(EMA(store$cl[startIndex:store$iter,i],n=params$shortEma.F[i]),crossingLookback)
  ShortEMA.Slow <- last(EMA(store$cl[startIndex:store$iter,i],n=params$shortEma.S[i]),crossingLookback)
  LongEMA.Fast <- last(EMA(store$cl[startIndex:store$iter,i],n=params$longEma.F[i]),crossingLookback)
  LongEMA.Slow <- last(EMA(store$cl[startIndex:store$iter,i],n=params$longEma.S[i]),crossingLookback)
  
  
  
  
  
  
  #FOR SHORTING
  if ((params$shortEma.S[i] != 11) && (params$shortEma.F[i] != 11)){  #CHECK FOR VALUE MEANING I DO NOT WISH TO TRADE THIS SERIES
    #CROSS ABOVE OR BELOW (TODAY)
    if((ShortEMA.Fast[crossingLookback-1] > ShortEMA.Slow[crossingLookback-1]) && ((ShortEMA.Fast[crossingLookback] < ShortEMA.Slow[crossingLookback])))  {
      crossedBelowToday <-  TRUE
    }

    #CROSSED ABOVE OR BELOW (IN THE PAST 3 DAYS)
    for (crossCounter in 2:4){
      if((ShortEMA.Fast[crossCounter] > ShortEMA.Slow[crossCounter]) && ((ShortEMA.Fast[crossCounter+1] < ShortEMA.Slow[crossCounter+1])))  {
        crossedBelow <-  TRUE
      }
    }
  }
  
  #FOR LONGING
  if ((params$longEma.S[i] != 11) && (params$longEma.F[i] != 11)){  #CHECK FOR VALUE MEANING I DO NOT WISH TO TRADE THIS SERIES
    #CROSS ABOVE OR BELOW (TODAY)
    if((LongEMA.Fast[crossingLookback-1] < LongEMA.Slow[crossingLookback-1]) && ((LongEMA.Fast[crossingLookback] > LongEMA.Slow[crossingLookback])))  {
      crossedAboveToday <-  TRUE
    } 

    #CROSSED ABOVE OR BELOW (IN THE PAST 3 DAYS)
    for (crossCounter in 2:4){
      if((LongEMA.Fast[crossCounter] < LongEMA.Slow[crossCounter]) && ((LongEMA.Fast[crossCounter+1] > LongEMA.Slow[crossCounter+1])))  {
        crossedAbove <-  TRUE
      } 
    }
  }
  
  #When Crossed, take todays hi/lo as my market entry point.
  if (crossedAboveToday) {
    store$longEntryStop[i] <- hi
  } else if (crossedBelowToday)  {
    store$shortEntryStop[i] <- lo
  }
  
  #TAKE UP A LONG POSITION
  if ((crossedAbove) && (LongEMA.Fast[crossingLookback] > LongEMA.Slow[crossingLookback])) {
     if (cl > store$longEntryStop[i]) {
      position <- posSize - currentPos[[i]]  #Go long (trend following)
      #
      #RISK STOP, aggregate performance? Rather than individual as it seems that individual is overoptimised....
      store$RiskStop[i] <- cl*0.99
      store$stopLoss[i] <- cl*0.99
     } else {
       ###TRY A LIMIT ORDER AT HI
       #print("Limit Order Attempted")
       #bidLimitOrderList[i] <- posSize
       #bidLimitPriceList[i] <- store$longEntryStop[i]
     }
  }
  
  #TAKE UP A SHORT POSITION
  if((crossedBelow) && (ShortEMA.Fast[crossingLookback] < ShortEMA.Slow[crossingLookback])) {
    if (cl < store$shortEntryStop[i]) {
      position <- -posSize - currentPos[[i]]  #Go short (trend following)
      #store$RiskStop[i] <- cl*1.05
      store$RiskStop[i] <- cl*1.02
      store$stopLoss[i] <- cl*1.02
      #crossAbove <<- crossAbove + 1
    } else {
      ###TRY A LIMIT ORDER AT LO
      #print("Limit Order Attempted")
      #bidLimitOrderList[i] <- posSize
      #bidLimitPriceList[i] <- store$shortEntryStop[i]
    }
  }
  
  
  #EXIT LOGIC
  if (currentPos[i] >= 1 || position >= 1) {  #Check if i have taken a long position
    if ((LongEMA.Fast[crossingLookback] < LongEMA.Slow[crossingLookback]) || (cl < store$RiskStop[i]) || (cl < store$stopLoss[i])) {#|| (cl < store$RiskStop[i])){
      #EXIT AS RSI NEARS OVERBOUGHT || (rsi > store$EntryRSI[i] * 1.1)
      position <- -currentPos[i] # Don't stay Long
      #crossAbove <<- crossAbove + 1
    }
    if (store$stopLoss[i] < cl*0.94) {
      store$stopLoss[i] <- cl*0.94
    }
  }

  else if (currentPos[i] <= -1 || position <= -1) { #Check if i have taken a short position
    if ((ShortEMA.Fast[crossingLookback] > ShortEMA.Slow[crossingLookback]) || (cl > store$RiskStop[i]) || (cl > store$stopLoss[i])) {#|| (cl > store$RiskStop[i])){
      #print("position Exit")
      position <- -currentPos[i] # Don't stay short
      #crossAbove <<- crossAbove + 1
    }
    if (store$stopLoss[i] > cl*1.08) {
     store$stopLoss[i] <- cl*1.08
    }
  }
  
  limitOrders1  <- bidLimitOrderList
  limitPrices1  <- bidLimitPriceList
  
  #print("----------")
  #print(limitOrders1)
  #print(limitPrices1)
  
  return(list(position=position, store=store,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1))
}


MeanRevStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  position <- 0
  
  cl <- newRowList[[params$series[i]]]$Close

  posSize <- 60000 / cl

  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter,i])
  
  #############################################################################################       FUNCTIONS

  #############################################################################################       KELTNER CHANNELS
  keltnerChannels <-
    function (HLC, n = 20, maType, atr = 2, ...)
    {
      atrHLC <- HLC
      HLC <- try.xts(HLC, error = as.matrix)
      if (NCOL(HLC) == 3) {
        if (is.xts(HLC)) {
          xa <- xcoredata(HLC)
          HLC <- xts(apply(HLC, 1, mean), index(HLC))
          xcoredata(HLC) <- xa
        }
        else {
          HLC <- apply(HLC, 1, mean)
        }
      }
      else if (NCOL(HLC) != 1) {
        stop("Price series must be either High-Low-Close, or Close/univariate.")
      }
      maArgs <- list(n = n, ...)
      if (missing(maType)) {
        maType <- "EMA"
      }
      mavg <- do.call(maType, c(list(HLC), maArgs))
      avgtruerange <- ATR(atrHLC, n = n)

      up <- mavg + atr * avgtruerange[,2]
      dn <- mavg - atr * avgtruerange[,2]

      res <- cbind(dn, mavg, up)
      colnames(res) <- c("dn", "mavg", "up")
      reclass(res, HLC)
    }

  #############################################################################################       RSI
  rsiCalc <-	function(clStore,column,iter) {
    startIndex <- iter - params$lookback - 1 # needs 2 extra periods
    rsi <- last(RSI(clStore[startIndex:iter,column],n=params$rsi))
    return(rsi)
  }
  
  rsi <- rsiCalc(store$cl,i,store$iter)
  
  #############################################################################################       UPDATE RSI STORE
  for (rsiCounter in 20:1){
    store$rsiStore[rsiCounter+1,i] <- store$rsiStore[rsiCounter,i]
  }
  store$rsiStore[1,i] <- rsi
  last10RSI <- rev(store$rsiStore[,i])  #From the store get the last x RSI values
  
  #############################################################################################       RSI DIVERGENCE

  rsiLowering <- FALSE
  rsiGrowing <- FALSE
  priceLowering <- FALSE
  priceGrowing <- FALSE
  
  if (!any(is.na(last10RSI))){
    peak20 <- max(last10RSI[1:5])
    peak15 <- max(last10RSI[6:10])
    peak10 <- max(last10RSI[11:15])
    peak05 <- max(last10RSI[16:20])
    
    peak11To20 <- max(peak20,peak15)
    peak01To10 <- max(peak10,peak05)
    
    if((peak11To20 > peak01To10))  {
      #print("LoweringPeaks")
      rsiLowering <- TRUE
    }
    if((peak11To20 < peak01To10))  {
      #print("GrowingPeaks")
      rsiGrowing <- TRUE
    }
    
    
    
    # if((peak15 > peak10) && (peak10 > peak05))  {
    #   #print("LoweringPeaks")
    #   rsiLowering <- TRUE
    # }
    # if((peak15 < peak10) && (peak10 < peak05))  {
    #   #print("GrowingPeaks")
    #   rsiGrowing <- TRUE
    # }
  }
  
  #############################################################################################       PRICE DIVERGENCE
  last10ClosePrice <- last(store$cl[startIndex:store$iter,i],21)  #LAST 10 DAYS (CLOSE PRICES)
  
  peak20Cl <- max(last10ClosePrice[1:5])
  peak15Cl <- max(last10ClosePrice[6:10])
  peak10Cl <- max(last10ClosePrice[11:15])
  peak05Cl <- max(last10ClosePrice[16:20])
  
  peak11To20Cl <- max(peak20Cl,peak15Cl)
  peak01To10Cl <- max(peak10Cl,peak05Cl)
  
  
  if((peak11To20Cl > peak01To10Cl) )  {
    #print("LoweringPeaks")
    priceLowering <- TRUE
  }
  if((peak11To20Cl < peak01To10Cl))  {
    #print("GrowingPeaks")
    priceGrowing <- TRUE
  }
  
  # if((peak15Cl > peak10Cl) && (peak10Cl > peak05Cl) )  {
  #   #print("LoweringPeaks")
  #   priceLowering <- TRUE
  # }
  # if((peak15Cl < peak10Cl) && (peak10Cl < peak05Cl))  {
  #   #print("GrowingPeaks")
  #   priceGrowing <- TRUE
  # }


  #############################################################################################       CALCULATE CROSSES
  #HOW MANY TIMES HAS SMA 3/10 CROSSED IN THE PAST x DAYS
  crossingLookback <- 30
  crossed <- 0
  # sma.3 <- last(SMA(store$cl[startIndex:store$iter,i],n=3),crossingLookback)
  # sma.10 <- last(SMA(store$cl[startIndex:store$iter,i],n=10),crossingLookback)
  ma.3 <- last(EMA(store$cl[startIndex:store$iter,i],n=3),crossingLookback)
  ma.10 <- last(EMA(store$cl[startIndex:store$iter,i],n=10),crossingLookback)

  for (crossCount in 2:crossingLookback){
    if((ma.3[crossCount-1] < ma.10[crossCount-1]) && ((ma.3[crossCount] > ma.10[crossCount])))  {
      crossed <-  crossed + 1
    } else if ((ma.3[crossCount-1] > ma.10[crossCount-1]) && ((ma.3[crossCount] < ma.10[crossCount])))  {
      crossed <-  crossed + 1
    }
  }

  #print(crossed)

  keltner <- last(keltnerChannels(HLCdf, atr=1))

  bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                             n=params$lookback,sd=params$mrSD[i]))     #was 1    
  #For main_optimisation only [i] and in adx for long and short trades

  #############################################################################################       ENTRY LOGIC
  
  #############################################################################################       SHORT
  
  adx <- last(ADX(HLCdf,n=params$adx))

  if (cl > bbands[,"up"]){
    #if (adx[,4] < 25) {
    if (crossed > params$mrShortCrosses[3]){  #was 3
      if(priceGrowing && rsiLowering){  #RSI DIVERGENCE
        position <- -posSize - currentPos[[i]]  #Go short (trend following)
        
        
        #print("Short opp")
        #print(index(cl))
        
        #position <- -params$MRposSizes[params$series[i]] - currentPos[[i]] #Go short (Contrarian)
        store$takeProfit[i] <- cl*0.98
        store$stopLoss[i] <- cl*1.005
        store$stopLossEntryDate[i] <- store$iter
        #store$stopLoss <-
        #print("HERE")
        #print("short opp")



        # #Plot Generation Code
        #          date <- index(cl)  #Trades Date
        #          dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
        #          dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
        #
        #          #PARAMS (for title)
        #          adxParam = 25
        #          emvParam = 0.3
        #          PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
        #
        #          #Generate and Plot Graph
        #          #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
        #          #     width = 10, # The width of the plot in inches
        #          #     height = 10) # The height of the plot in inches
        #          d1 <- dataList[[i]]
        #          graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
        #          print(c("plotted",graphTitle))
        #
        #          #Plotting Function
        #          chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1)),subset = dateLimits, name = graphTitle, plot = TRUE)
        #          addTA(EMA(d1$Close,n=10),on=1)
        #          addTA(EMA(d1$Close,n=3),on=1)
        #          abline(v = 8,col = 'blue')   #X-Axis line, date of enter
        #
        #
        #
        #          #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
        #          #addTA(ADX(HLCdf2,n=14),on=1)
        #
        #          #dev.off()





      }
    }
  }


  #############################################################################################       LONG
  if (cl < bbands[,"dn"]){
    #if (adx[,4] < 25) {
    if (crossed > params$mrCrosses[3]){  #was 3
      if(priceLowering && rsiGrowing){
        position <- posSize - currentPos[[i]]  #Go long (trend following)
        
        #print("long opp")
        #print(index(cl))
        #position <- params$MRposSizes[params$series[i]] - currentPos[[i]] #Go long (Contrarian)
        store$takeProfit[i] <- cl*1.02
        store$stopLoss[i] <- cl*0.995
        store$stopLossEntryDate[i] <- store$iter



        #Plot Generation Code
        # date <- index(cl)  #Trades Date
        # dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
        # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
        #
        # #PARAMS (for title)
        # adxParam = 25
        # emvParam = 0.3
        # PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
        #
        # #Generate and Plot Graph
        # #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
        # #     width = 10, # The width of the plot in inches
        # #     height = 10) # The height of the plot in inches
        # d1 <- dataList[[i]]
        # graphTitle <- paste("Long Opportunity Traded - Series", i,"-DATE:",index(cl))
        # print(c("plotted",graphTitle))
        #
        # #Plotting Function
        # chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1)),subset = dateLimits, name = graphTitle, plot = TRUE)
        # addTA(EMA(d1$Close,n=10),on=1)
        # addTA(EMA(d1$Close,n=3),on=1)
        # abline(v = 8,col = 'blue')   #X-Axis line, date of enter



        #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
        #addTA(ADX(HLCdf2,n=14),on=1)

        #dev.off()

      }
    }
  }

  # print("--------------------------------------")
  # print(store$stopLoss)
  # print(store$stopLossEntryDate)
  #print(keltner[,"mavg"])

  #############################################################################################       EXIT STRATEGY
  
  #My Exit Strategy

  if (currentPos[i] >= 1 || position >= 1) {  #Check if i have taken a long position
    #if ((cl >= keltner[,"mavg"]) || (cl <= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
    if ((cl <= store$takeProfit[i]) || (cl <= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
      position <- -currentPos[i] # Don't stay Long
      store$stopLoss[i] <- 0
      store$stopLossEntryDate[i] <- 0
    }
  }

  else if (currentPos[i] <= -1 || position <= -1) { #Check if i have taken a short position
    #if ((cl <= keltner[,"mavg"]) || (cl >= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
    if ((cl <= store$takeProfit[i]) || (cl >= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
      position <- -currentPos[i] # Don't stay Short
      store$stopLoss[i] <- 0
      store$stopLossEntryDate[i] <- 0
    }
  }

  return(list(position=position, store=store))
}

# MeanRevStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
#   position <- 0
#   cl <- newRowList[[params$series[i]]]$Close
#   bbands <- last(BBands(store$cl[startIndex:store$iter,i],
#                         n=params$lookback,sd=params$sdParamShort))
# 
#   posSize <- 30000 / cl
#   
#   
#   #macdlookback  = 7
#   macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),2)
#   #rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi))
#   rsi.2 <- last(RSI(store$cl[startIndex:store$iter,i],n=2))
#   #print(c(rsi,rsi.2))
#   ema <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
#   
#   HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
#                       Low = store$low[startIndex:store$iter,i],
#                       Close = store$cl[startIndex:store$iter,i])
#   adx <- last(ADX(HLCdf,n=params$adx))
#   
#   
#   ema.20 <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
#   #ema.50 <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort, wilder = FALSE, ratio = NULL))
#   #print(c(ema.20,ema.50))
#   sma.20 <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookback))
#   #sma.50 <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort))
#   
#   obv <- last(OBV(store$cl[startIndex:store$iter,i],store$vol[startIndex:store$iter,i]))
#   #print(obv)
#   
#   #perChange <- ((sma.50-sma.20) / sma.50)
#   #perChange <- (((sma.50-cl) / cl) * 100)
#   #print(perChange)
#   #print(is.matrix(HLCdf))
#   
#   HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
#                       Low = store$low[startIndex:store$iter,i],
#                       Close = store$cl[startIndex:store$iter,i])
#   
#   #ohlcMatrix <- as.xts(OHLCdf)
#   #print(is.xts(ohlcMatrix)
#   
#   
#   # #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA #NEW IDEA
#   
#   #KELTNER CHANNELS FUNCTION
#   
#   keltnerChannels <-
#     function (HLC, n = 20, maType, atr = 2, ...)
#     {
#       atrHLC <- HLC
#       HLC <- try.xts(HLC, error = as.matrix)
#       if (NCOL(HLC) == 3) {
#         if (is.xts(HLC)) {
#           xa <- xcoredata(HLC)
#           HLC <- xts(apply(HLC, 1, mean), index(HLC))
#           xcoredata(HLC) <- xa
#         }
#         else {
#           HLC <- apply(HLC, 1, mean)
#         }
#       }
#       else if (NCOL(HLC) != 1) {
#         stop("Price series must be either High-Low-Close, or Close/univariate.")
#       }
#       maArgs <- list(n = n, ...)
#       if (missing(maType)) {
#         maType <- "EMA"
#       }
#       mavg <- do.call(maType, c(list(HLC), maArgs))
#       avgtruerange <- ATR(atrHLC, n = n)
#       
#       up <- mavg + atr * avgtruerange[,2]
#       dn <- mavg - atr * avgtruerange[,2]
#       
#       res <- cbind(dn, mavg, up)
#       colnames(res) <- c("dn", "mavg", "up")
#       reclass(res, HLC)
#     }
#   
#   #RSI CALCUALTION FUNCTION 
#   
#   rsiCalc <-	function(clStore,column,iter) {
#     startIndex <- iter - params$lookback - 1 # needs 2 extra periods
#     rsi <- last(RSI(clStore[startIndex:iter,column],n=params$rsi))
#     return(rsi)
#   }
#   
#   
# 
#   
#   #print(crossed)
#   
#   #RSI DIVERGENCE
#   
#   last10ClosePrice <- last(store$cl[startIndex:store$iter,i],21)  #LAST 10 DAYS (CLOSE PRICES)
#   #last10ClosePrice <- head(last10ClosePrice,-1) #REMOVE TODAY
#   #print(last10ClosePrice)
#   localHigh <- max(last10ClosePrice)
#   localLow <- min(last10ClosePrice)
#   
#   rsi <- rsiCalc(store$cl,i,store$iter)
#   for (rsiCounter in 20:1){
#     store$rsiStore[rsiCounter+1,i] <- store$rsiStore[rsiCounter,i]
#   }
#   store$rsiStore[1,i] <- rsi
#   
#   #My METHOD USUING THE STORE
#   last10RSI <- rev(store$rsiStore[,i])  #SELECT FOR THIS SERIES ONLY!!!
#   
#   #print("---------------")
#   #print(rsi)
#   #print(last10RSI)
#   #print(last10ClosePrice)
#   
#   rsiLowering <- FALSE
#   rsiGrowing <- FALSE
#   priceLowering <- FALSE
#   priceGrowing <- FALSE
#   
#   if (!any(is.na(last10RSI))){
#     peak20 <- max(last10RSI[1:5])
#     peak15 <- max(last10RSI[6:10])
#     peak10 <- max(last10RSI[11:15])
#     peak05 <- max(last10RSI[16:20])
#     
#     peak11To20 <- max(peak20,peak15)
#     peak01To10 <- max(peak10,peak05)
#     
#     if((peak11To20 > peak01To10))  {
#       #print("LoweringPeaks")
#       rsiLowering <- TRUE
#     }
#     if((peak11To20 < peak01To10))  {
#       #print("GrowingPeaks")
#       rsiGrowing <- TRUE
#     }
#     
#     
#     
#     # if((peak15 > peak10) && (peak10 > peak05))  {
#     #   #print("LoweringPeaks")
#     #   rsiLowering <- TRUE
#     # }
#     # if((peak15 < peak10) && (peak10 < peak05))  {
#     #   #print("GrowingPeaks")
#     #   rsiGrowing <- TRUE
#     # }
#   }
#   
#   
#   peak20Cl <- max(last10ClosePrice[1:5])
#   peak15Cl <- max(last10ClosePrice[6:10])
#   peak10Cl <- max(last10ClosePrice[11:15])
#   peak05Cl <- max(last10ClosePrice[16:20])
#   
#   peak11To20Cl <- max(peak20Cl,peak15Cl)
#   peak01To10Cl <- max(peak10Cl,peak05Cl)
#   
#   
#   if((peak11To20Cl > peak01To10Cl) )  {
#     #print("LoweringPeaks")
#     priceLowering <- TRUE
#   }
#   if((peak11To20Cl < peak01To10Cl))  {
#     #print("GrowingPeaks")
#     priceGrowing <- TRUE
#   }
#   
#   # if((peak15Cl > peak10Cl) && (peak10Cl > peak05Cl) )  {
#   #   #print("LoweringPeaks")
#   #   priceLowering <- TRUE
#   # }
#   # if((peak15Cl < peak10Cl) && (peak10Cl < peak05Cl))  {
#   #   #print("GrowingPeaks")
#   #   priceGrowing <- TRUE
#   # }
#   
#   
#   
#   
#   
#   #####THESE DIVERGENCES ARE RAREEEEEE MAYBE LOWER FROM 20 to 15?
#   
#   #print(last10RSI)
#   
#   #can test for divergence here....
#   
#   #last10rsi <- tail(rsi, n =10)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   #print("-------------------------------------------------")
#   #rsi <- rsiCalc(store$cl,i,store$iter)
#   #last10Rsi <- tail(rsi, n = 5)#
#   #print(last10Rsi)
#   
#   
#   
#   
#   
#   #print("-------------------------------------------------")
#   # rsiTMP <- RSI(store$cl[startIndex:store$iter,i])
#   # #if(i == 8)  {
#   # #  print("IN 8")
#   # #}
#   # last5TMP <- tail(rsiTMP, n = 5)
#   # #print(last5TMP)
#   
#   
#   
#   
#   
#   #print(last10RSI)
#   #print(!any(is.na(last10RSI)))
#   #print(last10ClosePrice)
#   
#   #print(abs(closePrice - last10RSI))  #If larger = Divergence
#   
#   
#   
#   #print(rev(store$rsiStore[,i])) ##This is the last 10 days (most recent day is far right) 
#   #print(cl)
#   #print(closePrice)     #Past 9 days then today...
#   #print(last10RSI)     #Past 9 days then today...
#   
#   
#   #print(store$stopLoss)
#   #print(store$rsiStore)
#   
#   #print(cl)
#   #print("BReak")
#   #print(cl*1.02)
#   
#   keltner <- last(keltnerChannels(HLCdf, atr=1))
#   #Crossed is calculated above
#   
#   #NEED TO SORT OUT DIVERGENCE
#   
#   bbands <- last(BBands(store$cl[startIndex:store$iter,i],
#                         n=params$lookback,sd=1))       #For main_optimisation only [i] and in adx for long and short trades
#   
#   #print(bbands)
#   
#   
#   
#   
#   #CROSSES FUNCTION
#   #HOW MANY TIMES HAS SMA 3/10 CROSSED IN THE PAST 10 DAYS
#   crossingLookback <- 20
#   crossed <- 0
#   # sma.3 <- last(SMA(store$cl[startIndex:store$iter,i],n=3),crossingLookback)
#   # sma.10 <- last(SMA(store$cl[startIndex:store$iter,i],n=10),crossingLookback)
#   sma.3 <- last(EMA(store$cl[startIndex:store$iter,i],n=3),crossingLookback)
#   sma.10 <- last(EMA(store$cl[startIndex:store$iter,i],n=10),crossingLookback)
#   
#   for (crossCount in 2:crossingLookback){
#     if((sma.3[crossCount-1] < sma.10[crossCount-1]) && ((sma.3[crossCount] > sma.10[crossCount])))  {
#       crossed <-  crossed + 1
#     } else if ((sma.3[crossCount-1] > sma.10[crossCount-1]) && ((sma.3[crossCount] < sma.10[crossCount])))  {
#       #crossed <-  crossed + 1
#     }
#   }
#   
#   #print(crossed)
#   
#   dpo <- (DPO(store$cl[startIndex:store$iter,i], n=14))
#   dpo <- (dpo)
#   
#   rsi <- rsiCalc(store$cl,i,store$iter)
#   
#   
#   
#   
#   
#   #if(dpo < 0){
#     if(rsi < 30){
#       print(dpo)
#       print(rsi)
#      # Plot Generation Code
#       date <- index(cl)  #Trades Date
#       dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 7)  #Set Date Limits, Pre-Week/Post-3Week
#       dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
#       
#       #PARAMS (for title)
#       adxParam = 25
#       emvParam = 0.3
#       PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
#       
#       #Generate and Plot Graph
#       #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
#       #     width = 10, # The width of the plot in inches
#       #     height = 10) # The height of the plot in inches
#       d1 <- dataList[[i]]
#       graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
#       print(c("plotted",graphTitle))
#       
#       #Plotting Function
#       chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1),addDPO(n=14),addRSI()),subset = dateLimits, name = graphTitle, plot = TRUE)
#       addTA(EMA(d1$Close,n=10),on=1)
#       addTA(EMA(d1$Close,n=3),on=1)
#       abline(v = 8,col = 'blue')   #X-Axis line, date of enter
#       
#       
#       
#       #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
#       #addTA(ADX(HLCdf2,n=14),on=1)
#       
#       #dev.off()                       
#   
#     }
#   #}
#   # if (cl < bbands[,"dn"]){
#   #   if(priceGrowing && rsiLowering){  #RSI DIVERGENCE
#   #     crossAbove <<- crossAbove + 1
#   #     #Plot Generation Code
#   #                  date <- index(cl)  #Trades Date
#   #                  dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
#   #                  dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
#   # 
#   #                  #PARAMS (for title)
#   #                  adxParam = 25
#   #                  emvParam = 0.3
#   #                  PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
#   # 
#   #                  #Generate and Plot Graph
#   #                  #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
#   #                  #     width = 10, # The width of the plot in inches
#   #                  #     height = 10) # The height of the plot in inches
#   #                  d1 <- dataList[[i]]
#   #                  graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
#   #                  print(c("plotted",graphTitle))
#   # 
#   #                  #Plotting Function
#   #                  chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1),addDPO()),subset = dateLimits, name = graphTitle, plot = TRUE)
#   #                  addTA(EMA(d1$Close,n=10),on=1)
#   #                  addTA(EMA(d1$Close,n=3),on=1)
#   #                  abline(v = 8,col = 'blue')   #X-Axis line, date of enter
#   # 
#   # 
#   # 
#   #                  #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
#   #                  #addTA(ADX(HLCdf2,n=14),on=1)
#   # 
#   #                  #dev.off()
#   #   }
#   # }
#   # 
#   # #if (crossed > 2){
#   #   if(priceGrowing && rsiLowering){  #RSI DIVERGENCE
#   #     crossAbove <<- crossAbove + 1
#   #   }
#   # #}
#     #crossAbove <<- crossAbove + 1
#     
#     # if(priceGrowing && rsiLowering){  #RSI DIVERGENCE
#     #     #print("Short opp")
#     #     #print(index(cl))
#     #     position <- -posSize - currentPos[[i]]  #Go long (trend following)
#     #     #position <- -params$MRposSizes[params$series[i]] - currentPos[[i]] #Go short (Contrarian)
#     #     store$takeProfit[i] <- cl*0.98
#     #     store$stopLoss[i] <- cl*1.005
#     #     store$stopLossEntryDate[i] <- store$iter
#     #     #store$stopLoss <- 
#     #     #print("HERE")
#     #     #print("short opp")
#     #     
#     #     
#     #     
#     #     # #Plot Generation Code
#     #     #          date <- index(cl)  #Trades Date
#     #     #          dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
#     #     #          dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
#     #     # 
#     #     #          #PARAMS (for title)
#     #     #          adxParam = 25
#     #     #          emvParam = 0.3
#     #     #          PlotTitle <- paste("Series",i,"-short-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
#     #     # 
#     #     #          #Generate and Plot Graph
#     #     #          #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
#     #     #          #     width = 10, # The width of the plot in inches
#     #     #          #     height = 10) # The height of the plot in inches
#     #     #          d1 <- dataList[[i]]
#     #     #          graphTitle <- paste("Short Opportunity Traded - Series", i,"-DATE:",index(cl))
#     #     #          print(c("plotted",graphTitle))
#     #     # 
#     #     #          #Plotting Function
#     #     #          chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1)),subset = dateLimits, name = graphTitle, plot = TRUE)
#     #     #          addTA(EMA(d1$Close,n=10),on=1)
#     #     #          addTA(EMA(d1$Close,n=3),on=1)
#     #     #          abline(v = 8,col = 'blue')   #X-Axis line, date of enter
#     #     # 
#     #     # 
#     #     # 
#     #     #          #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
#     #     #          #addTA(ADX(HLCdf2,n=14),on=1)
#     #     # 
#     #     #          #dev.off()
#     #     
#     #     
#     #     
#     #     
#     #     
#     # 
#     # }
#   #}
#   
#   
#   
#   # if (cl < bbands[,"dn"]){
#   #   if (crossed > 3){
#   #     if(priceLowering && rsiGrowing){
#   #       #print("long opp")
#   #       #print(index(cl))
#   #       position <- posSize - currentPos[[i]]  #Go long (trend following)
#   #       #position <- params$MRposSizes[params$series[i]] - currentPos[[i]] #Go long (Contrarian)
#   #       store$takeProfit[i] <- cl*1.02
#   #       store$stopLoss[i] <- cl*0.995
#   #       store$stopLossEntryDate[i] <- store$iter
#   #       
#   #       
#   #       
#   #       #Plot Generation Code
#   #       # date <- index(cl)  #Trades Date
#   #       # dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
#   #       # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
#   #       # 
#   #       # #PARAMS (for title)
#   #       # adxParam = 25
#   #       # emvParam = 0.3
#   #       # PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,"-emv=",emvParam,sep = "")
#   #       # 
#   #       # #Generate and Plot Graph
#   #       # #pdf(file = (paste("Plots/Momentum/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
#   #       # #     width = 10, # The width of the plot in inches
#   #       # #     height = 10) # The height of the plot in inches
#   #       # d1 <- dataList[[i]]
#   #       # graphTitle <- paste("Long Opportunity Traded - Series", i,"-DATE:",index(cl))
#   #       # print(c("plotted",graphTitle))
#   #       # 
#   #       # #Plotting Function
#   #       # chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=1)),subset = dateLimits, name = graphTitle, plot = TRUE)
#   #       # addTA(EMA(d1$Close,n=10),on=1)
#   #       # addTA(EMA(d1$Close,n=3),on=1)
#   #       # abline(v = 8,col = 'blue')   #X-Axis line, date of enter
#   #       
#   #       
#   #       
#   #       #HLCdf2 <- data.frame(d1$High, d1$Low,d1$Close)
#   #       #addTA(ADX(HLCdf2,n=14),on=1)
#   #       
#   #       #dev.off()
#   #       
#   #     }
#   #   }
#   # }
#   # 
#   # # print("--------------------------------------")
#   # # print(store$stopLoss)
#   # # print(store$stopLossEntryDate)
#   # #print(keltner[,"mavg"])
#   # 
#   # #My Exit Strategy
#   # 
#   # if (currentPos[i] >= 1 || position >= 1) {  #Check if i have taken a long position
#   #   #if ((cl >= keltner[,"mavg"]) || (cl <= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
#   #   if ((cl <= store$takeProfit[i]) || (cl <= store$stopLoss[i])) {
#   #     position <- -currentPos[i] # Don't stay Long
#   #     store$stopLoss[i] <- 0
#   #     store$stopLossEntryDate[i] <- 0
#   #   }
#   # }
#   # 
#   # else if (currentPos[i] <= -1 || position <= -1) { #Check if i have taken a short position
#   #   #if ((cl <= keltner[,"mavg"]) || (cl >= store$stopLoss[i]) || (store$iter >= store$stopLossEntryDate[i] + 6)) {
#   #   if ((cl <= store$takeProfit[i]) || (cl >= store$stopLoss[i])) {
#   #     position <- -currentPos[i] # Don't stay Short
#   #     store$stopLoss[i] <- 0
#   #     store$stopLossEntryDate[i] <- 0
#   #   } else {
#   #     #IMPLMENT COUNTING FOR THE NEXT 9 DAYS, then also check if there are no na values also exit
#   #     #store$stopLoss[i,(which(is.na(a)))[1]] <- 2
#   #   }
#   # }
#   
#   
#   #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED #NEW STRAT ENDED
#   
#   #RSI(2) Mean Reversion Strategy P/D = 1
#   ema.n <- last(EMA(store$cl[startIndex:store$iter,i],n=params$mrLookback, wilder = FALSE, ratio = NULL)) # 38 < x < 50             #44 was decent results
#   
#   if (rsi.2 <= 10) {  #Oversold Condition could be a buy opportunity (Undervalued)
#     if (cl < ema.n) {   #Close is greater than ema (Mean reversion is possible)
#       if (adx[,4] < params$mrADX) { #Do not want strong upwards momentum #This is just weak momentum regardless                                   # Less than 25
#         #position <- params$posSizes[params$series[i]] - currentPos[[i]] #Go Long (Mean Reversion)
#         
#         #print(index(cl))
#         
#         # date <- index(cl)
#         # dateLimit <- paste(as.Date(date) - 7,"/", as.Date(date)+ 7)  #Set Date Limits, Pre-Week/Post-3Week
#         # dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
#         # d1 <- dataList[[i]]
#         # one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addRSI(n=2)),subset = dateLimit, name = paste("Mean Rev +/-7Days - DATE:",entryPoints[i]) )
#         # one.AxisX.Maximum = 5;
#         # addTA(SMA(d1$Close,n=50),on=NA)
#         # abline(v = 8,col = 'blue')   #X-Axis line, date of enter
#         
#         #Plot Generation Code
#         # date <- index(cl)  #Trades Date
#         # dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
#         # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
#         # 
#         # #PARAMS (for title)
#         # adxParam = 25
#         # PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,sep = "")
#         # 
#         # #Generate and Plot Graph
#         # #pdf(file = (paste("Plots/Mean/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
#         # #    width = 10, # The width of the plot in inches
#         # #    height = 10) # The height of the plot in inches
#         # d1 <- dataList[[i]]
#         # graphTitle <- paste("Long Opportunity Traded - Series", i,"-DATE:",index(cl))
#         # print(c("plotted",graphTitle))
#         # 
#         # #Plotting Function
#         # chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addRSI(n=2)),subset = dateLimits, name = graphTitle, plot = TRUE)
#         # #addTA(EMA(d1$Close,n=50),on=NA)
#         # #addTA(EMA(d1$Close,n=50),on=NA)
#         # abline(v = 7,col = 'black')   #X-Axis line, date of enter
#         # 
#         # 
#         # #dev.off()
#         
#       }
#     }
#   }
#   
#   #SHORTING
#   if (rsi.2 <= 90) { #Overbought Condition could be a sell opportunity (Overvalued)
#     if (cl > ema.n) { #
#       if (adx[,4] < params$mrADX) {
#         #position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
#         
#         #enteredMarketAt <<- cl
#         #print(index(cl))
#       }
#     }
#   }
#   
#   # #My Exit Strategy
#   # if (currentPos[i] >= 1 || position >= 1) {  #Check if i have taken a long position
#   #   if (cl >= ema.n) {
#   #     position <- -currentPos[i] # Don't stay Long
#   #   }
#   # }
#   # 
#   # else if (currentPos[i] <= -1 || position <= -1) { #Check if i have taken a short position
#   #   if (cl <= ema.n) {
#   #     position <- -currentPos[i] # Don't stay Short
#   #   }
#   # }
#   
#   
#   # #RSI(2) Mean Reversion Strategy P/D = 1
#   # 
#   # ema.34 <- last(EMA(store$cl[startIndex:store$iter,i],n=44, wilder = FALSE, ratio = NULL)) # 38 < x < 50
#   # 
#   # # if (cl < bbands[,"dn"]) {   #Possible option for trend reversal
#   # #   if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
#   # #     if (rsi <= 30) { 
#   # if (rsi.2 <= 10) {
#   #   if (cl > ema.34){
#   #     #       if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
#   #     if (adx[,4] < 25) {    #WE DO NOT WANT STRONG MOMENTUM AGAINST UP (THIS IS JUST WEAK MOMENTUM REGARDLESS)
#   #       #           if(cl < sma.20) {
#   #       #             if(ema.20 < ema.50) {
#   #       #               #if close is relatively low go long (i.e., Mean Reversion)
#   #       #               #print("ping")
#   #       position <- params$posSizes[params$series[i]] - currentPos[[i]] 
#   #       #             }
#   #       #           }
#   #     }
#   #     #       }
#   #   }
#   # }
#   # #   }
#   # # }
#   # #} #else if (cl >= ema){
#   # # position <- 0
#   # #}
#   # 
#   # 
#   # 
#   # 
#   # # if (cl > bbands[,"up"]) {
#   # #   if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
#   # #     if (rsi >= 70) { 
#   # if (rsi.2 <= 90) { 
#   #   if (cl < ema.34){
#   #     #       if(cl > sma.50) {
#   #     #         if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
#   #     if (adx[,4] < 25) {
#   #       #             if(cl > sma.20) {
#   #       #               #if close is relatively low go long (i.e., Mean Reversion)
#   #       #               #print("pong")
#   #       position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
#   #       #             }
#   #     }
#   #     #         }
#   #     #       }
#   #   }
#   # }
#   # # }
#   # 
#   # #} else if (cl <= ema){
#   # #position <- 0
#   # #}
#   
#   
#   
#   
#   # } else if (cl > bbands[,"up"]) {
#   #   if ((macd[1,1] > macd[1,2]) && (macd[2,1] < macd[2,2])){ ##CROSS BELOW
#   #     if (rsi > 70) { 
#   #       #if close is relatively low go long (i.e., Mean Reversion)
#   #       position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
#   #     }
#   #   }
#   # }
#   
#   #Update the store, with the past days rsi values
#   #store$prevRSI.2[i] <- store$prevRSI.1[i]
#   #store$prevRSI.1[i] <- rsi
#   
#   # for (rsiCounter in 9:1){
#   #   store$rsiStore[rsiCounter+1,i] <- store$rsiStore[rsiCounter,i]
#   # }
#   # store$rsiStore[1,i] <- rsi
#   #print(store$rsiStore)
#   
#   return(list(position=position, store=store))
# }


MeanRevStrategy2 <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  
  position <- 0
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
    position <- params$posSizes[params$series[i]] - currentPos[[i]] 
  }
  
  if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
    if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
      position <- params$posSizes[params$series[i]] - currentPos[[i]]
    }
  }
  
  if (sma < ema)  {
    position <- params$posSizes[params$series[i]] - currentPos[[i]]
  }
  
  if (rsi <= 30){
    position <- params$posSizes[params$series[i]] - currentPos[[i]]
  }
  
  if (vwap < bbands[,"dn"]) {
    position <- params$posSizes[params$series[i]] - currentPos[[i]]
  }
  
  
  
  
  #EXIT THE MARKET
  if (cl[i] > bbands[,"up"]) {
    # if close is relatively high go short (again, Mean Reversion)
    position <- -params$posSizes[params$series[i]] - currentPos[[i]]
  }
  
  
  if ((macd[1,1] > macd[1,2]) && (macd[2,1] < macd[2,2])){ ##CROSS BELOW
    position <- -params$posSizes[params$series[i]] - currentPos[[i]]
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
  
  return(position)
}

MarketMakingStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  obv <- last(OBV(store$cl[startIndex:store$iter,i],store$vol[startIndex:store$iter,i]))
  position <- 0
  #cat("currentPos", formatC(currentPos,3),"\n")
  
  askLimitOrderList <- c(0,0,0,0,0,0,0,0,0,0)
  bidLimitOrderList <- c(0,0,0,0,0,0,0,0,0,0)
  askLimitPriceList <- c(0,0,0,0,0,0,0,0,0,0)
  bidLimitPriceList <- c(0,0,0,0,0,0,0,0,0,0)
  
  # check if current inventory is above a limit and if so exit completely
  # with a market order
  #print(currentPos)
  marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)      #Security Measur
  #print("Market ~Orders Below")
  #print(marketOrders)
  
  #print(i)
  if ((i>2) && (i<8)){##8
    #print(i)
    
    #VOLATILITY INDICATORS
    #HLC Data Frame
    HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                        Low = store$low[startIndex:store$iter,i],
                        Close = store$cl[startIndex:store$iter,i])
    HL <- data.frame(High = store$hi[startIndex:store$iter,i],
                        Low = store$low[startIndex:store$iter,i])
    last5ClosePrice <- last(store$cl[startIndex:store$iter,i],5)  #LAST 5 DAYS (CLOSE PRICES)
    close <- store$cl[startIndex:store$iter,i]  #LAST 5 DAYS (CLOSE PRICES)
    
    #ATR SET UP
    atr <- ATR(HLCdf)
    atr <- last(atr)  ##maybe
    ttr <- atr[,"tr"]

    #AVERAGE TRUE RANGE
    atr <- atr[,"atr"]
    normalisedATR <- (as.double(atr) / last5ClosePrice[5]) * 100
    #print("-----------------")
    
    #print(atr)
    #print(normalisedATR)
    
    #print(last5ClosePrice[5])

    #VOLATILITY RATIO (SCHWAGER)
    vr <- ttr/atr
    #print(vr)
    #VR2 WAS NOT USED----

    #CHAIKIN VOLATILITY
    chaikin <- chaikinVolatility(HL)
    chaikin <- last(chaikin)
    #chaikin <- chaikin[[1]
    #print(chaikin)

    #BBandWidth
    
    bbands <- last(BBands(store$cl[startIndex:store$iter,i],n=14,sd=1))
    
    #bbands <- (BBands(cl,n=14,sd=1))
    BW <- ((bbands[,"up"] - bbands[,"dn"])/bbands[,"mavg"])*100
    #print(BW)
    

    #close[5] is today
    #close[1] is 5 days ago
    PerChange5 <- ((as.double(last5ClosePrice[5]) - as.double(last5ClosePrice[1])) / as.double(last5ClosePrice[1])) * 100 
    
    
    
    #SERIES 3
    # LoAtrVal <- 0
    # UpAtrVal <- 1.5
    # LoBBandVal <- 0
    # UpBBandVal <- 2.4
    # LoChaikinVal <- -0.4
    # UpChaikinVal <- 0.5
    # Lo5DayVal <- SKIPVALUE
    # Up5DayVal <- SKIPVALUE
    # Lo10DayVal <- SKIPVALUE
    # Up10DayVal <- SKIPVALUE
    # LoSchwagerVal <- 0.5
    # UpSchwagerVal <- 1.7
    # LoVR2Val <- 0.6
    # UpVR2Val <- 1.3
    
    todaysClosePrice <- last(close)
    atrSpreadPercentage <- 0.1
    atrSpreadPercentage2 <- 0.2
    
    if (i == 3){
      #print(normalisedATR)
      if ((normalisedATR > 0) && (normalisedATR <= 1.5))  {
        #print(BW)
        if ((as.double(BW > 0)) && (as.double(BW <= 2.4)))  {
          #print(chaikin)
          if ((chaikin > -0.4) && (chaikin <= 0.5))  {
            #print(vr)
            if ((vr > 0.5) && (vr <= 1.7))  {
              #print("HERE")
              Series3 <<- Series3 + 1
              bidLimitOrderList[3] <- -1
              bidLimitPriceList[3] <- todaysClosePrice - ((atr/2)*0.3)
              
                
                #print(atr)
              
              
              askLimitOrderList[3] <- 1
              askLimitPriceList[3] <- todaysClosePrice + ((atr/2)*0.3)
              #print(normalisedATR)
            }
          }
        }
      }
    }
    #######set the correct values in these
    if (i == 4){
      #print(PerChange5)
      if ((PerChange5 > -0.2) && (PerChange5 <= 0.9))  {     #PERCENTAGECHANGE NEEDS TO BE CALUCATED
        if ((BW > 0.6) && (BW <= 1.6))  {
          if ((chaikin > -0.3) && (chaikin <= 0.3))  {
            #print("HERE")
            Series4 <<- Series4 + 1
            bidLimitOrderList[4] <- -1
            bidLimitPriceList[4] <- todaysClosePrice - ((atr/2)*0.4)
            
            askLimitOrderList[4] <- 1
            askLimitPriceList[4] <- todaysClosePrice + ((atr/2)*0.4)
            #print(Series4)
          }
        }
      }
    }

    if (i == 5){
      # if ((normalisedATR > 0.2) && (normalisedATR <= 0.7))  {
      #   if ((BW > 0.2) && (BW <= 1.1))  {
      #     #print("HERE")
      #     Series5 <<- Series5 + 1
      #     bidLimitOrderList[5] <- -1
      #     bidLimitPriceList[5] <- todaysClosePrice - ((atr/2)*0.2)
      # 
      #     askLimitOrderList[5] <- 1
      #     askLimitPriceList[5] <- todaysClosePrice + ((atr/2)*0.2)
      #   }
      # }
    }

    if (i == 6){
      if ((normalisedATR > 0) && (normalisedATR <= 1.1))  {
        if ((BW > 1.1) && (BW <= 2))  {
          if ((chaikin > -0.2) && (chaikin <= 0.2))  {
            if ((vr > 0.6) && (vr <= 1.1))  {
              #print("HERE")
              Series6 <<- Series6 + 1
              bidLimitOrderList[6] <- -1
              bidLimitPriceList[6] <- todaysClosePrice - ((atr/2)*0.4)
              
              askLimitOrderList[6] <- 1
              askLimitPriceList[6] <- todaysClosePrice + ((atr/2)*0.4)
            }
          }
        }
      }
    }

    if (i == 7){
      #print(normalisedATR)
      if ((normalisedATR > 1) && (normalisedATR <= 1.8))  {
        if ((chaikin > -0.1) && (chaikin <= 0.4))  {
          #print("HERE")
          Series7 <<- Series7 + 1
          bidLimitOrderList[7] <- -1 #-1
          bidLimitPriceList[7] <- todaysClosePrice - ((atr/2)*0.4)
          
          askLimitOrderList[7] <- 1 #1
          askLimitPriceList[7] <- todaysClosePrice + ((atr/2)*0.4)
        }
      }
    }

    #askLimitPriceList[7] <- todaysClosePrice + ((atr/2)*0.1)


# 
# 
# 
#     #print(spread)
# 
#     #print(last20ClosePrice)
# 
#     #diffs <- diff(last20ClosePrice)
#     #PerChange <- (diffs/last20ClosePrice[1:length(last20ClosePrice)-1])*100
# 
# 
# 
#     #TestValue <- as.double(close[c-1]) * (1 + (slipavgPerChange/2))
# 
# 
# 
# 
#     #print(mean(PerChange))
#     #sapply(1:length(newRowList),function(i)
#       #   newRowList[[i]]$Close)
# 
#     #print(last10ClosePrice)
# 
# 
#     # limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
#     # limitPrices1  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$Close - spread[i]/2)
#     #
#     # limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
#     # limitPrices2  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$Close + spread[i]/2)
# 
#     last20ClosePrice <- last(store$cl[startIndex:store$iter,i],2)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     last20OpenPrice <- last(store$op[startIndex:store$iter,i],2)
#     last20ClosePrice <- head(last20ClosePrice, -1)
#     last20OpenPrice <- tail(last20OpenPrice,length(last20OpenPrice)-1)
# 
# 
# 
#     overnightSlippage <- ((last20OpenPrice-last20ClosePrice) / last20ClosePrice ) * 100
# 
#     avgSlippage <- mean(overnightSlippage)
#     #print(overnightSlippage)
#     #print(avgSlippage)
# 
# 
#     cl <- newRowList[[params$series[i]]]$Close
#     op <- newRowList[[params$series[i]]]$Open
# 
#     #print("-------------------------")
#     #print(last20ClosePrice)
#     #print(last20OpenPrice)
#     #print(slippage)
#     #print(avgSlippage)
# 
#     #l
#     last10HighPrices <- last(store$hi[startIndex:store$iter,i],11)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     last10LowPrices <- last(store$low[startIndex:store$iter,i],11)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     tmp <- last10HighPrices - last10LowPrices
#     #print("---------------------------------")
#     #print(tmp)
# 
# 
#     #ema.10 <- last((EMA(tmp,n=10)),1)
#     #print(ema.10)
# 
#     # use the range (High-Low) as a indicator for a reasonable "spread" for
#     # this pseudo market making strategy
#     spread <- sapply(1:length(newRowList),function(i)
#       params$spreadPercentage * (newRowList[[i]]$High -
#                                    newRowList[[i]]$Low))
# 
#     #spread <- sapply(1:length(newRowList),function(i)
#     #spread <- spread * (1 + overnightSlippage)
# 
#     avgSlippage <- (1 + avgSlippage)
# 
#     #print(avgSlippage)
# 
#     #spread <- spread / 2
#     spread <- spread * avgSlippage
# 
#     #print((spread))
# 
#     # limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
#     # limitPrices1  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$Close * 1.001)
#     #   #newRowList[[i]]$Close + spread[i]*(1+avgSlippage)/2)
#     #   #newRowList[[i]]$Close * (1 + (avgSlippage/2)))
#     #
#     # limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
#     # limitPrices2  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$Close * 0.999)
# 
# 
# 
#     ###c(24,20,142,0,13,96,0,0,0,306)
# 
#     last10HighPrices <- last(store$hi[startIndex:store$iter,i],10)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     last10LowPrices <- last(store$low[startIndex:store$iter,i],10)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     last10ClosePrices <- last(store$hi[startIndex:store$iter,i],10)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
#     last10OpenPrices <- last(store$low[startIndex:store$iter,i],10)  #LAST 20 DAYS (CLOSE PRICES)    -far right is the most recent
# 
#     # print("------------------------")
#     # print(last10HighPrices)
#     # print(last10OpenPrices)
#     # print(last10ClosePrices)
#     # print(last10LowPrices)
# 
# 
# 
# 
# 
# 
# 
#     # limitOrders1  <- c(1,1,1,0,1,1,0,0,0,1)
#     # limitPrices1  <- sapply(1:length(newRowList),function(i)
#     #   (mean(newRowList[[i]]$Close,newRowList[[i]]$Open,newRowList[[i]]$Close) - spread[i]))
#     #
#     # limitOrders2  <- c(-1,-1,-1,0,-1,-1,0,0,0,-1)
#     # limitPrices2  <- sapply(1:length(newRowList),function(i)
#     #   (mean(newRowList[[i]]$Close,newRowList[[i]]$Open,newRowList[[i]]$Close) + spread[i]))
# 
# 
#     #posSizes=c(24,20,142,6,13,96,89,1,42,306)
# 
#     # limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
#     # #limitOrders1  <- c(1,1,0,0,1,0,0,0,0,0) # BUY LIMIT ORDERS
#     # limitPrices1  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$Low * 1.00222)
#     #
#     # limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
#     # #limitOrders2  <- c(-1,-1,0,0,-1,0,0,0,0,0) # SELL LIMIT ORDERS
#     # limitPrices2  <- sapply(1:length(newRowList),function(i)
#     #   newRowList[[i]]$High * 0.99788)
# 
#     #print(limitPrices1[i])
#     #print(limitPrices2[i])
# 
# 
# 
# 
# 
#   }
#   
#   # limitOrders1  <- c(1,1,1,0,1,1,0,0,0,1)
#   # limitPrices1  <- sapply(1:length(newRowList),function(i)
#   #   (mean(newRowList[[i]]$Close,newRowList[[i]]$Open,newRowList[[i]]$Close) - spread[i]))
#   # 
#   # limitOrders2  <- c(-1,-1,-1,0,-1,-1,0,0,0,-1)
#   # limitPrices2  <- sapply(1:length(newRowList),function(i)
#   #   (mean(newRowList[[i]]$Close,newRowList[[i]]$Open,newRowList[[i]]$Close) + spread[i]))
#   
#   #print(askLimitOrderList)
#   #print(bidLimitOrderList)
    
  }
  
  limitOrders1  <- askLimitOrderList
  limitPrices1  <- askLimitPriceList

  limitOrders2  <- bidLimitOrderList
  limitPrices2  <- bidLimitPriceList
  
  # if ('3' %in% limitPrices1){
  #   print("HERE")
  # }
  # if ('3' %in% limitPrices2){
  #   print("HERE")
  # }
  
  
  
  # for (i in 1:10){
  #   if (limitPrices1[i] > limitPrices2[i]){  #if my low price is larger than my high price... as the spread is very small... do not trade
  #       limitOrders1[i] <- 0
  #       limitOrders2[i] <- 0
  #   }
  # }
  
  #print(limitPrices1)
  #print(limitPrices2)
  
  # 29216 37066
  
  #print(marketOrders)
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))

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
    currentHigh <- vector(mode="double",length=length(series))
    prevRSI.1 <- vector(mode="double",length=length(series))
    prevRSI.2 <- vector(mode="double",length=length(series))
    rsiStore <- matrix(ncol= 10, nrow = 21)
    takeProfit <- vector(mode="double",length=length(series))
    stopLoss <- vector(mode="double",length=length(series))
    RiskStop <- vector(mode="double",length=length(series))
    EntryRSI <- vector(mode="double",length=length(series))
    stopLossEntryDate <- vector(mode="double",length=length(series))
    longEntryStop <- vector(mode="double",length=length(series))
    shortEntryStop <- vector(mode="double",length=length(series))
    
    breakout <- vector(mode="double",length=length(series))
    BreakoutRiskStop <- vector(mode="double",length=length(series))
    stMomentum <- vector(mode="double",length=length(series))
    #stopLoss <- matrix(ncol= 10, nrow = 10)
    count <- vector(mode="numeric",length=length(series)) # stores # of days in trade
    return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series)
                ,hi=initHiStore(newRowList,series),low=initLowStore(newRowList,series)
                ,op=initLowStore(newRowList,series),count = count,currentHigh = currentHigh
                ,prevRSI.1 = prevRSI.1,prevRSI.2 = prevRSI.2,rsiStore = rsiStore,stopLoss = stopLoss
                ,stopLossEntryDate = stopLossEntryDate,takeProfit = takeProfit, longEntryStop = longEntryStop,
                shortEntryStop = shortEntryStop, RiskStop = RiskStop, EntryRSI= EntryRSI,BreakoutRiskStop = BreakoutRiskStop ,breakout = breakout, stMomentum = stMomentum))
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