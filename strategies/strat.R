#"strat"=list(lookback=50,lookbackShort=34,MOsdParam=c(1.5,1.5,2,1.5,99,1.5,1.5,1.5,1.5,1.5),MOShortSdParams=c(1.5,99,2,2,99.5,1.5,99,99,99,1.5),sdParamShort=1,adx=14,MOadxSignal=c(25,25,20,25,0,35,25,35,35,25),MOShortadxSignal=c(25,0,25,30,0,30,0,0,0,30),emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(19,18,18,21,0,21,16,21,8,5),shortHoldPeriod=c(21,0,16,14,0,18,0,0,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=c(24,20,142,6,13,96,89,1,42,306),spreadPercentage=0.001,inventoryLimits=rep(10,10))
maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  pos <- allzero
  MMLO1 <- allzero
  MMLP1 <- allzero
  MMLO2 <- allzero
  MMLP2 <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    #for (i in 4:4) {  #One Series
    for (i in 1:length(params$series)) {
      #MO - Momentum Strategy
      x <- MomentumStrategy(store, newRowList, currentPos, info, params, i, startIndex)
      position <- x$position
      store <- x$store
      
      
      #MR - Mean Reversion Strategy
      #x <- MeanRevStrategy(store, newRowList, currentPos, info, params, i, startIndex) #(2)
      #position <- x
      
      #MM - Market Making Strategy
      # MM <- MarketMakingStrategy(store, newRowList, currentPos, info, params, i, startIndex)
      # position <- 0
      # MMLO1[params$series[i]] <- MM$limitOrders1[i]
      # MMLP1[params$series[i]] <- MM$limitPrices1[i]
      # MMLO2[params$series[i]] <- MM$limitOrders2[i]
      # MMLP2[params$series[i]] <- MM$limitPrices2[i]
      
      
      if (position != 0){
        pos[params$series[i]] <- position
      }
    }
  }
  #For market Orders
  marketOrders <- pos
  store <- store
  limitOrders1 <- allzero
  limitPrices1 <- allzero
  limitOrders2 <- allzero
  limitPrices2 <- allzero

  #For Limit Orders (Market Making)
  #limitOrders1 <- pos
  # limitOrders1 <- MMLO1
  # limitPrices1 <- MMLP1
  # limitOrders2 <- MMLO2
  # limitPrices2 <- MMLP2
  
  #print(marketOrders)
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
  
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
                      Close = store$cl[startIndex:store$iter])
  HLdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                     Low = store$low[startIndex:store$iter,i])
  
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
  #Generate MACD Histogram Values
  histogramValue.old <- (macd[macdlookback-2,1] - macd[macdlookback-2,2])   
  histogramValue.new <- (macd[macdlookback,1] - macd[macdlookback,2])   #MOMENTUM IS INCREASING #Most recent   (IF NEW > OLD = Acceleration in momentum) + if crossing above signal line (- if crossing below)
  
  #Look for a short
  #print(bbands)
  #print(store$iter)
  
  if(store$iter >= 968) {}else{     #if you there are less than 31 days of data left, dont enter any trades...
  
  #print(c("param recieced is",params))
    
  #Could also use adx (+/-DMI to confirm up/downtrends)
  if (cl < Shortbbands[,"dn"]) {
     if (histogramValue.new < histogramValue.old && histogramValue.new < 0){ #Downward Momentum & Increasing
       if (adx[,4] > params$MOShortadxSignal[i]) {  #VALUES (0-25:Weak : 25-50:Strong : 50-75:V.Strong : 75-100:Ext.Strong)                                    ##DETECT Sufficient STRENGTH to the breakout        //if very strong maybe give more funds, increase position?
         if (emv[,1] < params$emvShort) {  #VALUES (Break Above 0 is price rise with ease (below is price fall): Further from 0 equals stronger)             ##Avoid Fakeout, Is able to move more and in a positive
           position <- -params$posSizes[params$series[i]] - currentPos[[i]] #Go short (trend following)
           
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
           position <- params$posSizes[params$series[i]] - currentPos[[i]]  #Go long (trend following)
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
  }
  
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
    } else if (store$count[i] == params$holdPeriod[i]) { # If holding period is reached
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
    } else if (store$count[i] == -params$shortHoldPeriod[i]) { # If holding period is reached
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

  #if ((store$count[i]) > 1) {print(store$count[i])}
  #print(store$count)
  #print(store$count[i])
  
  
  return(list(position=position, store=store))
}


MeanRevStrategy <- function(store, newRowList, currentPos, info, params, i, startIndex){ #params inside of the function.
  position <- 0
  cl <- newRowList[[params$series[i]]]$Close
  bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                        n=params$lookback,sd=params$sdParamShort))
  #macdlookback  = 7
  macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),2)
  rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$rsi))
  rsi.2 <- last(RSI(store$cl[startIndex:store$iter,i],n=2))
  #print(c(rsi,rsi.2))
  ema <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
  
  HLCdf <- data.frame(High = store$hi[startIndex:store$iter,i],
                      Low = store$low[startIndex:store$iter,i],
                      Close = store$cl[startIndex:store$iter])
  adx <- last(ADX(HLCdf,n=params$adx))
  
  
  ema.20 <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
  ema.50 <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort, wilder = FALSE, ratio = NULL))
  #print(c(ema.20,ema.50))
  sma.20 <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookback))
  sma.50 <- last(SMA(store$cl[startIndex:store$iter,i],n=params$lookbackShort))
  
  obv <- last(OBV(store$cl[startIndex:store$iter,i],store$vol[startIndex:store$iter,i]))
  #print(obv)
  
  #perChange <- ((sma.50-sma.20) / sma.50)
  perChange <- (((sma.50-cl) / cl) * 100)
  #print(perChange)
  
  
  #RSI(2) Mean Reversion Strategy P/D = 1
  ema.n <- last(EMA(store$cl[startIndex:store$iter,i],n=44, wilder = FALSE, ratio = NULL)) # 38 < x < 50

  if (rsi.2 <= 10) {  #Oversold Condition could be a buy opportunity (Undervalued)
    if (cl > ema.n) {   #Close is greater than ema (Mean reversion is possible)
      if (adx[,4] < 25) { #Do not want strong upwards momentum #This is just weak momentum regardless
        position <- params$posSizes[params$series[i]] - currentPos[[i]] #Go Long (Mean Reversion)
        
        #print(index(cl))
        
        date <- index(cl)
        dateLimit <- paste(as.Date(date) - 7,"/", as.Date(date)+ 7)  #Set Date Limits, Pre-Week/Post-3Week
        dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
        d1 <- dataList[[i]]
        one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addRSI(n=2)),subset = dateLimit, name = paste("Mean Rev +/-7Days - DATE:",entryPoints[i]) )
        one.AxisX.Maximum = 5;
        addTA(SMA(d1$Close,n=50),on=NA)
        abline(v = 8,col = 'blue')   #X-Axis line, date of enter
        
  #Plot Generation Code
        # date <- index(cl)  #Trades Date
        # dateLimits <- paste(as.Date(date) - 7,"/", as.Date(date)+ 21)  #Set Date Limits, Pre-Week/Post-3Week
        # dateLimits <- gsub(" ", "", dateLimits, fixed = TRUE)  #Formatting
        # 
        # #PARAMS (for title)
        # adxParam = 25
        # PlotTitle <- paste("Series",i,"-long-",index(cl),"-adx=",adxParam,sep = "")
        # 
        # #Generate and Plot Graph
        # #pdf(file = (paste("Plots/Mean/",i,"/",PlotTitle,".pdf",sep ="")),   # The directory you want to save the file in
        # #    width = 10, # The width of the plot in inches
        # #    height = 10) # The height of the plot in inches
        # d1 <- dataList[[i]]
        # graphTitle <- paste("Long Opportunity Traded - Series", i,"-DATE:",index(cl))
        # print(c("plotted",graphTitle))
        # 
        # #Plotting Function
        # chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addRSI(n=2)),subset = dateLimits, name = graphTitle, plot = TRUE)
        # #addTA(EMA(d1$Close,n=50),on=NA)
        # #addTA(EMA(d1$Close,n=50),on=NA)
        # abline(v = 7,col = 'black')   #X-Axis line, date of enter
        # 
        # 
        # #dev.off()

      }
    }
  }
  
  if (rsi.2 <= 90) { #Overbought Condition could be a sell opportunity (Overvalued)
    if (cl < ema.n) { #
      if (adx[,4] < 25) {
        position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
        #print(index(cl))
      }
    }
  }
  
  
  
  
  
  
  # #RSI(2) Mean Reversion Strategy P/D = 1
  # 
  # ema.34 <- last(EMA(store$cl[startIndex:store$iter,i],n=44, wilder = FALSE, ratio = NULL)) # 38 < x < 50
  # 
  # # if (cl < bbands[,"dn"]) {   #Possible option for trend reversal
  # #   if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
  # #     if (rsi <= 30) { 
  # if (rsi.2 <= 10) {
  #   if (cl > ema.34){
  #     #       if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
  #     if (adx[,4] < 25) {    #WE DO NOT WANT STRONG MOMENTUM AGAINST UP (THIS IS JUST WEAK MOMENTUM REGARDLESS)
  #       #           if(cl < sma.20) {
  #       #             if(ema.20 < ema.50) {
  #       #               #if close is relatively low go long (i.e., Mean Reversion)
  #       #               #print("ping")
  #       position <- params$posSizes[params$series[i]] - currentPos[[i]] 
  #       #             }
  #       #           }
  #     }
  #     #       }
  #   }
  # }
  # #   }
  # # }
  # #} #else if (cl >= ema){
  # # position <- 0
  # #}
  # 
  # 
  # 
  # 
  # # if (cl > bbands[,"up"]) {
  # #   if (macd[2,1] < 0){ #BELOW ZERO LINE (MAKES BULLISH MORE SIGNIFICANT)
  # #     if (rsi >= 70) { 
  # if (rsi.2 <= 90) { 
  #   if (cl < ema.34){
  #     #       if(cl > sma.50) {
  #     #         if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
  #     if (adx[,4] < 25) {
  #       #             if(cl > sma.20) {
  #       #               #if close is relatively low go long (i.e., Mean Reversion)
  #       #               #print("pong")
  #       position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
  #       #             }
  #     }
  #     #         }
  #     #       }
  #   }
  # }
  # # }
  # 
  # #} else if (cl <= ema){
  # #position <- 0
  # #}
  
  
  
  
  # } else if (cl > bbands[,"up"]) {
  #   if ((macd[1,1] > macd[1,2]) && (macd[2,1] < macd[2,2])){ ##CROSS BELOW
  #     if (rsi > 70) { 
  #       #if close is relatively low go long (i.e., Mean Reversion)
  #       position <- -params$posSizes[params$series[i]] - currentPos[[i]] 
  #     }
  #   }
  # }
  return(position)
}




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
  position <- 0
  #cat("currentPos", formatC(currentPos,3),"\n")
  
  # check if current inventory is above a limit and if so exit completely
  # with a market order
  marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)      #Security Measure
  
  # macd <- last(MACD(store$cl[startIndex:store$iter,i],nFast = params$nfast, nSlow = params$nSlow, nSig = params$nSig, percent = FALSE),2)
  # if ((macd[1,1] < macd[1,2]) && (macd[2,1] > macd[2,2])){ ##CROSS ABOVE SIGNAL LINE (BULLISH)
  #   #we expect to see a price rise (opposite for bullish and price fall)
  # }
  # obv <- last(OBV(store$cl[startIndex:store$iter,i],store$vol[startIndex:store$iter,i]))
  # cl <- newRowList[[params$series[i]]]$Close
  # ema <- last(EMA(store$cl[startIndex:store$iter,i],n=params$lookback, wilder = FALSE, ratio = NULL))
  
  
  
  # use the range (High-Low) as a indicator for a reasonable "spread" for
  # this pseudo market making strategy
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -
                                 newRowList[[i]]$Low))
  
  #print(spread)


  
  
  limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i)
    newRowList[[i]]$Close - spread[i]/2)

  limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i)
    newRowList[[i]]$Close + spread[i]/2)
  
  
  # 29216 37066
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
    count <- vector(mode="numeric",length=length(series)) # stores # of days in trade
    return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series)
                ,hi=initHiStore(newRowList,series),low=initLowStore(newRowList,series)
                ,op=initLowStore(newRowList,series),count = count))
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