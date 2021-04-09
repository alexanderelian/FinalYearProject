source('framework/data.R')
dataList <- getData("PART1")
series <- (dataList[[1]])
#print(typeof(series))
#dnEntry <- series["1970-08-04"]
#dnExit <- dnEntry["/1970-08-28"]
#print(typeof(dnExit))
#print(series$Close)

justClList <- lapply(dataList[1:5], function(x) x$Close)
clInSingleXts <- do.call(cbind, justClList)
colnames(clInSingleXts) <- 1:5

justSpreadList <- lapply(dataList[1:10], function(x) x$High - x$Low)
spreadInSingleXts <- do.call(cbind, justSpreadList)
colnames(spreadInSingleXts) <- 1:10
avgspreadMatrix <- matrix(ncol= 1, nrow = 0)


for (i in 1:10) {
  avgspreadMatrix <- rbind(avgspreadMatrix, (mean(spreadInSingleXts[,i]))/mean(clInSingleXts))
  #print(mean(clInSingleXts[,i]))
}
print(avgspreadMatrix)

#The Size of the spread (% of getting larger/smaller or equal in size)

#print("Spread Changes in Size (%)")
#print("Does the size of the spread change? (up = larger) (%)")
print("How often was MY PREDICTION within todays spread")
#print("how often is todays high greater than yesterdays close and todays low less than yesterdays close?")
#print("High Prices (%)")
#print("Low Prices (%)")

spreadMatrix <- matrix(ncol= 3, nrow = 0)
colnames(spreadMatrix) <- c("Yes","SAFE","No")
plotdataHIT <- 0
plotdataMIS <- 0

successMatrix <- matrix(data=0, ncol= 8, nrow = 3)
colnames(successMatrix) <- c("Control","ATR","BBands","Chaikin","%Change(5)","%Change(10)","Schwager","VR2")
#rownames(successMatrix) <- C("Hit", "Miss", "HITPercent")


for (i in 6:6){
  plotdataHIT <- 0
  plotdataHIT2 <- 0
  plotdataMIS <- 0
  plotdataMIS2 <- 0

  
  plotTitle <-  paste("Series", i, "Hits", " - ", "Indicator: %Change (5Days)")
  plotTitle2 <-  paste("Series", i, "Hits", " - ", "Indicator: %Change (10Days)")
  plotTitleMIS <-  paste("Series", i, "Miss", " - ", "Indicator: %Change (5Days)")
  plotTitleMIS2 <-  paste("Series", i, "Miss", " - ", "Indicator: %Change (10Days)")
  
  
  
  series <- (dataList[[i]])
  obv <- (OBV(series$Close,series$Volume))
  vwap <- (VWAP(series$Close, series$Volume))
  #print(obv)
  s <- series$High - series$Low
  close <- series$Close
  open <- series$Open
  low <- series$Low
  high <- series$High
  len <- length(s)
  
  HLCdf <- data.frame(High = high,
                      Low = low,
                      Close = close)
  HL <- data.frame(High = high,
                   Low = low)
  
  
  #ATR,VR(schwager),VR2,Chaikin,BBands, CHOP
  
  atr <- ATR(HLCdf)
  ttr <- atr[,"tr"]
  
  atr <- atr[,"atr"] 
  vr <- ttr/atr

  ema <- EMA(ttr)
  vr2 <- (ttr/ema)
  
  chaikin <- chaikinVolatility(HL)
  chaikin <- chaikin[[1]]
  
  bbands <- (BBands(close,n=14,sd=1))
  BW <- ((bbands[,"up"] - bbands[,"dn"])/bbands[,"mavg"])*100
  colnames(BW) <- "bndWidth"
  bbands <- cbind(bbands, BW)
  
  #ChopinessIndex
  n <- 10 #User Defined Period
  logN <- log10(n)
  atr.1 <- ATR(HLCdf,n=1)
  atr.1 <- atr.1[,"atr"]
  #print(atr.1)
  
  
  
  #ChopinessIndex <- (100 * log10(chopSum) / ()) / logN
  
  #last20ClosePrice <- last(as.double(close,20))  #LAST 10 DAYS (CLOSE PRICES)
  
  #print(close)
  
  #diffs <- diff(last20ClosePrice)
  #PerChange <- (diffs/last20ClosePrice[1:length(last20ClosePrice)-1])*100
 
  #print(mean(PerChange))
  
  
  upcount <- 0 ; eqcount <- 0 ; dncount <- 0
  for (c in 20:len) { #FOR THE FIRST 20 days to the end
    
    yesterdaySpread = high[c-1] - close[c-1]
    HalfSpread <- yesterdaySpread/10
    
    last20 <- c(as.double(close[c-20]),close[c-19],close[c-18],close[c-17],close[c-16],
            close[c-15],close[c-14],close[c-13],close[c-12],close[c-11],close[c-10],
            close[c-9],close[c-8],close[c-7],close[c-6],close[c-5],close[c-4],
            close[c-3],close[c-2],close[c-1])
    
    last10 <- c(as.double(close[c-10]),close[c-9],close[c-8],close[c-7],close[c-6],
                           close[c-5],close[c-4],close[c-3],close[c-2],close[c-1])
    last5 <- c(as.double(close[c-5]),close[c-4],close[c-3],close[c-2],close[c-1])
    last3 <- c(as.double(close[c-3]),close[c-2],close[c-1])
    last2 <- c(as.double(close[c-2]),close[c-1])
    
    #print(last10)
    #print(length(last20))
    diffs <- diff(last3)
    #print(length(diffs))
    PerChange <- (diffs/last3[1:length(last3)-1])
    #print((PerChange))
    avgPerChange <- mean(PerChange)
    
    #print(avgPerChange)
    
    
    last20Open <- c(as.double(open[c-20]),open[c-19],open[c-18],open[c-17],open[c-16],
                    open[c-15],open[c-14],open[c-13],open[c-12],open[c-11],open[c-10],
                    open[c-9],open[c-8],open[c-7],open[c-6],open[c-5],open[c-4],
                    open[c-3],open[c-2],open[c-1])

    last10Open <- c(as.double(open[c-10]),open[c-9],open[c-8],open[c-7],open[c-6],
                    open[c-5],open[c-4],open[c-3],open[c-2],open[c-1])


    Overnightslippage <- c(as.double(close[c-20]) - as.double(open[c-19]),as.double(close[c-19]) - as.double(open[c-18]),as.double(close[c-18]) - as.double(open[c-17]),
                           as.double(close[c-17]) - as.double(open[c-16]),as.double(close[c-16]) - as.double(open[c-15]),as.double(close[c-15]) - as.double(open[c-14]),
                           as.double(close[c-14]) - as.double(open[c-13]),as.double(close[c-13]) - as.double(open[c-12]),as.double(close[c-12]) - as.double(open[c-11]),
                           as.double(close[c-11]) - as.double(open[c-10]),as.double(close[c-10]) - as.double(open[c-9]),as.double(close[c-9]) - as.double(open[c-8]),
                           as.double(close[c-8]) - as.double(open[c-7]),as.double(close[c-7]) - as.double(open[c-6]),as.double(close[c-6]) - as.double(open[c-5]),
                           as.double(close[c-5]) - as.double(open[c-4]),as.double(close[c-4]) - as.double(open[c-3]),as.double(close[c-3]) - as.double(open[c-2]),
                           as.double(close[c-2]) - as.double(open[c-1]),as.double(close[c-1]) - as.double(open[c]))

    Overnightslippage5 <- c(as.double(close[c-5]) - as.double(open[c-4]),as.double(close[c-4]) - as.double(open[c-3]),as.double(close[c-3]) - as.double(open[c-2]),
                           as.double(close[c-2]) - as.double(open[c-1]),as.double(close[c-1]) - as.double(open[c]))
    
    Overnightslippage3 <- c(as.double(close[c-3]) - as.double(open[c-2]), as.double(close[c-2]) - as.double(open[c-1]),as.double(close[c-1]) - as.double(open[c]))
    Overnightslippage2 <- c(as.double(close[c-3]) - as.double(open[c-2]), as.double(close[c-2]) - as.double(open[c-1]))
    
    
    # print("--------------------------------------")
    # print(c(as.double(close[c-3]), as.double(close[c-2])))
    # print(c(as.double(open[c-2]), as.double(open[c-1])))
    # 
    # 
    # print(Overnightslippage2)
    
    
    if ((abs(Overnightslippage2[1]) < 0.5) && (abs(Overnightslippage2[2]) < 0.5)) {
      #print("LOW VOLATILITY")
      #print(Overnightslippage3) #NEXT SLIPPAGE TO SEE HOW IT GOES
    }
    
    Overnightslippage3 <- Overnightslippage3[Overnightslippage3 != 0]
    
    #print(Overnightslippage3)
    
    slipdiffs <- diff(Overnightslippage3)

    #print((slipdiffs))
    slipPerChange <- (slipdiffs/Overnightslippage3[1:length(Overnightslippage3)-1])   #*100 for percentage?
    #print((slipPerChange))
    slipavgPerChange <- sum(slipPerChange) / 3
    ###MASSIVE VALUES DOES NOT SEEM CPRRECT
    #print(slipavgPerChange)
    
    
    # print("-------------------------------")
    # print(avgPerChange)
    # print(close[c-1])
    # print(close[c-1] * (1 + avgPerChange))
    
    #TestValue <- mean(high[c-1], low[c-1], open[c-1], close[c-1])    #Value to see if it can effectivly predict a value within tommorows spread
    #TestValue <- as.double(close[c-1]) * (1 + (slipavgPerChange/2))
    
    spread <- 0.001 * high[c-1]-low[c-1]
    
    #TestValue <- close[c-1] - ((high[c-1] - low[c-1])/2)
    #TestValue <- mean(close[c-1],open[c-1],close[c-1])
    #TestValue <- (close[c-1] + close[-1] + open[c-1] )/ 3
    TestValue <- close[c-1] * 1.03966
    #print("--------------")
    #print(as.double(close[c-1]))
    #print(as.double(open[c-1]))
    #print(as.double(TestValue))
    if (close[c-1] < open[c-1]){
      #TestValue <- TestValue * 
    }
    
    chopSum <- sum(atr.1[c-1],n)
    #print(chopSum)
    last10High <- c(as.double(high[c-11]),high[c-10],high[c-9],high[c-8],high[c-7],high[c-6],
                    high[c-5],high[c-4],high[c-3],high[c-2],high[c-1])
    last10Low <- c(as.double(low[c-11]),low[c-10],low[c-9],low[c-8],low[c-7],low[c-6],
                   low[c-5],low[c-4],low[c-3],low[c-2])
    
    
    maxHi <- max(last10High)
    minLo <- min(last10Low)
    #print(minLo)
    
    
    chop <- 100 * (log10(chopSum) / (maxHi - minLo) ) / logN
    
    #print(chop[c])#The last 10 data is acc last 11 days so this is correct!
    
    
    TestValue <- close[c-1]
    #How often was my prediction within todays spread?
    if ((as.double(TestValue) >= as.double(low[c])) && (as.double(TestValue) <= as.double(high[c]))) {
      upcount <- upcount+1
      #When i hit, check the volatility indicaots, and look for a pattern...
      #print("-----------------------")
      #print(vr[c-1])
      #print(vr2[c-1])
      
      #I have hit, so lets check what yesterdays ATR value was....
      
      #print(bbands[c-1])
      #print(chaikin[c-1])
      #print(vr[c-1])
      #print(vr2[c-1])
      #print(chop)

    
      
      #print(atr[c-1])
      #plottingATRHITTER <- (atr[c-1] / close[c-1]) * 100
      #plotdataHIT <- rbind(plotdataHIT, as.double(plottingATRHITTER))
      
      plotdataHIT <- rbind(plotdataHIT, as.double(chaikin[c-1]))
      
      #plotdataHIT <- rbind(plotdataHIT, as.double(vr[c-1]))
      
      #plotdataHIT <- rbind(plotdataHIT, as.double(vr2[c-1]))
      
      #PerChange.5 <- ((as.double(close[c-1]) - as.double(close[c-5])) / as.double(close[c-5])) * 100 
      #plotdataHIT <- rbind(plotdataHIT, as.double(PerChange.5))
      #PerChange.10 <- ((as.double(close[c-1]) - as.double(close[c-10])) / as.double(close[c-10])) * 100 
      #plotdataHIT <- rbind(plotdataHIT, as.double(PerChange.10))
      
      #bband <- bbands[c-1]
      #bband <- bband[,"bndWidth"]
      #plotdataHIT <- rbind(plotdataHIT, as.double(bband))
      
    } else {
      dncount <- dncount+1
      
      #plottingATRMISS <- (atr[c-1] / close[c-1]) * 100
      #plotdataMIS <- rbind(plotdataMIS, as.double(plottingATRMISS))
      
      plotdataMIS <- rbind(plotdataMIS, as.double(chaikin[c-1]))
      
      #plotdataMIS <- rbind(plotdataMIS, as.double(vr[c-1]))
      
      #plotdataMIS <- rbind(plotdataMIS, as.double(vr2[c-1]))
      
      #PerChange.5 <- ((as.double(close[c-1]) - as.double(close[c-5])) / as.double(close[c-5])) * 100 
      #plotdataMIS <- rbind(plotdataMIS, as.double(PerChange.5))
      #PerChange.10 <- ((as.double(close[c-1]) - as.double(close[c-10])) / as.double(close[c-10])) * 100 
      #plotdataMIS <- rbind(plotdataMIS, as.double(PerChange.10))
      
      #bband <- bbands[c-1]
      #bband <- bband[,"bndWidth"]
      #plotdataMIS <- rbind(plotdataMIS, as.double(bband))
      
      
      
      
      
      
      
      
      # plottingATR <- (atr[c-1] / close[c-1]) * 100
      # plotdata <- rbind(plotdata, as.double(plottingATR))
      #plotdata <- rbind(plotdata, chop)
    }
    # print(c("close:", close[c-1]))
    # print(c("prediction:", TestValue))
    # print(c("high:", high[c], "low:", low[c]))
    
    
    
    
    # if (close[c-1] > open[c-1]){  #if today it closed higger than it opened
    #   if ((as.double(open[c])) > (as.double(close[c-1]))){
    #     upcount <- upcount+1
    #   }
    # }
    # if (close[c-1] < open[c-1]){  #if today it closed higger than it opened
    #   if ((as.double(open[c])) < (as.double(close[c-1]))){
    #     upcount <- upcount+1
    #   }
    # }
    
    #How often was todays spread the same size as yesterdays spread?
    # if (as.integer(high[c-1]-low[c-1]) == as.integer(high[c]-low[c])) {
    #   upcount <- upcount+1
    # } else {
    #   dncount <- dncount+1
    # }
    
    
    
    # if (as.double(close[c-1]) > as.double(open[c])) {upcount <- upcount+1}
    # if (as.double(close[c-1]) < as.double(open[c])) {dncount <- dncount+1}
    # if (as.double(close[c-1]) == as.double(open[c])) {eqcount <- eqcount+1}
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #highDifference <- ((as.integer(high[c-1]) - as.integer(high[c]))/as.integer(high[c]))*100
    
    #if (as.integer(high[c]) > as.integer(close[c-1]) & as.integer(low[c]) < as.integer(close[c-1])) {upcount <- upcount+1} else {dncount <- dncount+1}
    
    # if (as.integer(close[c-1]) < as.integer(low[c])) {upcount <- upcount+1}
    # if (as.integer(close[c-1]) > as.integer(low[c])) {dncount <- dncount+1}
    # if (as.integer(close[c-1]) == as.integer(low[c])) {eqcount <- eqcount+1}
    
    
    #print(close[c])
    
    #how often was todays high greater than yesterdays - 0.22%
    # if (as.integer(high[c-1]*0.9978) < as.integer(high[c])) {upcount <- upcount+1}  #If tommorows high is larger - i will execute
    # if (as.integer(high[c-1]*0.9978) == as.integer(high[c])) {upcount <- upcount+1} #If tommorows high is the same - i will execute
    # 
    # if (as.integer(high[c-1]*0.9978) > as.integer(high[c])) {dncount <- dncount+1}  #If tommorows high is lower - i will NOT execute
    # 
    # if (as.integer(low[c-1]*1.0022) < as.integer(low[c])) {dncount <- dncount+1}  #If tommorows low is larger - i will NOT execute
    # if (as.integer(low[c-1]*1.0022) == as.integer(low[c])) {upcount <- upcount+1} #If tommorows high is the same - i will execute
    # 
    
    #if ((as.integer(high[c-1]*0.9978) <= as.integer(high[c])) && (as.integer(low[c-1]*1.0022) >= as.integer(low[c]))) 
    # if ((as.integer(high[c-1]*0.9978) <= as.integer(high[c])) && (as.integer(low[c-1]*1.0022) >= as.integer(low[c]))) {
    #   upcount <- upcount+1  #If tommorows high is larger AND tommorows low is smaller (I WILL EXECUTE)
    #   if (as.integer(high[c-1]*0.9978) > as.integer(low[c-1]*1.0022)) {
    #     eqcount <- eqcount + 1
    #   }
    # }
    # 
    # else
    # {dncount <- dncount+1}

    
    
    
    # if (as.integer(low[c-1]*1.0022) > as.integer(low[c])) {upcount <- upcount+1}  #If tommorows high is lower - i will  execute
    
    # if (as.integer(obv[c-1]) < as.integer(obv[c])) {
    #   if(as.integer(series$High[c-1]) < as.integer(series$High[c])) {
    #     upcount <- upcount + 1
    #   }
    # }
    # if (as.integer(obv[c-1]) > as.integer(obv[c])) {
    #   if(as.integer(series$High[c-1]) > as.integer(series$High[c])) {
    #     dncount <- dncount + 1
    #   }
    # }
    
    #How close is vwap to predicting acc price
    # if (as.integer(vwap[c]) < as.integer(close[c])) {upcount <- upcount+1}
    # if (as.integer(vwap[c]) > as.integer(close[c])) {dncount <- dncount+1}
    # if (as.integer(vwap[c]) == as.integer(close[c])) {eqcount <- eqcount+1}
    
    #If everyday the spread gets (larger / smaller / same )
    # if (as.integer(s[c-1]) < as.integer(s[c])) {upcount <- upcount+1}
    # if (as.integer(s[c-1]) > as.integer(s[c])) {dncount <- dncount+1}
    # if (as.integer(s[c-1]) == as.integer(s[c])) {eqcount <- eqcount+1}
    
    #Does the high price change? on how many % of days
    # if (as.integer(series$High[c-1]) < as.integer(series$High[c])) {upcount <- upcount+1}
    # if (as.integer(series$High[c-1]) > as.integer(series$High[c])) {dncount <- dncount+1}
    # if (as.integer(series$High[c-1]) == as.integer(series$High[c])) {eqcount <- eqcount+1}
    
    #Does the low price change? on how many % of days
    # if (as.integer(series$Low[c-1]) < as.integer(series$Low[c])) {upcount <- upcount+1}
    # if (as.integer(series$Low[c-1]) > as.integer(series$Low[c])) {dncount <- dncount+1}
    # if (as.integer(series$Low[c-1]) == as.integer(series$Low[c])) {eqcount <- eqcount+1}
    
    
    ###ACCESS THE CORRECT VALUES PER VOLATILITY INDEX
    
    #print(atr[c-1])
    plottingATRHITTER <- (atr[c-1] / close[c-1]) * 100
    YestATR <- as.double(plottingATRHITTER)
    
    YestChaikin <- as.double(chaikin[c-1])
    
    YestVR <- as.double(vr[c-1])
    
    YestVR2 <- as.double(vr2[c-1])
    
    PerChange.5 <- ((as.double(close[c-1]) - as.double(close[c-5])) / as.double(close[c-5])) * 100 
    YestChange5 <- as.double(PerChange.5)
    
    PerChange.10 <- ((as.double(close[c-1]) - as.double(close[c-10])) / as.double(close[c-10])) * 100 
    YestChange10 <- as.double(PerChange.10)
    
    bband <- bbands[c-1]
    bband <- bband[,"bndWidth"]
    YestBBand <- as.double(bband)
    
    TestValue <- close[c-1]
    SKIPVALUE <- 999999999
    
    
    LoAtrVal <- 0
    UpAtrVal <- 1
    LoBBandVal <- SKIPVALUE
    UpBBandVal <- SKIPVALUE
    LoChaikinVal <- -0.2
    UpChaikinVal <- 0.2
    Lo5DayVal <- SKIPVALUE
    Up5DayVal <- SKIPVALUE
    Lo10DayVal <- SKIPVALUE
    Up10DayVal <- SKIPVALUE
    LoSchwagerVal <- SKIPVALUE
    UpSchwagerVal <- SKIPVALUE
    LoVR2Val <- SKIPVALUE
    UpVR2Val <- SKIPVALUE
    
    
    
    if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
      successMatrix[1,"Control"] <- successMatrix[1,"Control"] + 1
    } else {
      successMatrix[2,"Control"] <- successMatrix[2,"Control"] + 1
    }
    
    if ((YestATR >= LoAtrVal) && (YestATR <= UpAtrVal)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"ATR"] <- successMatrix[1,"ATR"] + 1
      } else {
        successMatrix[2,"ATR"] <- successMatrix[2,"ATR"] + 1
      }
    }
    
    if ((YestBBand >= LoBBandVal) && (YestBBand <= UpBBandVal)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"BBands"] <- successMatrix[1,"BBands"] + 1
      } else {
        successMatrix[2,"BBands"] <- successMatrix[2,"BBands"] + 1
      }
    }

    if (c-1 == 19) {
      #FIRST VALUE IS NA FOR SOME REASON JUST SKIP THIS ONE
    } else { 
      if ((YestChaikin >= LoChaikinVal) && (YestChaikin <= UpChaikinVal)){
        if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
          successMatrix[1,"Chaikin"] <- successMatrix[1,"Chaikin"] + 1
        } else {
          successMatrix[2,"Chaikin"] <- successMatrix[2,"Chaikin"] + 1
        }
      }
    }
    
    if ((YestChange5 >= Lo5DayVal) && (YestChange5 <= Up5DayVal)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"%Change(5)"] <- successMatrix[1,"%Change(5)"] + 1
      } else {
        successMatrix[2,"%Change(5)"] <- successMatrix[2,"%Change(5)"] + 1
      }
    }
    
    if ((YestChange10 >= Lo10DayVal) && (YestChange10 <= Up10DayVal)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"%Change(10)"] <- successMatrix[1,"%Change(10)"] + 1
      } else {
        successMatrix[2,"%Change(10)"] <- successMatrix[2,"%Change(10)"] + 1
      }
    }
    
    if ((YestVR >= LoSchwagerVal) && (YestVR <= UpSchwagerVal)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"Schwager"] <- successMatrix[1,"Schwager"] + 1
      } else {
        successMatrix[2,"Schwager"] <- successMatrix[2,"Schwager"] + 1
      }
    }
    
    if ((YestVR2 >= LoVR2Val) && (YestVR2 <= UpVR2Val)){
      if ((as.double(TestValue) > as.double(low[c])) && (as.double(TestValue) < as.double(high[c]))) {
        successMatrix[1,"VR2"] <- successMatrix[1,"VR2"] + 1
      } else {
        successMatrix[2,"VR2"] <- successMatrix[2,"VR2"] + 1
      }
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  spreadMatrix <- rbind(spreadMatrix, c(upcount,eqcount,dncount))
  
  par(mfrow = c(1, 1))  ##HOW MANY PLOTS ON ONE PDF
  
  plotdataHIT <- na.omit(plotdataHIT[-1])  #REMOVE THE FIRST VALUE
  plotdataMIS <- na.omit(plotdataMIS[-1])
  maxHIT <- max(plotdataHIT)
  maxMIS <- max(plotdataMIS)
  maxVAL <- max(c(maxHIT, maxMIS))
  
  minHIT <- min(plotdataHIT)
  minMIS <- min(plotdataMIS)
  minVAL <- min(c(minHIT, minMIS))
  
  
  #plotdataHIT2 <- na.omit(plotdataHIT2[-1])  #REMOVE THE FIRST VALUE
  #plotdataMIS2 <- na.omit(plotdataMIS2[-1])
  #maxHIT2 <- max(plotdataHIT2)
  #maxMIS2 <- max(plotdataMIS2)
  #maxVAL2 <- max(c(maxHIT2, maxMIS2))
#  
  #minHIT2 <- min(plotdataHIT2)
  #minMIS2 <- min(plotdataMIS2)
  #minVAL2 <- min(c(minHIT2, minMIS2))
  
  #SAVE TO THE FOLDER
  #dev.copy(pdf,file=paste('/Users/alexe/OneDrive/Desktop/backtester_v5.5 (1)/backtester_v5.5/plots/Market Making/Volatility Plots/PerChange/Histogram',i,'.pdf') ,
  #         width = 10, # The width of the plot in inches
  #          height = 10) # The height of the plot in inches
  #h1 <- hist(plotdataHIT, xlab ="Volatility Value", xlim = c(floor(minVAL),ceiling(maxVAL)), ylim = c(0,300), breaks = seq(floor(minVAL),ceiling(maxVAL), 0.2), main = plotTitle)
  #h2 <- hist(plotdataHIT2, xlab ="Volatility Value", xlim = c(floor(minVAL2),ceiling(maxVAL2)), ylim = c(0,300), breaks = seq(floor(minVAL2),ceiling(maxVAL2), 0.2), main = plotTitle2)
  
  #m1 <- hist(plotdataMIS, xlab ="Volatility Value", xlim = c(floor(minVAL),ceiling(maxVAL)), ylim = c(0,300), breaks = seq(floor(minVAL),ceiling(maxVAL), 0.2), main = plotTitleMIS)
  #m2 <- hist(plotdataMIS2, xlab ="Volatility Value", xlim = c(floor(minVAL2),ceiling(maxVAL2)), ylim = c(0,300), breaks = seq(floor(minVAL2),ceiling(maxVAL2), 0.2), main = plotTitleMIS2)
  #dev.off()
  
  
  #print(plotdataHIT)
  #print(plotdataMIS)
  
  #print(minVAL)
  #print(maxVAL)
  
  #Floor and Ceiling the values
  
#NEED TO FLOOR TO ONE DECIMAL PLACE!
  minBin <- floor(minVAL)
  maxBin <- ceiling(maxVAL)
  
  #print(minVAL)
  #print(minBin)
  #print(maxVAL)
  #print(maxBin)
  
  binWidth <- 0.1
  range <- maxBin - minBin
  #print(range)
  totalBins <- range / binWidth
  #print(totalBins)
  
  #vgspreadMatrix <- matrix(ncol= 1, nrow = 0)
  HMMatrix <- matrix(data=0,nrow=2,ncol=totalBins)
  colnames(HMMatrix) <- as.character(seq(from=minBin, to=maxBin-binWidth, by=binWidth))
  rownames(HMMatrix) <- c("Hit", "Miss")
  #print(HMMatrix)
  
  #FOR LOOP FROM LOW VALUE TO TOP VALUE BY BIN SIZE
  
  ####for(i in seq(from=, to=, by=)){}        HOW TO HANDLE NEGATIVESSSS
  
  for (hitLoop in 1:length(plotdataHIT))  {
    for(counter in seq(from=minBin, to=maxBin-binWidth, by=binWidth)){
      #print(counter)
    #for (counter in 1:totalBins) {
      x <- plotdataHIT[hitLoop]
      #print(paste("bindwith is: ", counter, " to ", counter+binWidth))
      #if((x > binWidth*(counter-1) && x < (binWidth*counter))){
      if ((x > counter) && (x <= (counter+binWidth))){
        #print(as.character(counter))
        HMMatrix[1,as.character(counter)] <- HMMatrix[1,as.character(counter)] + 1
        #print(HMMatrix[1,as.character(counter)])
      }
    }
  }
  
  for (misLoop in 1:length(plotdataMIS))  {
    for(counter in seq(from=minBin, to=maxBin-binWidth, by=binWidth)){
      x <- plotdataMIS[misLoop]
      if ((x > counter) && (x <= (counter+binWidth))){
        #print(as.character(counter))
        #print(paste("bindwith is: ", binWidth*(counter-1), " to ", binWidth*(counter)))
        HMMatrix[2,as.character(counter)] <- HMMatrix[2,as.character(counter)] + 1
      }
    }
  }
  
  #print(length(plotdataHIT))
  #print(length(plotdataMIS))
  
  #HMMatrix
  
  library(RColorBrewer)

  coul <- brewer.pal(3, "Set1") 
  coul <- head(coul, -1)
  coul <- rev(coul)
  # Transform this data in %
  data_percentage <- apply(HMMatrix, 2, function(x){x*100/sum(x,na.rm=T)})
  
  # Make a stacked barplot--> it will be in %!
  print(paste("-------- SERIES ",i," --------"))
  print(HMMatrix)
  
  #dev.copy(pdf,file=paste('/Users/alexe/OneDrive/Desktop/backtester_v5.5 (1)/backtester_v5.5/plots/Market Making/Volatility Plots/chaikin/Attempt2StackedPercentage',i,'.pdf') ,
  #         width = 10, # The width of the plot in inches
  #          height = 10) # The height of the plot in inches
  StackedChart <-  paste("Series", i, " - ", "Indicator: Chaikin","      (Blue = Hit / Red = Miss)")
  barplot(main=StackedChart,data_percentage, col=coul , border="white", xlab="Volatility Values (LessThan or EqualTo)", ylab="Percentage (%)")
  
  #dev.off()
  #addtable2plot(table=HMMatrix)
  
}

print(spreadMatrix)

rownames(successMatrix) <- c("Hit", "Miss", "Hit%")

for (i in 1:8){
  successMatrix[3,i] <- (successMatrix[1,i] / (successMatrix[1,i] + successMatrix[2,i])) * 100
}

print(successMatrix)
print(paste("CONTROL ","|","ATR: ",LoAtrVal,":",UpAtrVal,"|","BBands: ",LoBBandVal,":",UpBBandVal,"|", "Chaikin: ",LoChaikinVal,":",UpChaikinVal,"|",
            "%change(5): ",Lo5DayVal,":",Up5DayVal,"|","%Change(10): ",Lo10DayVal,":",Up10DayVal,"|","Schwager: ",LoSchwagerVal,":",UpSchwagerVal,"|",
            "VR2: ",LoVR2Val,":",UpVR2Val,"|"))









#print(rep(1:length(plotdataHIT)))
#if(length(plotdataHIT) %% 10 != 0){
#  toAdd <- length(plotdataHIT) %% 10
#  for (modCounter in 1:(10-toAdd)) {
#    plotdataHIT <- rbind(plotdataHIT, 0)
#  }
#}


# par(mfrow = c(2, 1))
# 
# plotdataHIT <- na.omit(plotdataHIT[-1])  #REMOVE THE FIRST VALUE
# plotdataMIS <- na.omit(plotdataMIS[-1])
# maxHIT <- max(plotdataHIT)
# maxMIS <- max(plotdataMIS)
# maxVAL <- max(c(maxHIT, maxMIS))
# 
# minHIT <- min(plotdataHIT)
# minMIS <- min(plotdataMIS)
# minVAL <- min(c(minHIT, minMIS))
# 
# dev.copy(pdf,file=paste('/Users/alexe/OneDrive/Desktop/backtester_v5.5 (1)/backtester_v5.5/plots/Market Making/Volatility Plots/VR Schwager/Histogram',i,'.pdf'))
# hist(plotdataHIT, xlab ="Volatility Value", xlim = c(floor(minVAL),ceiling(maxVAL)), ylim = c(0,300), breaks = seq(floor(minVAL),ceiling(maxVAL), 0.2), main = plotTitle)
# hist(plotdataMIS, xlab ="Volatility Value", xlim = c(floor(minVAL),ceiling(maxVAL)), ylim = c(0,300), breaks = seq(floor(minVAL),ceiling(maxVAL), 0.2), main = plotTitleMIS)
# dev.off()

#print(length(plotdata))
#library(ggplot2)
#HIT <- ggplot(plotdataHIT, aes(x=volatility, y=Organisation)) + geom_point()


# plot(plotdataHIT, 
#      c(rep(1.025,length(plotdataHIT)/10),rep(1.02,length(plotdataHIT)/10),
#        rep(1.015,length(plotdataHIT)/10),rep(1.01,length(plotdataHIT)/10),
#        rep(1.005,length(plotdataHIT)/10),rep(1,length(plotdataHIT)/10),
#        rep(0.995,length(plotdataHIT)/10),rep(0.99,length(plotdataHIT)/10),
#        rep(0.985,length(plotdataHIT)/10),rep(0.98,length(plotdataHIT)/10)),
#      ylim=c(0.8,1.2), ylab = "To Spread The Data",
#      xlab = "Volatility Value", main = plotTitle)
# 
# #print(rep(1:length(plotdataMIS)))
# #if(length(plotdataMIS) %% 10 != 0){
# #  toAdd <- length(plotdataMIS) %% 10
# #  for (modCounter in 1:(10-toAdd)) {
# #    plotdataMIS <- rbind(plotdataMIS, 0)
# #  }
# #}
# 
# #print(length(plotdata))
# MISS <- plot(plotdataMIS, 
#      c(rep(1.025,length(plotdataMIS)/10),rep(1.02,length(plotdataMIS)/10),
#        rep(1.015,length(plotdataMIS)/10),rep(1.01,length(plotdataMIS)/10),
#        rep(1.005,length(plotdataMIS)/10),rep(1,length(plotdataMIS)/10),
#        rep(0.995,length(plotdataMIS)/10),rep(0.99,length(plotdataMIS)/10),
#        rep(0.985,length(plotdataMIS)/10),rep(0.98,length(plotdataMIS)/10)),
#      ylim=c(0.8,1.2), ylab = "To Spread The Data",
#      xlab = "Volatility Value", main = plotTitleMIS,)




#plotdataMIS
    
#print(plotdata)


#c(rep(1.025,length(plotdata)/10),rep(1.02,length(plotdata)/10),
#  rep(1.015,length(plotdata)/10),rep(1.01,length(plotdata)/10),
#  rep(1.005,length(plotdata)/10),rep(1,length(plotdata)/10),rep(0.995,length(plotdata)/10),
#  rep(0.99,length(plotdata)/10),rep(0.985,length(plotdata)/10),
#  rep(0.98,length(plotdata)/10))

#(rep(1.01,length(plotdata)/5),rep(1.005,length(plotdata)/5),
#  rep(1,length(plotdata)/5),rep(0.995,length(plotdata)/5),
#  rep(0.99,length(plotdata)/5)),

#Spread as a line plot

# spreadList <- lapply(dataList[1], function(x) x$High - x$Low)
# spreadInXts <- do.call(cbind, spreadList)
# 
# maxSpread <- max(spreadInXts)
# minSpread <- min(spreadInXts)
# 
# print(c(maxSpread, minSpread))
# print(c("Avg spread is:",mean(spreadInXts)))
# count <- 0
# 
# p <- ggplot(fortify.zoo(spreadInXts,melt=TRUE),aes(x=Index, y = Value, group = Series))
# p <- p + geom_line()
# p <- p + labs(title= "The Spread")
# p <- p + xlab("Date")
# p <- p + facet_wrap (~ Series, scales = "free")
# p <- p + facet_free()
# plot(p)

#print(dataList[7])

#currseries <- 8

#justHiList <- lapply(dataList[1:10], function(x) x$High - x$Low)
# justHiList <- lapply(dataList[1:10], function(x) x$High)
# hiInSingleXts <- do.call(cbind, justHiList)
# colnames(hiInSingleXts) <- 1:10


# maxHigh <- max(dataList[3]$High)
# minHigh <- min(dataList[3]$High)

# p <- ggplot(fortify.zoo(opInSingleXts,melt=TRUE),aes(x=Index, y = Value, group = Series))
# p <- p + geom_line()
# p <- p + labs(title= "The Highs")
# p <- p + xlab("Date")
# p <- p + facet_wrap (~ Series, scales = "free")
# p <- p + facet_free()
# #plot(p)


#print((diff(hiInSingleXts)/hiInSingleXts)*100)

#justClList <- lapply(dataList[1:10], function(x) x$Low)
#justClList <- lapply(dataList[1:10], function(x) (x$Open + x$Close / 2))
justClList <- lapply(dataList[1:10], function(x) (x$Close))
clInSingleXts <- do.call(cbind, justClList)
colnames(clInSingleXts) <- 1:10

#print(clInSingleXts)

# 
# # 
# # # 
# # # # q <- ggplot(fortify.zoo((diff(clInSingleXts)/clInSingleXts)*100,melt=TRUE),aes(x= Value, fill = Series))
# # # # #q <- ggplot(fortify.zoo(diff(clInSingleXts),melt=TRUE),aes(x= Value, fill = Series))
# # # # #q <- ggplot(fortify.zoo(diff(hiInSingleXts),melt=TRUE),aes(x= Value, fill = Series))
# # # # q <- q + geom_bar(stat='bin', binwidth = 1)
# # # # #q <- q + labs(title= "The Low Differences")
# # # # #q <- q + xlab("Date")
# # # # q <- q + facet_wrap (~ Series, ncol = 5)
# # # # q <- q + coord_flip(xlim=c(-2.5,2.5))
# # # # q <- q + labs(title = "Low % Change - (binwidth = 0.222%)", fontface="bold")
# # # # #q <- q + facet_free()
# # # # #plot(q)
# # # 

# justLoList <- lapply(dataList[1:10], function(x) x$Low)
# loInSingleXts <- do.call(cbind, justLoList)
# colnames(loInSingleXts) <- 1:10
# 
# p <- ggplot(fortify.zoo((((diff(loInSingleXts)/loInSingleXts))*100),melt=TRUE),aes(x= Value, fill = Series))
# p <- p + geom_bar(stat='bin', binwidth = 5)
# #q <- q + labs(title= "The Low Differences")
# #q <- q + xlab("Date")
# p <- p + facet_wrap (~ Series, ncol = 1)
# p <- p + coord_flip(xlim=c(-30,30))
# p <- p + labs(title = "Low Differences", fontface="bold")
# #q <- q + facet_free()
# #plot(p)
# 
# grid.arrange(q,p)



#MARKET MAKING GRAPHS
plotting <- 1:10
# #plotting <- 6:10
# 
# justDiffList <- lapply(dataList[plotting], function(x) x$High - x$Low)
# diffInSingleXts <- do.call(cbind, justDiffList)
# colnames(diffInSingleXts) <- plotting
# 
# d <- ggplot(fortify.zoo(diff(diffInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
# d <- d + geom_bar(stat='bin', binwidth = 5)
# d <- d + labs(title= "Spread Differences")
# d <- d + xlab("Date")
# d <- d + facet_wrap (~ Series, ncol = 2)
# d <- d + coord_flip(xlim=c(-15,15))
# d <- d + facet_free()
# plot(d)
# 
# justHiList <- lapply(dataList[plotting], function(x) x$High)
# hiInSingleXts <- do.call(cbind, justHiList)
# colnames(hiInSingleXts) <- plotting
# 
# h <- ggplot(fortify.zoo(diff(hiInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
# h <- h + geom_bar(stat='bin', binwidth = 5)
# h <- h + labs(title= "Highs Differences")
# h <- h + xlab("Date")
# h <- h + facet_wrap (~ Series, ncol = 2)
# h <- h + coord_flip(xlim=c(-30,30))
# h <- h + facet_free()
# plot(h)
# 
# justLoList <- lapply(dataList[plotting], function(x) x$Low)
# loInSingleXts <- do.call(cbind, justLoList)
# colnames(loInSingleXts) <- plotting
# 
# l <- ggplot(fortify.zoo(diff(loInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
# l <- l + geom_bar(stat='bin', binwidth = 5)
# l <- l + labs(title= "Lows Differences")
# l <- l + xlab("Date")
# l <- l + facet_wrap (~ Series, ncol = 2)
# l <- l + coord_flip(xlim=c(-30,30))
# l <- l + facet_free()
# plot(l)


#Close Prices and Close Differences Grpahs

# library(ggplot2)
# 
# p <- ggplot(fortify.zoo(clInSingleXts,melt=TRUE),aes(x=Index, y = Value, group = Series))
# p <- p + geom_line()
# p <- p + labs(title= "Close Prices")
# p <- p + xlab("Date")
# p <- p + facet_wrap (~ Series, scales = "free")
# p <- p + facet_free()
# plot(p)
# 
# justClList <- lapply(dataList[1:5], function(x) x$Close)
# clInSingleXts <- do.call(cbind, justClList)
# colnames(clInSingleXts) <- 1:5
# 
# q <- ggplot(fortify.zoo(diff(clInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
# q <- q + geom_bar(stat='bin', binwidth = 5)
# q <- q + labs(title= "Close Differences")
# q <- q + xlab("Date")
# q <- q + facet_wrap (~ Series, ncol = 2)
# q <- q + coord_flip(xlim=c(-30,30))
# q <- q + facet_free()
# plot(q)


#Plot the next 21 days after a breakout to check its performance

# HLCdf <- data.frame(High = series$High,
#                     Low = series$Low,
#                     Close = series$Close)
# justClList <- lapply(dataList[1:10], function(x) x$Close)
# clInSingleXts <- do.call(cbind, justClList)
# colnames(clInSingleXts) <- 1:10

#  entryPoints <- c("1970-05-13"
#                   ,"1971-04-02"
#                   ,"1971-04-14"
#                   ,"1971-04-15"
#                   ,"1971-04-16"
#                   ,"1971-04-17"
#                   ,"1971-04-20"
#                   ,"1972-06-24")
# m <- matrix(ncol= 0, nrow = 22)
# 
# for (i in 1:length(entryPoints)) {
#   dateLimit <- paste(entryPoints[i],"/", as.Date(entryPoints[i])+ 10) #subset for entry date and next 3 weeks
#   dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
#   d1 <- dataList[[2]]
#   #BoughtAt <- as.double(d1$Close[entryPoints[i]])
#   #returnVa2 <- (d1$Close[dateLimit]/BoughtAt) * 100   #%return at this point
#   #m <- cbind(m,matrix(returnVa2))
#   one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(sd=1)),subset = dateLimit, name = "MeanRev + 10days" )
#   one.AxisX.Maximum = 5;
#   #addTA(EMA(d1$Close,n=10),on=NA)
# }
# for (i in 1:22){
#   m[i,]<-(mean(m[i,]))
# }
# print(max(m))

#Plot the next 21 days after a breakout to check its performance
#(in development)

# HLCdf <- data.frame(High = series$High,
#                     Low = series$Low,
#                     Close = series$Close)
# justClList <- lapply(dataList[1:10], function(x) x$Close)
# clInSingleXts <- do.call(cbind, justClList)
# colnames(clInSingleXts) <- 1:10
# 
# entryPoints <- c("1970-06-25", "1971-08-26","1971-08-27","1971-12-10","1972-02-21","1972-02-22","1972-06-03","1972-07-26")
# m <- matrix(ncol= 0, nrow = 15)
# 
# for (i in 1:length(entryPoints)) {
#   dateLimit <- paste(as.Date(entryPoints[i]) - 7,"/", as.Date(entryPoints[i])+ 7)  #Set Date Limits, Pre-Week/Post-3Week
#   dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
#   d1 <- dataList[[1]]
#   #BoughtAt <- as.double(d1$Close[entryPoints[i]])
#   #returnVa2 <- (d1$Close[dateLimit]/BoughtAt) * 100   #%return at this point
#   #m <- cbind(m,matrix(returnVa2))
#   one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2),addRSI(n=2)),subset = dateLimit, name = paste("Mean Rev +/-7Days - DATE:",entryPoints[i]) )
#   one.AxisX.Maximum = 5;
#   addTA(SMA(d1$Close,n=50),on=NA)
#   abline(v = 8,col = 'blue')   #X-Axis line, date of enter
# }
#for (i in 1:15){
#  m[i,]<-(mean(m[i,]))
#}
#print(max(m))


#Returm on Investment - Locate the best holding periods per series
  #Plotting within the strategy code

# entryPoints <- c("1970-04-15"
#                  ,"1970-05-27"
#                  ,"1970-05-30"
#                  ,"1970-05-31"
#                  ,"1970-06-07"
#                  ,"1970-06-08"
#                  ,"1970-06-09"
#                  ,"1970-06-10"
#                  ,"1970-06-11"
#                  ,"1970-12-26"
#                  ,"1971-06-26"
#                  ,"1971-08-08")
# m <- matrix(ncol= 0, nrow = 22)
# 
# for (i in 1:length(entryPoints)) {
#   dateLimit <- paste(entryPoints[i],"/", as.Date(entryPoints[i])+ 21) #subset for entry date and next 3 weeks
#   dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
#   
#   d1 <- dataList[[1]]
#   
#   BoughtAt <- as.double(d1$Close[entryPoints[i]])
#   returnVa2 <- (d1$Close[dateLimit]/BoughtAt) * 100   #%return at this point
#   
#   m <- cbind(m,matrix(returnVa2))
# 
# }
# print(m)    #All returns for the 21 days following the entry
# 
# 
# for (i in 1:22){
#   m[i,]<-(mean(m[i,]))    #get avg
# }
# print(max(m[2,]))

#Geom_Tile Example
# install.pachakges('repshape2')
# library(reshape2)
# p <- ggplot(melt(volcano),aes(x=Var1, y=Var2, fill=value))
# p <- p + geom_tile()
# plot(p)


#Open Differences and POSTION SIZING
# dataList <- getData("PART1")
# print(is.list(dataList))
# opendiffs <- lapply(dataList,function(x) diff(x$Open))
# toplot <- do.call(cbind,opendiffs)
# colnames(toplot) <- paste("Series", sprintf("%02d",1:10))
# 
# plot.zoo(toplot,
#          main="Open on open simple differences",
#          cex.axis=1.2,
#          cex.main=2)
# 
# absOpenDiffs <- lapply(opendiffs, abs)
# avgAbsDiffs <- sapply(absOpenDiffs, mean, na.rm=TRUE)
# opensOnFirstDay <- sapply(dataList, function(x) first(x)$Open)
# tab <- cbind(opensOnFirstDay, 
#              avgAbsDiffs,
#              abs(avgAbsDiffs)/opensOnFirstDay)
# colnames(tab) <- c("Open", "Mean abs diff", "Mean abs diff/Open")
# print(tab)
# 
# largestAvgAbsDiffs <- max(avgAbsDiffs)
# positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
# params <- list(sizes = positionSizes)
# 
# print("--- Position Sizes ---")
# print(params)























# dateLimit <- paste(entryPoints[1],"/", as.Date(entryPoints[1])+ 21)
# dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
# 
# d1 <- dataList[[1]]
# one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2)),subset = dateLimit, name = dateLimit )
# one.AxisX.Maximum = 5;
# addTA(SMA(d1$Open,n=50),on=NA)
# 
# dateLimit <- paste(entryPoints[2],"/", as.Date(entryPoints[2])+ 21)
# dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
# 
# two <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2)),subset = dateLimit )
# one.AxisX.Maximum = 5;
# addTA(SMA(d1$Open,n=50),on=2)
# #addTA(addBBands())
# #addTA(BBands(HLCdf,n=50,sd=2),on=1)
# 
# ind1 <- ifelse((d1$Close-d1$Open) > 5,1,0)
# ind2 <- ifelse((d1$Close-d1$Open) < -5,1,0)
# addTA(ind1,col='blue',type='h',on=NA,yrange=c(-1,1))
# addTA(ind2,col='red',type='h',on=2)
# 
# #plot(a)
# #grid.arrange(p,q)


#dev.copy(pdf,file='/Users/alexe/OneDrive/Desktop/backtester_v5.5 (1)/backtester_v5.5/plots/Seriesx-BBands.pdf')
#dev.off()


#print(entryPoints)







#copy and paste the whole thing into the console and it runs fine, work with it untill i can find a fix.
# for (i in 1:length(series)) {
#   cl <- series$Close
#   bbands <- last(BBands(cl,n=50,sd=2))
#   print(bbands)
#   #if (cl < bbands[,"dn"]) {
#   #  print("here")
#   #}
# }