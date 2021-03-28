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

#The Size of the spread (% of getting larger/smaller or equal in size)

print("Spread Changes in Size (%)")
spreadMatrix <- matrix(ncol= 3, nrow = 0)
colnames(spreadMatrix) <- c("up","eq","dn")

for (i in 1:10){
  series <- (dataList[[i]])
  s <- series$High - series$Low
  len <- length(s)
  upcount <- 0 ; eqcount <- 0 ; dncount <- 0
  for (c in 2:len) {
    if (as.integer(s$High[c-1]) < as.integer(s$High[c])) {upcount <- upcount+1}
    if (as.integer(s$High[c-1]) > as.integer(s$High[c])) {dncount <- dncount+1}
    if (as.integer(s$High[c-1]) == as.integer(s$High[c])) {eqcount <- eqcount+1}
  }
  spreadMatrix <- rbind(spreadMatrix, c((dncount/len)*100,(eqcount/len)*100,(upcount/len)*100))
}
print(spreadMatrix)

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


#MARKET MAKING GRAPHS
#plotting <- 1:5
plotting <- 6:10

justDiffList <- lapply(dataList[plotting], function(x) x$High - x$Low)
diffInSingleXts <- do.call(cbind, justDiffList)
colnames(diffInSingleXts) <- plotting

d <- ggplot(fortify.zoo(diff(diffInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
d <- d + geom_bar(stat='bin', binwidth = 5)
d <- d + labs(title= "Spread Differences")
d <- d + xlab("Date")
d <- d + facet_wrap (~ Series, ncol = 2)
d <- d + coord_flip(xlim=c(-30,30))
d <- d + facet_free()
plot(d)

justHiList <- lapply(dataList[plotting], function(x) x$High)
hiInSingleXts <- do.call(cbind, justHiList)
colnames(hiInSingleXts) <- plotting

h <- ggplot(fortify.zoo(diff(hiInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
h <- h + geom_bar(stat='bin', binwidth = 5)
h <- h + labs(title= "Highs Differences")
h <- h + xlab("Date")
h <- h + facet_wrap (~ Series, ncol = 2)
h <- h + coord_flip(xlim=c(-30,30))
h <- h + facet_free()
plot(h)

justLoList <- lapply(dataList[plotting], function(x) x$Low)
loInSingleXts <- do.call(cbind, justLoList)
colnames(loInSingleXts) <- plotting

l <- ggplot(fortify.zoo(diff(loInSingleXts),melt=TRUE),aes(x=Value, fill = Series))
l <- l + geom_bar(stat='bin', binwidth = 5)
l <- l + labs(title= "Lows Differences")
l <- l + xlab("Date")
l <- l + facet_wrap (~ Series, ncol = 2)
l <- l + coord_flip(xlim=c(-30,30))
l <- l + facet_free()
plot(l)


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
# 
# entryPoints <- c("1970-08-04","1970-08-05","1970-11-10","1970-11-29","1970-11-30","1970-12-03","1970-12-04","1971-01-08","1971-01-09","1971-01-14","1971-01-15","1971-01-18","1971-01-26","1971-01-27","1971-01-28","1971-09-18","1971-09-19","1971-09-21","1971-09-22","1971-09-23","1971-10-06")
# m <- matrix(ncol= 0, nrow = 22)
# 
# for (i in 1:length(entryPoints)) {
#   dateLimit <- paste(entryPoints[i],"/", as.Date(entryPoints[i])+ 21) #subset for entry date and next 3 weeks
#   dateLimit <- gsub(" ", "", dateLimit, fixed = TRUE)
#   d1 <- dataList[[1]]
#   BoughtAt <- as.double(d1$Close[entryPoints[i]])
#   returnVa2 <- (d1$Close[dateLimit]/BoughtAt) * 100   #%return at this point
#   m <- cbind(m,matrix(returnVa2))
#   one <- chartSeries(d1$Close,theme='white',TA=c(addBBands(n=50,sd=2)),subset = dateLimit, name = "At Breakout + 21days" )
#   one.AxisX.Maximum = 5;
#   addTA(SMA(d1$Close,n=50),on=NA)
# }
# for (i in 1:22){
#   m[i,]<-(mean(m[i,]))
# }
# print(max(m))


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
dataList <- getData("PART1")
print(is.list(dataList))
opendiffs <- lapply(dataList,function(x) diff(x$Open))
toplot <- do.call(cbind,opendiffs)
colnames(toplot) <- paste("Series", sprintf("%02d",1:10))

plot.zoo(toplot,
         main="Open on open simple differences",
         cex.axis=1.2,
         cex.main=2)

absOpenDiffs <- lapply(opendiffs, abs)
avgAbsDiffs <- sapply(absOpenDiffs, mean, na.rm=TRUE)
opensOnFirstDay <- sapply(dataList, function(x) first(x)$Open)
tab <- cbind(opensOnFirstDay, 
             avgAbsDiffs,
             abs(avgAbsDiffs)/opensOnFirstDay)
colnames(tab) <- c("Open", "Mean abs diff", "Mean abs diff/Open")
print(tab)

largestAvgAbsDiffs <- max(avgAbsDiffs)
positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
params <- list(sizes = positionSizes)

print("--- Position Sizes ---")
print(params)























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