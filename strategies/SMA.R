maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors


  cl <- newRowList[[params$series[1]]]$Close
  closeprices <- cl[,1]
  sma <- SMA(closeprices, n=10)
  print(sma)
}