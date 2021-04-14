example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "tester",
                        "MyStrategy",
                        "plotter",
                        "strat"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=3),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "tester"=list(lookback=40,lookbackShort=20,nfast=12,nSlow=26,nSig = 9,sdParam=1.5,rsi=14,adx=14,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "MyStrategy"=list(lookback=40,lookbackShort=20,nfast=12,nSlow=26,nSig = 9,sdParam=1.5,rsi=14,adx=14,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "plotter"=list(lookback=50,sdParam=2,series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(1,10)),
                    #"strat"=list(lookback=50,lookbackShort=34,sdParam=2,sdParamShort=1,adx=14,adxSignal=25,emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(19,22,13,21,10,0,8,21,9,0),shortHoldPeriod=c(22,18,16,16,0,18,2,13,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
                    #"strat"=list(lookback=50,lookbackShort=34,sdParam=2,sdParamShort=1,adx=14,adxSignal=25,emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(0,0,0,0,0,0,0,0,0,0),shortHoldPeriod=c(22,18,16,16,0,18,2,13,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=rep(100,10),spreadPercentage=0.001,inventoryLimits=rep(10,10))
                    #Long periods based on fitness - holdPeriod=c(19,18,13,21,0,21,16,21,9,0) - 5 is poor and 10 is unknown
                    #"strat"=list(lookback=50,mrLookback=44,mrADX=25,MOsdParam=c(1.5,1.5,2,1.5,99,1.5,1.5,1.5,1.5,1.5),MOShortSdParams=c(1.5,99,2,2,99.5,1.5,99,99,99,1.5),sdParamShort=1,adx=14,MOadxSignal=c(25,25,20,25,0,35,25,35,35,25),MOShortadxSignal=c(25,0,25,30,0,30,0,0,0,30),emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(19,18,18,21,0,21,16,21,8,5),shortHoldPeriod=c(21,0,16,14,0,18,0,0,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=c(24,20,142,6,13,96,89,1,42,306),MRposSizes=c(24,20,142,6,13,96,89,1,42,306),spreadPercentage=0.001,inventoryLimits=rep(5,10))
                    "strat"=list(lookback=50,shortEma.F=c(4,4,3,11,11,11,11,11,11,3),shortEma.S=c(35,30,35,11,11,11,11,11,11,35),longEma.F=c(11,11,4,2,11,3,3,11,3,11),longEma.S=c(11,11,30,40,11,35,35,11,35,11),LongRiskStopPercentage=c(0,0.99,0.98,0.98,0,0.98,0.98,0.99,0.98,0.98),ShortRiskStopPercentage=c(1.02,1.02,1.01,0,0,0,0,0,0,1.01),maLong=c(1,25,1,30,1,30,45,30,1,1),mrLookback=44,mrADX=25,mrShortCrosses=c(1,2,3,4,5,6,7,8,9,10),mrCrosses=c(1,2,3,4,5,6,7,8,9,10),mrSD=rep(1,10),MOsdParam=c(1.5,1.5,2,1.5,99,1.5,99,99,1.5,99),MOShortSdParams=c(1.5,99,2,99,99,99,99,99,99,1.5),sdParamShort=1,adx=14,MOadxSignal=c(25,25,20,25,0,35,0,0,35,0),MOShortadxSignal=c(25,0,25,0,0,0,0,0,0,30),emvLong=0.3,emvShort=-0.3,nfast=12,nSlow=26,nSig = 9,rsi=14,holdPeriod=c(19,18,18,21,0,21,16,21,8,5),shortHoldPeriod=c(21,0,16,14,0,18,0,0,0,21),series=c(1,2,3,4,5,6,7,8,9,10),posSizes=c(24,20,142,6,13,96,89,1,42,306),MRposSizes=c(24,20,142,6,13,96,89,1,42,306),spreadPercentage=0.002,inventoryLimits=rep(0,10))
                    )

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
