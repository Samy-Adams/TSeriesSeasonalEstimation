

for (i in 1:1000)
{
  seasPeriod <- sample.int(floor(60/2), 1)
  seasonality <- 2*runif(seasPeriod) - 1 #between -1 and 1
  X <- rep(seasonality, ceiling(60/seasPeriod))[1:60] + rnorm(60, 0, 0.1)
  
  #ML method for finding periodicity
  ac <- LessBiasACF(X) #LessBiasACF(X) #c(acf(X, plot = FALSE, length(X)/2)$acf)
  pksArgs <- which(diff(sign(diff(ac, na.pad = FALSE)), na.pad = FALSE) < 0) #Find all crests
  pksArgs <- pksArgs[which(ac[pksArgs+1] > 0)] #Remove negative crests (want positive correlation with lags)
  #Want to pick smallest periodicity that works (for more smoothening)
  
  firstPk <- pksArgs[1]
  maxPk <- pksArgs[which.max(ac[pksArgs + 1])]
  
  if ( (firstPk != maxPk) && (0.17 + median(ac[pksArgs[which(pksArgs%%maxPk != 0)]+1]) > median(ac[pksArgs[which(pksArgs%%maxPk == 0)]+1])) )
  {
    seasPerEst <- firstPk
  } else
    seasPerEst <- maxPk
  
  if (seasPeriod < seasPerEst && seasPeriod != 1 && Box.test(X, lag=30, type = c("Ljung-Box"))[["p.value"]] < 0.01)
  {
    print(seasPeriod)
    print(i)
    print(Box.test(X, lag=30, type = c("Ljung-Box"))[["p.value"]])
    ts.plot(X)
    lines(X[(seasPerEst+1):60], col = 2)
    ts.plot(X)
    lines(X[(seasPeriod+1):60], col = 2)
    ts.plot(LessBiasACF(X))
    break
  }

}