# utilities.R

# Detection rate
# TTS = Time to Symptoms. The median time is 5 days
# confirmedCases and hospitalizations must be vectors
estDetRate <- function(confirmedCases, hospitalizations, tts=5, hospitalizationRate=.07) {
  casesLessTTS <- confirmedCases[length(confirmedCases)-tts]
  inferredCases <- floor(hospitalizations[length(hospitalizations)]/hospitalizationRate)
  detectionRate <- min(casesLessTTS/inferredCases, 1)
  return(detectionRate)
}

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(infd, deaths, cfr = 0.01, ttd=17, window=5){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/cfr #expected new cases given cfr
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  out<-mean(detRate, na.rm = TRUE)
  if (is.nan(out)) return(NA)
  if (out>1) out<-1
  out
}

growthRate <- function(cases) {
  return(c(NA, diff(cases)/cases[1:(length(cases)-1)]))
}

# Simple projection based on growth over last inWindow days
# returns coefficients
projSimpleSlope<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  coefficients(mFit)
}

# Simple projection based on growth over last inWindow days
# returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10, outWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  # x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  x <- c(rawTime, rawTime[nn]+1:outWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  list(x=x, y=y)
}


# calculates doubling time over the last inWindow days.
doubTime <- function(cases, time, inWindow = 10){
  r <- projSimpleSlope(cases, time)[2]
  log(2)/r
}

# calculates the curve flatenning index.
# it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  lnact <-log(active)
  cfiInd <- -diff(diff(lnact))/abs(diff(lnact)[-1])
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}


# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}


# Adjusts cumulative infections to get active cases
# cumulative infections and deaths, ttr = time to recovery
recLag <- function(infections, deaths, datCols = dateCols(infections), ttr = 22){
  matI<-as.matrix(infections[, datCols])
  matD<-as.matrix(deaths[, datCols])
  matA<-matI-matD #remove deaths
  matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
  matA <- matA - matR
  
  out <- data.frame(infections[,!datCols], matA) # active cases
  colnames(out) <- colnames(infections)
  out
}

# aggregates results to country
countryAgg<-function(x){
  xSelect<-x[, dateCols(x)]
  aggregate(xSelect, by = list(Country = x$Country.Region), FUN = sum)
}


# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}

# convert a provincial time series
convertTs <- function(ts) {
  temp <- ts[, 5:ncol(ts)]
  dts <- as.Date(names(temp), format = '%m/%d/%y')
  dta <- as.vector(t(temp))
  xts(dta, order.by=dts)
}

alignDf <- function(xtsDta) {
  tt <- as.data.frame(xtsDta)
  on <- tt$on[5:nrow(tt)]
  nn <- length(on)
  sk <- c(tt$sk[tt$sk>0], rep(NA, nn-length(tt$sk[tt$sk>0])))
  ab <- c(tt$ab[tt$ab>0], rep(NA, nn-length(tt$ab[tt$ab>0])))
  bc <- c(tt$bc[tt$bc>0], rep(NA, nn-length(tt$bc[tt$bc>0])))
  it <- c(tt$it[tt$it>0], rep(NA, nn-length(tt$it[tt$it>0])))
  outData <- data.frame(
    on=on,
    sk=sk,
    ab=ab,
    bc=bc,
    it=it
  )
  return(outData)
}
