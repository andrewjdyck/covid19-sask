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
projSimple<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  # x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  x <- c(rawTime, rawTime[nn]+1:inWindow)
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
