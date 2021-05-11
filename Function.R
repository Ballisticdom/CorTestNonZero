CorTestNonZero <- function(rho0,x,y,tailed=c("right","left","two")) 
{
  r12 <- cor(x,y,method = "pearson")
  n <- length(x)
  t0 <- (0.5 * log((1+r12)/(1-r12)) - 0.5 * log((1+rho0)/(1-rho0))) / (1 / (n-3)^0.5)
  if(tailed == "left")
  {
    Setup <- c("Left Tailed Test")
    Tstat <- t0
    DF <- n-2
    pValue <- pt(t0)
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }
  if(tailed == "right")
  {
    Setup <- c("Right Tailed Test")
    Tstat <- t0
    DF <- n-2
    pValue <- 1-pt(t0)
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }  
  if(tailed ==  "two")
  {
    Setup <- c("Two Tailed Test")
    Tstat <- t0
    DF <- n-2
    if(t0 > 0)
    {
      pValue <- 2*(1-pt(t0))
    }
    if(t0 < 0)
    {
      pValue <- 2*pt(t0)
    }
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }
  return(results)
}
