CorTestNonZero <- function(rho0,x,y,tailed=c("right","left","two")) 
{
  r12 <- cor(x,y,method = "pearson")
  n <- length(x)
  z0 <- (0.5 * log((1+r12)/(1-r12)) - 0.5 * log((1+rho0)/(1-rho0))) / (1 / (n-3)^0.5)
  if(tailed == "left")
  {
    Setup <- c("Left Tailed Test")
    Tstat <- z0
    DF <- n-2
    pValue <- pnorm(z0)
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }
  if(tailed == "right")
  {
    Setup <- c("Right Tailed Test")
    Tstat <- z0
    DF <- n-2
    pValue <- 1-pnorm(z0)
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }  
  if(tailed ==  "two")
  {
    Setup <- c("Two Tailed Test")
    Tstat <- z0
    DF <- n-2
    if(z0 > 0)
    {
      pValue <- 2*(1-pnorm(z0))
    }
    if(z0 < 0)
    {
      pValue <- 2*pnorm(z0)
    }
    CorrelationCoefficient <- r12
    results <- data.frame(Setup,Tstat,DF,pValue,CorrelationCoefficient)
  }
  return(results)
}
