ACF <- function(X) {
  
  N = length(X)
  Mean = mean(X)
  MyACF = 1:N


  for (k in (0:(N-1)))
  {
    Sum = 0
    for (t in (k+1):N)
    {
      Sum = Sum + (X[t] - Mean)*(X[t-k] - Mean)
    }
  
    MyACF[k+1] = Sum/(N-0.5*k)  #Custom bias
  }

  return (MyACF / var(X))
}
