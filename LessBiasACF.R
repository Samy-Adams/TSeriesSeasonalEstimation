LessBiasACF <- function(X) {
  
  N = length(X)
  Mean = mean(X)
  MyACF = 1:N #1:(floor(N/2)+2)


  for (k in (0:(N-1))) #Was (0:(floor(N/2)+1)) but for seasonality, makes more sense to only do up to 1 more than N/2 (so that N/2 can still be registered as a possible seasonal-period) 
  {
    Sum = 0
    for (t in (k+1):N)
    {
      Sum = Sum + (X[t] - Mean)*(X[t-k] - Mean)
    }
  
    MyACF[k+1] = Sum/(N-0.5*k)  #Custom bias. Works well.
  }

  return (MyACF / var(X))
}