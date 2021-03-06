EstimateRadiusByDistance=function(DistanceMatrix){
  # 
  # INPUT
  # DistanceMatrix    symmetric distance Matrix of n cases
  # 
  # OUTPUT
  # Numerical scalar defining the radius
  # 

  if (!requireNamespace('ABCanalysis',quietly = TRUE)) {
    message(
      'Subordinate package (ABCanalysis) is missing. No computations are performed.
            Please install the package (ABCanalysis) which is defined in "Suggests".'
    )
    return(
      list(
        Radius = 0,
        Message = "Subordinate package (ABCanalysis) is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  x=DistanceMatrix[lower.tri(DistanceMatrix, diag = FALSE)]
  xx=ABCanalysis::ABCRemoveSmallYields(x,0.5)
  x=xx$SubstantialData
  res=ABCanalysis::ABCanalysis(x)
  Radius=min(x[res$Aind])/max(x[res$Cind])
  return(Radius)
}