findCov <- function(mat)
{
  # Define LISREL name:
  if (mat=="ObsCov") lisName <- "Covariance Matrix"
  if (mat=="ImpCov") lisName <- "Fitted Covariance Matrix"
  
  Res <- NULL
  
  if (length(Mats[[mat]]) > 0)
  {
    IndStart <- min(Mats[[mat]])
    # Find end:
    IndEnd <- IndStart
    repeat
    {
      IndEnd <- IndEnd + 1
      if (Out[IndEnd]=="" & !grepl(lisName,Out[IndEnd+1]) & !grepl(lisName,Out[IndEnd-1])) break
    }
    Res <- getMatrix(Out[IndStart:IndEnd],lisName,TRUE,TRUE)
  }
  return(Res)
}