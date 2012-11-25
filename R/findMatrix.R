findMatrix <-
function(mat,type)
{
  # Define LISREL name:
  if (mat=="LambdaX") lisName <- "LAMBDA-X"
  if (mat=="Phi") lisName <- "PHI"
  if (mat=="ThetaDelta") lisName <- "THETA-DELTA"
  if (mat=="Gamma") lisName <- "GAMMA"
  if (mat=="LambdaY") lisName <- "LAMBDA-Y"
  if (mat=="Psi") lisName <- "PSI"
  if (mat=="ThetaEpsilon") lisName <- "THETA-EPSILON"
  if (mat=="Beta") lisName <- "BETA"

  Res <- NULL
  
  if (length(Struc[[type]]) > 0 & length(Mats[[mat]]) > 0)
  {
    IndStart <- min(Mats[[mat]][Mats[[mat]]>Struc[[type]][1]])
    if (!any(StrucUL[StrucUL > Struc[[type]][1]] < IndStart))
    {
      # Find end:
      IndEnd <- IndStart
      repeat
      {
        IndEnd <- IndEnd + 1
        if (Out[IndEnd]=="" & !grepl(lisName,Out[IndEnd+1]) & !grepl(lisName,Out[IndEnd-1])) break
      }
      Res <- getMatrix(Out[IndStart:IndEnd],lisName,grepl("Theta",mat)|grepl("Psi",mat)|grepl("Phi",mat),grepl("Theta",mat)|grepl("Psi",mat)|grepl("Phi",mat))
    }
  }
  return(Res)
}
