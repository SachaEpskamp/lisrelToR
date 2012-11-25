findMatrix <- function(mat,type,Mats,Struc,Out)
{
  StrucUL <- unlist(Struc)
  
  # Define LISREL name:
  if (mat=="LX") lisName <- "LAMBDA-X"
  if (mat=="PH") lisName <- "PHI"
  if (mat=="TD") lisName <- "THETA-DELTA"
  if (mat=="GA") lisName <- "GAMMA"
  if (mat=="LY") lisName <- "LAMBDA-Y"
  if (mat=="PS") lisName <- "PSI"
  if (mat=="TE") lisName <- "THETA-EPSILON"
  if (mat=="BE") lisName <- "BETA"

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
