readLisrel <- function(x)
{
  
  # Read output:
  Out <- readLines(x)
  
  # Empty output structure (S3):
  Res <- list(
    fitIndices = data.frame, # named character vector
    matrices = list( ), # List containing per matrix a list contaning 'est', 'par', 'se', 't' matrices
    variables = data.frame(), # data frame contaning information on each variable: manifest or latent and exo or endo.
    covariances = list(
      implied = NULL, # model Implied covariance matrix
      observed = NULL # observed covariance matrix
    )
  )
  
  ### Find linenumbers of output structure:
  Struc <- list(
    parSpec = grep("Parameter Specifications",Out),
    est = grep("LISREL Estimates",Out),
    fit = grep("Goodness of Fit Statistics",Out),
    modInd = grep("Modification Indices and Expected Change",Out),
    std = grep("Standardized Solution",Out),
    stdComp = grep("Completely Standardized Solution",Out)
  )
  
  Struc$std <- Struc$std[!Struc$std%in%Struc$stdComp]
  StrucUL <- unlist(Struc)
  
  ### Find linenumbers of matrices:
  Mats <- list(
    ObsCov = grep("Covariance Matrix",Out),
    ImpCov = grep("Fitted Covariance Matrix",Out),
    LambdaX = grep("LAMBDA-X",Out),
    Phi = grep("PHI",Out),
    ThetaDelta = grep("THETA-DELTA",Out),
    Gamma = grep("GAMMA",Out),
    LambdaY = grep("LAMBDA-Y",Out),
    Psi = grep("PSI",Out),
    ThetaEpsilon = grep("THETA-EPSILON",Out),
    Beta = grep("BETA",Out)
  )
  
  Mats$ObsCov <- Mats$ObsCov[!Mats$ObsCov%in%Mats$ImpCov]
  
  ### EXTRACT MATRICES ###
  for (mat in c("LambdaX","Phi","ThetaDelta","Gamma","LambdaY","Psi","ThetaEpsilon","Beta"))
  {
    Res$matrices[[mat]] <- list()
    for (type in c("est","std","stdComp","parSpec"))
    {
      Res$matrices[[mat]][[type]] <- findMatrix(mat,type,Mats,Struc,Out)
    }
  }
  
  Res$covariances$implied <- findCov("ImpCov",Mats,Out)
  Res$covariances$observed <- findCov("ObsCov",Mats,Out)
  
  
  ### Extract fit statistics:
  IndStart <- Struc$fit
  # Find end:
  IndEnd <- IndStart
  repeat
  {
    IndEnd <- IndEnd + 1
    if (!(grepl("\\s*",Out[IndEnd]) | grepl("=",Out[IndEnd]))) break
  }
  modTxt <- Out[IndStart:IndEnd]
  modTxt <- modTxt[grepl("=",modTxt)]
  modTxt <- strsplit(modTxt,split="=")
  modTxt <- lapply(modTxt,gsub,pattern="^\\s*",replacement="")
  modTxt <- lapply(modTxt,gsub,pattern="\\s*$",replacement="")
  
  Res$fitIndices <- data.frame(Statstic=sapply(modTxt,"[",1),Value=sapply(modTxt,"[",2))
  
  # Return:
  return(Res)
}
