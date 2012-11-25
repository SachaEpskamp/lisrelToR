\name{readLisrel}
\alias{readLisrel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readLisrel(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    Out <- readLines(x)
    Res <- list(fitIndices = data.frame, matrices = list(), variables = data.frame(), 
        covariances = list(implied = NULL, observed = NULL))
    Struc <- list(parSpec = grep("Parameter Specifications", 
        Out), est = grep("LISREL Estimates", Out), fit = grep("Goodness of Fit Statistics", 
        Out), modInd = grep("Modification Indices and Expected Change", 
        Out), std = grep("Standardized Solution", Out), stdComp = grep("Completely Standardized Solution", 
        Out))
    StrucUL <- unlist(Struc)
    Struc$std <- Struc$std[!Struc$std \%in\% Struc$stdComp]
    Mats <- list(ObsCov = grep("Covariance Matrix", Out), ImpCov = grep("Fitted Covariance Matrix", 
        Out), LambdaX = grep("LAMBDA-X", Out), Phi = grep("PHI", 
        Out), ThetaDelta = grep("THETA-DELTA", Out), Gamma = grep("GAMMA", 
        Out), LambdaY = grep("LAMBDA-Y", Out), Psi = grep("PSI", 
        Out), ThetaEpsilon = grep("THETA-EPSILON", Out), Beta = grep("BETA", 
        Out))
    Mats$ObsCov <- Mats$ObsCov[!Mats$ObsCov \%in\% Mats$ImpCov]
    for (mat in c("LambdaX", "Phi", "ThetaDelta", "Gamma", "LambdaY", 
        "Psi", "ThetaEpsilon", "Beta")) {
        Res$matrices[[mat]] <- list()
        for (type in c("est", "std", "stdComp", "parSpec")) {
            Res$matrices[[mat]][[type]] <- findMatrix(mat, type)
        }
    }
    Res$covariances$implied <- findCov("ImpCov")
    Res$covariances$observed <- findCov("ObsCov")
    IndStart <- Struc$fit
    IndEnd <- IndStart
    repeat {
        IndEnd <- IndEnd + 1
        if (!(grepl("\\s*", Out[IndEnd]) | grepl("=", Out[IndEnd]))) 
            break
    }
    modTxt <- Out[IndStart:IndEnd]
    modTxt <- modTxt[grepl("=", modTxt)]
    modTxt <- strsplit(modTxt, split = "=")
    modTxt <- lapply(modTxt, gsub, pattern = "^\\s*", replacement = "")
    modTxt <- lapply(modTxt, gsub, pattern = "\\s*$", replacement = "")
    Res$fitIndices <- data.frame(Statstic = sapply(modTxt, "[", 
        1), Value = sapply(modTxt, "[", 2))
    return(Res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
