\name{readLisrel}
\alias{readLisrel}

\title{
Read LISREL matrices into R
}
\description{
This function scans LISREL (Joreskog & Sorbom, 1996) output for model matrices and fit indices.
}
\usage{
readLisrel(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
String indicating the location of a LISREL output file.
}
}
\details{
\code{LisrelToR} uses the following names for the model matrices:
\describe{
  \item{LY}{
Lambda-Y matrix.
}
  \item{PS}{
Psi matrix.
}
  \item{BE}{
Beta matrix.
}
  \item{TE}{
Theta-Epsilon matrix.
}
  \item{TY}{
Tau-Y matrix.
}
  \item{AL}{
Alpha matrix.
}
  \item{LX}{
Lambda-X matrix.
}
  \item{PH}{
Phi matrix.
}
  \item{GA}{
Gamma matrix.
}
  \item{TD}{
Theta-Delta matrix.
}
  \item{TX}{
Tau-X matrix.
}
  \item{KA}{
Kappa
}
  \item{ObsCovs}{
The observed covariance matrix, or a list of such matrices for each group.
}
  \item{ImpCovs}{
The implied covariance matrix, or a list of such matrices for each group.
}
}
Furthermore, \code{lisrelToR} uses the following names for matrix types:
\describe{
 \item{est}{Parameter estimates}
 \item{se}{Standard errors}
 \item{t}{t-values}
 \item{parSpec}{Parameter numbers}
}
}
\value{
A list of class \code{"lisrel"} contaning:
\item{fitIndices}{Fit indices, currently not supported.}
\item{matrices}{A list contaning the model matrices. For each group this list conains a list with for each matrix (using LISREL style naming, see details) a list contaning elements \code{est} for parameter estimates, \code{se} for standard errors, \code{t} for t-values and \code{parSpec} for parameter numbers. Use \code{\link{lisrelMatrix}} to extract the matrices.}
\item{variables}{Currently not used.}
\item{Covariances}{A list with elements \code{implied} and \code{observed} containing the implied and observed covariance matrices.}
}
\references{
Joreskog, K. G., & Sorbom, D. (1996). LISREL 8 user's reference guide. Scientific Software.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


\examples{
## Measurment invariance example:
download.file("http://sachaepskamp.com/files/mi1.OUT",modFile <- tempfile(fileext=".OUT"))
Lis <- readLisrel(modFile)

# Extract Lambda-Y for group 2:
lisrelMatrix(Lis,"LY", group = 2)

# Structure of object:
str(Lis)
}

\seealso{
\code{\link{lisrelMatrix}}
}