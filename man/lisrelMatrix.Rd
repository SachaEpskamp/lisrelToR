\name{lisrelMatrix}
\alias{lisrelMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract LISREL matrices from \code{lisrel} object.
}
\description{
This function can be used to extract matrices from the output of \code{\link{readLisrel}}.
}
\usage{
lisrelMatrix(object, matrix, group = 1, type = "est")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A \code{"lisrel"} object obtained by \code{\link{readLisrel}}.
}
  \item{matrix}{
Specification of the matrix to be extracted. See details.
}
  \item{group}{
An integer specifying which group the matrix should be extracted from.
}
  \item{type}{
Specification of the type of matrix to be extracted.
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
A matrix.
}
\references{
Joreskog, K. G., & Sorbom, D. (1996). LISREL 8 user's reference guide. Scientific Software.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
\seealso{
\code{\link{readLisrel}}
}
\examples{
## Measurment invariance example:
download.file("http://sachaepskamp.com/files/mi1.OUT",modFile <- tempfile(fileext=".OUT"))
Lis <- readLisrel(modFile)

# Extract Lambda-Y for group 2:
lisrelMatrix(Lis,"LY", group = 2)
}

