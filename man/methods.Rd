\name{lisrel-methods}
\alias{print.lisrel}
%\alias{plot.lisrel}
%\alias{summary.lisrel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods for lisrel objects
}
\description{
Print method prints LISREL output file to the console, summary method returns RAM of parameter estimates as given by \code{semPlotModel} from the \code{semPlot} package and plot method calls \code{semPaths} from the \code{semPlot} package.
}
\usage{
\method{print}{lisrel}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
output of \code{\link{readLisrel}}
}
  \item{\dots}{Not used}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
