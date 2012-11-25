\name{getMatrix}
\alias{getMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMatrix(x, name, diag = FALSE, symmetrical = FALSE, estimates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{diag}{
%%     ~~Describe \code{diag} here~~
}
  \item{symmetrical}{
%%     ~~Describe \code{symmetrical} here~~
}
  \item{estimates}{
%%     ~~Describe \code{estimates} here~~
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
function (x, name, diag = FALSE, symmetrical = FALSE, estimates) 
{
    if (length(x) == 1) 
        x <- strsplit(x, split = "\n")[[1]]
    if (missing(estimates)) {
        estimates <- any(grepl("\\(", x))
    }
    if (estimates) {
        for (i in which(grepl("- -", x[-length(x)]) & grepl("\\(", 
            x[-1]))) {
            emLocs <- gregexpr("- -", x[i])[[1]]
            substring(x[i + 1], emLocs, emLocs + 2) <- "- -"
            substring(x[i + 2], emLocs, emLocs + 2) <- "- -"
        }
    }
    x <- gsub("^\\s+", "", x)
    x <- gsub("\\s+$", "", x)
    x <- x[x != ""]
    x <- gsub("- -", "NA", x)
    sub <- grep(name, x)
    X <- list()
    for (i in seq_along(sub)) {
        if (i < length(sub)) {
            X[[i]] <- x[(sub[i] + 1):(sub[i + 1] - 1)]
        }
        else {
            X[[i]] <- x[(sub[i] + 1):length(x)]
        }
    }
    X <- lapply(X, "[", -2)
    if (estimates) {
        seLocs <- lapply(X, function(x) grep("\\(", x))
        Xse <- X
        Xt <- X
        for (i in 1:length(X)) {
            Xse[[i]][-seLocs[[i]]] <- gsub("^.+? ", "", Xse[[i]][-seLocs[[i]]])
            Xse[[i]][-seLocs[[i]]] <- gsub("[^ ]+", "NA", Xse[[i]][-seLocs[[i]]])
            Xse[[i]] <- Xse[[i]][-c(seLocs[[i]] - 1, seLocs[[i]] + 
                1)]
            Xse[[i]] <- gsub("\\(|\\)", "", Xse[[i]])
            Xse[[i]] <- Xse[[i]][-1]
            Xt[[i]][-(seLocs[[i]] + 1)] <- gsub("^.+? ", "", 
                Xt[[i]][-(seLocs[[i]] + 1)])
            Xt[[i]][-(seLocs[[i]] + 1)] <- gsub("[^ ]+", "NA", 
                Xt[[i]][-(seLocs[[i]] + 1)])
            Xt[[i]] <- Xt[[i]][-c(seLocs[[i]] - 1, seLocs[[i]])]
            Xt[[i]] <- Xt[[i]][-1]
            X[[i]] <- X[[i]][-c(seLocs[[1]], seLocs[[1]] + 1)]
        }
    }
    rowNames <- regmatches(X[[1]][-1], regexpr("^.+?(?= )", X[[1]][-1], 
        perl = TRUE))
    if (any(grepl("[a-z]", rowNames, ignore.case = TRUE))) 
        X <- lapply(X, function(txt) c(txt[1], gsub("^.+? ", 
            "", txt[-1])))
    X <- lapply(X, paste, collapse = "\n")
    Tabs <- lapply(X, function(txt) as.matrix(read.table(text = txt, 
        header = TRUE, fill = TRUE)))
    if (length(unique(sapply(Tabs, nrow))) > 1) {
        if (missing(symmetrical)) 
            symmetrical <- TRUE
        totR <- nrow(Tabs[[1]])
        for (i in 2:length(Tabs)) {
            Tabs[[2]] <- rbind(matrix(NA, totR - nrow(Tabs[[2]]), 
                ncol(Tabs[[2]])), Tabs[[2]])
            if (estimates) {
                Xse[[i]] <- c(rep(NA, totR - length(Xse[[i]])), 
                  Xse[[i]])
                Xt[[i]] <- c(rep(NA, totR - length(Xt[[i]])), 
                  Xt[[i]])
            }
        }
    }
    Tab <- as.matrix(do.call(cbind, Tabs))
    rownames(Tab) <- rowNames
    if (!any(dim(Tab) == 1)) 
        diag <- FALSE
    if (diag) {
        nm <- colnames(Tab)
        Tab <- diag(c(Tab))
        rownames(Tab) <- colnames(Tab) <- nm
    }
    if (symmetrical) {
        Tab[upper.tri(Tab)] <- t(Tab)[upper.tri(Tab)]
    }
    Tab[is.na(Tab)] <- 0
    if (estimates) {
        seTabs <- lapply(Xse, function(txt) as.matrix(read.table(text = txt, 
            header = FALSE, fill = TRUE)))
        seTab <- do.call(cbind, seTabs)
        tTabs <- lapply(Xt, function(txt) as.matrix(read.table(text = txt, 
            header = FALSE, fill = TRUE)))
        tTab <- do.call(cbind, tTabs)
        if (diag) {
            seTab <- diag(c(seTab))
            tTab <- diag(c(tTab))
        }
        colnames(seTab) <- colnames(tTab) <- colnames(Tab)
        rownames(seTab) <- rownames(tTab) <- rownames(Tab)
        if (symmetrical) {
            seTab[upper.tri(seTab)] <- t(seTab)[upper.tri(seTab)]
            tTab[upper.tri(tTab)] <- t(tTab)[upper.tri(tTab)]
        }
        return(list(est = Tab, se = seTab, t = tTab))
    }
    return(Tab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
