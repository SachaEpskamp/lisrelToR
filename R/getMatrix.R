getMatrix <- function(x,name,diag=FALSE,symmetrical=FALSE,estimates)
{
   
  # Make similar to readLines if length==1:
  if (length(x)==1) x <- strsplit(x,split="\n")[[1]]
  
  # Check if estimate matrix:
  if (missing(estimates))
  {
    estimates <- any(grepl("\\(",x))
  }
  
  if (estimates)
  {
    # Loop over lines, check if one contains "- -" and the next a bracket, and fill in "- -" signs
    for (i in which(grepl("- -",x[-length(x)]) & grepl("\\(",x[-1])))
    {
      emLocs <- gregexpr("- -",x[i])[[1]]
      substring(x[i+1],emLocs,emLocs+2) <- "- -"
      substring(x[i+2],emLocs,emLocs+2) <- "- -"
    }
  }
  
  # Remove leading and trailing spaces:
  x <- gsub("^\\s+","",x)
  x <- gsub("\\s+$","",x)
  # Remove empty lines:
  x <- x[x!=""]
  # Normalize spacing:
  # x <- gsub("\\s+"," ",x)
  # Make "- -" NA:
  x <- gsub("- -","NA",x)
  
  # Identify start of submatrices:
  sub <- grep(name,x)
  
  # Extract submatrices:
  X <- list()
  for (i in seq_along(sub))
  {
    if (i<length(sub))
    {
      X[[i]] <- x[(sub[i]+1):(sub[i+1]-1)]
    } else {
      X[[i]] <- x[(sub[i]+1):length(x)]
    }
  }
  
  # Remove second line:
  X <- lapply(X,'[',-2)
  
  ## If estimate matrix, remove standard errors and t-values:
  if (estimates)
  {
    seLocs <- lapply(X,function(x)grep("\\(",x))
    Xse <- X
    Xt <- X
    for (i in 1:length(X))
    {
      Xse[[i]][-seLocs[[i]]] <- gsub("^.+? ","",Xse[[i]][-seLocs[[i]]])
      Xse[[i]][-seLocs[[i]]] <- gsub("[^ ]+","NA",Xse[[i]][-seLocs[[i]]])
      Xse[[i]] <- Xse[[i]][-c(seLocs[[i]]-1,seLocs[[i]]+1)]
      Xse[[i]] <- gsub("\\(|\\)","",Xse[[i]])
      Xse[[i]] <- Xse[[i]][-1]
  
      Xt[[i]][-(seLocs[[i]]+1)] <- gsub("^.+? ","",Xt[[i]][-(seLocs[[i]]+1)])
      Xt[[i]][-(seLocs[[i]]+1)] <- gsub("[^ ]+","NA",Xt[[i]][-(seLocs[[i]]+1)])
      Xt[[i]] <- Xt[[i]][-c(seLocs[[i]]-1,seLocs[[i]])]
      Xt[[i]] <- Xt[[i]][-1]
      
      X[[i]] <- X[[i]][-c(seLocs[[1]],seLocs[[1]]+1)]
    }
  }
  
  # Extract and remove rownames:
  rowNames <- regmatches(X[[1]][-1],regexpr("^.+?(?= )",X[[1]][-1],perl=TRUE))
  if (any(grepl("[a-z]",rowNames,ignore.case=TRUE))) X <- lapply(X,function(txt)c(txt[1],gsub("^.+? ","",txt[-1])))
  
  # Combine:
  X <- lapply(X,paste,collapse="\n")
  
  # Read table:
  Tabs <- lapply(X,function(txt)as.matrix(read.table(text=txt,header=TRUE,fill=TRUE)))
  
  # Check if same number of rows, or append (triangle matrices):
  if (length(unique(sapply(Tabs,nrow)))>1)
  {
    if (missing(symmetrical)) symmetrical <- TRUE
    totR <- nrow(Tabs[[1]])
    for (i in 2:length(Tabs))
    {
      Tabs[[2]] <- rbind(matrix(NA,totR-nrow(Tabs[[2]]),ncol(Tabs[[2]])),Tabs[[2]])
      if (estimates)
      {
        Xse[[i]] <- c(rep(NA,totR-length(Xse[[i]])),Xse[[i]])
        Xt[[i]] <- c(rep(NA,totR-length(Xt[[i]])),Xt[[i]])
      }
    }
  }
  
  # Combine:
  Tab <- as.matrix(do.call(cbind,Tabs))
  rownames(Tab) <- rowNames
  
  if (!any(dim(Tab)==1)) diag <- FALSE
  # Diagonalize:
  if (diag)
  {
    nm <- colnames(Tab)
    Tab <- diag(c(Tab))
    rownames(Tab) <- colnames(Tab) <- nm
  }
  
  # Symmetrize:
  if (symmetrical)
  {
    Tab[upper.tri(Tab)] <- t(Tab)[upper.tri(Tab)]
  }
  
  Tab[is.na(Tab)] <- 0
  
  # Compute SE and t-value matrices if needed:
  if (estimates)
  {
    seTabs <- lapply(Xse,function(txt)as.matrix(read.table(text=txt,header=FALSE,fill=TRUE)))
    seTab <- do.call(cbind,seTabs)
  
    tTabs <- lapply(Xt,function(txt)as.matrix(read.table(text=txt,header=FALSE,fill=TRUE)))
    tTab <- do.call(cbind,tTabs)
        
    # Diagonalize:
    if (diag)
    {      
      seTab <- diag(c(seTab))
      tTab <- diag(c(tTab))
    }
    colnames(seTab) <- colnames(tTab) <- colnames(Tab)
    rownames(seTab) <- rownames(tTab) <- rownames(Tab)
    
    # Symmetrize:
    if (symmetrical)
    {
      seTab[upper.tri(seTab)] <- t(seTab)[upper.tri(seTab)]
      tTab[upper.tri(tTab)] <- t(tTab)[upper.tri(tTab)]
    }
    
    # Return list:
    return(list(est=Tab,se=seTab,t=tTab))
  }
  
  # Return matrix:
  return(Tab)
}
