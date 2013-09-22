
CaiLoadRData <- function() {
  # 加载默认的RData
  
  #load("/home/windows/Dropbox/code/r/R/data/workspace.RData")
}

CaiInstallPackages <- function(pkgs) {
  ins.pkgs <- .packages(all.available=T)
  pkgs <- pkgs[!(pkgs %in% ins.pkgs)]
  lapply(pkgs, FUN=install.packages)
}

CaiTapply <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) 
{
  FUN <- if (!is.null(FUN)) 
    match.fun(FUN)
  if (!is.list(INDEX)) 
    INDEX <- list(INDEX)
  nI <- length(INDEX)
  if (!nI) 
    stop("'INDEX' is of length zero")
  namelist <- vector("list", nI)
  names(namelist) <- names(INDEX)
  extent <- integer(nI)
  nx <- length(X)
  one <- 1L
  group <- rep.int(one, nx)
  ngroup <- one
  for (i in seq_along(INDEX)) {
    index <- as.factor(INDEX[[i]])
    if (length(index) != nx) 
      stop("arguments must have same length")
    namelist[[i]] <- levels(index)
    extent[i] <- nlevels(index)
    group <- group + ngroup * (as.integer(index) - one)
    ngroup <- ngroup * nlevels(index)
  }
  if (is.null(FUN)) 
    return(group)
  ans <- lapply(X = split(X, group), FUN = FUN, ...)
  index <- as.integer(names(ans))
  if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
    ansmat <- array(dim = extent, dimnames = namelist)
    ans <- unlist(ans, recursive = FALSE)
  }
  else {
    ansmat <- array(vector("list", prod(extent)), dim = extent, 
                    dimnames = namelist)
  }
  if (length(index)) {
    names(ans) <- NULL
    ansmat[index] <- ans
  }
  ansmat
}
