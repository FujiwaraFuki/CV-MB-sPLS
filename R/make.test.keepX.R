make.test.keepX <- function(X, v, ncomp, mode = 1) {

  if (mode == 1) {

    x <- gtools::permutations(n = length(v),
                              r = length(X),
                              v = v,
                              repeats.allowed = TRUE)

    keepX <- list()

    for (i in 1:nrow(x)) {

      l <- list()
      for (j in 1:length(X)) {
        l[[j]] <- rep(x[i,j], ncomp)
      }
      keepX[[i]]<- l
      names(keepX[[i]]) <- names(X)
    }

  } else if (mode == 2) {

    x <- gtools::permutations(n = length(v),
                              r = length(X)*ncomp,
                              v = v,
                              repeats.allowed=TRUE)

    keepX <- list()

    for (i in 1:nrow(x)) {

      l <- list()

      for (j in 1:length(X)) {
        l[[j]] <- x[i,(ncomp*(j-1)+1):(ncomp*j)]
      }

      keepX[[i]]<- l
      names(keepX[[i]]) <- names(X)
    }

  } else {
    print("mode must be set to 1 or 2")
  }

  return(keepX)

}

