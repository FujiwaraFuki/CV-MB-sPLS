cv.block.spls <- function(X, Y, test.keepX, design, ncomp = 2, nrepeat = 1, fold = 5, eval.mode = "MSE") {

  res <- foreach(r=1:nrepeat,.packages=c("mixOmics","doParallel"),.combine = "rbind") %do% {

    set.seed(100)
    fold_i <- sample(rep(1:fold, length.out = length(Y)))

    cores <- detectCores()-1
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    cv_i <- foreach(i=1:fold,.packages=c("mixOmics"),.combine = "rbind") %dopar% {

      test_i <- which(fold_i == i)

      train_data <- lapply(X,function(x) x[-test_i,])
      test_data <- lapply(X,function(x) x[test_i,])

      train_y <- as.matrix(Y[-test_i,])
      test_y <- as.matrix(Y[test_i,])


      models <- lapply(test.keepX, function(x)
        block.spls(X = train_data, Y = train_y,
                   ncomp = ncomp,
                   keepX = x,
                   design = design))

      predict = lapply(models, function(x) predict(x, newdata = test_data)$WeightedPredict) #予測値

      correlation <- sapply(predict, function(x) cor(x[1:length(test_y)], test_y)) #相関係数

      mse <- sapply(predict, function(x) {
        ((as.numeric(x) - as.numeric(test_y)) ^ 2) %>%
          mean() %>%
          sqrt()
        })

      return(cbind(t(correlation), t(mse)))

    }
    stopCluster(cl)

    return(cv_i)
  } %>% colMeans()

  R <- res[1:(length(res)/2)] %>% max()
  MSE <- res[(length(res)/2 +1):length(res)] %>% min()

  if (eval.mode == "R") {
    list.keepX <- test.keepX[[which(R == max(R))]]
  } else if (eval.mode == "MSE") {
    list.keepX <- test.keepX[[which(MSE == max(MSE))]]
  }

  return(list(list.keepX = list.keepX, R = R, MSE = MSE))


}
