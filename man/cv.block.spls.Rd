\name{cv.block.spls}
\alias{cv.block.spls}
\title{
Cross-Validation for Parameter Tuning of Multiblock sPLS
}
\description{
Computes M-fold or Leave-One-Out Cross-Validation scores based on a user-input grid to determine the optimal parsity parameters values for method block.spls in mixOmics package.
}
\usage{
cv.block.spls(
  X,
  Y,
  test.keepX,
  design,
  ncomp = 2,
  nrepeat = 1,
  fold = 5,
  eval.mode = "MSE")
}
\arguments{
\item{X}{A list of data sets (blocks) measured on the same samples. Data in the list should be arranged in matrices, samples x variables, with samples order matching in all data sets.}
\item{Y}{Matrix response for a multivariate regression framework. Data should be continuous variables.}
\item{test.keepX}{A list of candidate combinations of the number of variables selected on each component for each block. This list can be made by mathod make.test.keepX.}
\item{design}{numeric matrix of size (number of blocks in X) x (number of blocks in X) with 0 or 1 values. A value of 1 (0) indicates a relationship (no relationship) between the blocks to be modelled.}
\item{ncomp}{the number of components to include in the model. Default = 2}
\item{nrepeat}{the number of times the Cross-Validation process is repeated. Default = 1.}
\item{fold}{the folds in the Mfold cross-validation. Default = 5.}
\item{eval.mode}{Mode of determining the method of evaluation of model performance, matching one of "MSE" or "R". Default = "MSE".}
}
\value{
\item{list.keepX}{returns the number of variables selected (optimal keepX) on each component, for each block.}
\item{MSE}{Minimum Mean Squared Error between predicted y and test y.}
\item{R}{Maximum correlation coefficient between predicted y and test y.}
}
\examples{
library(mixOmics)
data("breast.TCGA")
# this is the X data as a list of mRNA, miRNA and proteins
X = list(mrna = breast.TCGA$data.train$mrna,
          mirna = breast.TCGA$data.train$mirna)
Y = breast.TCGA$data.train$protein[,1]


ncomp <- 2
design = matrix(0.1, ncol = length(X), nrow = length(X), dimnames = list(names(X), names(X)))
diag(design) = 0

v <- c(5,10)
test.keepX <- makeTestKeepX(X, v, ncomp = ncomp, mode = 1)

res <- cv.block.spls(X, Y, test.keepX = test.keepX, design = design, ncomp = ncomp, nrepeat = 10, fold = 5, eval.mode = "MSE")

list.keepX <- res$list.keepX

model <- block.spls(X, Y,
                    ncomp = nomp,
                    keepX = list.keepX,
                    design = design)

}
