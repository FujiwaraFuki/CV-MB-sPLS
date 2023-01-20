\name{cv.block.spls}
\alias{cv.block.spls}
\title{
Cross-Validation for Parameter Tuning of Multiblock sPLS
}
\description{
Computes M-fold, Leave-One-Out Cross-Validation scores based on a user-input grid to determine the optimal parsity parameters values for method block.spls in mixOmics package.
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
\item{X}{A list of exexplanatory variable datasets (blocks) measured on the same samples. Data in the list should be arranged in matrices, samples x variables, with samples order matching in all data sets.}
\item{Y}{A matrix of response variable. Data should be continuous variables. Y should have the same rownames as each dataste of X}
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
\seealso{
\code{\link[tuneBlocksPLS]{make.test.keepX}}
}
\examples{

# Read sample dataset
# -------------
# TCGA breast cancer dataset containing mRNA, miRNA, and proteins block
data("breast.TCGA")

# input mRNA and miRNA datasets as X data
X = list(mrna = breast.TCGA$data.train$mrna,
          mirna = breast.TCGA$data.train$mirna)
# responsive variable must be input as matrix having the same rownames as each entry of X list
Y = as.matrix(breast.TCGA$data.train$protein[,3])


# Tuning parameters
# -------------
# set number of component per data set
ncomp <- 2

# design matrix is the same in mixOmics
design = matrix(0.1, ncol = length(X), nrow = length(X), dimnames = list(names(X), names(X)))
diag(design) = 0 # set diagonal to 0s

# vector v is the possible values for the number of selected variables for each component in each block
v <- c(5, 10)
# make.test.keepX makes a list of keepX used in cv.block.spls
# mode is first recommended to be set at 1 (Default)
# when the length of v is n, the length of the set of tested keepX is n^length(X)
test.keepX <- make.test.keepX(X, v, ncomp = ncomp, mode = 1)

# cv.block.spls tuning keepX parameters using (repeated) M-fold cross-validation
# chose "MSE"or "R" for model selection method (Default = "MSE")
# Note the computation time especially when repeating cross validation
res <- cv.block.spls(X, Y, test.keepX = test.keepX, design = design, ncomp = ncomp, nrepeat = 1, fold = 5, eval.mode = "MSE")

# check (average) MSE at the best parameter keepX
# print(res$MSE)
#
list.keepX <- res$list.keepX


# Fit multiblock sPLS method (mixOmics)
# -------------
# fit block.spls method in mixOmics package using estimated optimal keepX parameter
model <- block.spls(X, Y,
                    ncomp = ncomp,
                    keepX = list.keepX,
                    design = design)

# for details of the output, see https://www.rdocumentation.org/packages/mixOmics/versions/6.3.2/topics/block.spls

}
