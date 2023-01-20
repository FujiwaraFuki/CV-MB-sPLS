\name{make.test.keepX}
\alias{make.test.keepX}
\title{make list test.keepX for block.spls}
\description{
make a list of candidate keepX to be tested in cv.block.spls
}
\usage{
make.test.keepX(
  X,
  v,
  ncomp,
  mode = 1)
}
\arguments{
\item{X}{A list of exexplanatory variable datasets (blocks) measured on the same samples. Data in the list should be arranged in matrices, samples x variables, with samples order matching in all data sets.}
\item{v}{A vector of possible values for the number of selected variables for each component in each block}
\item{ncomp}{the number of components to include in the model. Default = 2}
\item{mode}{Default = 1. When mode is set as 2, the number of selected variables in each component in the same block can take different value. Note that mode 2 will require more computation time due for more combinations to be tested!}
}
\seealso{
\code{\link[tuneBlocksPLS]{cv.block.spls}}
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

