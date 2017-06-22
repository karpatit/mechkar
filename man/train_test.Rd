\name{train_test}
\alias{train_test}
\title{Generates a training and test dataset}
\usage{
train_test(data=NULL,train=NULL,test=NULL,prop=0.6,seed=123)
}
\description{
This function generates a training and test datasets by randomly assigning  individuals to each dataset.
}
\arguments{
\item{data}{original dataset}
\item{train}{a string that defines the name to be assifned to the train dataset object}
\item{test}{a string that defines the name to be assifned to the test dataset object}
\item{prop}{the proportion of the training dataset. The value is a fractional number between 0 and 1. The value default value is set to 0.6, indicating that the training dataset will contain 60\% of the cases and the test dataset will contain the 40\% of the cases.}
\item{seed}{the desired seed. Using a constant seed value allows to obtain the same individuals on each group when running many times (important feature needed for replicability)}
}
\author{
Tomas Karpati M.D.
}
\examples{
### the following example will generate a train dataset named "ckd.train" which
### includes 70\% of the records in the "ckd" dataset, while generating a test
### dataset called "ckd.test" and that includes 30\% of the the original dataset.

train_test(data=ckd,train="ckd.train",test="ckd.test",prop=0.7,seed=74746)
}
\keyword{ package }
