\name{train_test}
\alias{train_test}
\title{Generates a training and test dataset}
\usage{
train_test(data=NULL,train_name=NULL,test_name=NULL,prop=NULL,seed=123,tableone = FALSE)
}
\description{
This function generates a training and test datasets by randomly assigning  individuals to each dataset.
}
\arguments{
\item{data}{original dataset}
\item{train_name}{a string that defines the name to be assifned to the train dataset object}
\item{test_name}{a string that defines the name to be assifned to the test dataset object}
\item{prop}{the proportion of the training dataset. The value is a fractional number between 0 and 1. The value default value is set to 0.6, indicating that the training dataset will contain 60\% of the cases and the test dataset will contain the 40\% of the cases.}
\item{seed}{the desired seed. Using a constant seed value allows to obtain the same individuals on each group when running many times (important feature needed for replicability)}
\item{tableone}{a logical value indicating if the Table1 function has to be generated for comparing the train and test division. Default is FALSE }
}
\value{This function creates new variables using the names entered for the train and test partitions. Additionally, it returns the a table (based on the Table1 function) comparing all the available variables by partition. This helps understanding if the partition is balanced.}
\author{
Tomas Karpati M.D.
}
\examples{
### the following example will generate a train dataset named "train" which
### includes 70\% of the records, while generating a test
### dataset called "test" and that includes 30\% of the the original dataset.
df <- Theoph
df$Subject <- NULL
train_test(data=df,train_name="train",test_name="test",prop=0.7,seed=2)
}

