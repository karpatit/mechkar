\name{MeanCI}
\alias{MeanCI}
\title{Calculates the mean and 95\% confidence interval of a numeric vector}
\usage{
MeanCI(x,round=3)
}
\description{
This function return a vector containing the mean and 95\% confidence interval of a numeric vector.
}
\arguments{
\item{x}{a numeric vector}
\item{round}{the number of decimals to be returned. Default is 3}
}
\value{This function returns a named numeric vector with the mean and the lower and upper confidence interval.}
\author{
Tomas Karpati M.D.
}
\examples{
### calculates the mean and 95\% CI of a given vector
x <- c(1,0,1,0,0,0,1,1,0)
mn <- MeanCI(x)
}

