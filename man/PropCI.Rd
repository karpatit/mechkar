\name{PropCI}
\alias{PropCI}
\title{Calculates the proportion and 95\% confidence interval of a binary or dichotomic vector}
\usage{
PropCI(x,round=3,multi=100,ref=2)
}
\description{
This function return a vector containing the Proportion and 95\% confidence interval of a vector. The returning vector will contain the category that was used to calculate the proportion. If the user prefer that the reported category will be that used as reference, then must set second to FALSE
}
\arguments{
\item{x}{a numeric vector}
\item{round}{the number of decimals to be returned. Default is 3}
\item{multi}{a number indicating the way the proportion will be reported. The default is 100 for percent. If the proportion required is per 10,000 set multi=10000}
\item{ref}{determine which category will be used as reference to be reported. The default is 2 so the proportion for the second category will be reported. If the proportion for the first category must be reported, then set ref to 1}
}
\value{This function returns a named numeric vector with the percentage of the highest value of the binary vector and its lower and upper conficence interval.}
\author{
Tomas Karpati M.D.
}
\examples{
### calculates the proportion and 95\% CI of a numeric dichotomic vector
x <- c(1,0,1,0,0,0,1,1,0)
pp <- PropCI(x)

### calculate the proportion and 95\% CI of a character dichotomic vector
n <- c("a","a","b","b","a","b","a")
pp <- PropCI(n)
### change the report to the other category
pp <- PropCI(n,ref=1)
}

