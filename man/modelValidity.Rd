\name{modelValidity}
\alias{modelValidity}
\docType{package}
\title{
Return discriminative and calibration measures for predictive models
}
\description{
This function returns common statistical validity metrics used for testing the discriminative power and calibration of a predictive model.
}
\usage{
modelValidity(data,model,class, train = FALSE, calib.graph=FALSE)
}
\arguments{
\item{data}{a data frame contining the data to be tested on the model}
\item{model}{the model to be tested}
\item{class}{the name (as a string) of the outcome variable}
\item{train}{a logical value indicating if the data comes from the same dataset used to train the model. Defaults is FALSE}
\item{calib.graph}{a logical value indicating if the calibration graph has to be generated. Deafault is FALSE }
}
\details{
The modelValidity function returns a summary table with the validity metrics most commonly used in predictive modeling.
}
\value{
A character matrix containing the following statistical metrics:
\item{auc}{the area under the curve of the model}
\item{cimin}{the lower 95\% confidence interval of the auc}
\item{cimax}{the higher 95\% confidence interval of the auc}
\item{SRME}{the squre root mean error}
\item{precision}{the precision of the model. This is also known in epidemiology as the positive predictive value (PPV)}
\item{recall}{the recall of the model. Also known in epidemilogy as the sensitivity}
\item{fscore}{the armonic mean of precision and recall (F1-score).}
\item{NPV}{the negative predictive value}
\item{D}{the Tjur\'s discriminative measure}
\item{TN}{the true negative value}
\item{mmce}{the mean missclassification error}
\item{Hosmer_Lemeshow}{the Hosmer-Lemeshow index}
}
\author{
Tomas Karpati M.D.
}
\examples{
set.seed(123)
n <- 100
x <- rnorm(n)
xb <- x
pr <- exp(xb)/(1+exp(xb))
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family="binomial")
vt <- modelValidity(data.frame(x=x,y=y),mod,"y")

}
\keyword{ package }
