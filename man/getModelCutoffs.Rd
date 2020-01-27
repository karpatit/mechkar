\name{getModelCutoffs}
\alias{getModelCutoffs}
\docType{package}
\title{
Function for selection of the prediction cutoffs by each quantile of the prediction
}
\description{
This function is intended to calculate the most important statistics that may help in making desicions on the optimal cutoff of the predictions for our needs.
}
\usage{
getModelCutoffs(pred, obs, div=10)
}
\arguments{
\item{pred}{a vector containing the predicted values of a model}
\item{obs}{a vector containing the values of the outcome variable}
\item{div}{the number of quantiles to calculate. Default=10}
}
\details{
This function return a list containing nine values (see values section).
It includes three types of standards: the European (euro), the American (us) and the World Health Organization (who).
The confidence interval (CI) is calculated using the binomial probabilities using the binconf function of the Hmisc package. The default is to calculate the 95\% CI (using an alpha of 0.05).
}
\value{
This function returns a table containing the following statistics:
\itemize{
\item TP - the number of true negatives
\item FP - the number of false positives
\item FN - the number of false negatives
\item TN - the number of true positives
\item sensitivity
\item specificity
\item PPV - positive predictive value
\item NVP - negative predictive value
\item accuracy
\item error
\item prevalence
\item lift
\item precision (same as PPV)
\item recall (same as sensitivity)
\item F1_score (harmonic mean of precision and recall)
}
}
\author{
Tomas Karpati M.D.
}
\examples{
mod <- glm(am ~ mpg + cyl + hp + wt, data=mtcars, family="binomial")
pred <- as.numeric(predict(mod, newdata = mtcars, type="response"))
tab2 <- getModelCutoffs(pred,mtcars$am)
}

