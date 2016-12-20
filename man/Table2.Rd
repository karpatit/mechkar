\name{Table2}
\alias{Table2}
\docType{package}
\title{
Print a formatted table with the Odds/Hazard ratios from a given model
}
\description{
This function creates a table with the Odds/Hazard ratios (depending on the model type) with their confidence intervals and p-values. 
}
\usage{
Table2(mod=NULL, nv=NULL)
}
\arguments{
\item{mod}{the model name}
\item{nv}{(Optional) the text to be used for each of the variables to be used. If ommited, the original variable names will be used. }
}
\details{
The Table2 function generates a summary table containing the exponent of the model coefficients and the respective confidence intervals and p-values.
}
\author{
Tomas Karpati M.D.
}
\examples{
nms <- c("age","sex","bmi","bmi_cat","ses","is_arab")
Table2(mod1, nv=nms)
}
\keyword{ package }
