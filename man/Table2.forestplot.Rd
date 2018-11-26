\name{Table2.forestplot}
\alias{Table2.forestplot}
\docType{package}
\title{
Generate a paper ready plot with a risk table and a foresplot
}
\description{
This function plots a table with the Odds/Hazard ratios (depending on the model type) with their confidence intervals and p-values and insert in the middle a foresplot.
}
\usage{
Table2.forestplot(mod, nr=NULL)
}
\arguments{
\item{mod}{the model name}
\item{nr}{(Optional) A data frame containing two columns: the first column with the text to be used
for each of the variables, and the second column with the names of the categories for nominal variables.
If ommited, the original variable names as appear in the model will be used. }
}
\details{
The Table2.foresplot function plots a summary table containing the model's risks and its respective confidence intervals and p-values. A forestplot graph is inserted in the middle of the table.
This function generates a plot that is paper ready. We recommend to save it as PDF file, which let modifying its size without blurring the text.
}
\author{
Tomas Karpati M.D.
}
\examples{
require(catdata)
data(medcare)
medcare$sex <- factor(medcare$male, levels=c(1,0), labels=c("Male","Female"))
medcare$male <-NULL
medcare$married <- factor(medcare$married, levels=c(0,1), labels=c("Not Married","Married"))
medmod <- glm(hosp ~. , data=medcare)
summary(medmod)
Table2.forestplot(medmod)
nr <- data.frame(v1=c("Intercept","Physician Visits","Poor health","Excellent health",
                      "# chronic diseases","Age","Married","Years in school","Sex"),
                 v2=c("","","","","","","Married","","Female"))
Table2.forestplot(medmod, nr=nr)
}
\keyword{ package }
