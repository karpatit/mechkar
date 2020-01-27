\name{ValidityTest}
\alias{ValidityTest}
\docType{package}
\title{
Print a formatted table with the validity test for the results of a screening or a predictive model
}
\description{
This function creates a table showing the most common statistical validity metrics used for testing the validity of a screening test or a predictive model. The way that the output is formated is as follows:

################# Validity Test #######################
\tabular{rrrrr}{
\cr
\tab            \tab     \tab   Observed\cr
\tab            \tab     +  \tab  - \cr
\tab         \tab -------------\tab---------------\tab-------------\cr
\tab Predicted  \tab +   TP \tab  FP \tab   PPV \cr
\tab		        \tab     a  \tab  b  \tab   e (e1-e2) \cr
\tab         \tab -------------\tab---------------\tab-------------\cr
\tab	          \tab  -  FN \tab  TN \tab   NPV \cr
\tab	          \tab   	 c  \tab   d  \tab   f (f1-f2) \cr
\tab         \tab -------------\tab---------------\tab-------------\cr
\tab	          \cr
\tab	          \tab  Sensitivity \tab  Specificity \tab   Prevalence \cr
\tab            \tab  g (g1-g2) \tab  h (h1-h2) \tab   i (i1-i2) \cr
\tab         \tab -------------\tab---------------\tab-------------\cr
\tab       \tab Error: \tab (FP+FN)\tab/(TP+FP+FN+TN) \cr
\tab       \tab Accuracy: \tab (TP+TN)\tab/(TP+FP+FN+TN) \cr
\tab       \tab Precision: \tab TP\tab/(TP+FP) \cr
\tab       \tab Recall:\tab TP\tab/(TP+FN) \cr
\tab       \tab f1-Score*: \tab 2*(Precision*Recall)\tab/(Precision+Recall) \cr
}
\emph{* F1-Score: Harmonic mean of precision and recall.}
######################################################
}
\usage{
ValidityTest(a,b,c,d,multi=100,caption = "Validity of the Model/Screening")
}
\arguments{
\item{a}{the true positive (TP) value}
\item{b}{the false positive (FP) value}
\item{c}{the false negative (FN) value}
\item{d}{the true negative (TN) value}
\item{multi}{(Optional) The multiplier for the values. The default is 100 for calculating the percentage.}
\item{caption}{the text to be printed as the title.}
}
\details{
The ValidityTest function returns a summary table with the validity metrics most commonly used in epidemiology and in statistical analysis.
}
\value{
A character matrix containing the following statistical metrics:
\item{TP}{the true positive value}
\item{FP}{the false positive value}
\item{PPV}{the positive predictive value}
\item{FN}{the false negative value}
\item{TN}{the true negative value}
\item{NPV}{the negative predictive value}
\item{FN}{the false negative value}
\item{TN}{the true negative value}
\item{NPV}{the negative predictive value}
\item{Sensitivity}{the sensitivity of the test}
\item{Specificity}{the true negative value}
\item{Prevalence}{the prevalence of the positive cases in the group}
\item{Error}{the of incorrectly assigned cases}
\item{Accuracy}{the true negative value}
\item{Precision}{is the same as the PPV}
\item{Recall}{Othe name for the sensitivity of the test}
\item{F1-Score}{the armonic mean of the precision and the recall}
All results are given with their confidence intervals.
}
\author{
Tomas Karpati M.D.
}
\examples{
tab1 <- ValidityTest(110,20,80,324)
}

