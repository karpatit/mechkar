\name{Table2}
\alias{Table2}
\title{
Print a formatted table with the Odds/Hazard ratios from a given model
}
\description{
This function creates a table with the Odds/Hazard ratios (depending on the model type) with their confidence intervals and p-values.
}
\usage{
Table2(mod, rv=NULL, level = 0.95, decimals = 3)
}
\arguments{
\item{mod}{the model name}
\item{rv}{(Optional) the text to be used for each of the variables to be used. If ommited, the original variable names will be used. }
\item{level}{the confidence level to be used. Default=0.95}
\item{decimals}{the number of decimals to be used. Default=3}
}
\details{
The Table2 function generates a summary table containing the exponent of the model coefficients and the respective confidence intervals and p-values.
}
\value{
This function returns a data frame object with the table of the variables with the coeficients of the model and their p-value
}
\author{
Tomas Karpati M.D.
}
\examples{
df <- MASS::biopsy[,2:11]
mod <- glm(class ~ ., data=df ,family = "binomial")
vn <- c("Intercept","clump thickness","uniformity cell size","uniformity cell shape",
        "marginal adhesion","epithelial cell size","bare nuclei",
        "bland chromatin","normal nucleoli","mitoses")
rn <- data.frame(vars=vn,cats=rep("",10))

tab2 <- Table2(mod=mod,rv = vn)
}

