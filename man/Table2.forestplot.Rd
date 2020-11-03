\name{Table2.forestplot}
\alias{Table2.forestplot}
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
\item{nr}{(Optional) A data frame containing two columns: the first column with the text to be used for each of the variables, and the second column with the names of the categories for nominal variables.
If ommited, the original variable names as appear in the model will be used. }
}
\details{
The Table2.foresplot function plots a summary table containing the model's risks and its respective confidence intervals and p-values. A forestplot graph is inserted in the middle of the table.
This function generates a plot that is paper ready. We recommend to save it as PDF file, which let modifying its size without blurring the text.
}
\value{
This function returns a data frame object with the table of the variables with the coeficients of the model and their p-value. Additionally, a graph which includes the table and a forestplot that shows graphically the coeficients is generated.
}
\author{
Tomas Karpati M.D.
}
\examples{
utils::data(biopsy, package = "MASS")
df <- biopsy[,2:11]
mod <- glm(class ~ ., data=df ,family = "binomial")
vn <- c("Intercept","clump thickness","uniformity cell size","uniformity cell shape",
        "marginal adhesion","epithelial cell size","bare nuclei",
        "bland chromatin","normal nucleoli","mitoses")
rn <- data.frame(vars=vn,cats=rep("",10))

tab2 <- Table2(mod=mod,rv = vn)
Table2.forestplot(mod=mod,nr=rn)
}

