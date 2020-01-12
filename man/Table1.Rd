\name{Table1}
\alias{Table1}
\docType{package}
\title{
Print or export a formatted Table 1
}
\description{
This function creates a table that complies with the table 1 required on most of the
research papers. The function create descriptive statistics for the entire population
and, if a categorical variable is defined for strata, then additional descriptive
statistics will be added to the table and a p-value will be calculated. Additionally,
if desired, the table may be exported to an excel file, were the table could be edited
to addapt to the formatting required by the journal it will be published.
}
\usage{
Table1(x=NULL, y=NULL, rn=NULL, data=NULL, miss=3, catmiss=TRUE, formatted=TRUE,
categorize=FALSE, factorVars=NULL, maxcat=10, delzero=TRUE, decimals = 1,
messages = TRUE,excel=0, excel_file=NULL, debug = FALSE)
}
\arguments{
\item{x}{a string vector containing the name of the variables for which descriptive
statistics will be calculated.}
\item{y}{the name of the categorical variable that defines the stratification that
will be used.}
\item{rn}{a string vector containing the text that will replace variables defined in
argument x. If y is not provided, the original variable name will be used.}
\item{data}{the name of the dataset to be used.}
\item{miss}{determines if missingness will be shown in the table for the variables.
The possible values are: 0=dont add missing statistics; 1=add missing statistics for
contiinuous (numerical) values only; 2=add missing statistics for categorical (factor)
values only;3=add missing statistics for both numerical and categorical values;}
\item{catmiss}{On categorical variables, adds a new category (Missing) to the available
categories. Default is FALSE. For activation change this to TRUE. If TRUE, the "miss"
parameter will not be used for category variables}
\item{formatted}{As default, the table output is formatted as text with values that
include parenthesis and percentages, e.g. 153 (26.5\%). If you are interested that the
table return each numeric value as aseparate cell, set this variable to FALSE.}
\item{categorize}{If there are categorical variables that are defined as numeric we can
force the function to take them as categorical (factor) by changing this to TRUE. Default
is FALSE.}
\item{factorVars}{If categorize is set to TRUE, a list of variables to be considered as
categoricals may be given. In this case, maxcat will not be used and only the variables
in the list will be converted to factors.}
\item{maxcat}{If we force categorize to be TRUE, maxcat will be used to define the
maximum number of unique values permited for a variable to be considered as categorical.
Default is 10.}
\item{delzero}{For dichotomic variables, the default behaviour is to delete the rows with
the first value (0,"No","Normal",etc.). If you want that all the values are presented
change it to FALSE. }
\item{decimals}{Determinate the number of decimal places of numerical data. Default is 1}
\item{messages}{This switch will show the iterations of the function through the
variables. In case you want to suppress those message set it to FALSE.}
\item{excel}{indicates if the table will be exported to excel. The default is not to
export (excel). For exporting to excel set excel=1.}
\item{excel_file}{A string variable defining the name of the excel file. If the directory
path is not included in the file name, the file will be saved on the current path
directory.}
\item{debug}{Normally, the Table1 function will show a progress bar when running.
Sometimes the command fails because some anomaly in a variable that was not detected by
the function. In such case, setting debug to TRUE will print each variable that is
processed. In case of error, the last printed variable must be checked. The default to
this parameter is FALSE}
}
\details{
The getTable1 function generates a descriptive statistical summary appropriate for
publishing in a scientific paper.
}
\author{
Tomas Karpati M.D.
}
\examples{
### get the table 1 with original variables name

rv <- names(trees)
tab1 <- Table1(x=rv, data=trees)

### get the table 1 with the specified given variables text
rv <- names(swiss)
rn <- rv
rn[6] <- "Infant Mortality"
tab1 <- Table1(x=rv, rn=rn, data=swiss)

### get the table 1 and stratify by one variable
tab1 <- Table1(data=data.frame(HairEyeColor), y="Sex")
}
\keyword{ package }
