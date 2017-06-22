\name{Table1}
\alias{Table1}
\docType{package}
\title{
Print or export a formatted Table 1
}
\description{
This function creates a table that complies with the table 1 required on most of the research papers. The function create descriptive statistics for the entire population and, if a categorical variable is defined for strata, then additional descriptive statistics will be added to the table and a p-value will be calculated. Additionally, if desired, the table may be exported to an excel file, were the table could be edited to addapt to the formatting required by the journal it will be published.
}
\usage{
Table1(x=NULL, y=NULL, rn=NULL, data=NULL, miss=1, excel=0, excel_file=NULL)
}
\arguments{
\item{x}{a string vector containing the name of the variables for which descriptive statistics will be calculated.}
\item{y}{the name of the categorical variable that defines the stratification that will be used.}
\item{rn}{a string vector containing the text that will replace variables defined in ob x. If y is not provided, the original variable name will be used.
data the name of the dataset to be used.}
\item{miss}{determines if missingness will be shown in the table for the variables. The possible values are: 0=don't add missing statistics; 1=add missing statistics for contiinuous (numerical) values only; 2=add missing statistics for categorical (factor) values only;3=add missing statistics for both numerical and categorical values;}
\item{excel}{indicates if the table will be exported to excel. The default is not to export (excel=0). For exporting to excel set excel=1.}
\item{excel_file}{a string variable defining the name of the excel file. If the directory path is not included in the file name, the file will be saved on the current path directory.}

}
\details{
The getTable1 function generates a descriptive statistical summary appropriate for publishing in a scientific paper.
}
\author{
Tomas Karpati M.D.
}
\examples{
### get the table 1 with original variables name
rv <- c("age","sex","bmi","bmi_cat","ses","is_arab")
Table1(x=rv, data=mdat)

### get the table 1 with the specified given variables text
rn <- c("Age","Sex","BMI","BMI (categories)","SES status","Ethnicity")
Table1(x=rv, rn=rn, data=mdat)

### get the table 1 and stratify by the precence or abcense of MACE
Table1(x=rv, y="sw_MACE",rn=rn, data=mdat)

### export the table 1 to excel
Table1(x=rv, y="sw_MACE",rn=rn, data=mdat, excel=1, excel_file="table1_mace.xlsx")
}
\keyword{ package }
