\name{getMissingness}
\alias{getMissingness}
\title{Determine the degree of missings on a dataset}
\usage{
getMissingness(data,getRows=FALSE)
}
\description{
This function calculates the missingness of the supplied dataset. The function returns a list of variables, in descending order, showing their number and percentage of missing values. It also reports the number of rows with complete data. If getRows is set to true, it will also return a list of indexes of the rows containing complete data. This list can be used to retrieve a new dataset containing only complete data or a dataset with all the data having at least one missing value.
}
\arguments{
\item{data}{the dataset to be checked. It must be a data.frame, data.table or tibble.}
\item{getRows}{indicates if a list of row indexes has to be returned. Default is FALSE}
}
\details{
The function getMissingness returns a list that include the list of variables and their percentage of missing values. You can retrieve any of the objects in the list by their names
}
\value{
A list that include the following objects:
\item{missingness}{a dataframe containing the variable names and the percentage of missingness}
\item{msg}{a message string stating the number of rows (and their percent) having complete data}
\item{rows}{a vector with the indexes of the rows having complete data only}
}
\author{
Tomas Karpati M.D.
}
\examples{
### Show only the list of variables with missing values and the number of complete rows.
data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA
aa <- getMissingness(data)

### Shows the list of variables with missing values and the number of complete rows
aa <- getMissingness(data,getRows=TRUE)
ind <- aa[[3]]
complete <- data[ind,]
incomplete <- data[-ind,]
}

