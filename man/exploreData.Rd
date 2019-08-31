\name{exploreData}
\alias{exploreData}
\title{
A tool for visual exploration of a dataset.
}
\description{
This function lets the user to have a panoramic view of a dataset. All variables with less than a specified unique values will be treated as a factor. The number of unique values to be used for the determination of factors in numeric variables can be changed with the factorSize argument. The default value for this argument is 10. String variables (including dates) will also be treated as factors. The output of the function is an html page showing four or five columns (in case if a dependent variable is included). The first column show the variable name. The second column a graph with the main distribution of the variable. In cases where the variable is continuous, an histogram will be shown and in cases when the variable is a factor, a bar graph is shown. The third column contains basic descriptive statistics, including the data type, the number of valid and missing values. For continuous variables, the mean and standard deviation, median and interquartile range and the minimum and maximal values are shown. For non-continuous variables, the number of levels and frequencies for the most common levels are shown. The fourth column shows a graph of the individual values and the number of outliers, if they exist. This column may be of help to show undesirable patterns, e.g. may point to problems in data recollection .
}
\usage{
exploreData(y = NULL, data = data, factorSize = 10, dir = "report", ...)
}
\arguments{
  \item{y}{
The dependent variable. If present, a fifth column showing a plot of each variable and the dependent variable. This may be helpful for early discovery of relationships between a variable and the outcome variable.
}
  \item{data}{
The name of the dataset
}
  \item{factorSize}{
The maximal number of unique values on a continuous variable for considering it as a factor.
}
  \item{dir}{
The name of the directory to write on the report.
}
  \item{\dots}{
additional arguments
}
}
\value{
This function creates a folder called report in the main user directory, and html file called report.html in the report directory, and a 'fig' folder where all the images will be saved.
We desicided to use html as the output format, and not other type (e.g. PDF), because we wanted to make available to the user the whole set of graphs that may be of help when writting the preliminary reports.
}
\author{
Tomas Karpati is an Data Researcher at the Clalit Research Institute.
}
\examples{
## Not run:
## --- Getting data stats for a given dataset ---
data(iris)
exploreData(data=iris)

## --- Getting data stats for a given dataset, defining also a dependent variable ---
exploreData('Species', data=iris)
## End(Not run)
}
\keyword{ package }
