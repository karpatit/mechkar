\name{age_adjusted}
\alias{age_adjusted}
\docType{package}
\title{
Returns age-adjusted proportions with its confidence interval
}
\description{
This function is intended to calculate the age-adjusted rates given a dataset that includes age and an binary outcome. It returns the crude rates with their confidence interval and the age-adjusted rates with its confidence interval.
}
\usage{
age_adjusted(dataset,outcome,age,agemin=0,agemax=130,source="who",alpha=0.05)
}
\arguments{
\item{dataset}{the name of the dataframe}
\item{outcome}{the name of the outcome variable  }
\item{age}{the name of the age variable}
\item{agemin}{the minimum age to be used to calculate the weights (default=0)}
\item{agemax}{the maximum age to be used to calculate the weights (default=130)}
\item{source}{the name of the standard to be used for the weights (see the details section). Default: 'who'}
\item{alpha}{the probability of a type I error. Default: 0.05 for the 95\% CI (1-alpha)}
}
\details{
This function return a list containing nine values (see values section).
It includes three types of standards: the European (euro), the American (us) and the World Health Organization (who).
The confidence interval (CI) is calculated using the binomial probabilities using the binconf function of the Hmisc package. The default is to calculate the 95\% CI (using an alpha of 0.05).
}
\value{
This function returns a list containing the following fields:
\itemize{
\item outcome - the name of the outcome variable
\item population - the total population on the dataset
\item events - the number of events for the outcome
\item crude - a list with the crude rates. This contains a list we three elements:
    \itemize{
    \item rate - the value of the crude rate
    \item CImin - the value of the lower CI
    \item CImax - the value of the higher CI
    }
\item weighted - a list with the weighted rates. This contains a list we three elements:
    \itemize{
    \item rate - the value of the crude rate
    \item CImin - the value of the lower CI
    \item CImax - the value of the higher CI
    }
}
}
\author{
Tomas Karpati M.D.
}
\references{

"The WHO Standard Population is defined to reflect the average age
structure of the world's population over the next generation, from the year
2000 to 2025."
https://www.nrscotland.gov.uk/files//statistics/age-standardised-death-rates-esp/age-standard-death-rates-background.pdf

American standard (2000): https://www.cdc.gov/nchs/data/statnt/statnt20.pdf

World (WHO 2000-2025) Standard: https://seer.cancer.gov/stdpopulations/world.who.html
}
\examples{
age <- floor(runif(150,min=30,max=65))
outcome <- round(runif(150,min=0,max=1),0)
df <- data.frame(age=age,disease=outcome)
aa <- age_adjusted(df,"disease","age",agemin=30,agemax=65,source="who",alpha=0.0)
}

