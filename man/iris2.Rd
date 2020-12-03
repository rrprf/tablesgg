\name{iris2}
\alias{iris2}
\docType{data}
\title{A Reshaped Version of Anderson's Iris Data}
\description{
This is the same as \R's built-in \code{iris} data set, but reshaped into 
\dQuote{long} format.  It contains four measurements for 50 
flowers from each of three species of iris.
}
\usage{iris2}
\format{
  A data frame with 600 observations on the following 5 variables.
  \describe{
    \item{\code{plant}}{a numeric vector, numbering the flowers from 1 to 150}
    \item{\code{Species}}{a factor with levels \code{setosa} \code{versicolor} \code{virginica}}
    \item{\code{flower_part}}{a factor with levels \code{Sepal} \code{Petal}}
    \item{\code{direction}}{a factor with levels \code{Length} \code{Width}}
    \item{\code{value}}{a numeric vector, the measured value, in centimeters}
  }
}
\source{
  \itemize{
    \item Fisher, R. A. (1936) The use of multiple measurements in taxonomic
     problems.  \emph{Annals of Eugenics}, \bold{7}, Part II, 179--188.
    \item The data were collected by Anderson, Edgar (1935).  The irises of
     the Gaspe Peninsula, \emph{Bulletin of the American Iris Society},
     \bold{59}, 2--5.
  }
}
\examples{
data(iris2)
str(iris2)
}
\keyword{datasets}
