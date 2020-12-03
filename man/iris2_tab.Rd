\name{iris2_tab}
\alias{iris2_tab}
\docType{data}
\title{Table of Summary Statistics for Anderson's Iris Data}
\description{
  A table of means and standard deviations of the four measurements 
  per iris plant, by species. 
}
\usage{iris2_tab}
\format{
  A \code{tabular} object as produced by version 0.9.6 of the \pkg{tables} 
  package by Duncan Murdoch (\verb{https://CRAN.R-project.org/package=tables}). 
  The table was produced with the following code, starting from the 
  \code{iris2} data frame:
  \preformatted{iris2_tab <- tables::tabular(
                     Species*Heading()*value*Format(digits=2)*(mean + sd) ~ 
                     Heading("Flower part")*flower_part*Heading()*direction, 
                     data=iris2)
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
\seealso{\code{\link{iris2}}}
\examples{
if (requireNamespace("tables", quietly=TRUE)) {
  print(iris2_tab)  # uses print method for 'tabular' objects
}
plot(iris2_tab)
}
\keyword{datasets}
