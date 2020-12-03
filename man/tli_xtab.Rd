\name{tli_xtab}
\alias{tli_xtab}
\docType{data}
\title{Table of Test Scores and Demographics for 20 Students}
\description{
  This is a table of the first 20 observations from the data frame \code{tli} 
  from package \pkg{xtable}.  The observations include demographic data and 
  math scores, from the Texas Assessment of Academic Skills, for 20 students. 
}
\usage{tli_xtab}
\format{
  An \code{xtable} object as produced by version 1.8-4 of the \pkg{xtable} 
  package (\verb{https://CRAN.R-project.org/package=xtable}). The table was 
  produced with the following code:
  \preformatted{data("tli", package="xtable")
  tli_xtab <- xtable::xtable(tli[1:20, ])
  xtable::display(tli_xtab)[c(2,6)] <- "f"
  xtable::digits(tli_xtab) <- matrix(0:4, nrow = 20, ncol = ncol(tli)+1)
  }
}
\source{
  \itemize{
    \item Texas Education Agency, <URL: http://www.tea.state.tx.us>
  }
}
\examples{
  str(tli_xtab)
}
\keyword{datasets}
