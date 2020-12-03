\name{mtcars_xtab}
\alias{mtcars_xtab}
\docType{data}
\title{Table of Data from Motor Trend Magazine}
\description{
  This is a table of selected observations and variables from the data frame 
  \code{mtcars} from package \pkg{datasets}.  
}
\usage{mtcars_xtab}
\format{
  An \code{xtableList} object as produced by version 1.8-4 of the \pkg{xtable} 
  package (\verb{https://CRAN.R-project.org/package=xtable}). The table was 
  produced with the following code:
  \preformatted{data("mtcars", package="datasets")
mtcars <- mtcars[, 1:6]
mtcarsList <- split(mtcars, f = mtcars$cyl)
### Reduce the size of the list elements
mtcarsList[[1]] <- mtcarsList[[1]][1,]
mtcarsList[[2]] <- mtcarsList[[2]][1:2,]
mtcarsList[[3]] <- mtcarsList[[3]][1:3,]
attr(mtcarsList, "subheadings") <- paste0("Number of cylinders = ",
                                          names(mtcarsList))
attr(mtcarsList, "message") <- c("Line 1 of Message", "Line 2 of Message")
mtcars_xtab <- xtable::xtableList(mtcarsList, digits = c(0,2,0,0,0,1,2),
                                caption = "Caption to List")
  }
}
\source{
  \itemize{
    \item Henderson and Velleman (1981), Building multiple regression models
     interactively.  \emph{Biometrics}, \bold{37}, 391-411.
  }
}
\seealso{\code{mtcars}}
\examples{
  ttbl <- textTable(mtcars_xtab)
  plot(ttbl, rowheadInside=TRUE)
}
\keyword{datasets}
