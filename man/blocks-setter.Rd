% Auto-generated documentation for function blocks<- 
%  2020-11-29 11:21:55 
\name{blocks<-}
\alias{blocks<-}
\title{Update the Blocks of a Plot-Ready or Plotted Table }
\description{
Update the blocks component of a plot-ready (\code{prTable}) or plotted
(\code{pltdTable}) table object.  This is an internal function, not
intended to be called by package users.
}
\usage{
blocks(x) <- value
}
\arguments{
  \item{x}{A plot-ready (\code{prTable}) or plotted (\code{pltdTable}) table object.
  }
  \item{value}{A \code{prBlocks} object containing the new plot-ready blocks.
  }
}
\value{
An object like \code{x}, with updated hvrules.
}
\keyword{internal}
