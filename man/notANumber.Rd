% Auto-generated documentation for function notANumber 
%  2020-11-29 11:21:55 
\name{notANumber}
\alias{notANumber}
\title{Identify Strings that Cannot be Interpreted as Numbers }
\description{
Identify strings in a character vector that cannot be interpreted as valid
numbers.  This is an internal utility function, not intended to be called
by package users.
}
\usage{
notANumber(x, result=c("logical", "values"))
}
\arguments{
  \item{x}{A character vector.
  }
  \item{result}{The form of result to be returned (see VALUE).
  }
}
\value{
If \code{result} is "logical", a logical vector as long as \code{x} that
is TRUE for those elements that cannot be interpreted as valid numbers.
If \code{result} is "values", a character vector containing the unique
values in \code{x} that cannot be interpreted as valid numbers.
}
\details{
Which character strings can or cannot be interpreted as valid numbers is
determined by the function \code{as.numeric}.  Note that the empty string
"" is considered a valid representation of NA.
}
\keyword{internal}
