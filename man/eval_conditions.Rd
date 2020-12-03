% Auto-generated documentation for function eval_conditions 
%  2020-11-29 11:21:55 
\name{eval_conditions}
\alias{eval_conditions}
\title{Evaluate a Set of Logical Conditions Within a Data Frame }
\description{
Evaluate each of a set of logical \code{conditions} within a data frame
\code{x}.  This is an internal utility function, not intended to be called
directly by package users.
}
\usage{
eval_conditions(x, conditions, NA_condition_value=TRUE)
}
\arguments{
  \item{x}{A data frame.
  }
  \item{conditions}{Character vector.  Each element should represent an expression that can be
evaluated within \code{x}, and produce a logical scalar or a vector with
one value per row of \code{x}.  Elements of \code{conditions} that are NA
or the empty string are treated specially, via \code{NA_condition_value}.
  }
  \item{NA_condition_value}{Logical scalar, TRUE, FALSE, or NA.  The value to use in place of
evaluation when an element of \code{conditions} is NA or the empty string.
  }
}
\value{
Logical matrix with one row for each row of \code{x} and one column for
each condition in \code{conditions}.  Element \code{[i, j]} contains the
result of evaluating \code{condition[j]} for the \code{i}-th row of
\code{x}.
}
\details{
Each condition should generally represent an expression involving columns
of \code{x}.  It is evaluated using \code{x} as the environment, and must
return either a logical vector with length equal to the number of rows in
\code{x}, or a logical scalar (which will be replicated into a vector of
the right length).  The resulting vectors, one per condition, form the
columns of the returned matrix.

A condition may be NA or the empty string.  In that case it is not
evaluated, and a vector containing \code{NA_condition_value} is used
instead.
}
\keyword{internal}
