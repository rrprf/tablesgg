% Auto-generated documentation for function dfSpecs 
%  2021-06-02 11:12:18 
\name{dfSpecs}
\alias{dfSpecs}
\title{Specifications for Fields Used in Certain Object Classes }
\description{
Return a data frame with specifications for descriptor and graphical
property fields required for certain object classes used within the
package.  This is an internal function, not intended to be called by
package users.
}
\usage{
dfSpecs(objClass)
}
\arguments{
  \item{objClass}{One of \code{tblEntries}, \code{prEntries}, \code{tblBlocks},
\code{prBlocks}, or \code{prHvrules}.
  }
}
\value{
A data frame with one row per descriptor or graphical property field.
Columns are \code{name} (the name of the field); \code{mode} (mode/type of
values taken by the field); and \code{NA_ok} (logical, TRUE if NA is a
valid value for the field).

Field names are used as the row names of the data frame.
}
\details{
Descriptor and graphical property fields are documented in
\code{?elements}.
}
\seealso{
\code{\link{df_from_spec}}, which uses the output from this function to
create a template data frame containing the required fields;
\code{\link{grSpecs}}, which returns specifications for the graphical
properties for each element type.
}
\keyword{internal}
