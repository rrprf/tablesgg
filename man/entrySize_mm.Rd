% Auto-generated documentation for function entrySize_mm 
%  2020-11-29 11:21:55 
\name{entrySize_mm}
\alias{entrySize_mm}
\title{Calculate the Widths and Heights of Table Entries, in Mm }
\description{
Calculate the widths and heights, in millimeters, of the text strings in a
\code{prEntries} object.  This is an internal function, not intended to be
called by package users.
}
\usage{
entrySize_mm(entryInfo)
}
\arguments{
  \item{entryInfo}{A \code{prEntries} object, currently implemented as a data frame with one
row per table entry.  It must include columns \code{text}, \code{math}
(logical), and aesthetics \code{size}, \code{family}, \code{fontface},
\code{angle}, and \code{lineheight}.  \code{size} should be in points,
\code{lineheight} should be specified as a multiple of text size (see
\code{?grid::gpar}).
  }
}
\value{
Data frame with the same row names as \code{entryInfo}, and columns
\code{width} and \code{height} containing entry dimensions in millimeters.

The data frame has an attribute \code{device}, containing the name of the
graphics device used when calculating text sizes.  (Currently this is
always "pdf".)
}
\details{
This relies on the functions \code{grid::convertWidth} and
\code{grid::convertHeight}.  They require that a graphics device to be
open.  It would be natural to use the currently active device, if any, but
this seems to generate empty pages on that device.  Therefore a temporary
PDF device is silently opened, and then deleted at the end.

String sizes may vary slightly between different graphics devices, even
with the same graphics properties (font size, family, face, etc).
}
\seealso{
\code{\link{Devices}}
}
\keyword{internal}
