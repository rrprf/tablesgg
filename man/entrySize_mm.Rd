% Auto-generated documentation for function entrySize_mm 
%  2021-06-02 11:12:19 
\name{entrySize_mm}
\alias{entrySize_mm}
\title{Calculate the Sizes of Table Entries, in Mm }
\description{
Calculate the horizontal and vertical sizes, in millimeters, of the text
strings in a \code{prEntries} object.  This is an internal function, not
intended to be called by package users.
}
\usage{
entrySize_mm(entryInfo, allowWrap, sizeAdjust=c(1, 1))
}
\arguments{
  \item{entryInfo}{A \code{prEntries} object, currently implemented as a data frame with one
row per table entry.  It must include columns \code{text},
\code{textspec}, \code{maxwidth}, and properties \code{size},
\code{family}, \code{fontface}, \code{hpad}, \code{vpad}, \code{angle},
and \code{lineheight}.  \code{size} should be in points, \code{angle} in
degrees, and \code{lineheight} should be specified as a multiple of text
size (see \code{?grid::gpar}).
  }
  \item{allowWrap}{Logical scalar.  If TRUE, entry sizes will be calculated after any text
wrapping required by \code{entryInfo$maxwidth}.
  }
  \item{sizeAdjust}{Two-element numeric vector containing multipliers to adjust the calculated
absolute size of table text.  The first element is applied to the
horizontal dimension and the second to the vertical dimension.  Ignored if
\code{allowWrap} is FALSE.
  }
}
\value{
Data frame with the same row names as \code{entryInfo}, and columns:
\code{hsize} and \code{vsize} containing entry dimensions in millimeters;
\code{horiz} (logical, TRUE if entry angle is 0 or 180 degrees);
\code{descender} (amount of space included in \code{hsize} (if
\code{horiz} is FALSE) or \code{vsize} (if \code{horiz} if TRUE) to allow
for descenders); and \code{wrap} (logical, whether \code{text} needed to
be wrapped to satisfy \code{maxwidth}).

The data frame has an attribute \code{device}, containing the name of the
graphics device used when calculating text sizes.  (Currently this is
always "pdf".)
}
\details{
The dimensions are for an unrotated rectangle that just encloses the
(possibly rotated) text.  (So \code{hsize} is always measured parallel to
the x-axis and \code{vsize} parallel to the y-axis; i.e., they are with
respect to the reference frame of the whole table.)

This relies on the functions \code{grid::convertWidth} and
\code{grid::convertHeight}.  They require that a graphics device to be
open.  It would be natural to use the currently active device, if any, but
this seems to generate empty pages on that device.  Therefore a temporary
PDF device is silently opened, and then deleted at the end.

String sizes may vary slightly between different graphics devices, even
with the same graphics properties (font size, family, face, etc).

The height of text processed with \code{richtext_grob} or
\code{textbox_grob} differs from the same text processed with
\code{textGrob}: (a) The former includes the height of character
descenders (whether or not the text actually includes characters with
descenders), whereas the latter does not.  (b) Interline spacing seems to
be smaller with the former than with the latter.  Descender height is
returned in column \code{descender}; it will be 0 for text not processed
with \code{richtext_grob} or \code{textbox_grob}.
}
\seealso{
\code{\link{Devices}}
}
\keyword{internal}
