% Auto-generated documentation for function angle_adj 
%  2021-06-02 11:12:19 
\name{angle_adj}
\alias{angle_adj}
\title{Horizontal and Vertical Dimensions of a Rectangle after Rotation }
\description{
Calculate the horizontal and vertical dimensions of a rectangle after
rotation by angle \code{theta} (in degrees).  This is an internal
function, not intended to be called by package users.
}
\usage{
angle_adj(hsize, vsize, theta)
}
\arguments{
  \item{hsize, vsize}{Numeric scalars, the width and height of a rectangle oriented parallel to
the coordinate axes, with one corner at the origin.
  }
  \item{theta}{Rotation angle, in degrees.
  }
}
\value{
Two-element vector with the new width and height, with respect to the
original axes, of the rotated rectangle.
}
\keyword{internal}
