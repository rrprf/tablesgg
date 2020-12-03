% Auto-generated documentation for function angle_adj 
%  2020-11-29 11:21:55 
\name{angle_adj}
\alias{angle_adj}
\title{Calculate the Width and Height of a Rectangles after Rotation ..}
\description{
Calculate the width and height of a rectangles after rotation by angle
\code{theta} (in degrees).  This is an internal function, not intended to
be called by package users.
}
\usage{
angle_adj(width, height, theta)
}
\arguments{
  \item{width, height}{Numeric scalars, the width and height of a rectangle oriented parallel to
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
