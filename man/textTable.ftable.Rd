% Auto-generated documentation for function textTable.ftable 
%  2021-06-02 11:12:19 
\name{textTable.ftable}
\alias{textTable.ftable}
\title{Create a \code{texttable} from an \code{ftable} }
\description{
Create a \code{textTable} object representing a flattened multiway
contingency table.
}
\usage{
\method{textTable}{ftable}(x, colheadLabels=c("layers", "none", "paste"), sep=": ", 
    title=character(0), subtitle=character(0), foot=character(0), ...)
}
\arguments{
  \item{x}{An \code{ftable} object, as produced by R's \code{ftable} function,
representing a flattened multiway contingency table.
  }
  \item{colheadLabels}{Character scalar; how to display names of column header variables.  "none"
means to not display them.  "layers" (the default) means to display them
as additional column header layers (so each header variable occupies two
rows instead of one).  "paste" means to paste the variable name in front
of each of its values, separated by \code{sep}.
  }
  \item{sep}{Character scalar; string that separates a variable name from its values
when \code{colheadLabels} is "paste".
  }
  \item{title, subtitle, foot}{Optional character vectors providing annotation for the table.  May be
empty (i.e., \code{character(0)}, the default).
  }
  \item{...}{Ignored, with a warning.  (Included for compatibility with the generic.)
  }
}
\value{
An object with S3 class \code{textTable}.  See the documentation for the
generic for details about its structure.
}
\seealso{
\code{ftable}, \code{format.ftable}
}
\examples{
# From examples in '?ftable':
data(Titanic, package="datasets")
ft <- ftable(Titanic, row.vars = 1:2, col.vars = "Survived")
ttbl <- textTable(ft, title="Plotting an 'ftable'")
plot(ttbl)

data(mtcars, package="datasets")
ft <- ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear, row.vars = c(2, 4),
             dnn = c("Cylinders", "V/S", "Transmission", "Gears"))
ttbl <- textTable(ft, colheadLabels="none")
plt1 <- plot(ttbl, title="Plotting an 'ftable'", 
             subtitle="No colheadLabels")
ttbl <- textTable(ft, colheadLabels="layers")
plt2 <- plot(ttbl, title="Plotting an 'ftable'", 
             subtitle="colheadLabels = 'layers'")
ttbl <- textTable(ft, colheadLabels="paste")
plt3 <- plot(ttbl, title="Plotting an 'ftable'", 
             subtitle="colheadLabels = 'paste'")
print(plt1, position=c("left", "top"))
print(plt2, position=c("left", "center"), newpage=FALSE)
print(plt3, position=c("left", "bottom"), newpage=FALSE)
  }
