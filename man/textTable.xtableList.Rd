% Auto-generated documentation for function textTable.xtableList 
%  2021-06-02 11:12:19 
\name{textTable.xtableList}
\alias{textTable.xtableList}
\title{Create a \code{texttable} from an \code{xtablelist} Object }
\description{
Create a \code{textTable} from an \code{xtableList} object produced by the
\code{xtable} package.  Such an object represents a set of subtables that
are to be stacked into a single table, with "subheadings" separating the
subtables.  The \code{textTable} that is produced by this function uses
the subheadings as an additional, outer row header layer.  That layer can
optionally be moved inside the table by setting \code{rowheadInside=TRUE}
when the table is plotted.
}
\usage{
\method{textTable}{xtableList}(x, title, subtitle=character(0), foot, ...)
}
\arguments{
  \item{x}{An \code{xtableList} object as produced by the \code{xtable} package.  All
the subtables in \code{x} must have the same column names.
  }
  \item{title}{Optional character vector containing title lines for the table.  May be
empty (\code{character(0)}).  The default is to use the first element of
the \code{caption} attribute of \code{x}, if present.
  }
  \item{subtitle, foot}{Optional character vectors providing additional annotation for the table.
May be empty (i.e., \code{character(0)}).  The default for \code{foot} is
the \code{message} attribute of \code{x}.
  }
  \item{...}{Additional arguments passed to \code{textTable.xtable} for each of the
subtables in \code{x}.  See the documentation for that function.
  }
}
\value{
An object with S3 class \code{textTable}.  See the documentation for the
generic for details about its structure.
}
\details{
This function was designed based on the structure of objects produced by
version 1.8-4 of the \code{xtable} package.

If \code{x} has a \code{message} attribute, it is used as the default
value of the \code{foot} component of the returned \code{textTable}.

If components of \code{x} have no \code{subheading} attribute, then the
subtables are simply stacked, with no additional row header layer to
separate or distinguish them.  It is an error if only some of the
components have a \code{subheading} attribute.
}
\examples{
# 'mtcars_xtab' is an 'xtableList', following an example in the 
# "listOfTablesGallery" vignette of the 'xtable' package.  (See '?mtcars_xtab'
# for the code to create it.)
plot(textTable(mtcars_xtab), rowheadInside=TRUE, 
     title="Example of plotting an 'xtableList'",
     subtitle="(With 'rowheadInside=TRUE')")
  }
