% Auto-generated documentation for function textTable.xtable 
%  2020-11-29 11:21:55 
\name{textTable.xtable}
\alias{textTable.xtable}
\title{Create a \code{texttable} from an \code{xtable} Object }
\description{
Create a \code{textTable} from an \code{xtable} object produced by the
\code{xtable} package.  The \code{textTable} can then be styled and
plotted.
}
\usage{
\method{textTable}{xtable}(x, title, subtitle=character(0), foot=character(0), 
    row.names="", na="", mathExponents=TRUE, ...)
}
\arguments{
  \item{x}{An \code{xtable} object as produced by the \code{xtable} package.
  }
  \item{title}{Optional character vector containing title lines for the table.  May be
empty (\code{character(0)}).  The default is to use the first element of
the \code{caption} attribute of \code{x}, if present.
  }
  \item{subtitle, foot}{Optional character vectors providing additional annotation for the table.
May be empty (i.e., \code{character(0)}, the default).
  }
  \item{row.names}{A logical scalar or a character string.  If FALSE, the row names of
\code{x} are not included in the table.  If TRUE, the row names are
included as row headers, with a row header label of "row.names".  If a
character string, row names are included as row headers, and if the string
is not empty, it is used as the row header label.
  }
  \item{na}{String to be used to represent missing values in \code{x}.  The default
value is the empty string "".
  }
  \item{mathExponents}{Logical scalar.  If TRUE, then numerical values in \code{x} that are
formatted into scientific notation (i.e., strings like "3.14e-02",
specified by values of \code{e} or \code{E} in the \code{display}
attribute of \code{x}) will be plotted in math style, with the power of 10
shown as a superscript.
  }
  \item{...}{Additional named arguments passed to \code{formatC}, when converting
values in \code{x} to character strings.  They must not include
\code{digits} or \code{format}, which are specified within \code{x}
itself.
  }
}
\value{
An object with S3 class \code{textTable}.  See the documentation for the
generic for details about its structure.
}
\details{
This function was designed based on the structure of objects produced by
version 1.8-4 of the \code{xtable} package.

An \code{xtable} object is a data frame that contains the columns of the
table and attributes that specify how those columns are to be formatted.
This function uses those attributes to created formatted character strings
for each table entry, and assembles them into a \code{textTable} object,
which may then be styled and plotted.

Formatting is done by \code{formatC} using the \code{digits} and
\code{display} attributes of \code{x}.  The \code{align} attribute is used
to set the \code{justification} attributes in the returned
\code{textTable}.  (Vertical rule characters, \code{|}, within
\code{align} are ignored; use an \code{hvruleStyle} or the
\code{addHvrule} function to insert vertical rules into the plotted table,
as shown in the examples.)
}
\examples{
# 'tli_xtab' is an 'xtable' object created using 'xtable::xtable':
class(tli_xtab)
# This package provides a 'textTable' method for such objects:
ttbl <- textTable(tli_xtab)
plot(ttbl)

if (requireNamespace("xtable", quietly=TRUE)) withAutoprint({
  data(tli, package="xtable")
  
  # ANOVA table.
  fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
  plt1 <- plot(textTable(fm1.table <- xtable::xtable(fm1), 
                         title="xtable: ANOVA table"))

  # Table of linear regression results.
  fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
  plt2 <- plot(textTable(fm2.table <- xtable::xtable(fm2), 
                         title="xtable: Linear regression"))
  
  # Time series table.
  temp.ts <- ts(cumsum(1 + round(rnorm(100), 0)), start = c(1954, 7), 
                frequency = 12)
  plt3 <- plot(textTable(xtable::xtable(temp.ts, digits = 0), 
                         title="xtable: Time series"))
  
  # Math style for scientific notation.
  plt4 <- plot(textTable(xtable::xtable(data.frame(text = c("foo","bar"),
                                                   googols = c(10e10,50e10),
                                                   small = c(8e-24,7e-5),
                                                   row.names = c("A","B")),
                                        display = c("s","s","g","g")),
                         mathExponents = TRUE, 
                     title=c("xtable:", "Math style for scientific notation")))
  print(plt1, position=c(0.1, 0.9))
  print(plt2, position=c(0.1, 0.5), newpage=FALSE)
  print(plt3, position=c(0.1, 0.1), newpage=FALSE)
  print(plt4, position=c(0.9, 0.9), newpage=FALSE)
  
  # By default vertical rules specified by '|' characters in 'align' are 
  # ignored.  They can be added afterward using the 'addHvrule' function 
  # as follows:
  tli.table <- xtable::xtable(tli[1:10, ])
  xtable::align(tli.table) <- "|rrl|l|lr|"
  plt <- plot(textTable(tli.table, 
                        title="xtable: Vertical rules derived from 'align'"))
  pipe_posn <- which(unlist(strsplit(attr(tli.table, "align"), "")) == "|")
  vrule_acol <- pipe_posn - seq_along(pipe_posn) + 0.5
  for (ac in vrule_acol)  plt <- addHvrule(plt, direction="vrule", acols=ac, 
                                           arows=arow(plt, "colhead_and_body"), 
                                           props=element_hvrule(linetype=1, 
                                                                color="black"))
  plt
})
  }
