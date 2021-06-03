#===== Source file: ../elements2.r on 2021-06-02
#-----

entries <- function(x, enabledOnly=TRUE)
{
  if (inherits(x, "pltdTable"))  x <- attr(x, "prTable")
  xx <- x$entries
  if (enabledOnly)  xx <- xx[xx[, "enabled"], , drop=FALSE]
  xx
}

#-----

'entries<-' <- function(x, value)
{
  value <- as.prEntries(value)  # validity checks
  if (inherits(x, "prTable")) {
    x <- prTable(entries=value, hvrules=hvrules(x, enabledOnly=FALSE), 
                 blocks=blocks(x, enabledOnly=FALSE))
  } else if (inherits(x, "pltdTable")) {
    prtbl <- attr(x, "prTable")
    prtbl <- prTable(entries=value, hvrules=hvrules(prtbl, enabledOnly=FALSE), 
                     blocks=blocks(prtbl, enabledOnly=FALSE))
    x <- plot(prtbl, plot.margin=attr(x, "plot.margin"), 
              sizeAdjust=attr(x, "sizeAdjust"))
  } else  stop("Object to be updated is not a 'prTable' or 'pltdTable'")
  x
}

#-----

hvrules <- function(x, enabledOnly=TRUE)
{
  if (inherits(x, "pltdTable"))  x <- attr(x, "prTable")
  xx <- x$hvrules
  if (enabledOnly && !is.null(xx))  xx <- xx[xx[, "enabled"], , drop=FALSE]
  xx
}

#-----

'hvrules<-' <- function(x, value)
{
  value <- as.prHvrules(value)  # validity checks
  if (inherits(x, "prTable")) {
    x <- prTable(entries=entries(x, enabledOnly=FALSE), hvrules=value, 
                 blocks=blocks(x, enabledOnly=FALSE))
  } else if (inherits(x, "pltdTable")) {
    prtbl <- attr(x, "prTable")
    prtbl <- prTable(entries=entries(prtbl, enabledOnly=FALSE), hvrules=value, 
                     blocks=blocks(prtbl, enabledOnly=FALSE))
    x <- plot(prtbl, plot.margin=attr(x, "plot.margin"), 
              sizeAdjust=attr(x, "sizeAdjust"))
  } else  stop("Object to be updated is not a 'prTable' or 'pltdTable'")
  x
}

#-----

blocks <- function(x, enabledOnly=TRUE)
{
  if (inherits(x, "pltdTable"))  x <- attr(x, "prTable")
  xx <- x$blocks
  if (enabledOnly && !is.null(xx))  xx <- xx[xx[, "enabled"], , drop=FALSE]
  xx
}

#-----

'blocks<-' <- function(x, value)
{
  value <- as.prBlocks(value)  # validity checks
  if (inherits(x, "prTable")) {
    x <- prTable(entries=entries(x, enabledOnly=FALSE), blocks=value, 
                 hvrules=hvrules(x, enabledOnly=FALSE))
  } else if (inherits(x, "pltdTable")) {
    prtbl <- attr(x, "prTable")
    prtbl <- prTable(entries=entries(prtbl, enabledOnly=FALSE), blocks=value, 
                     hvrules=hvrules(prtbl, enabledOnly=FALSE))
    x <- plot(prtbl, plot.margin=attr(x, "plot.margin"), 
              sizeAdjust=attr(x, "sizeAdjust"))
  } else  stop("Object to be updated is not a 'prTable' or 'pltdTable'")
  x
}

