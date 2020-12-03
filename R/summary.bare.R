#===== Source file: ../summary.r on 2020-11-29
#-----

summary.textTable <- function(object, ...)
{
  chkDots(...)
  structure(list(adim=adim(object), 
                 parts=tblParts(object)[, c("nr", "nc"), drop=FALSE]), 
            class="summary.textTable")
}

#-----

print.summary.textTable <- function(x, ...)
{
  chkDots(...)
  stopifnot(inherits(x, "summary.textTable"))
  cat("'textTable' with augmented row-column grid dimensions: (", 
      toString(x$adim), ")\n", sep="")
  cat("Table parts:\n")
  print(x$parts)
  invisible(x)
}

#-----

summary.pltdTable <- function(object, ...)
{
  chkDots(...)
  entries <- entries(object)
  blocks <- blocks(object)
  structure(list(adim=adim(object), size=pltdSize(object, units="mm"), 
                 parts=tblParts(object)[, c("nr", "nc"), drop=FALSE], 
                 mergeRuns=attr(entries, "mergeRuns"), 
                 rowheadInside=attr(entries, "rowheadInside"), 
                 rowgroupSize=attr(blocks, "rowgroupSize"),
                 scale=attr(prTable(object), "current_scale"), 
                 plot.margin=attr(object, "plot.margin"), 
                 sizeAdjust=attr(object, "sizeAdjust")), 
            class="summary.pltdTable")
}

#-----

print.summary.pltdTable <- function(x, ...)
{
  chkDots(...)
  stopifnot(inherits(x, "summary.pltdTable"))
  toString2 <- function(y) { 
    if (length(y) > 1)  paste0("c(", toString(y), ")")  else  toString(y) }
  cat("'pltdTable' with augmented row-column grid dimensions: ", 
      toString2(x$adim), "\n", sep="")
  cat("Dimensions of table parts:\n")
  print(x$parts)
  nms <- setdiff(names(x), c("adim", "parts", "size"))
  dispopt <- paste(paste(nms, sapply(x[nms], toString2), sep="= "), 
                   collapse="; ")
  cat(strwrap(paste0("Display options: ", dispopt), exdent=2, simplify=TRUE), 
      sep="\n")
  cat("Displayed size (mm): ", toString2(round(x$size, 2)), "\n", sep="")
  invisible(x)
}

