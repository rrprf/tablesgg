#===== Source file: ../plot.textTable.r on 2020-11-29
#-----

plot.textTable <- function(x, title=NULL, subtitle=NULL, foot=NULL, 
                           rowheadLabels=NULL, 
                           entryStyle=tablesggOpt("entryStyle"), 
                           hvruleStyle=tablesggOpt("hvruleStyle"), 
                           blockStyle=tablesggOpt("blockStyle"), scale=1.0, 
                           mergeRuns=c(TRUE, TRUE), rowheadInside=FALSE, 
                           rowgroupSize=0, 
                           plot.margin=tablesggOpt("plot.margin"), 
                           sizeAdjust=c(1.0, 1.0), ...)
{
  chkDots(...)
  x <- update(x, title=title, subtitle=subtitle, foot=foot, 
              rowheadLabels=rowheadLabels)  # update and check the textTable 'x'.

  # Convert 'x' to a 'tblEntries' object ...
  x <- tblEntries(x, mergeRuns=mergeRuns, rowheadInside=rowheadInside)
  # ... create standard blocks ...
  blocks <- tblBlocks(x, rowgroupSize=rowgroupSize)
  # ... apply styles ...
  scale <- rep(scale, length.out=2)
  entries <- prEntries(x, style=entryStyle, scale=scale[1])
  blocks <- prBlocks(blocks, style=blockStyle, scale=scale[1])
  hvrules <- prHvrules(blocks, style=hvruleStyle, scale=scale[2])
  # ... create plot-ready table ...
  pr <- prTable(entries, hvrules=hvrules, blocks=blocks)
  
  plot(pr, plot.margin=plot.margin, sizeAdjust=sizeAdjust)
}

#-----

plot.tblEntries <- function(x, entryStyle=tablesggOpt("entryStyle"), ...)
{
  entries <- prEntries(x, style=entryStyle)
  plot(entries, ...)
}

#-----

plot.prEntries <- function(x, hvruleStyle=tablesggOpt("hvruleStyle"), 
                           blockStyle=tablesggOpt("blockStyle"), 
                           scale=attr(x, "current_scale"), 
                           rowgroupSize=0, ...)
{
  # ... create standard blocks ...
  blocks <- tblBlocks(x, rowgroupSize=rowgroupSize)
  # ... apply styles ...
  scale <- rep(scale, length.out=2)
  x <- prEntries(x, scale=scale[1])  # updates scaling
  blocks <- prBlocks(blocks, style=blockStyle, scale=scale[1])
  hvrules <- prHvrules(blocks, style=hvruleStyle, scale=scale[2])
  # ... create plot-ready table ...
  pr <- prTable(x, hvrules=hvrules, blocks=blocks)
  
  plot(pr, ...)
}

#-----

plot.pltdTable <- function(x, ...)
{
  print(x, ...)
}

#-----

plot.tabular <- function(x, ...)
{
  dots <- list(...)
  # Separate '...' into arguments intended for 'textTable.tabular' and 
  # arguments intended for 'plot.textTable'.  (Args not specifically named 
  # by the latter should go to the former.)
  nms <- names(dots)
  argnames2 <- setdiff(names(formals(plot.textTable)), "...")  
  if (is.null(nms)) {
    if (length(dots) > 0)  stop("Arguments after the first must be named")
    ttarg <- list()
    plarg <- list()
  } else {
    ttarg <- dots[setdiff(nms, argnames2)]
    plarg <- dots[intersect(nms, argnames2)]
  }
  
  x <- do.call(textTable, c(list(x=x), ttarg))
  do.call(plot, c(list(x=x), plarg))
}

