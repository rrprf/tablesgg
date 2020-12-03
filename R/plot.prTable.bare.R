#===== Source file: ../plot.prTable.r on 2020-11-29
#-----

plot.prTable <- function(x, plot.margin=tablesggOpt("plot.margin"), 
                         sizeAdjust=c(1.0, 1.0), ...)
{
  chkDots(...)
  entryInfo <- as.prEntries(entries(x, enabledOnly=FALSE))  # validity checks
  # (Wait to remove disabled entries.)
  if (NROW(entryInfo) == 0 || sum(entryInfo[, "enabled"]) == 0) {
    warning("Table has no enabled entries.  Nothing to plot.  Returning NULL.")
    return(invisible())
  }
  enabled <- entryInfo[, "enabled"]
  blocks <- blocks(x, enabledOnly=TRUE)
  hvrules <- hvrules(x, enabledOnly=TRUE)
  if (!is.null(hvrules)) {
    hvrules <- as.prHvrules(hvrules)  # validity checks
    hruleInfo <- hvrules[hvrules$direction == "hrule", , drop=FALSE]
    vruleInfo <- hvrules[hvrules$direction == "vrule", , drop=FALSE]
  }
  
  #----- 
  # The following are meaningless initializations, required only because 
  # package checking does not recognize variables defined using 'ggplot's 
  # non-standard evaluation.
  fill <- NA;  fill_alpha <- NA;  border_color <- NA;  border_size <- NA
  gg_hjust <- NA;  gg_vjust <- NA;  size <- NA;  angle <- NA;  color <- NA
  family <- NA;  fontface <- NA;  lineheight <- NA;  text <- NA
  linetype <- NA
  
  #----- Initialize a plot.
  # Set the widths of the margins added on each side of the table.
  plotmargin <- do.call(margin, c(as.list(plot.margin), list(unit="mm")))
  plt <- ggplot() + labs(x=NULL, y=NULL)
  # Turn off axes, grid, panel border.
  plt <- plt + theme(axis.line=element_blank(), axis.text=element_blank(), 
                     axis.title=element_blank(), axis.ticks=element_blank(), 
                     panel.border=element_blank(), 
                     panel.grid.major=element_blank(), 
                     panel.grid.minor=element_blank(), 
                     panel.background=element_rect(fill=NA), 
                     plot.margin=plotmargin)
  
  #----- Column widths, row heights, in mm.
  nr_tbl <- max(entryInfo[, "arow2"])
  nc_tbl <- max(entryInfo[, "acol2"])
  sz <- entrySize_mm(entryInfo)
  xwidth <- ifelse(enabled, sizeAdjust[1]*sz$width + 2*entryInfo[, "hpad"], 0)
  xheight <- ifelse(enabled, sizeAdjust[2]*sz$height + 2*entryInfo[, "vpad"], 0)
  # Initialize column widths, row heights based on entries that span only a 
  # single column or row.  Will be updated below.
  colWidth <- rep_len(0, nc_tbl)
  for (j in seq_along(colWidth)) {
    sngl <- with(entryInfo, acol1 == j & acol2 == j)
    if (any(sngl)) colWidth[j] <- max(xwidth[sngl])
  }
  rowHeight <- rep_len(0, nr_tbl)
  for (i in seq_along(rowHeight)) {
    sngl <- with(entryInfo, arow1 == i & arow2 == i)
    if (any(sngl)) rowHeight[i] <- max(xheight[sngl])
  }
  
  #----- Widths of vrules, heights of hrules, in mm.
  vruleWidth <- rep_len(0, nc_tbl+1)  # allow for outside rules
  hruleHeight <- rep_len(0, nr_tbl+1)
  if (!is.null(hvrules)) {
    acol <- sort(unique(vruleInfo[, "acol1"]))  # half-integers
    for (j in seq_along(acol)) {
      idx <- which(vruleInfo[, "acol1"] == acol[j])
      vruleWidth[ceiling(acol[j])] <- max(vruleInfo[idx, "space"])
    }
    arow <- sort(unique(hruleInfo[, "arow1"]))  # half-integers
    for (j in seq_along(arow)) {
      idx <- which(hruleInfo[, "arow1"] == arow[j])
      hruleHeight[ceiling(arow[j])] <- max(hruleInfo[idx, "space"])
    }
  }

  #-----
  # If there are entries that span multiple columns or rows, check whether 
  # the entry fits in the sum of the corresponding single column/row sizes 
  # (plus sizes of any spanned hvrules). 
  # If not, expand each of the spanned columns/rows proportionately.  Do not 
  # expand hvrule sizes.
  multcol <- with(entryInfo, which((acol1 != acol2) & (xwidth > 0)))  # might be empty
  for (k in multcol) {
    idx <- seq(from=entryInfo[k, "acol1"], to=entryInfo[k, "acol2"])
    spannedcol_width <- sum(colWidth[idx])
    spannedvrule_width <- sum(vruleWidth[(idx[2]):(idx[length(idx)])])
    if (spannedcol_width > 0) {
      # Expand spanned columns proportionately.
      expfac <- max(1, (xwidth[k] - spannedvrule_width) / spannedcol_width)
      colWidth[idx] <- expfac * colWidth[idx]
    } else {
      # Expand (empty) spanned columns by the same absolute amount.
      colWidth[idx] <- (xwidth[k] - spannedvrule_width) / length(idx)
    }
  }
  multrow <- with(entryInfo, which((arow1 != arow2) & (xheight > 0)))  # might be empty
  for (k in multrow) {
    idx <- seq(from=entryInfo[k, "arow1"], to=entryInfo[k, "arow2"])
    spannedrow_height <- sum(rowHeight[idx])
    spannedhrule_height <- sum(hruleHeight[(idx[2]):(idx[length(idx)])])
    if (spannedrow_height > 0) {
      # Expand spanned rows proportionately.
      expfac <- max(1, (xheight[k] - spannedhrule_height) / spannedrow_height)
      rowHeight[idx] <- expfac * rowHeight[idx]
    } else {
      # Expand (empty) spanned rows by the same absolute amount.
      rowHeight[idx] <- (xheight[k] - spannedhrule_height) / length(idx)
    }
  }
  
  #-----
  # Calculate the x, y positions of the boundaries of each row and column, 
  # in mm.  The origin is at the upper left of the table, with the y 
  # coordinate increasing downward, and the x coordinate increasing to the 
  # right.  The x positions can be indexed by 'acol*', the y positions by 
  # 'arow*'.
  xmax <- cumsum(colWidth) + cumsum(vruleWidth)[-(nc_tbl+1)]
  xmin <- xmax - colWidth
  ymax <- cumsum(rowHeight) + cumsum(hruleHeight)[-(nr_tbl+1)]
  ymin <- ymax - rowHeight
  
  # Maximum x, y coordinates, including all entries and hvrules, in mm. 
  # (Minimum is 0.)
  maxX <- sum(colWidth) + sum(vruleWidth)
  maxY <- sum(rowHeight) + sum(hruleHeight)
  
  #----- Delete disabled entries.  (Didn't do this earlier because it would 
  #      cause complications in calculating and retaining row heights and 
  #      column widths. )
  entryInfo <- entryInfo[enabled, , drop=FALSE]
  
  #----- Draw rectangles for enabled blocks.
  if (!is.null(blocks) && nrow(blocks) > 0) {
    blockInfo <- as.prBlocks(blocks)  # validity checks
    acol1 <- blockInfo[, "acol1"]
    acol2 <- blockInfo[, "acol2"]
    arow1 <- blockInfo[, "arow1"]
    arow2 <- blockInfo[, "arow2"]
    dfr_blks <- data.frame(xmin=xmin[acol1], xmax=xmax[acol2], 
                           ymin=ymin[arow1], ymax=ymax[arow2], 
                           blockInfo[, c("fill", "fill_alpha", "border_size", 
                                         "border_color")], 
                           stringsAsFactors=FALSE)
    plt <- plt + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, 
                               fill=fill, alpha=fill_alpha, color=border_color, 
                               size=border_size), 
                           data=dfr_blks, show.legend=FALSE)  #+ 
           # These will be added unconditionally further down:
           #scale_colour_identity() + scale_size_identity() + 
           #scale_fill_identity() + scale_linetype_identity()
  }

  #----- Draw cell backgrounds.
  acol1 <- entryInfo[, "acol1"]
  acol2 <- entryInfo[, "acol2"]
  arow1 <- entryInfo[, "arow1"]
  arow2 <- entryInfo[, "arow2"]
  dfr_cell <- data.frame(xmin=xmin[acol1], xmax=xmax[acol2], 
                         ymin=ymin[arow1], ymax=ymax[arow2], 
                         entryInfo[, c("fill", "fill_alpha", "border_size", 
                                       "border_color")], 
                         stringsAsFactors=FALSE)
  plt <- plt + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, 
                             fill=fill, alpha=fill_alpha, color=border_color, 
                             size=border_size), 
                         data=dfr_cell, show.legend=FALSE) + 
         scale_colour_identity() + scale_size_identity() + 
         scale_fill_identity() + scale_linetype_identity()

  #----- Draw entry text for each cell.
  # Calculate x, y position for text based on cell boundaries and 
  # justification.  
  ctr <- (xmin[acol1] + xmax[acol2]) / 2
  xw <- xmax[acol2] - xmin[acol1] - 2*entryInfo[, "hpad"]
  hjust <- entryInfo[, "hjust"]
  xx <- ctr + (hjust - 0.5)*xw
  ctr <- (ymin[arow1] + ymax[arow2]) / 2
  yh <- ymax[arow2] - ymin[arow1] - 2*entryInfo[, "vpad"]
  vjust <- entryInfo[, "vjust"]
  yy <- ctr + (vjust - 0.5)*yh
  # Note:  Need to allow for the fact that in 'geom_text', 'hjust' and 'vjust' 
  # specify on which side of an (x, y) point the label is placed (0 = to the 
  # right of/above the point, 1 = to the left of/below the point).  This is 
  # different from interpreting justification with respect to cell boundaries.
  pt2mm <- function(pts) { (pts / 72.27) * 25.4 }
  dfr_txt <- data.frame(xx, yy, size=pt2mm(entryInfo[, "size"]), 
                        gg_hjust=hjust, gg_vjust=(1-vjust), 
                        entryInfo[, c("text", "math", "color", "alpha", 
                                      "family", "fontface", "lineheight", 
                                      "angle")], 
                        stringsAsFactors=FALSE)
  use_math <- dfr_txt[, "math"]
  if (any(use_math)) {
    plt <- plt + geom_text(aes(x=xx, y=yy, hjust=gg_hjust, vjust=gg_vjust, 
                               size=size, 
                               alpha=alpha, angle=angle, colour=color, 
                               family=family, fontface=fontface, 
                               lineheight=lineheight, label=text), 
                           data=dfr_txt[use_math, , drop=FALSE], parse=TRUE, 
                           show.legend=FALSE)
  }
  if (any(!use_math)) {
    plt <- plt + geom_text(aes(x=xx, y=yy, hjust=gg_hjust, vjust=gg_vjust, 
                               size=size, 
                               alpha=alpha, angle=angle, colour=color, 
                               family=family, fontface=fontface, 
                               lineheight=lineheight, label=text), 
                           data=dfr_txt[!use_math, , drop=FALSE], parse=FALSE, 
                           show.legend=FALSE)
  }
  plt <- plt + scale_alpha_identity()

  #----- Draw horizontal rules.
  if (!is.null(hvrules) && nrow(hruleInfo) > 0) {
    # Calculate the boundaries of the rectangle enclosing each rule, in mm.
    xminr <- xmin[hruleInfo[, "acol1"]]
    xmaxr <- xmax[hruleInfo[, "acol2"]]
    idxr <- ceiling(hruleInfo[, "arow1"])
    # Need to allow for outside rules (idxr = 1 or nr_tbl+1)
    ymin_extended <- c(ymin, maxY)
    yy <- ymin_extended[idxr] - (hruleHeight[idxr] / 2)  # center of rectangle
    yminr <- yy - (hruleInfo[, "space"] / 2)
    ymaxr <- yy + (hruleInfo[, "space"] / 2)
    dfr_hr <- data.frame(xminr, xmaxr, yminr, ymaxr, yy, 
                         hruleInfo[, c("color", "linetype", "size", "fill", 
                                       "fill_alpha", "space")], 
                         stringsAsFactors=FALSE)
    rect <- (dfr_hr$space > 0)
    if (any(rect) > 0) {
      plt <- plt + geom_rect(aes(xmin=xminr, xmax=xmaxr, ymin=yminr, 
                                 ymax=ymaxr, fill=fill, alpha=fill_alpha), 
                             data=dfr_hr[rect, ], show.legend=FALSE)
    }
    plt <- plt + geom_segment(aes(x=xminr, y=yy, xend=xmaxr, yend=yy, 
                                  colour=color, size=size, linetype=linetype), 
                              data=dfr_hr, show.legend=FALSE)
  }

  #----- Draw vertical rules.
  if (!is.null(hvrules) && nrow(vruleInfo) > 0) {
    # Calculate the boundaries of the rectangle enclosing each rule, in mm.
    yminr <- ymin[vruleInfo[, "arow1"]]
    ymaxr <- ymax[vruleInfo[, "arow2"]]
    idxr <- ceiling(vruleInfo[, "acol1"])
    # Need to allow for outside rules (idxr = 1 or nc_tbl+1)
    xmin_extended <- c(xmin, maxX)
    xx <- xmin_extended[idxr] - (vruleWidth[idxr] / 2)  # center of rectangle
    xminr <- xx - (vruleInfo[, "space"] / 2)
    xmaxr <- xx + (vruleInfo[, "space"] / 2)
    dfr_vr <- data.frame(xminr, xmaxr, yminr, ymaxr, xx, 
                         vruleInfo[, c("color", "linetype", "size", "fill", 
                                       "fill_alpha", "space")], 
                         stringsAsFactors=FALSE)
    rect <- (dfr_vr$space > 0)
    if (any(rect) > 0) {
      plt <- plt + geom_rect(aes(xmin=xminr, xmax=xmaxr, ymin=yminr, 
                                 ymax=ymaxr, fill=fill, alpha=fill_alpha), 
                             data=dfr_vr[rect, ], show.legend=FALSE)
    }
    plt <- plt + geom_segment(aes(x=xx, y=yminr, xend=xx, yend=ymaxr, 
                                  colour=color, size=size, linetype=linetype), 
                              data=dfr_vr, show.legend=FALSE)
  }

  #----- Explicitly set axis limits.  Needed to avoid clipping, since text 
  #      added with geom_text is treated as having 0 size.
  #      Also reverse y axis (since row number increases from top to bottom).
  plt <- plt + scale_x_continuous(limits=c(0, maxX), expand=c(0, 0)) + 
         scale_y_reverse(limits=c(maxY, 0), expand=c(0, 0))

  #----- Finish up by adding attributes to the plot.
  attr(plt, "plot.margin") <- plot.margin
  attr(plt, "sizeAdjust") <- sizeAdjust
  size_mm <- structure(c(maxX + plot.margin[2] + plot.margin[4], 
                         maxY + plot.margin[1] + plot.margin[3]), 
                       device=attr(sz, "device"))
  attr(plt, "size_mm") <- size_mm
  attr(plt, "colBoundaries") <- xmax
  attr(plt, "rowBoundaries") <- ymax
  attr(plt, "prTable") <- x
  class(plt) <- c("pltdTable", class(plt))
  plt
}

