#===== Source file: ../plot.prTable.r on 2021-06-02
#-----

plot.prTable <- function(x, plot.margin=tablesggOpt("plot.margin"), 
                         sizeAdjust=c(1.0, 1.0), ...)
{
  chkDots(...)
  entryInfo <- entries(x, enabledOnly=FALSE)
  enabled <- entryInfo[, "enabled"]
  if (sum(enabled) == 0) {
    warning("Table has no enabled entries.  Nothing to plot.  Returning NULL.")
    return(invisible())
  }
  blocks <- blocks(x, enabledOnly=TRUE)
  hvrules <- hvrules(x, enabledOnly=TRUE)
  if (!is.null(hvrules)) {
    hvrules <- as.prHvrules(hvrules)  # validity checks
    hruleInfo <- hvrules[hvrules$direction == "hrule", , drop=FALSE]
    vruleInfo <- hvrules[hvrules$direction == "vrule", , drop=FALSE]
  }
  ad <- adim(entryInfo)
  nr_tbl <- ad[1]
  nc_tbl <- ad[2]
  # Remove disabled entries.
  entryInfo <- entryInfo[enabled, , drop=FALSE]
  
  #----- 
  # The following are meaningless initializations, required only because 
  # package checking does not recognize variables defined using 'ggplot's 
  # non-standard evaluation.
  fill <- NA;  fill_alpha <- NA;  border_color <- NA;  border_size <- NA
  gg_hjust <- NA;  gg_vjust <- NA;  size <- NA;  angle <- NA;  color <- NA
  family <- NA;  fontface <- NA;  lineheight <- NA;  text <- NA
  linetype <- NA;  y <- NA;  orientation <- NA
  
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

  #----- Column widths, row heights, in mm.
  method <- { if (tablesggOpt("allowWrap"))  internalOpt[["rcsize_method_wrap"]]
              else  internalOpt[["rcsize_method_nowrap"]] }
  rcsize <- calc_rcsize(entryInfo, 
                        vrule_hsize=vruleWidth, hrule_vsize=hruleHeight, 
                        sizeAdjust=sizeAdjust, 
                        nominal_rcmin=internalOpt[["rcsize_rcmin"]], 
                        method=method)
  colWidth <- rcsize$hsizeCol
  rowHeight <- rcsize$vsizeRow
  sz <- rcsize$sizeInfo  # data frame with sizes of individual entries + 'wrap'
  
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
  size_mm <- structure(c(maxX + plot.margin[2] + plot.margin[4], 
                         maxY + plot.margin[1] + plot.margin[3]), 
                       device=attr(sz, "device"))  # full table size, in mm
  
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
                           data=dfr_blks, show.legend=FALSE)
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
  # Markdown text requires 'allowMarkdown', wrapping requires 'allowWrap'.
  if (any(chk <- (entryInfo[, "textspec"] == "markdown")) &&
      !tablesggOpt("allowMarkdown"))  stop(
    "Use of markdown in entry text is not enabled, but following entries ", 
    "specify markdown: ", toString(entryInfo[chk, "id"], width=60))
  if (any(chk <- (sz$wrap)) && !tablesggOpt("allowWrap"))  stop(
    "Wrapping of entry text is not enabled, but following entries ", 
    "require wrapping due to 'maxwidth': ", 
    toString(entryInfo[chk, "id"], width=60))

  # Calculate x, y position for text based on cell boundaries, justification, 
  # and rotation angle.
  dfr_xy <- coord_justif(entryInfo, x0=xmin[acol1], x1=xmax[acol2], 
                         y0=ymin[arow1], y1=ymax[arow2], size=sz)
  
  pt2mm <- function(pts) { (pts / 72.27) * 25.4 }
  dfr_txt <- data.frame(dfr_xy, size=pt2mm(entryInfo[, "size"]), 
                        entryInfo[, c("text", "textspec", "color", "alpha", 
                                      "family", "fontface", "lineheight", 
                                      "angle")], 
                        stringsAsFactors=FALSE)
  dfr_txt$orientation <- with(dfr_txt, 
                              ifelse(angle == 0, "upright", 
                                ifelse(angle == 90, "left-rotated", 
                                  ifelse(angle == 180, "inverted", 
                                         "right-rotated"))))
  # 'geom_textbox' treats all text as markdown.  Therefore in wrapped plain 
  # text entries, convert newlines to '<br>' tags.  (Same in 'entrySize_mm')
  chg <- (sz$wrap & dfr_txt[, "textspec"] == "plain")
  dfr_txt[chg, "text"] <- gsub("\\n", "<br>", dfr_txt[chg, "text"])
  width <- ifelse(dfr_txt$angle %in% c(0, 180), sz$hsize, sz$vsize)
  for (i in which(sz$wrap)) {
    # Text wrapping has to be done one entry at a time.
    plt <- plt + ggtext::geom_textbox(aes(x=x, y=y, hjust=gg_hjust, 
                                  vjust=gg_vjust, halign=gg_hjust, 
                                  size=size, alpha=alpha, 
                                  orientation=orientation, colour=color, 
                                  family=family, fontface=fontface, 
                                  lineheight=lineheight, label=text, 
                                  fill=NA, box.colour=NA), 
                               box.padding=unit(c(0, 0, 0, 0), "mm"), 
                               box.margin=unit(c(0, 0, 0, 0), "mm"), 
                               box.r=unit(0, "mm"), 
                               width=unit(width[i], "mm"), 
                               data=dfr_txt[i, , drop=FALSE], 
                               show.legend=FALSE)
  }
  dfr_txt <- dfr_txt[!sz$wrap, , drop=FALSE] 
  if (any(use <- (dfr_txt[, "textspec"] == "plotmath"))) {
    plt <- plt + geom_text(aes(x=x, y=y, hjust=gg_hjust, vjust=gg_vjust, 
                               size=size, 
                               alpha=alpha, angle=angle, colour=color, 
                               family=family, fontface=fontface, 
                               lineheight=lineheight, label=text), 
                           data=dfr_txt[use, , drop=FALSE], parse=TRUE, 
                           show.legend=FALSE)
  }
  if (any(use <- (dfr_txt[, "textspec"] == "plain"))) {
    plt <- plt + geom_text(aes(x=x, y=y, hjust=gg_hjust, vjust=gg_vjust, 
                               size=size, 
                               alpha=alpha, angle=angle, colour=color, 
                               family=family, fontface=fontface, 
                               lineheight=lineheight, label=text), 
                           data=dfr_txt[use, , drop=FALSE], parse=FALSE, 
                           show.legend=FALSE)
  }
  if (any(use <- (dfr_txt[, "textspec"] == "markdown"))) {
    plt <- plt + ggtext::geom_richtext(aes(x=x, y=y, hjust=gg_hjust, 
                                   vjust=gg_vjust, size=size, 
                                   alpha=alpha, angle=angle, colour=color, 
                                   family=family, fontface=fontface, 
                                   lineheight=lineheight, label=text, 
                                   fill=NA, label.colour=NA), 
                               label.padding=unit(c(0, 0, 0, 0), "mm"), 
                               label.margin=unit(c(0, 0, 0, 0), "mm"), 
                               label.r=unit(0, "mm"), 
                               data=dfr_txt[use, , drop=FALSE], 
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
    yminr <- pmax(0, yy - (hruleInfo[, "space"] / 2))
    ymaxr <- pmin(maxY, yy + (hruleInfo[, "space"] / 2))
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
    xminr <- pmax(0, xx - (vruleInfo[, "space"] / 2))
    xmaxr <- pmin(maxX, xx + (vruleInfo[, "space"] / 2))
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
  attr(plt, "size_mm") <- size_mm
  attr(plt, "colBoundaries") <- xmax
  attr(plt, "rowBoundaries") <- ymax
  attr(plt, "prTable") <- x
  class(plt) <- c("pltdTable", class(plt))
  plt
}

