#===== Source file: ../entrySize_mm.r on 2021-06-02
#-----

entrySize_mm <- function(entryInfo, allowWrap, sizeAdjust=c(1, 1))
{
  n <- nrow(entryInfo)
  hsize <- rep(NA_real_, n)
  vsize <- rep(NA_real_, n)
  descender <- rep(0, n)
  wrap <- rep(FALSE, n)
  textspec <- entryInfo[, "textspec"]
  angle <- entryInfo[, "angle"]
  orient <- ifelse(angle == 0, "upright", 
              ifelse(angle == 90, "left-rotated", 
                ifelse(angle == 180, "inverted", "right-rotated")))
  horiz <- (orient %in% c("upright", "inverted"))
  if (allowWrap) {
    # Convert 'maxwidth' from table level (which includes padding and size 
    # adjustment) to just the width of the text itself.
    wdpad <- ifelse(horiz, entryInfo[, "hpad"], entryInfo[, "vpad"])
    wdAdj <- ifelse(horiz, sizeAdjust[1], sizeAdjust[2])
    maxwidth <- (entryInfo[, "maxwidth"] - 2*wdpad) / wdAdj  # may be NA
  }
  
  # If no graphics device is already open, the 'grid::convert*' functions 
  # will open one.  Rather than letting a default window be opened on screen, 
  # silently open a temporary PDF file and delete it at the end.
  if (n > 0) {
    devcur <- dev.cur()
    grfile <- paste0(tempfile(), ".pdf")
    pdf(grfile)
    on.exit({dev.off();  unlink(grfile);  if (devcur > 1)  dev.set(devcur) })
  }
  
  # Identify groups of entries that share the same viewport parameters.
  vpparm <- paste(entryInfo[, "size"], entryInfo[, "family"], 
                  entryInfo[, "fontface"], entryInfo[, "lineheight"], 
                  sep="\t")
  esets <- split(seq_len(n), factor(vpparm), drop=TRUE)
  
  for (i in seq_along(esets)) {
    idx <- esets[[i]]
    idx1 <- idx[1]
    vp <- grid::viewport(gp=grid::gpar(fontsize=entryInfo[idx1, "size"], 
                                       fontfamily=entryInfo[idx1, "family"], 
                                       fontface=entryInfo[idx1, "fontface"], 
                                       lineheight=entryInfo[idx1, "lineheight"], 
                                       cex=1))
    grid::pushViewport(vp, recording=FALSE)
    # 'richtext_grob', 'textbox_grob' include descenders in their heights, 
    # whereas 'textGrob' does not.  Extra height due to descenders (based 
    # on gridtext:::font_info (v0.1.4)):
    descent_mm <- grid::convertHeight(
                    grid::grobDescent(
                      grid::textGrob(label = "gjpqyQ")), 
                    "mm", valueOnly = TRUE)
    # Further split entries by their text spec.
    ijdx <- idx[textspec[idx] == "markdown"]
    for (j in ijdx) {  # these need to be processed one by one
      grobj <- gridtext::richtext_grob(text=entryInfo[j, "text"], 
                                       rot=entryInfo[j, "angle"], 
                                       margin=unit(rep(0, 4), "mm"), 
                                       padding=unit(rep(0, 4), "mm"), 
                                       r=unit(0, "mm"), 
                                       use_markdown=TRUE)
      hsize[j] <- grid::convertWidth(grid::unit(1, units="grobwidth", 
                                                data=grobj), 
                                     "mm", valueOnly=TRUE)
      vsize[j] <- grid::convertHeight(grid::unit(1, units="grobheight", 
                                                  data=grobj), 
                                       "mm", valueOnly=TRUE)
    }
    descender[ijdx] <- descent_mm
    ijdx <- idx[textspec[idx] %in% c("plain", "plotmath")]
    if ((nij <- length(ijdx)) > 0) {  # these can be processed together
      value <- as.list(entryInfo[ijdx, "text"])
      math <- (textspec[ijdx] == "plotmath")
      value[math] <- lapply(value[math], function(y) { parse(text=y) })
      # Could use 'grid::textGrob' similar to 'richtext_grob' above, but takes 
      # >50% longer than working directly with string lengths.
      w <- grid::convertWidth(grid::unit(rep(1, nij), units="strwidth", 
                                         data=value), 
                              "mm", valueOnly=TRUE)
      h <- grid::convertHeight(grid::unit(rep(1, nij), units="strheight", 
                                          data=value), 
                               "mm", valueOnly=TRUE)
      hsize[ijdx] <- ifelse(horiz[ijdx], w, h)
      vsize[ijdx] <- ifelse(horiz[ijdx], h, w)
      # For more general angles, adjust entries one by one:
      #  wh <- angle_adj(w, h, angle)
      #  hsize[i] <- wh[1]
      #  vsize[i] <- wh[2]
    }
    #----- Check whether any entries in this set need to be wrapped.
    if (allowWrap) {
      ntlwidth <- ifelse(horiz[idx], hsize[idx], vsize[idx])
      # Entries for which 'maxwidth' is NA are not included:
      ijdx <- idx[which(ntlwidth > maxwidth[idx] + 0.01)]  # ignore trivial diff
      if (length(ijdx) > 0 && !tablesggOpt("allowWrap"))  stop(
        "Width of the following entries is greater than their 'maxwidth', ", 
        "but 'allowWrap' option is FALSE: ", 
        toString(row.names(entryInfo)[ijdx], width=60))
      if (any(chk <- (textspec[ijdx] == "plotmath")))  stop(
        "Width of the following plotmath entries is greater than their ", 
        "'maxwidth', but plotmath cannot be wrapped: ", 
        toString(row.names(entryInfo)[ijdx[chk]], width=60))
      # 'geom_textbox' treats all text as markdown.  For consistency, need 
      # to have 'textbox_grob' also do that here.  Issues: (a) ordinary 
      # text may be misinterpreted as markdown tags; (b) text height may 
      # differ; (c) newlines in plain text will be ignored.  Fix the later 
      # problem here:
      ijdx2 <- ijdx[textspec[ijdx] == "plain"]
      entryInfo[ijdx2, "text"] <- gsub("\\n", "<br>", entryInfo[ijdx2, "text"])
      for (j in ijdx) {
        grobj <- gridtext::textbox_grob(text=entryInfo[j, "text"], 
                                        x=unit(0.5, "npc"), y=unit(0.5, "npc"), 
                                        width=unit(maxwidth[j], "mm"), 
                                        orientation=orient[j], 
                                        margin=unit(rep(0, 4), "mm"), 
                                        padding=unit(rep(0, 4), "mm"), 
                                        r=unit(0, "mm"), 
                                        # Need this since geom_textbox assumes 
                                        # it:
                                        use_markdown=TRUE)
        hsize[j] <- grid::convertWidth(grid::unit(1, units="grobwidth", 
                                                  data=grobj), 
                                       "mm", valueOnly=TRUE)
        vsize[j] <- grid::convertHeight(grid::unit(1, units="grobheight", 
                                                    data=grobj), 
                                         "mm", valueOnly=TRUE)
      }
      descender[ijdx] <- descent_mm
      wrap[ijdx] <- TRUE
    }
    grid::popViewport(recording=FALSE)
  }
  structure(data.frame(hsize=hsize, vsize=vsize, horiz=horiz, 
                       descender=descender, wrap=wrap, 
                       row.names=row.names(entryInfo), stringsAsFactors=FALSE), 
            device=names(dev.cur()))
}

