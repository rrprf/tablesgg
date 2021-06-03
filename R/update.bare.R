#===== Source file: ../update.r on 2021-06-02
#-----

update.prObj <- function(object, style=NULL, scale=NULL, ...)
{
  chkDots(...)
  if (inherits(object, "prEntries")) {
    element_type <- "entry"
    base_style <- styles_pkg$entryStyle_pkg_base
  } else if (inherits(object, "prHvrules")) {
    element_type <- "hvrule"
    base_style <- styles_pkg$hvruleStyle_pkg_base
  } else if (inherits(object, "prBlocks")) {
    element_type <- "block"
    base_style <- styles_pkg$blockStyle_pkg_base
  } else  stop("Invalid 'object' argument")
  
  if (element_type == "hvrule" && !is.null(style))  stop(
      "Cannot update the style of an existing 'prHvrules' object; ", 
      "recreate it from a 'tblBlocks' object")
  xattr <- attributes(object)
  current_scale <- xattr[["current_scale"]]

  if (!is.null(style)) {  
    if (!inherits(style, "styleObj"))  stop("'style' is not a 'styleObj' object")
    if ((chk <- attr(style, "element_type")) != element_type)  stop(
      "Element type for 'style' (", chk, ") does not match 'object' (", 
      element_type, ")")
    use <- !(object[, "style_row"] %in% 0)  # NA is not selected
    object[use, ] <- apply_style(object[use, , drop=FALSE], style=style, 
                                 replace=TRUE, setEnabled=FALSE, 
                                 unstyled="base", base_style=base_style)
    object[use, ] <- apply_scale(object[use, , drop=FALSE], type=element_type, 
                                 scale=current_scale)
    xattr[["style"]] <- style
  }
  
  if (!is.null(scale)) {
    scale_factor <- scale / current_scale
    object <- apply_scale(object, type=element_type, scale=scale_factor)
    xattr[["current_scale"]] <- scale
  }
  
  attributes(object) <- xattr  # including class
  object
}

#-----

update.prTable <- function(object, entryStyle=NULL, blockStyle=NULL, 
                            hvruleStyle=NULL, scale=NULL, ...)
{
  chkDots(...)
  entries <- object$entries
  blocks <- object$blocks
  hvrules <- object$hvrules
  
  current_scale <- attr(object, "current_scale")
  if (attr(entries, "current_scale") != current_scale[1] || 
      (!is.null(hvrules) && attr(hvrules, "current_scale") != 
                              current_scale[2]))  stop(
    "'current_scale' attributes within 'object' are inconsistent")
  # (Add more validity checks on 'object'?)
  
  if (is.null(scale)) {
    scale1 <- current_scale[1]
    scale2 <- current_scale[2]
  } else {
    scale <- rep(scale, length.out=2)
    scale1 <- scale[1]
    scale2 <- scale[2]
  }
  
  if (!is.null(entryStyle) || !is.null(scale)) {
    entries <- update.prObj(entries, style=entryStyle, scale=scale1)
  }

  if (!is.null(blockStyle) || !is.null(scale)) {
    blocks <- update.prObj(blocks, style=blockStyle, scale=scale1)
  }
  
  if (!is.null(scale))  hvrules <- update.prObj(hvrules, scale=scale2)
  if (!is.null(hvruleStyle)) {
    # Generate new hvrules from 'blocks'.
    hvrules2 <- prHvrules(blocks, style=hvruleStyle, scale=scale2)
    # Add the new hvrules to 'hvrules', replacing any with the same ID.  Any 
    # hvrules that were added to the original 'hvrules' manually will be 
    # retained.
    keep <- !(hvrules[, "id"] %in% hvrules2[, "id"])
    hvrules <- structure(rbind(hvrules2, hvrules[keep, , drop=FALSE]), 
                         current_scale=scale2, style=hvruleStyle)
    hvrules <- as.prHvrules(hvrules)
  }
  
  prTable.prEntries(entries=entries, blocks=blocks, hvrules=hvrules)
}

#-----

update.pltdTable <- function(object, entryStyle=NULL, blockStyle=NULL, 
                             hvruleStyle=NULL, scale=NULL, 
                             plot.margin=attr(object, "plot.margin"), 
                             sizeAdjust=attr(object, "sizeAdjust"), ...)
{
  if (any(c("title", "subtitle", "foot") %in% names(list(...))))  stop(
    "Annotation cannot be updated in a plotted table; update or re-plot ", 
    "a 'textTable' instead")
  chkDots(...)
  force(plot.margin)  # need to evaluate defaults before 'object' is changed
  force(sizeAdjust)
  object <- update(prTable(object), entryStyle=entryStyle, 
                   blockStyle=blockStyle, hvruleStyle=hvruleStyle, 
                   scale=scale)
  plot(object, plot.margin=plot.margin, sizeAdjust=sizeAdjust)
}

#-----

update.textTable <- function(object, title=NULL, subtitle=NULL, foot=NULL, 
                             rowheadLabels=NULL, ...)
{
  chkDots(...)
  
  # Update annotation and rowheadLabels.
  if (!is.null(title))  object$title <- title
  if (!is.null(subtitle))  object$subtitle <- subtitle
  if (!is.null(foot))  object$foot <- foot
  if (!is.null(rowheadLabels)) {
    if (length(rowheadLabels) > 0) {
      object$rowheadLabels <- matrix(rowheadLabels, nrow=1)
    } else {
      object$rowheadLabels <- matrix(rowheadLabels, nrow=0)
    }
  }
  
  # Finish processing and validity checking.
  textTable.default(object)
}

