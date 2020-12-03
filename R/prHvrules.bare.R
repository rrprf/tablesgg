#===== Source file: ../prHvrules.r on 2020-11-29
#-----

prHvrules <- function(x, style=tablesggOpt("hvruleStyle"), scale=1.0)
{
  x <- as.tblBlocks(x)  # validity checks
  
  if (!inherits(style, "styleObj"))  stop("'style' is not a 'styleObj' object")
  if ((chk <- attr(style, "element_type")) != "hvrule")  stop(
    "'style' is not a style for hvrules, its 'element_type' is '", chk, "'")
  # Remove empty blocks (containing no cells).
  keep <- with(x, nr > 0 & nc > 0)
  x <- x[keep, , drop=FALSE]
  # Generate and style hvrules for each side of each block in 'x'.
  if (nrow(x) == 0) {
    hvrules <- as.prHvrules(NULL)  # empty data frame
  } else {
    hvrules <- apply_style(x, style=style, replace=TRUE, scale=scale, 
                           setEnabled=TRUE, unstyled="base", 
                           base_style=tablesgg::styles_pkg$hvruleStyle_pkg_base)
    # Disable any hvrules associated with blocks that do not contain any 
    # enabled entries.
    if ("had_enabled_entries" %in% names(x)) {
      disable <- !x[hvrules[, "block"], "had_enabled_entries"]
      hvrules[disable, "enabled"] <- FALSE
    }
  }

  hvrules <- structure(hvrules, row.names=hvrules[, "id"], current_scale=scale, 
                       style=style)
  class(hvrules) <- c("prHvrules", "data.frame")
  hvrules
}

