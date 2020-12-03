#===== Source file: ../prBlocks.r on 2020-11-29
#-----

prBlocks <- function(x, style=tablesggOpt("blockStyle"), scale=1.0)
{
  x <- as.tblBlocks(x)  # validity checks
  # All blocks start out as disabled (not to be highlighted visually).
  x[, "enabled"] <- rep(FALSE, nrow(x))
  xattr <- attributes(x)
  
  if (!inherits(style, "styleObj"))  stop("'style' is not a 'styleObj' object")
  if ((chk <- attr(style, "element_type")) != "block")  stop(
    "'style' is not a style for blocks, its 'element_type' is '", chk, "'")
  x <- apply_style(x, style=style, replace=TRUE, scale=scale, 
                   setEnabled=FALSE, unstyled="base", 
                   base_style=tablesgg::styles_pkg$blockStyle_pkg_base)

  x <- structure(x, row.names=x[, "id"], current_scale=scale, style=style, 
                 rowheadInside=xattr[["rowheadInside"]], 
                 rowgroupSize=xattr[["rowgroupSize"]])
  class(x) <- c("prBlocks", "tblBlocks", "data.frame")
  x
}

