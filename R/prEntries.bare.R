#===== Source file: ../prEntries.r on 2020-11-29
#-----

prEntries <- function(x, style=tablesggOpt("entryStyle"), scale=1.0)
{
  x <- as.tblEntries(x)  # validity checks
  props_replace <- NA  # don't want to overwrite 'hjust' inherited from 
                       # 'textTable' object
  xattr <- attributes(x)
  
  if (!inherits(style, "styleObj"))  stop("'style' is not a 'styleObj' object")
  if ((chk <- attr(style, "element_type")) != "entry")  stop(
    "'style' is not a style for entries, its 'element_type' is '", chk, "'")
  x <- apply_style(x, style=style, replace=props_replace, scale=scale, 
                   setEnabled=FALSE, unstyled="base", 
                   base_style=tablesgg::styles_pkg$entryStyle_pkg_base)
  
  x <- structure(x, row.names=x[, "id"], current_scale=scale, style=style, 
                 mergeRuns=xattr[["mergeRuns"]], 
                 rowheadInside=xattr[["rowheadInside"]], 
                 rowhier=xattr[["rowhier"]], colhier=xattr[["colhier"]])
  class(x) <- c("prEntries", "tblEntries", "data.frame")
  x
}

