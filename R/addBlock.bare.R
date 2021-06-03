#===== Source file: ../addBlock.r on 2021-06-02
#-----

addBlock <- function(x, arows, acols, id, props=NULL, enabled=FALSE)
{
  if (inherits(x, c("pltdTable", "prTable"))) {
    blocks <- blocks(x, enabledOnly=FALSE)
  } else {
    stop("'x' is not a plotted table ('pltdTable' object)")
  }
  if (missing(id)) {
    nextnum <- nrow(blocks) + 1
    while ((id <- paste0("block", nextnum)) %in% blocks[, "id"]) {
      nextnum <- nextnum + 1
    }
  } else if (!is.character(id) || length(id) != 1 || is.na(id))  stop(
    "'id' is not a character scalar")
  if (id %in% blocks[, "id"])  stop(
    "A block with 'id' ", id, " is already present in 'x'")
  arows <- range(arows)
  acols <- range(acols)

  # Create a new row for 'blocks', initially filled with NAs.
  if (nrow(blocks) > 0) {
    newrow <- blocks[1, , drop=FALSE]
    for (i in names(newrow))  newrow[1, i] <- NA
  } else {
    newrow <- as.list(blocks)
    for (i in names(newrow))  newrow[[i]] <- rep(newrow[[i]], length.out=1)
    newrow <- data.frame(newrow, stringsAsFactors=FALSE)
  }
  
  newrow[1, "id"] <- id
  newrow[1, "nr"] <- arows[2] - arows[1] + 1
  newrow[1, "nc"] <- acols[2] - acols[1] + 1
  newrow[1, "arow1"] <- arows[1]
  newrow[1, "arow2"] <- arows[2]
  newrow[1, "acol1"] <- acols[1]
  newrow[1, "acol2"] <- acols[2]
  newrow[1, "enabled"] <- enabled
  propnms <- row.names(grSpecs("block"))
  newrow[1, propnms] <- styles_pkg$blockStyle_pkg_base[1, propnms]
  newrow[1, "style_row"] <- 0  # so graphical props are not chgd by restyling

  newblocks <- structure(as.prBlocks(rbind(blocks, newrow)), 
                         current_scale=attr(blocks, "current_scale"), 
                         style=attr(blocks, "style"), 
                         rowgroupSize=attr(blocks, "rowgroupSize"), 
                         rowheadInside=attr(blocks, "rowheadInside"))
  blocks(x) <- newblocks
  if (!is.null(props)) {
    if (!inherits(props, "element_block"))  stop(
      "'props' is not an 'element_block' object")
    props(x, id=id, setEnabled=FALSE) <- props
  }
  x
}

