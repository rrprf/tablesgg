#===== Source file: ../prTable.r on 2021-06-02
#-----

prTable <- function(x, ...)
{
  UseMethod("prTable")
}

#-----

prTable.prEntries <- function(x, entries=x, blocks=prBlocks(tblBlocks(x)), 
                              hvrules, ...)
{
  chkDots(...)
  if (!missing(x) && !missing(entries))  stop(
    "One of 'x' and 'entries' should be specified, but not both")

  entries <- as.prEntries(entries)
  scale1 <- attr(entries, "current_scale")
  adm <- adim(entries)

  blocks <- as.prBlocks(blocks)  # handles NULL
  chk <- with(blocks, nc*nr > 0)
  if (any(chk)) {
    blk_maxarow <- max(blocks[chk, "arow2"], na.rm=TRUE)
    blk_maxacol <- max(blocks[chk, "acol2"], na.rm=TRUE)
    if (blk_maxarow > adm[1] || blk_maxacol > adm[2]) stop(
      "Block dimensions (", blk_maxarow, ", ", blk_maxacol, ") exceed ", 
      "table dimensions (", adm[1], ", ", adm[2], ")")
  }
  hvrules <- { if (missing(hvrules))  prHvrules(blocks)
               else  as.prHvrules(hvrules) }  # handles NULL
  scale2 <- attr(hvrules, "current_scale")
  if (nrow(hvrules) > 0) {
    hvr_maxarow <- max(hvrules[, "arow2"], na.rm=TRUE)
    hvr_maxacol <- max(hvrules[, "acol2"], na.rm=TRUE)
    if (hvr_maxarow > adm[1] + 0.5 || hvr_maxacol > adm[2] + 0.5) stop(
      "hvrule dimensions (", hvr_maxarow, ", ", hvr_maxacol, ") exceed ", 
      "table dimensions (", adm[1], ", ", adm[2], ")")
  }
  
  structure(list(entries=entries, blocks=blocks, hvrules=hvrules), 
            current_scale=c(scale1, scale2), class="prTable")
}

#-----

prTable.pltdTable <- function(x, ...)
{
  chkDots(...)
  attr(x, "prTable")
}

