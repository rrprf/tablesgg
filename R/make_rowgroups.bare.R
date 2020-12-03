#===== Source file: ../make_rowgroups.r on 2020-11-29
#-----

make_rowgroups <- function(stdblks, rowheadruns, rowgroupSize)
{
  # Start with standard blocks, including for the row header hierarchy.
  d_rhb <- c(stdblks["rowhead_and_body", "nr"], 
             stdblks["rowhead_and_body", "nc"])
  emptyblks <- stdblks[0, , drop=FALSE]
  if (!is.finite(rowgroupSize) || rowgroupSize < 1 || 
      d_rhb[1] == 0 || d_rhb[2] == 0) {
    return(as.tblBlocks(emptyblks))
  }
  n_rhi <- sum(grepl("rowblock/A/0/", stdblks[, "id"]))
  rowheadInside <- (n_rhi > 0)
  if (rowheadInside)  stopifnot(n_rhi == nrow(rowheadruns[[1]]))
  n_rowhead <- length(rowheadruns)
  nc_rowhead <- n_rowhead - as.numeric(rowheadInside)
  nr_body <- d_rhb[1] - n_rhi  # not counting interior row header entries
  rhi_adds_rows <- (nr_body > 0)
  # All row group blocks extend through to right edge of table:
  acol2 <- d_rhb[2]
  
  rgSize <- rowgroupSize
  # Row header layers with runs of repeated values.
  hasruns <- { if (n_rowhead == 0)  numeric(0)
               else  which(sapply(rowheadruns, nrow) < nr_body) }
  if (length(hasruns) > 0) {
    # Find the innermost row header layer with runs of repeated values.
    i <- max(hasruns)
    layer <- n_rowhead - i + 1
    if (rowheadInside && layer == n_rowhead)  layer <- 0
    # Construct 'jrows', a two-column matrix with the arow1, arow2 values for 
    # each level within 'layer' (i.e., for each level within which rows will 
    # be grouped).
    runsi <- rowheadruns[[i]]
    nlvls <- nrow(runsi)
    # Extract arow*, acol1 values from 'rowblock/C' (not 'rowblock/A') blocks.
    blkids <- paste("rowblock/C", layer, seq_len(nlvls), sep="/")
    rblks <- stdblks[blkids, , drop=FALSE]
    jrows <- cbind(rblks[, "arow1"], rblks[, "arow2"])  # may be NA
    # 'rowgroup' block starts one column further inside than 'i':
    stopifnot(is.na(rblks[1, "acol1"]) || 
              all(rblks[, "acol1"] == rblks[1, "acol1"]))
    acol1 <- min(rblks[1, "acol1"] + 1, d_rhb[2])  # may be NA
  } else {
    # No row headers, or none have runs of repeated values.  
    layer <- NA_real_
    nlvls <- 1  # all rows will be grouped as if in a single level
    jrows <- matrix(c(stdblks["rowhead_and_body", "arow1"], 
                      stdblks["rowhead_and_body", "arow2"]), nrow=1)
    acol1 <- 1
    if (rowheadInside && rhi_adds_rows) {
      # Each logical table row takes two actual rows (one for row header 
      # entry), so need to double the effective 'rowgroupSize'.
      rgSize <- 2 * rowgroupSize
    }
  }
    
  #-- For each level in layer i, divide rows into groups and create a block 
  # for each group.
  grpblks <- emptyblks  # initialize
  for (j in seq_len(nlvls)) {
    nr <- jrows[j, 2] - jrows[j, 1] + 1  # total rows in level j
    if (is.na(nr) || nr == 0)  next  # this level has no rows
    grpstart <- seq(from=1, to=nr, by=rowgroupSize)
    grpend <- c(grpstart[-1] - 1, nr)
    arow1 <- jrows[j, 1] + (grpstart - 1)
    arow2 <- jrows[j, 1] + (grpend - 1)
    stopifnot(tail(arow2, 1) == jrows[j, 2])
    group_in_level <- seq_along(grpstart)
    if (is.na(layer)) {
      level_in_layer <- NA_real_
      id <- paste0("rowblock/G///", group_in_level)
    } else {
      level_in_layer <- j
      id <- paste("rowblock/G", layer, j, group_in_level, sep="/")
    }
    blks <- data.frame(id=id, type="rowblock", subtype="G", 
                       headlayer=layer, level_in_layer=level_in_layer, 
                       group_in_level=group_in_level, 
                       nr=arow2 - arow1 + 1, nc=acol2 - acol1 + 1, 
                       arow1=arow1, arow2=arow2, acol1=acol1, acol2=acol2, 
                       stringsAsFactors=FALSE)
    grpblks <- rbind(grpblks, blks)
  }
  
  row.names(grpblks) <- grpblks$id  
  attr(grpblks, "rowgroupSize") <- rowgroupSize
  as.tblBlocks(grpblks)  # includes validity checks
}

