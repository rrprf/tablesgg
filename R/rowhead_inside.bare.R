#===== Source file: ../rowhead_inside.r on 2020-11-29
#-----

rowhead_inside <- function(x)
{
  x <- as.tblEntries(x)  # includes validity checks
  if (isTRUE(attr(x, "rowheadInside")))  return(x)
  partinfo <- tblParts(x)
  tbldim <- adim(x)
  n_rowhead <- partinfo["rowhead", "nc"]
  nc_body <- partinfo["body", "nc"]
  # Edge case:  no row headers to move inside.
  if (n_rowhead == 0)  return(x)
  
  # Get the entries for the outermost row header layer.
  rh1 <- with(x, part == "rowhead" & headlayer == n_rowhead)
  rh1lbl <- with(x, part == "rowheadLabels" & headlayer == n_rowhead)
  rh1entries <- x[rh1, , drop=FALSE]
  otherentries <- x[!(rh1 | rh1lbl), , drop=FALSE]
  # Consistency check:
  nlvls <- nrow(rh1entries)
  lil <- rh1entries$level_in_layer
  if (max(lil) != nlvls)  stop(
    "Number of entries for the outermost row header (", nlvls, ") does not ", 
    "match 'level_in_layer' max (", max(lil), ")")

  # Modify the rowheaderLabel entry for the header layer that will be 
  # moved.  May be useful for improving row group labels in the table.
  x[rh1lbl, "headlayer"] <- 0
  x[rh1lbl, "enabled"] <- FALSE
  
  if (n_rowhead == 1 && nc_body == 0) {
    # Edge case:  the row headers to be moved are the only non-annotation 
    # entries in the table.  No shifting of rows or columns is needed.
  } else {
    rh1entries <- rh1entries[order(lil), , drop=FALSE]
    # Start of each level (a new row will be inserted above each).
    grpstart <- rh1entries$arow1
    # Map original row numbers to new row numbers after inserting rows for labels.
    orig_arow <- seq_len(tbldim[1])
    incr <- cumsum(ifelse(orig_arow %in% grpstart, 1, 0))
    new_arow <- orig_arow + incr
    otherentries$arow1 <- new_arow[otherentries$arow1]
    otherentries$arow2 <- new_arow[otherentries$arow2]
    # Row numbers for moved header entries are the inserted row numbers.
    inserted_arow <- grpstart + incr[grpstart] - 1
    stopifnot(isTRUE(all.equal(sort(c(new_arow, inserted_arow)), 
                               seq_len(tbldim[1] + nlvls))))  # logic error
    rh1entries$arow1 <- inserted_arow
    rh1entries$arow2 <- inserted_arow
    rh1entries$acol1 <- rep(1, nlvls)
    rh1entries$acol2 <- max(1, tbldim[2] - 1)  # span all remaining columns
    
    # Shift column numbers left, because a column has been removed.
    otherentries$acol1 <- pmax(1, otherentries$acol1 - 1)
    otherentries$acol2 <- pmax(1, otherentries$acol2 - 1)
    
    x[!(rh1 | rh1lbl), ] <- otherentries
  }
  rh1entries$headlayer <- 0
  rh1entries$multirow <- FALSE
  rh1entries$multicolumn <- with(rh1entries, acol2 - acol1 > 0)
  x[rh1, ] <- rh1entries

  x <- structure(x[, , drop=FALSE], rowheadInside=TRUE)
  as.tblEntries(x)
}

#-----

undo_rowhead_inside <- function(x)
{
  if (!isTRUE(attr(x, "rowheadInside")))  return(x)
  chk <- setdiff(c("id", "part", "headlayer", "multirow", "multicolumn", 
                   "enabled", "arow1", "arow2", "acol1", "acol2"), 
                 names(x))
  if (length(chk) > 0)  stop(
    "Following columns are missing from 'x': ", toString(chk))
  tbldim <- adim(x)
  body <- with(x, part == "body")
  rh <- with(x, part == "rowhead")
  rh1 <- with(x, part == "rowhead" & headlayer == 0)
  rh1lbl <- with(x, part == "rowheadLabels" & headlayer == 0)
  stopifnot(any(rh1))  # inconsistent with rowheadInside check above
  rh1entries <- x[rh1, , drop=FALSE]
  otherentries <- x[!(rh1 | rh1lbl), , drop=FALSE]
  n_rowhead <- max(x[rh, "headlayer"]) + 1
  if (n_rowhead < 2 && sum(body) == 0) {
    # Edge case:  the row headers to be moved are the only non-annotation 
    # entries in the table.  No shifting of rows or columns is needed.  
  } else {
    nlvls <- sum(rh1)
    rh1arow <- sort(x[rh1, "arow1"])  # row numbers to be removed
    endarow <- max(x[rh | body, "arow2"])
    grpsize <- c(diff(rh1arow) - 1, endarow - rh1arow[nlvls])
    
    # Map original row numbers to new row numbers after removing rows for labels.
    orig_arow <- seq_len(tbldim[1])
    decr <- cumsum(ifelse(orig_arow %in% (rh1arow+1), 1, 0))
    new_arow <- orig_arow - decr
    otherentries$arow1 <- new_arow[otherentries$arow1]
    otherentries$arow2 <- new_arow[otherentries$arow2]
    if (attr(x, "mergeRuns")[1]) {
      rh1entries$arow1 <- new_arow[rh1entries$arow1]
      rh1entries$arow2 <- rh1entries$arow1 + pmax(grpsize - 1, 0)
    } else {
      # Expand each entry into multiple entries based on number of rows per grp.
      expand <- rep(seq_len(nlvls), grpsize)
      old_id <- rh1entries$id[expand]
      start <- as.numeric(sapply(strsplit(old_id, ","), "[[", 2))
      rh1entries <- rh1entries[expand, , drop=FALSE]
      seq_in_level <- unlist(lapply(grpsize, seq_len), use.names=FALSE)
      rh1entries$arow1 <- new_arow[rh1entries$arow1] + seq_in_level - 1
      rh1entries$arow2 <- rh1entries$arow1
      rh1entries$id <- paste("rowhead", start + seq_in_level - 1, 1, sep=",")
    }
    
    # Shift column numbers right.
    chg_acol1 <- with(otherentries, !(part %in% c("title", "subtitle", "foot")))
    otherentries[chg_acol1, "acol1"] <- otherentries[chg_acol1, "acol1"] + 1
    otherentries[, "acol2"] <- otherentries[, "acol2"] + 1
    
    rh1entries[, "acol1"] <- 1
    rh1entries[, "acol2"] <- 1
    
    x[!(rh1 | rh1lbl), ] <- otherentries
  }
  rh1entries[, "headlayer"] <- n_rowhead
  rh1entries[, "multicolumn"] <- FALSE
  rh1entries[, "multirow"] <- with(rh1entries, arow2 > arow1)
  # Restore rowheadLabel for the moved row header.
  x[rh1lbl, "headlayer"] <- n_rowhead
  x[rh1lbl, "enabled"] <- (x[rh1lbl, "text"] != "")
  
  # Can't just plug 'rh1entries' back into original rows of 'x', because 
  # entries may have been added.
  x <- rbind(x[!rh1, , drop=FALSE], rh1entries)
  row.names(x) <- x$id
  x <- structure(x, rowheadInside=FALSE)
  as.tblEntries(x)
}

