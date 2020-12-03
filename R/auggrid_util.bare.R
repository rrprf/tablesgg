#===== Source file: ../auggrid_util.r on 2020-11-29
#-----

adim <- function(x)
{
  if (inherits(x, c("prTable", "pltdTable")))  x <- entries(x, enabledOnly=FALSE)
  if (inherits(x, "textTable")) {
    pts <- x$partdim  # 'nr', 'nc' may be 0
    nr_tbl <- sum(pts[c("title", "subtitle", "colhead", "body", "foot"), "nr"])
    nc_tbl <- sum(pts[c("rowhead", "body"), "nc"])
  } else if (inherits(x, c("tblEntries", "prEntries"))) {
    nr_tbl <- { if (nrow(x) == 0)  0
                else if (anyNA(x[, "arow2"]))  NA_real_ 
                else  max(x[, "arow2"]) }
    nc_tbl <- { if (nrow(x) == 0)  0
                else if (anyNA(x[, "acol2"]))  NA_real_ 
                else  max(x[, "acol2"]) }
  } else  stop("Invalid 'x' argument")
  
  c(nr_tbl, nc_tbl)
}

#-----

arow <- function(x, id=NULL, hpath=NULL)
{
  if (!inherits(x, c("textTable", "pltdTable")))  stop(
    "'x' is not a 'textTable' or plotted table ('pltdTable') object")
  if ((is.null(id) + is.null(hpath)) != 1)  stop(
    "Exactly one of args 'id' and 'hpath' must be set to a non-NULL value")
  partdim <- tblParts(x)
  
  # Function to merge sequences of numbers specified by vectors of starting 
  # and ending values.  NA in start or end gives numeric(0) for that sequence.
  mseq <- function(start, end) {  # vectors of starting and ending values
    lst <- vector("list", length(start))
    for (i in seq_along(start)) {
      lst[[i]] <- { if (is.na(start[i] + end[i]))  numeric(0) 
                    else  seq(from=start[i], to=end[i], by=1) }
    }
    vals <- unlist(lst, use.names=FALSE)
    if (length(vals) > 0)  sort(unique(vals))  else  numeric(0)
  }
  
  if (!is.null(id)) {
    if(length(id) != 1 || !is.character(id))  stop(
      "'id' is not a scalar character string")
    if (inherits(x, "textTable")) {
      if (!(id %in% rownames(partdim)))  stop(
        "ID '", id, "' does not match any table part")
      rslt <- mseq(partdim[id, "arow1"], partdim[id, "arow2"])
    } else {
      id_pts <- rownames(partdim)
      id_blk <- ids(x, type="block", enabledOnly=FALSE)
      id_ent <- ids(x, type="entry", enabledOnly=FALSE)
      id_hvr <- ids(x, type="hvrule", enabledOnly=FALSE)
      if (!is.na(match(id, id_pts))) {  # ID is a part name
        ent <- entries(x, enabledOnly=FALSE)
        temp <- ent[ent$part == id, , drop=FALSE]
        if (nrow(temp) == 0 && 
            id %in% c("rowhead", "rowheadLabels", "colhead", "body")) {
          # Empty parts may still span rows.
          rhi <- attr(ent, "rowheadInside")
          n_rowhead <- partdim["rowhead", "nc"]
          if (!rhi || id %in% c("colhead", "rowheadLabels") || n_rowhead == 0) {
            # Unaffected by rowheadInside
            temp <- data.frame(arow1=partdim[id, "arow1"], 
                               arow2=partdim[id, "arow2"], 
                               stringsAsFactors=FALSE)
          } else {  # rhi, rowhead is not empty, id=="body", and body is empty
            # Rows associated with (empty) body are those associated with 
            # row header layers not equal to 0.
            temp <- ent[ent$part == "rowhead" & ent$headlayer > 0, , drop=FALSE]
               # (might be empty, but that's OK:  will return numeric(0))
          }
        }
        idx <- seq_len(nrow(temp))
      } else if (!is.na(idx <- match(id, id_blk))) {
        temp <- blocks(x, enabledOnly=FALSE)
      } else if (!is.na(idx <- match(id, id_ent))) {
        temp <- entries(x, enabledOnly=FALSE)
      } else if (!is.na(idx <- match(id, id_hvr))) {
        temp <- hvrules(x, enabledOnly=FALSE)
      } else {
        stop("ID '", id, "' not found in table 'x'")
      }
      rslt <- mseq(temp[idx, "arow1"], temp[idx, "arow2"])
    }
  } else {  # use 'hpath'
    nlayer <- partdim["rowhead", "nc"]
    if (length(hpath) > nlayer)  stop(
      "Length of 'hpath' (", length(hpath), ") exceeds number of rowhead ", 
      "layers (", nlayer, ")")
    hpath <- c(hpath, rep(NA_character_, nlayer-length(hpath)))
    if (inherits(x, "textTable")) {
      mch <- sweep(x$rowhead, 2, as.array(hpath), 
                   FUN=function(y, z) { is.na(z) | y == z })
      rslt <- mseq(partdim["rowhead", "arow1"], 
                   partdim["rowhead", "arow2"])[apply(mch, 1, all)]
    } else {
      ent <- entries(x, enabledOnly=FALSE)
      temp <- ent[ent$part == "rowhead", , drop=FALSE]
      if (nrow(temp) > 0) {
        rhi <- attr(ent, "rowheadInside")
        if (rhi) {
          # Text of interior header labels may have been changed, so won't 
          # match 'hpath'.  Need to patch that.
          rowhier1 <- attr(ent, "rowhier")[[1]]  # data frame for outermost 
                                                 # layer, one row per level
          for (j in seq_len(nrow(rowhier1))) {
            idx <- with(temp, which(headlayer == 0 & level_in_layer == j))
            temp[idx, "text"] <- rowhier1[rowhier1$level_in_layer == j, "value"]
          }
        }
        # Re-create a 'rowhead' matrix from 'temp'.
        rowhead <- matrix(NA_character_, nrow=max(temp$partrow), 
                          ncol=max(temp$partcol))
        partrc <- cbind(temp$partrow, temp$partcol)
        rowhead[partrc] <- temp$text
        for (j in seq_len(nlayer)) {  # fill in NA's for runs
          rowhead[, j] <- rowhead[fill_NA_idx(rowhead[, j]), j]
        }
        # Find rows that match 'hpath'.
        mch <- sweep(rowhead, 2, as.array(hpath), 
                     FUN=function(y, z) { is.na(z) | (!is.na(y) & y == z) })
        partr <- which(apply(mch, 1, all))  # row numbers in 'rowhead'
        # Get 'arow1', 'arow2' values for entries at the innermost layer of 
        # the matching rows.
        ar <- temp[temp$partrow %in% partr & temp$headlayer == 1, 
                   c("arow1", "arow2")]
        # If there are interior row header labels, add those entries 
        # if all of 'hpath[-1]' is NA.
        if (rhi && all(is.na(hpath[-1]))) {
          ar <- rbind(ar, temp[temp$partrow %in% partr & temp$headlayer == 0, 
                               c("arow1", "arow2")])
        }
        rslt <- mseq(ar[, "arow1"], ar[, "arow2"])
      } else {
        # With no row header, and length(hpath) == 0, by definition all 
        # rowhead and body associated rows are selected.
        rslt <- mseq(partdim["rowhead", "arow1"], partdim["rowhead", "arow2"])
      }
    }
  }
  rslt
}

#-----

acol <- function(x, id=NULL, hpath=NULL)
{
  if (!inherits(x, c("textTable", "pltdTable")))  stop(
    "'x' is not a 'textTable' or plotted table ('pltdTable') object")
  if ((is.null(id) + is.null(hpath)) != 1)  stop(
    "Exactly one of args 'id' and 'hpath' must be set to a non-NULL value")
  partdim <- tblParts(x)
  
  # Function to merge sequences of numbers specified by vectors of starting 
  # and ending values.  NA in start or end gives numeric(0) for that sequence.
  mseq <- function(start, end) {  # vectors of starting and ending values
    lst <- vector("list", length(start))
    for (i in seq_along(start)) {
      lst[[i]] <- { if (is.na(start[i] + end[i]))  numeric(0) 
                    else  seq(from=start[i], to=end[i], by=1) }
    }
    vals <- unlist(lst, use.names=FALSE)
    if (length(vals) > 0)  sort(unique(vals))  else  numeric(0)
  }
  
  if (!is.null(id)) {
    if(length(id) != 1 || !is.character(id))  stop(
      "'id' is not a scalar character string")
    if (inherits(x, "textTable")) {
      if (!(id %in% rownames(partdim)))  stop(
        "ID '", id, "' does not match any table part")
      rslt <- mseq(partdim[id, "acol1"], partdim[id, "acol2"])
    } else {
      id_pts <- rownames(partdim)
      id_blk <- ids(x, type="block", enabledOnly=FALSE)
      id_ent <- ids(x, type="entry", enabledOnly=FALSE)
      id_hvr <- ids(x, type="hvrule", enabledOnly=FALSE)
      if (!is.na(idx <- match(id, id_pts))) {  # ID is a part name
        temp <- partdim
      } else if (!is.na(idx <- match(id, id_blk))) {
        temp <- blocks(x, enabledOnly=FALSE)
      } else if (!is.na(idx <- match(id, id_ent))) {
        temp <- entries(x, enabledOnly=FALSE)
      } else if (!is.na(idx <- match(id, id_hvr))) {
        temp <- hvrules(x, enabledOnly=FALSE)
      } else {
        stop("ID '", id, "' not found in table 'x'")
      }
      rslt <- mseq(temp[idx, "acol1"], temp[idx, "acol2"])
    }
  } else {  # use 'hpath'
    nlayer <- partdim["colhead", "nr"]
    if (length(hpath) > nlayer)  stop(
      "Length of 'hpath' (", length(hpath), ") exceeds number of colhead ", 
      "layers (", nlayer, ")")
    hpath <- c(hpath, rep(NA_character_, nlayer-length(hpath)))
    if (inherits(x, "textTable")) {
      mch <- sweep(x$colhead, 1, as.array(hpath), 
                   FUN=function(y, z) { is.na(z) | y == z })
      rslt <- mseq(partdim["colhead", "acol1"], 
                   partdim["colhead", "acol2"])[apply(mch, 2, all)]
    } else {
      ent <- entries(x, enabledOnly=FALSE)
      temp <- ent[ent$part == "colhead", , drop=FALSE]
      if (nrow(temp) > 0) {
        # Re-create a 'colhead' matrix from 'temp'.
        colhead <- matrix(NA_character_, nrow=max(temp$partrow), 
                          ncol=max(temp$partcol))
        partrc <- cbind(temp$partrow, temp$partcol)
        colhead[partrc] <- temp$text
        for (i in seq_len(nlayer)) {  # fill in NA's for runs
          colhead[i, ] <- colhead[i, fill_NA_idx(colhead[i, ])]
        }
        # Find rows that match 'hpath'.
        mch <- sweep(colhead, 1, as.array(hpath), 
                     FUN=function(y, z) { is.na(z) | (!is.na(y) & y == z) })
        partc <- which(apply(mch, 2, all))  # column numbers in 'colhead'
        # Get 'acol1', 'acol2' values for entries at the innermost layer of 
        # the matching columns.
        ar <- temp[temp$partcol %in% partc & temp$headlayer == 1, 
                   c("acol1", "acol2")]
        rslt <- mseq(ar[, "acol1"], ar[, "acol2"])
      } else {
        # With no column header, and length(hpath) == 0, by definition all 
        # colhead and body associated columns are selected.
        rslt <- mseq(partdim["colhead", "acol1"], partdim["colhead", "acol2"])
      }
    }
  }
  rslt
}

