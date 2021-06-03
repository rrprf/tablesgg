#===== Source file: ../tblParts.r on 2021-06-02
#-----

tblParts <- function(x)
{
  if (inherits(x, c("prTable", "pltdTable")))  x <- entries(x, enabledOnly=FALSE)
  
  # Initialize the result.
  parts <- c("title", "subtitle", "rowhead", "rowheadLabels", "colhead", 
             "body", "foot")  # order should be consistent w/ textTable.default
  mat <- matrix(NA_real_, nrow=length(parts), ncol=6, 
                dimnames=list(parts, c("nr", "nc", "arow1", "arow2", 
                                       "acol1", "acol2")))
  mat[, c("nr", "nc")] <- 0
  
  if (is.null(x))  return(mat)

  if (inherits(x, "textTable")) {
    stopifnot(identical(dimnames(mat), dimnames(x$partdim)))  # internal consistency chk
    mat <- x$partdim
  } else if (inherits(x, c("tblEntries", "prEntries")) && nrow(x) > 0) {
    # 'x' is a data frame of table entries.
    if (isTRUE(attr(x, "rowheadInside"))) {
      x <- undo_rowhead_inside(x)
    } else if (!isFALSE(attr(x, "rowheadInside")))  stop(
      "'rowheadInside' status of 'x' is unknown")
    chk <- setdiff(c("part", "arow1", "arow2", "acol1", "acol2"), names(x))
    if (length(chk) > 0)  stop(
      "Following columns are missing from 'x': ", toString(chk))
    chk <- setdiff(unique(x$part), parts)
    if (length(chk) > 0)  stop(
      "Unrecognized table part(s) in 'x': ", toString(chk, width=40))
    # Infer dimensions of each table part from arow/acol values of its entries.
    # NA's are not allowed for entries.
    if (any(chk <- is.na(x$arow1 + x$arow2 + x$acol1 + x$acol2)))  stop(
      "NA found in arow1/2 or acol1/2 for entries: ", 
      toString(which(chk), width=40))
    # Dimensions of whole table and each part:
    nr_tbl <- max(x$arow2)
    nc_tbl <- max(x$acol2)
    stopifnot(nr_tbl > 0, nc_tbl > 0)
    for (part in parts) {
      dfr <- x[x$part == part, , drop=FALSE]
      if (nrow(dfr) > 0) {
        mat[part, c("arow1", "arow2", "acol1", "acol2")] <- 
            c(min(dfr$arow1), max(dfr$arow2), min(dfr$acol1), max(dfr$acol2))
        # These dimensions are tentative, since consistency may force changes 
        # to arow1/2, acol1/2 below:
        mat[part, "nr"] <- mat[part, "arow2"] - mat[part, "arow1"] + 1
        mat[part, "nc"] <- mat[part, "acol2"] - mat[part, "acol1"] + 1
      } 
    }
    # Check that table parts do not overlap.
    f <- function(y, z) { is.na(y) | is.na(z) | y > z }
    chk1 <- outer(mat[, "arow1"], mat[, "arow2"], FUN=f)
    chk2 <- outer(mat[, "acol1"], mat[, "acol2"], FUN=f)
    ok <- (chk1 | t(chk1)) | (chk2 | t(chk2)) | (row(chk1) == col(chk1))
    if (any(!ok))  stop(
      "arow1/2 or acol1/2 for entries imply overlapping table parts")
      
    # Check that there is at least one entry in every row and column.  
    # (Otherwise reconstruction of table part dimensions is ambiguous.)
    rowcovrd <- rep(FALSE, nr_tbl)
    for (i in seq_len(nr_tbl)) rowcovrd[i] <- any(x$arow1 <= i & x$arow2 >= i)
    colcovrd <- rep(FALSE, nc_tbl)
    for (i in seq_len(nc_tbl)) colcovrd[i] <- any(x$acol1 <= i & x$acol2 >= i)
    if (any(!rowcovrd) || any(!colcovrd))  stop(
      "There are table rows/columns with no entries.  Can't reconstruct ", 
      "the dimensions of table parts")
    
    # Infer arow1/2 for each table part, working from the top down.
    if (mat["title", "nr"] > 0) {
      stopifnot(mat["title", "arow1"] == 1, 
                mat["title", "arow2"] == mat["title", "nr"])
    }
    if (mat["subtitle", "nr"] > 0) {
      stopifnot(mat["subtitle", "arow1"] == mat["title", "nr"] + 1, 
                mat["subtitle", "arow2"] == mat["title", "nr"] + 
                                            mat["subtitle", "nr"])
    }
    mat["colhead", "nr"] <- max(mat[c("colhead", "rowheadLabels"), "nr"])
    if (mat["colhead", "nr"] > 0) {
      mat["colhead", "arow1"] <- sum(mat[c("title", "subtitle"), "nr"]) + 1
      mat["colhead", "arow2"] <- mat["colhead", "arow1"] + 
                                 mat["colhead", "nr"] - 1
    }
    if (mat["rowheadLabels", "nr"] > 0) {
      mat["rowheadLabels", "arow2"] <- mat["colhead", "arow2"]
      mat["rowheadLabels", "arow1"] <- mat["rowheadLabels", "arow2"] - 
                                       mat["rowheadLabels", "nr"] + 1
    }
    mat[c("rowhead", "body"), "nr"] <- max(mat[c("rowhead", "body"), "nr"])
    if (mat["rowhead", "nr"] > 0) {
      mat[c("rowhead", "body"), "arow1"] <- 
                  sum(mat[c("title", "subtitle", "colhead"), "nr"]) + 1
      mat[c("rowhead", "body"), "arow2"] <- mat["rowhead", "arow1"] +  
                                            mat["rowhead", "nr"] - 1
    }
    if (mat["foot", "nr"] > 0) {
      mat["foot", "arow1"] <- 
           sum(mat[c("title", "subtitle", "colhead", "body"), "nr"]) + 1
      mat["foot", "arow2"] <- mat["foot", "arow1"] +  mat["foot", "nr"] - 1
    }
    stopifnot(max(mat[, "arow2"], na.rm=TRUE) == nr_tbl)
      
    # Infer acol1/2 for each table part, working from right to left. 
    # Annotation always spans all columns:
    mat[c("title", "subtitle", "foot"), "acol2"] <- nc_tbl  # here always > 0
    mat[c("title", "subtitle", "foot"), "acol1"] <- 1
    mat[c("body", "colhead"), "nc"] <- max(mat[c("body", "colhead"), "nc"])
    if (mat["body", "nc"] > 0) {
      mat[c("body", "colhead"), "acol2"] <- nc_tbl
      mat[c("body", "colhead"), "acol1"] <- mat["body", "acol2"] - 
                                            mat["body", "nc"] + 1
    }
    mat[c("rowhead", "rowheadLabels"), "nc"] <- 
        max(mat[c("rowhead", "rowheadLabels"), "nc"])
    if (mat["rowhead", "nc"] > 0) {
      mat[c("rowhead", "rowheadLabels"), "acol2"] <- nc_tbl - mat["body", "nc"]
      mat[c("rowhead", "rowheadLabels"), "acol1"] <- mat["rowhead", "acol2"] - 
                                                     mat["rowhead", "nc"] + 1
    }
    stopifnot(min(mat[, "acol1"], na.rm=TRUE) == 1)

    # Recompute nr, nc for table parts using updated arow/acol.
    nr <- mat[, "arow2"] - mat[, "arow1"] + 1
    mat[, "nr"] <- ifelse(is.na(nr), 0, nr)
    nc <- mat[, "acol2"] - mat[, "acol1"] + 1
    mat[, "nc"] <- ifelse(is.na(nc), 0, nc)

  } else if (!inherits(x, c("tblEntries", "prEntries")))  stop(
    "Invalid 'x' argument")
  
  mat[c("title", "subtitle", "foot"), "nc"] <- NA
  mat[parts, , drop=FALSE]
}

