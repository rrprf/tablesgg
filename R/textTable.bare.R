#===== Source file: ../textTable.r on 2020-11-29
#-----

textTable <- function(x, ...)
{
  UseMethod("textTable")
}

#-----

textTable.default <- function(x, ...)
{
  chkDots(...)
  x <- unclass(x)  # Want to treat it as a bare list (e.g., avoid special
                   # methods for '[').

  partnames <- c("title", "subtitle", "rowhead", "rowheadLabels", "colhead", 
                 "body", "foot")  # order should be consistent w/ tblParts

  # Check component types.
  chk <- setdiff(partnames, names(x))
  if (length(chk) > 0)  stop(
    "Following table parts are missing from 'x': ", toString(chk))
  ann <- c("title", "subtitle", "foot")
  chk <- sapply(x[ann], function(y) { !is.character(y) || !is.null(dim(y)) })
  if (any(chk)) stop("Following annotation parts are not character vectors: ", 
    toString(ann[chk]))
  mats <- c("body", "rowhead", "colhead", "rowheadLabels")
  chk <- sapply(x[mats[1:3]], function(y) { !is.character(y) || !is.matrix(y) })
  if (any(chk)) stop("Following table parts are not character matrices: ", 
    toString(mats[chk]))
  if (length(x$rowheadLabels) == 0) {
    dim(x$rowheadLabels) <- c(0, ncol(x$rowhead))
  } else if (!is.matrix(x$rowheadLabels)) {
    dim(x$rowheadLabels) <- c(1, length(x$rowheadLabels))
  }
  if (!is.character(x$rowheadLabels) || nrow(x$rowheadLabels) > 1)  stop(
    "'rowheadLabels' is not a character matrix with at most one row")
  if (nrow(x$rowheadLabels) == 1 && nrow(x$colhead) == 0) {
      x$colhead <- matrix("", nrow=1, ncol=ncol(x$body))
  }
  
  # When a matrix part is empty its two dimensions are not uniquely defined.  
  # Resolve the ambiguity by using the smallest value for each dimension 
  # that is consistent with the non-empty matrix part(s).
  # The four matrix parts are quadrants of a rectangle, with dimensions 
  # determined by four degrees of freedom:  lengths north, south, east, 
  # west of the meeting point of the quadrants.  (Strictly, the northwest 
  # quadrant is the stub, of which rowheadLabels is the last row.)
  empty <- setNames(sapply(x[mats], function(y) { length(y) == 0 }), mats)
  east <- { if (empty["body"] && empty["colhead"])  0 
            else if (empty["body"])  ncol(x$colhead)
            else  ncol(x$body) }
  south <- { if (empty["body"] && empty["rowhead"])  0 
             else if (empty["body"])  nrow(x$rowhead)
             else  nrow(x$body) }
  west <- { if (empty["rowhead"] && empty["rowheadLabels"])  0 
            else if (empty["rowhead"])  ncol(x$rowheadLabels)
            else  ncol(x$rowhead) }
  north <- { if (empty["colhead"] && empty["rowheadLabels"])  0 
             else if (empty["colhead"])  nrow(x$rowheadLabels)
             else  nrow(x$colhead) }
  if (empty["body"])  dim(x$body) <- c(south, east)
  if (empty["rowhead"])  dim(x$rowhead) <- c(south, west)
  if (empty["colhead"])  dim(x$colhead) <- c(north, east)
  if (empty["rowheadLabels"])  dim(x$rowheadLabels) <- c(0, west)
  
  # Check dimensional consistency.
  if (nrow(x$rowhead) != nrow(x$body))  stop(
      "'rowhead' and 'body' have incompatible dimensions")
  if (ncol(x$colhead) != ncol(x$body))  stop(
      "'colhead' and 'body' have incompatible dimensions")
  if (ncol(x$rowhead) != ncol(x$rowheadLabels))  stop(
      "'rowhead' and 'rowheadLabels' have incompatible dimensions")
  if (all(lengths(x[mats]) == 0) && any(lengths(x[ann]) > 0)) {
    warning("The table has no body and no row/column headers.  ", 
            "Annotation will be removed")
    x[c("title", "subtitle", "foot")] <- rep(list(character(0)), 3)
  }

  # Check/add 'justification' attributes.
  for (i in partnames) {
    part <- x[[i]]
    just <- attr(part, "justification")
    if (length(just) == 0) {  # NULL or empty
      just <- rep(NA_character_, length(part))
      dim(just) <- dim(part)
      attr(x[[i]], "justification") <- just
    } else {
      just[] <- as.character(just)  #  might be logical if all NA
      if (length(just) != length(part) || 
          !identical(dim(just), dim(part)))  stop(
          "Invalid 'justification' attribute for table part: ", i)
      # Don't check _value_ of 'just' here:  wait until it needs to be 
      # converted to an 'hjust' value in 'tblEntries'.
    }
  }
  
  # Check/add 'type' attributes.
  type <- attr(x[["body"]], "type")
  if (length(type) == 0) {  # NULL or empty
    attr(x[["body"]], "type") <- array(NA_character_, dim(x[["body"]]))
  } else if (!is.character(type) || 
             !identical(dim(type), dim(x[["body"]]))) {
    stop("Invalid 'type' attribute for the table body.")
  }
  for (i in c("rowhead", "colhead")) {
    type <- attr(x[[i]], "type")
    reqd_len <- c("rowhead"=ncol(x[[i]]), "colhead"=nrow(x[[i]]))[i]
    if (is.null(type) || length(x[[i]]) == 0) {
      attr(x[[i]], "type") <- rep(NA_character_, reqd_len)
    } else if (!is.character(type) || length(type) != reqd_len) {
      stop("Invalid 'type' attribute for table part: ", i)
    }
  }
  
  # Remove names or dimnames from all components and from their 'justification' 
  # and 'type' attributes.
  for (i in partnames) {
    part <- unname(x[[i]])
    attr(part, "justification") <- unname(attr(x[[i]], "justification"))
    attr(part, "type") <- unname(attr(x[[i]], "type"))
    x[[i]] <- part
  }
  
  # Put components in standard order.
  x <- x[partnames]
  
  # Add dimensions of table parts and their position in the augmented 
  # row-column grid.
  mat <- matrix(NA_real_, nrow=length(partnames), ncol=6, 
                dimnames=list(partnames, c("nr", "nc", "arow1", "arow2", 
                                           "acol1", "acol2")))
  mat[, "nr"] <- c(length(x$title), length(x$subtitle), nrow(x$rowhead), 
                   nrow(x$rowheadLabels), nrow(x$colhead), nrow(x$body), 
                   length(x$foot))
  nc_tbl <- ncol(x$rowhead) + ncol(x$body)  # number of columns in whole table
  mat[, "nc"] <- c(nc_tbl, nc_tbl, ncol(x$rowhead), ncol(x$rowheadLabels), 
                   ncol(x$colhead), ncol(x$body), nc_tbl)
  mat[, "arow2"] <- c(mat[1, "nr"], sum(mat[1:2, "nr"]), 
                      sum(mat[c(1,2,5,6), "nr"]), sum(mat[c(1,2,5), "nr"]), 
                      sum(mat[c(1,2,5), "nr"]), sum(mat[c(1,2,5,6), "nr"]), 
                      sum(mat[c(1,2,5,6,7), "nr"]))
  mat[, "acol2"] <- c(nc_tbl, nc_tbl, mat[3, "nc"], mat[3, "nc"], 
                      nc_tbl, nc_tbl, nc_tbl)
  mat[, "arow1"] <- mat[, "arow2"] - mat[, "nr"] + 1
  mat[, "acol1"] <- mat[, "acol2"] - mat[, "nc"] + 1
  mat[mat[, "nr"] == 0, c("arow1", "arow2")] <- NA
  mat[mat[, "nc"] == 0, c("acol1", "acol2")] <- NA
  x$partdim <- mat
  
  # Add/update header hierarchy information.
  x$rowhier <- headerRuns(x$rowhead, which_head="row")
  x$colhier <- headerRuns(x$colhead, which_head="col")
  
  class(x) <- "textTable"
  x
}

