#===== Source file: ../textTable_util.r on 2020-11-29
#-----

'[.textTable' <- function(x, i, j, drop=FALSE)
{
  x <- textTable(x)  # makes sure 'x' is valid and complete
  nrnc <- adim(x)
  partdim <- x$partdim
  partnames <- c("title", "subtitle", "rowhead", "rowheadLabels", "colhead", 
                 "body", "foot")
  stopifnot(identical(partnames, rownames(partdim)))  # internal consistency chk
  # Annotation parts are vectors, other parts are matrices.
  ann <- c("title", "subtitle", "foot")
  mats <- c("body", "rowhead", "colhead", "rowheadLabels")

  x <- unclass(x)  # Want to treat it as a bare list (e.g., avoid recusively
                   # calling methods for '[').

  # Put indices in positive integer form.
  if (missing(i)) {
    i <- seq_len(nrnc[1])
  } else if (is.matrix(i)) {
    stop("A matrix cannot be used to index a 'textTable' object")
  } else if (is.logical(i)) {
    i <- which(rep(i, length.out=nrnc[1]))
  } else if (any(i < 0))  i <- seq_len(nrnc[1])[i]
  if (missing(j)) {
    j <- seq_len(nrnc[2])
  } else if (is.logical(j)) {
    j <- which(rep(j, length.out=nrnc[2]))
  } else if (any(j < 0))  j <- seq_len(nrnc[2])[j]
  
  stopifnot(length(numeric(0) + 1) == 0)  # code assumes empty + scalar = empty
  
  # Row indexing.
  for (partnm in ann) {  # vector components
    part <- x[[partnm]]
    if (length(part) == 0)  next
    just <- attr(part, "justification")
    arow1 <- partdim[partnm, "arow1"]
    arow2 <- partdim[partnm, "arow2"]
    idx <- i[i >= arow1 & i <= arow2] - (arow1 - 1)
    part <- part[idx]
    attr(part, "justification") <- just[idx]
    x[[partnm]] <- part
  }
  for (partnm in mats) {  # matrix components
    if (partdim[partnm, "nr"] == 0)  next  # part has no rows
    part <- x[[partnm]]
    just <- attr(part, "justification")
    type <- attr(part, "type")  # NULL for 'rowheadLabels'
    arow1 <- partdim[partnm, "arow1"]
    arow2 <- partdim[partnm, "arow2"]
    idx <- i[i >= arow1 & i <= arow2] - (arow1 - 1)
    part <- part[idx, , drop=FALSE]
    attr(part, "justification") <- just[idx, , drop=FALSE]
    if (partnm == "body") {
      attr(part, "type") <- type[idx, , drop=FALSE]
    } else if (partnm == "colhead") {
      attr(part, "type") <- type[idx]
    }
    x[[partnm]] <- part
  }

  # Column indexing.
  for (partnm in mats) {  # matrix components
    if (partdim[partnm, "nc"] == 0)  next  # part has no columns
    part <- x[[partnm]]
    just <- attr(part, "justification")
    type <- attr(part, "type")  # NULL for 'rowheadLabels'
    acol1 <- partdim[partnm, "acol1"]
    acol2 <- partdim[partnm, "acol2"]
    idx <- j[j >= acol1 & j <= acol2] - (acol1 - 1)
    part <- part[, idx, drop=FALSE]
    attr(part, "justification") <- just[, idx, drop=FALSE]
    if (partnm == "body") {
      attr(part, "type") <- type[, idx, drop=FALSE]
    } else if (partnm == "rowhead") {
      attr(part, "type") <- type[idx]
    }
    x[[partnm]] <- part
  }
    
  # If the table is now empty (no body, no headers, no stub), remove any 
  # annotation.
  if (all(lengths(x[c("body", "rowhead", "colhead", "rowheadLabels")]) == 0)) {
    x[c("title", "subtitle", "foot")] <- rep(list(character(0)), 3)
  }
  
  textTable(x)  # validity checking, recreate 'partdim', 'rowhier', 'colhier'
}

#-----

fill_NA_idx <- function(y)
{
  n <- length(y)
  idx <- which(!is.na(y))
  if (length(idx) == 0 || length(idx) == n)  return(seq_len(n))
  start <- idx[1]  # position of first non-missing element of y
  temp <- rep(0, n)
  temp[idx] <- idx
  locf.idx <- c(seq_len(start-1), cummax(temp)[start:n])
  locf.idx
}

