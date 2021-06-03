#===== Source file: ../textTable_methods.r on 2021-06-02
#-----

textTable.matrix <- function(x, rcnames=c(TRUE, TRUE), title=character(0), 
                             subtitle=character(0), foot=character(0), na="NA", 
                             ...)
{
  type <- typeof(x)
  body <- format(x, ...)
  nr <- nrow(body)
  nc <- ncol(body)
  if (length(na) > 0) {  # not NULL or character(0)
    body[is.na(x)] <- na
  }
  body <- structure(body, 
                    justification=array(NA_character_, dim=dim(body)),
                    type=array(type, dim=dim(body)))
  # (Leave decisions about horizontal justification to table style.)
  
  if (!(length(rcnames) == 2 && 
        (is.logical(rcnames) || is.character(rcnames))))  stop(
     "'rcnames' argument is not a logical or character vector of length 2")
  dn <- dimnames(x)
  dnn <- names(dn)
  dn1 <- dn[[1]]
  dn2 <- dn[[2]]
  dnn1 <- { if (is.character(rcnames))  rcnames[1]  else  dnn[1] }
  dnn2 <- { if (is.character(rcnames))  rcnames[2]  else  dnn[2] }
  
  if (isFALSE(rcnames[1]) || is.null(dn1)) {
    rowhead <- matrix(character(0), nrow=nr, ncol=0)
  } else {
    rowhead <- matrix(dn1, ncol=1)
    if (any(notANumber(rowhead))) {
      just <- matrix(NA_character_, nrow=nr, ncol=1)
      type <- "character"
    } else {
      just <- matrix("r", nrow=nr, ncol=1)
      type <- "numeric"
    }
    if (!is.null(dnn1) && dnn1 != "") {
      rowhead <- cbind(rep(dnn1, nr), rowhead)
      just <- cbind(rep(NA_character_, nr), just)
      type <- c("character", type)
    }
    rowhead <- structure(rowhead, justification=just, type=type)
  }
  rowheadLabels <- matrix(character(0), nrow=0, ncol=ncol(rowhead))
  
  if (isFALSE(rcnames[2]) || is.null(dn2)) {
    colhead <- matrix(character(0), nrow=0, ncol=nc)
  } else {
    colhead <- matrix(dn2, nrow=1)
    if (any(notANumber(colhead))) {
      just <- matrix(NA_character_, nrow=1, ncol=nc)
      type <- "character"
    } else {
      just <- matrix("r", nrow=1, ncol=nc)
      type <- "numeric"
    }
    if (!is.null(dnn2) && dnn2 != "") {
      colhead <- rbind(rep(dnn2, nc), colhead)
      just <- rbind(rep(NA_character_, nc), just)
      type <- c("character", type)
    }
    colhead <- structure(colhead, justification=just, type=type)
  }

  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)
  
  # Use 'textTable.default' to finish up processing and for validity checks.
  textTable(z)
}

#-----

textTable.data.frame <- function(x, title=character(0), subtitle=character(0), 
                                 foot=character(0), row.names="", na="NA", 
                                 ...)
{
  type <- vapply(x, function(y) { class(y)[1] }, character(1))
  body <- as.matrix(format(x, ...))  # logical matrix if 'x' has 0 rows, so ...
  body[] <- as.character(body)
  if (length(type) != ncol(body))  stop(
    "Number of variables in 'x' (", length(type), ") does not match ", 
    "the number of columns after formatting (", ncol(body), ")")
  # (Could happen if variables are not simple vectors, e.g., matrices.)
  if (length(na) > 0) {  # not NULL or character(0)
    body[is.na(x)] <- na
  }
  body <- structure(body, 
                    justification=array(NA_character_, dim=dim(body)),
                    type=array(rep(type, each=nrow(body)), dim=dim(body)))
  # (Leave decisions about horizontal justification to table style.)
  stopifnot(length(row.names) == 1)
  if (isFALSE(row.names)) {
    rowhead <- matrix(character(0), nrow=nrow(body), ncol=0)
    rowheadLabels <- matrix(character(0), nrow=0, ncol=0)
  } else {
    rowhead <- matrix(row.names(x), ncol=1)
    if (any(notANumber(rowhead))) {
      just <- NA_character_
      type <- "character"
    } else {
      just <- "r"
      type <- "numeric"
    }
    rowhead <- structure(rowhead, justification=array(just, dim=dim(rowhead)), 
                         type=rep(type, ncol(rowhead)))
    rowheadLabels <- { if (is.character(row.names))  row.names 
                       else  "row.names" }
    rowheadLabels <- structure(matrix(rowheadLabels, nrow=1), 
                               justification=matrix(just, nrow=1))
    if (rowheadLabels == "")  rowheadLabels <- matrix(character(0), nrow=0, 
                                                      ncol=1)
  }
  colhead <- matrix(names(x), nrow=1)
  colhead <- structure(colhead, 
                       justification=array(NA_character_, dim=dim(colhead)), 
                       type=rep("character", nrow(colhead)))
  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)
  
  # Use 'textTable.default' to finish up processing and for validity checks.
  textTable(z)
}

#-----

textTable.ftable <- function(x, colheadLabels=c("layers", "none", "paste"), 
                             sep=": ", title=character(0), subtitle=character(0), 
                             foot=character(0), ...)
{
  chkDots(...)
  colheadLabels <- match.arg(colheadLabels)
  d <- dim(x)  # dimensions of table body
  body <- array(as.character(as.vector(x)), dim=d)
  attr(body, "justification") <- array(NA_character_, dim=d)  # 'r' instead?
  attr(body, "type") <- array("numeric", dim=d)
  
  row.vars <- attr(x, "row.vars")  # list, one character vector per header var
  rowheadLabels <- matrix(names(row.vars), ncol=length(row.vars))
  # 'rev' needed twice b/c expand.grid varies first column fastest:
  rh <- rev(do.call(expand.grid, c(rev(row.vars), 
                                   list(stringsAsFactors=FALSE)))) # data frame
  rowhead <- as.matrix(rh)
  
  col.vars <- attr(x, "col.vars")  # list, one character vector per header var
  ch <- rev(do.call(expand.grid, c(rev(col.vars), 
                                   list(stringsAsFactors=FALSE)))) # data frame
  ch <- t(as.matrix(ch))
  if (colheadLabels == "layers" && length(col.vars) > 0) {
    chL <- matrix(rep(names(col.vars), ncol(ch)), ncol=ncol(ch))
    colhead <- t(matrix(rbind(t(chL), t(ch)), nrow=ncol(ch)))
  } else if (colheadLabels == "paste") {
    colhead <- ch
    for (i in seq_len(nrow(colhead))) {
      colhead[i, ] <- paste(names(col.vars)[i], ch[i, ], sep=sep)
    }
  } else  colhead <- ch
  
  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)
  textTable(z)
}

#-----

textTable.table <- function(x, colheadLabels=c("layers", "none", "paste"), 
                             sep=": ", title=character(0), subtitle=character(0), 
                             foot=character(0), ...)
{
  textTable(ftable(x, ...), colheadLabels=colheadLabels, sep=sep, title=title, 
            subtitle=subtitle, foot=foot)
}

