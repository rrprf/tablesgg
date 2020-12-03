#===== Source file: ../textTable_methods2.r on 2020-11-29
#-----

textTable.tabular <- function(x, title=character(0), subtitle=character(0), 
                              foot=character(0), rowheadLabels=TRUE, ...)
{
  form <- attr(x, "formula")  # Formula used to generate the table
  funs <- setdiff(all.names(form, functions=TRUE), 
                  all.names(form, functions=FALSE))  # function calls in the formula
  chk <- intersect(funs, c("PlusMinus", "Paste", "RowFactor", "Multicolumn", 
                           "Hline"))
  if (length(chk) > 0)  stop(
    "The following function(s) in the table formula generate code intended ", 
    "only for LaTeX: ", toString(chk))

  #body <- tables::format.tabular(x, ...)  # Should blanks added around text by format be removed?
  body <- format(x, ...)  # Should blanks added around text by format be removed?
  attr(body, "justification") <- attr(x, "justification")
  rowhead <- attr(x, "rowLabels")   # These have attributes of their own,
  colhead <- attr(x, "colLabels")   # including justification info
  # 'tabular' silently escapes underscores; undo that.
  body[] <- gsub("\\_", "_", body, fixed=TRUE)
  rowhead[] <- gsub("\\_", "_", rowhead, fixed=TRUE)
  colhead[] <- gsub("\\_", "_", colhead, fixed=TRUE)
  
  # Row and column labels in 'tabular' objects already have runs identified 
  # and NA's inserted.  Reverse that for now.  (Also needs to be done for 
  # 'justification' attribute.)
  just <- attr(rowhead, "justification")
  for (j in seq_len(ncol(rowhead))) {
    idx <- fill_NA_idx(rowhead[, j])
    rowhead[, j] <- rowhead[idx, j]
    just[, j] <- just[idx, j]
  }
  attr(rowhead, "justification") <- just
  just <- attr(colhead, "justification")
  for (j in seq_len(nrow(colhead))) {
    idx <- fill_NA_idx(colhead[j, ])
    colhead[j, ] <- colhead[j, idx]
    just[j, ] <- just[j, idx]
  }
  attr(colhead, "justification") <- just
  
  if (isTRUE(rowheadLabels))  rowheadLabels <- colnames(rowhead)
  if (isFALSE(rowheadLabels) || length(rowheadLabels) == 0) {
    rowheadLabels <- matrix(character(0), nrow=0, ncol=ncol(rowhead))
  } else {
    rowheadLabels <- matrix(rowheadLabels, nrow=1)
  }
  rowheadLabels[] <- gsub("\\_", "_", rowheadLabels, fixed=TRUE)
  attr(rowheadLabels, "justification") <- 
      matrix(rep(attr(rowhead, "colnamejust"), 
                 length.out=length(rowheadLabels)), 
             nrow=nrow(rowheadLabels), ncol=ncol(rowheadLabels))
  
  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)

  # Check/set/remove attributes for each component.
  keep_attr <- c("dim", "dimnames", "justification", "type")
  for (i in names(z)) {
    part <- z[[i]]
    just <- attr(part, "justification")
    if (!is.null(just)) {
      just[] <- as.character(just)  #  might be logical if all NA
      stopifnot(length(just) == length(part))
      # Might have strings intended for LaTeX:
      just[] <- tolower(substr(just, 1, 1))
      chk <- !(just %in% c("l", "c", "r", "n", NA))
      if (any(chk))  stop(
        "Unrecognized justification code(s) found for table part '", i, 
        "': ", toString(unique(just[chk])))
      just[just == "n"] <- NA
      attr(part, "justification") <- just
    }
    if (i == "body") {
      type <- array(vapply(x, function(y) { class(y)[1] }, character(1), 
                           USE.NAMES=FALSE), dim=dim(x))  # 'x', not 'part'!
      attr(part, "type") <- type
    } else if (i == "rowhead") {
      attr(part, "type") <- rep(NA_character_, ncol(part))
    } else if (i == "colhead") {
      attr(part, "type") <- rep(NA_character_, nrow(part))
    }
    # Remove any extraneous attributes.
    extra_attr <- setdiff(names(attributes(part)), keep_attr)
    for (a in extra_attr)  attr(part, a) <- NULL
    z[[i]] <- part
  }
  
  # Use 'textTable.default' to finish up processing.
  textTable(z)
}

#-----

textTable.xtable <- function(x, title, subtitle=character(0), foot=character(0), 
                             row.names="", na="", mathExponents=TRUE, ...)
{
  if (missing(title)) {
    title <- attr(x, "caption")
    title <- { if (is.null(title))  character(0)  else  title[1] }
  }
  align <- attr(x, "align")
  # Remove "|" values that xtable uses to request vertical rules in LaTeX.
  align <- align[align != "|"]
  # Replace "p{<size>}" values intended for LaTeX with "l", as for HTML
  align[grepl("^p", align)] <- "l"
  stopifnot(length(align) == (ncol(x) + 1))  # misunderstand 'xtable' structure
  stopifnot(all(align %in% c("l", "r", "c")))
  
  type <- vapply(x, function(y) { class(y)[1] }, character(1))
  has_rnms <- !isFALSE(row.names)
  # Format columns of 'x' (possibly including row names) into matrix of 
  # character strings.
  mat <- format_xtable(x, row.names=has_rnms, na=na, 
                       mathExponents=mathExponents, ...)
  stopifnot(ncol(mat) == (ncol(x) + has_rnms))
  body <- { if (has_rnms)  mat[, -1, drop=FALSE]  else  mat }
  body <- structure(body, 
                    justification=array(rep(align[-1], each=nrow(body)), 
                                        dim=dim(body)), 
                    type=array(rep(type, each=nrow(body)), dim=dim(body)))
  if (has_rnms) {
    rowhead <- mat[, 1, drop=FALSE]
    mat <- mat[, -1, drop=FALSE]
    type_rnms <- { if (any(notANumber(rowhead)))  "character"  else  "numeric" }
    rowhead <- structure(rowhead, 
                         justification=array(align[1], dim=dim(rowhead)), 
                         type=type_rnms)
    rowheadLabels <- { if (is.character(row.names))  row.names 
                       else  "row.names" }
    if (rowheadLabels == "") {
      rowheadLabels <- matrix(character(0), nrow=0, ncol=1)
    } else {
      rowheadLabels <- structure(matrix(rowheadLabels, nrow=1), 
                                 justification=matrix(align[1], nrow=1), 
                                 type="character")
    }
  } else {
    rowhead <- matrix(character(0), nrow=nrow(body), ncol=0)
    rowheadLabels <- matrix(character(0), nrow=0, ncol=0)
  }
  colhead <- matrix(names(x), nrow=1)
  colhead <- structure(colhead, 
                       justification=array(align[-1], dim=dim(colhead)), 
                       type=rep("character", nrow(colhead)))
  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=colhead, body=body, foot=foot)
  
  # Use 'textTable.default' to finish up processing and for validity checks.
  textTable(z)
}

#-----

textTable.xtableList <- function(x, title, subtitle=character(0), foot, ...)
{
  nsubtbl <- length(x)
  stopifnot(nsubtbl > 0)  # could return an empty textTable instead
  nms <- lapply(x, names)
  if (!all(sapply(nms, function(y, y1) { identical(y, y1) }, y1=nms[[1]]))) {
    stop("Not all subtables in 'x' have the same column names")
  }
  if (missing(title)) {
    title <- attr(x, "caption")
    title <- { if (is.null(title))  character(0)  else  title[1] }
  }
  if (missing(foot)) {
    foot <- attr(x, "message")
    if (is.null(foot))  foot <- character(0)
  }
  subhead <- unlist(lapply(x, attr, "subheading"))
  if (length(subhead) > 0) {
    if (length(subhead) != nsubtbl)  stop(
      "Number of subheadings does not match the number of subtables in 'x'")
    subhead <- rep(subhead, sapply(x, nrow))
  }
  x <- lapply(x, textTable, ...)
  
  # Stack the row headers and bodies of the 'textTable's in 'x'.
  rowheadLabels <- x[[1]]$rowheadLabels
  rowhead <- do.call(rbind, lapply(x, "[[", "rowhead"))
  attr(rowhead, "type") <- attr(x[[1]]$rowhead, "type")
  attr(rowhead, "justification") <- 
    do.call(rbind, lapply(x, function(y) { attr(y$rowhead, "justification") }))
  body <- do.call(rbind, lapply(x, "[[", "body"))
  attr(body, "type") <- 
    do.call(rbind, lapply(x, function(y) { attr(y$body, "type") }))
  attr(body, "justification") <- 
    do.call(rbind, lapply(x, function(y) { attr(y$body, "justification") }))
  
  # Add the subheadings as an additional, leading column of 'rowhead'.
  if (length(subhead) > 0) {
    start <- rowhead
    rowhead <- cbind(subhead, start)
    attr(rowhead, "type") <- c(NA_character_, attr(start, "type"))
    attr(rowhead, "justification") <- cbind(rep(NA_character_, length(subhead)), 
                                            attr(start, "justification"))
    if (nrow(rowheadLabels) > 0) {
      start <- rowheadLabels
      rowheadLabels <- structure(cbind("", start), 
                                 justification=cbind(NA_character_, 
                                                     attr(start, "justification")))
    }
  }
  
  z <- list(title=title, subtitle=subtitle, rowhead=rowhead, 
            rowheadLabels=rowheadLabels, colhead=x[[1]]$colhead, 
            body=body, foot=foot)

  # Use 'textTable.default' to finish up processing and for validity checks.
  textTable(z)
}

