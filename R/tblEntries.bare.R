#===== Source file: ../tblEntries.r on 2021-06-02
#-----

tblEntries <- function(x, mergeRuns=c(TRUE, TRUE), rowheadInside=FALSE)
{
  x <- textTable(x)  # Rely on this for validity checks
  mergeRuns <- rep(mergeRuns, length.out=2)
  # Logical dimensions of table parts (treats 'rowheadInside' as FALSE).
  partinfo <- tblParts(x)  # matrix, one row per part
  n_rowhead <- partinfo["rowhead", "nc"]
  n_colhead <- partinfo["colhead", "nr"]
  # Set 'nc' for annotation parts to table width:
  nc_tbl <- adim(x)[2]
  partinfo[c("title", "subtitle", "foot"), "nc"] <- nc_tbl
  # First row, column associated with each part of the table.
  arows1 <- setNames(partinfo[, "arow1"], rownames(partinfo))
  acols1 <- setNames(partinfo[, "acol1"], rownames(partinfo))
  
  # Define an empty data frame with the right column names and types for the 
  # result.
  empty_df <- data.frame(id=character(0), part=character(0), 
                         subpart=character(0), partrow=numeric(0), 
                         partcol=numeric(0), headlayer=numeric(0), 
                         level_in_layer=numeric(0), text=character(0), 
                         justify=character(0), type=character(0), 
                         arow1=numeric(0), arow2=numeric(0), acol1=numeric(0), 
                         acol2=numeric(0), 
                         stringsAsFactors=FALSE)
  # ('multirow', 'multicolumn', 'textspec', 'enabled' are added at the end.)

  #-----
  make_part_info <- function(x, part, subpart=NA_character_)
  # Given a character vector/matrix containing the entries for a specified 
  # part of the table, return a data frame with one row per entry.
  # For parts 'title', 'subtitle', 'foot' this function assumes they 
  # span the whole table; for 'body' and 'rowheadLabels' it assumes no merging 
  # of cells.  
  {
    # Defined by lexical scoping:  arows1, acols1, nc_tbl, partinfo, empty_df
    if (length(x) == 0)  return(empty_df)
    justify <- as.vector(attr(x, "justification"))
    if (is.null(justify))  justify <- rep(NA_character_, length(x))
    type <- as.vector(attr(x, "type"))
    if (!is.matrix(x))  dim(x) <- c(length(x), 1)
    text <- as.vector(x)
    irow <- as.vector(row(x))  # row number in 'x'
    icol <- as.vector(col(x))  # column number in 'x'
    nr <- nrow(x)
    nc <- ncol(x)
    prt <- rep(part, length(text))
    subpart <- rep(subpart, length.out=nr*nc)
    # Row and column numbers with respect to the augmented table grid.
    if (part %in% c("title", "subtitle", "foot")) {
      id <- paste(part, irow, sep=",")
      icol[] <- NA  # since annotation is in vectors, not matrices
      headlayer <- irow + 
                   { if (part == "foot")  0 
                     else if (part == "subtitle")  partinfo["colhead", "nr"] 
                     else  sum(partinfo[c("colhead", "subtitle"), "nr"]) }
      level_in_layer <- rep(1, length(headlayer))
      arow1 <- arows1[part] + irow - 1
      arow2 <- arow1
      acol1 <- rep(acols1[part], nr)
      acol2 <- rep(nc_tbl, nr)
      if (is.null(type))  type <- rep("character", length(text))
    } else if (part %in% c("body")) {
      id <- paste("body", irow, icol, sep=",")
      headlayer <- rep(0, nr*nc)
      level_in_layer <- seq_len(nr*nc)  # as if treating matrix as vector
      arow1 <- arows1[part] + irow - 1
      arow2 <- arow1
      acol1 <- acols1[part] + icol - 1
      acol2 <- acol1
      if (is.null(type))  type <- rep(NA_character_, length(text))
    } else if (part %in% c("rowheadLabels")) {
      id <- paste("rowheadLabels", irow, icol, sep=",")
      headlayer <- rev(seq_along(text))
      level_in_layer <- rep(1, length(text))
      arow1 <- arows1[part] + irow - 1
      arow2 <- arow1
      acol1 <- acols1[part] + icol - 1
      acol2 <- acol1
      if (is.null(type))  type <- rep("character", length(text))
    } else {
      stop("Unrecognized table part: ", part)
    }
    data.frame(id, part=prt, subpart, partrow=irow, partcol=icol, 
               headlayer, level_in_layer, text, 
               justify, type, arow1, arow2, acol1, acol2, 
               stringsAsFactors=FALSE)   
  }
  #-----
  make_header_info <- function(x, xhier, part, merge_runs) 
  # Given a character matrix containing the entries for the row or column 
  # headers of the table, return a data frame with one row per entry.  Allow 
  # for merged cells.
  {
    # Defined by lexical scoping:  arows1, acols1, empty_df
    if (length(x) == 0)  return(empty_df)
    justify <- attr(x, "justification")
    type <- attr(x, "type")
    df <- make_header_entries(x, xhier=xhier, which_head=substr(part, 1, 3), 
                              offset=c(arows1[part] - 1, acols1[part] - 1), 
                              mergeRuns=merge_runs)  # data frame
    ij <- attr(df, "i,j")  # row, col numbers from 'x' for each entry
    justify <- justify[ij]
    varnum <- { if (part == "rowhead")  ij[, 2]  else  ij[, 1] }
    type <- type[varnum]
    data.frame(df[, c("id", "part", "subpart"), drop=FALSE], 
               partrow=ij[, 1], partcol=ij[, 2], 
               df[, c("headlayer", "level_in_layer", "text"), drop=FALSE], 
               justify, type, 
               df[, c("arow1", "arow2", "acol1", "acol2"), drop=FALSE], 
               stringsAsFactors=FALSE)
  }
  
  #-----
  # Generate entries for each table part.
  # 'make_part_info', 'make_header_info' handle NULL parts by returning a
  # data frame with no entries.
  annot <- list(title=x$title, subtitle=x$subtitle, foot=x$foot)
  for (part in names(annot)) {
    a_part <- annot[[part]]
    annot[[part]] <- make_part_info(a_part, part=part)  # now data frame
  }
  body <- make_part_info(x$body, part="body")
  # When 'rowheadInside' is TRUE, force merging in the outermost row header 
  # layer even when 'mergeRuns' is FALSE.
  rowhead <- make_header_info(x$rowhead, xhier=x$rowhier, part="rowhead", 
                              merge_runs={ if (mergeRuns[1])  n_rowhead 
                                           else  as.numeric(rowheadInside) })
  colhead <- make_header_info(x$colhead, xhier=x$colhier, part="colhead", 
                              merge_runs={ if (mergeRuns[2])  n_colhead 
                                           else  0 })
  # Row header labels, in the upper left corner of the table.
  rowheadLabels <- make_part_info(x$rowheadLabels, part="rowheadLabels", 
                                      subpart=x$rowheadLabels)
  #-----
  # Combine all table parts into a single data frame.
  rslt <- do.call(rbind, c(annot, list(body, rowhead, colhead, rowheadLabels)))
  stopifnot(!any(duplicated(rslt$id)))
  enabled <- rep(TRUE, nrow(rslt))
  text <- rslt$text
  # NA's in 'text' are disabled, with a warning.
  na_text <- is.na(text)
  if (any(na_text)) {
    warning(sum(na_text), " entries with missing text will be disabled")
    enabled[na_text] <- FALSE
  }
  # Empty strings in 'text' are disabled.
  enabled[!na_text & text == ""] <- FALSE
  rslt$enabled <- enabled
  # Identify entries to be treated specially for display (plotmath and 
  # markdown) by looking for prefixes:
  rslt$textspec <- spec_from_text(text)  # never NA
  rslt$text <- prefix_text(text, action="remove")  # NA's retained
  # Add columns indicating whether the entry spans multiple rows or columns 
  # of the table.
  rslt$multicolumn <- (rslt$acol2 > rslt$acol1)
  rslt$multirow <- (rslt$arow2 > rslt$arow1)
  # Convert character column 'justify' to numeric 'hjust'.
  just <- tolower(substr(rslt[, "justify"], 1, 1))
  okay <- (is.na(just) | just %in% c("l", "c", "r"))
  if (!all(okay)) stop(
    "Invalid 'justification' values in 'x': ", 
    toString(unique(rslt[!okay, "justify"]), width=40))
  rslt[, "hjust"] <- c(0, 1, 0.5)[match(just, c("l", "r", "c"))]  # NA propagates
  rslt[, "justify"] <- NULL
  
  rslt <- structure(rslt, mergeRuns=mergeRuns, rowheadInside=FALSE, 
                    rowhier=x$rowhier, colhier=x$colhier, 
                    class=c("tblEntries", "data.frame"))
  row.names(rslt) <- rslt[, "id"]

  if (n_rowhead == 0)  rowheadInside <- FALSE
  if (rowheadInside)  rslt <- rowhead_inside(rslt, paste_rhiLabel=TRUE)
  rslt
}

