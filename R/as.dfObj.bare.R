#===== Source file: ../as.dfObj.r on 2020-11-29
#-----

as.dfObj <- function(x, objClass)
{
  classes <- c("tblEntries", "tblBlocks", "prEntries", "prHvrules", "prBlocks")
  objClass <- match.arg(objClass, classes)
  if (is.na(objClass))  stop("Invalid 'objClass' value")
  
  # For each class, specify the columns that must be present in 'x', and the 
  # columns that may not contain NA's for enabled rows.
  # ('required' also represents a valid, 0-row starting point for creating 
  # the requested object.)
  required <- list(
    tblEntries=data.frame(id=character(0), part=character(0), 
                          partrow=numeric(0), partcol=numeric(0), 
                          headlayer=numeric(0), level_in_layer=numeric(0), 
                          text=character(0), type=character(0), 
                          arow1=numeric(0), arow2=numeric(0), 
                          acol1=numeric(0), acol2=numeric(0), 
                          stringsAsFactors=FALSE), 
    prHvrules=data.frame(id=character(0), direction=character(0), 
                         arow1=numeric(0), arow2=numeric(0), 
                         acol1=numeric(0), acol2=numeric(0), enabled=logical(0), 
                         grProps()[["hvrule"]][0, , drop=FALSE], 
                         stringsAsFactors=FALSE), 
    tblBlocks=data.frame(id=character(0), nr=numeric(0), nc=numeric(0), 
                         arow1=numeric(0), arow2=numeric(0), 
                         acol1=numeric(0), acol2=numeric(0), 
                         stringsAsFactors=FALSE))
  required$prEntries <- data.frame(required$tblEntries, math=logical(0), 
                                   grProps()[["entry"]][0, , drop=FALSE], 
                                   stringsAsFactors=FALSE)
  required$prBlocks <- data.frame(required$tblBlocks, 
                                  grProps()[["block"]][0, , drop=FALSE], 
                                  stringsAsFactors=FALSE)
  NA_verboten <- list(
    tblEntries=c("id", "part", "partrow", "headlayer", 
                 "level_in_layer", "text", "arow1", "arow2", "acol1", "acol2", 
                 "math", "enabled", "multicolumn", "multirow"), 
    prEntries=c("id", "text", "arow1", "arow2", "acol1", "acol2", 
                "math", "enabled", "hjust", "vjust", "size", "hpad", 
                "vpad", "border_size"),
    prHvrules=c("id", "direction", "arow1", "arow2", "acol1", "acol2", 
                "enabled", "linetype", "size", "space"), 
    tblBlocks=c("id", "nr", "nc"), 
    prBlocks=c("id", "nr", "nc", "arow1", "arow2", "acol1", "acol2", "enabled", 
               "fill_alpha", "border_size"))
  id_col <- "id"
  required <- required[[objClass]]
  NA_verboten <- NA_verboten[[objClass]]
  
  if (is.null(x))  x <- required
  if (!is.data.frame(x))  stop("'x' is not a data frame")
  xattr <- attributes(x) 
  
  #----- Check required columns.
  chk <- setdiff(names(required), names(x))
  if (length(chk) > 0)  stop(
    "Following required columns not found in 'x': ", toString(chk))
  # Check ID column.
  if (anyNA(x[, id_col]))  stop("NA values are not allowed in '", id_col, "'")
  if (!is.character(x[, id_col]) || any(duplicated(x[, id_col])))  stop(
    "'", id_col, "'s are not unique character strings")
  # If present, check 'enabled' column.
  if ("enabled" %in% names(x)) {
    if (!is.logical(x[, "enabled"]) || anyNA(x[, "enabled"]))  stop(
      "Column 'enabled' of 'x' must of type logical, with no NAs")
  }
  
  #----- Class-specific code.
  if (objClass %in% c("tblEntries", "prEntries")) {
    # Add missing columns that can be filled by default.
    text <- as.character(x$text)
    na_text <- is.na(text)
    if (!("enabled" %in% names(x))) {
      x$enabled <- (!na_text & text != "") 
    } else if (!is.logical(x$enabled))  stop("'enabled' must be TRUE/FALSE")
    if (!("math" %in% names(x))) {
      x$math <- rep(FALSE, nrow(x))
    } else if (!is.logical(x$math))  stop("'math' must be TRUE/FALSE")
    if (!("type" %in% names(x)))  x$type <- rep(NA_character_, nrow(x))
    #-- Identify entries to be treated as 'plotmath' expressions:
    math <- !na_text & ((!is.na(x$math) & x$math) | grepl("^MATH_", text))
    text[math] <- sub("^MATH_", "", text[math])
    x$text <- text
    x$math <- math
    #-- Add columns indicating whether the entry spans multiple rows or columns 
    #   of the table.
    x$multicolumn <- (x$acol2 > x$acol1)
    x$multirow <- (x$arow2 > x$arow1)
    # Add missing attributes.
    if (is.null(mR <- xattr[["mergeRuns"]])) {
      mergeRuns <- with(x, c(any(part == "rowhead" & multirow), 
                             any(part == "colhead" & multicolumn)))
      attr(x, "mergeRuns") <- mergeRuns
    } else {
      if (!is.logical(mR) || length(mR) != 2)  stop(
        "Invalid 'mergeRuns' attribute")
    }
    rowheadInside <- xattr[["rowheadInside"]]
    if (is.null(rowheadInside)) {
      attr(x, "rowheadInside") <- with(x, any(part == "rowhead" & 
                                              headlayer == 0))
    } else if (!is.logical(rowheadInside) || length(rowheadInside) != 1) {
      stop("'x' has an invalid 'rowheadInside' attribute")
    }
    if (any(x$part == "rowhead")) {
      if (is.null(xattr[["rowhier"]]))  stop(
        "Missing row header hierarchy info (no 'rowhier' attribute)")
    } else  attr(x, "rowhier") <- list()
    if (any(x$part == "colhead")) {
      if (is.null(xattr[["colhier"]]))  stop(
        "Missing column header hierarchy info (no 'colhier' attribute)")
    } else  attr(x, "colhier") <- list()
  } else if (objClass %in% c("tblBlocks", "prBlocks")) {
    if (any(grepl(";", x$id)))  stop("Block id must not contain semicolons")  
      # due to format of 'adjacent_blocks' in 'tblHvrules', 'prHvrules'
    if (anyNA(x$nr) || anyNA(x$nc))  stop(
      "NA values are not allowed in 'nr' or 'nc'")
  
    chk <- with(x, (nr != 0 & nr != (arow2-arow1+1)) | 
                   (nr == 0 & (!is.na(arow1) | !is.na(arow2))))
    if (any(chk))  stop(
      "'nr' value is not consistent with 'arow1', 'arow2' for blocks ", 
      toString(which(chk), width=40))
    chk <- with(x, (nc != 0 & nc != (acol2-acol1+1)) | 
                   (nc == 0 & (!is.na(acol1) | !is.na(acol2))))
    if (any(chk))  stop(
      "'nc' value is not consistent with 'acol1', 'acol2' for blocks ", 
      toString(which(chk), width=40))
  
    rowgroupSize <- xattr[["rowgroupSize"]]
    if (is.null(rowgroupSize)) {
      attr(x, "rowgroupSize") <- 0
    } else {
      if (!is.numeric(rowgroupSize) || length(rowgroupSize) != 1)  stop(
        "'x' has an invalid 'rowgroupSize' attribute")
    }
    rowheadInside <- xattr[["rowheadInside"]]
    if (is.null(rowheadInside)) {
      attr(x, "rowheadInside") <- { if (nrow(x) == 0)  FALSE 
                                    else with(x, any(type %in% "rowblock" & 
                                                     headlayer %in% 0)) }
    } else {
      if (!is.logical(rowheadInside) || length(rowheadInside) != 1)  stop(
        "'x' has an invalid 'rowheadInside' attribute")
    }
  }
  if (objClass == "prEntries") {
    # Checks already done for 'tblEntries'.
    eltype <- "entry"
    # Style, scale checks are below.
    objClass <- c("prEntries", "tblEntries")
  } else if (objClass == "prHvrules") {
    eltype <- "hvrule"
    chk <- setdiff(unique(x$direction), c("hrule", "vrule"))
    if (length(chk) > 0)  stop("Column 'direction' contains invalid values: ", 
                               toString(chk, width=40))
    # Add missing columns that can be filled by default.
    def_cols <- data.frame(block=NA_character_, side=NA_character_, 
                           adjacent_blocks=NA_character_, enabled=FALSE, 
                           stringsAsFactors=FALSE)
    missing <- setdiff(names(def_cols), names(x))
    x <- data.frame(x, def_cols[rep(1, nrow(x)), missing, drop=FALSE],
                    stringsAsFactors=FALSE)  # loses attributes
    # Style, scale checks are below.
  } else if (objClass == "prBlocks") {
    # Checks already done for 'tblBlocks'.
    eltype <- "block"
    if (!("enabled" %in% names(x)))  x$enabled <- rep(FALSE, nrow(x))
    # Style, scale checks are below.
    objClass <- c("prBlocks", "tblBlocks")
  }
  
  # Style/scale checks common to all plot-ready objects.
  if (any(objClass %in% c("prEntries", "prHvrules", "prBlocks"))) {
    scale <- xattr[["current_scale"]]
    if (is.null(scale))  scale <- 1.0
    if (!is.finite(scale) || length(scale) != 1)  stop(
      "Attribute 'current_scale' is missing or invalid")
    if (!("style_row" %in% names(x)))  x$style_row <- rep(NA_integer_, nrow(x))
    if (!is.null(style <- xattr[["style"]])) {
      if (!inherits(style, "styleObj") || 
          attr(style, "element_type") != eltype)  stop(
        "'style' attribute is not a valid 'styleObj' object for ", eltype)
    }
    x <- structure(x, current_scale=scale, style=style)
  }
  
  #----- Check for illegal NAs (only check enabled entries).
  use <- { if ("enabled" %in% names(x))  x$enabled  else  rep(TRUE, nrow(x)) }
  if (any(chk <- sapply(x[use, NA_verboten, drop=FALSE], anyNA)))  stop(
    "Following columns contain missing (NA) values in enabled rows: ", 
    toString(NA_verboten[chk]))

  attr(x, "row.names") <- x[, "id"]
  class(x) <- c(objClass, "data.frame")  # removes any other classes
  x
}

#-----

as.tblEntries <- function(x)
{
  as.dfObj(x, objClass="tblEntries")
}

#-----

as.tblBlocks <- function(x) 
{
  as.dfObj(x, objClass="tblBlocks")
}

#-----

as.prEntries <- function(x)
{
  as.dfObj(x, objClass="prEntries")
}

#-----

as.prHvrules <- function(x)
{
  as.dfObj(x, objClass="prHvrules")
}

#-----

as.prBlocks <- function(x)
{
  as.dfObj(x, objClass="prBlocks")
}

