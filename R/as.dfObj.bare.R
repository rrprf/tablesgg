#===== Source file: ../as.dfObj.r on 2021-06-02
#-----

as.dfObj <- function(x, objClass)
{
  classes <- c("tblEntries", "tblBlocks", "prEntries", "prHvrules", "prBlocks")
  objClass <- match.arg(objClass, classes)
  if (is.na(objClass))  stop("Invalid 'objClass' value")
  
  # For each class, get info about the columns that must be present in 'x', 
  # and the columns that may not contain NA's for enabled rows.
  spec <- dfSpecs(objClass)
  required <- spec$name
  NA_verboten <- spec$name[!(spec$NA_ok)]
  # 'template' represents a valid, 0-row starting point for creating 
  # the requested object:
  template <- df_from_spec(spec, n=0)
  id_col <- "id"
  
  if (is.null(x))  x <- template
  if (!is.data.frame(x))  stop("'x' is not a data frame")
  xattr <- attributes(x) 
  
  #----- Check required columns.
  chk <- setdiff(required, names(x))
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
    if (!is.character(x$textspec))  stop(
      "'textspec' must be a character string")
    if (any(chk <- !(x$textspec %in% c("plain", "plotmath", "markdown")))) {
      stop("Invalid values for 'textspec': ", 
           toString(unique(x$textspec[chk]), width=40)) }
    if (!("type" %in% names(x)))  x$type <- rep(NA_character_, nrow(x))
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
    # Most checks already done for 'tblEntries'.  Fill in some NA's, do some 
    # validity checks.
    x$angle[is.na(x$angle)] <- 0
    x$angle <- round(x$angle %% 360, 1)
    if (any(chk <- !(x$angle %in% c(0, 90, 180, 270))))  stop(
      "'angle' must be a multiple of 90 degrees; failed for entries: ", 
      toString(x$id[chk], width=60))
    x$minwidth[is.na(x$minwidth)] <- 0   # NA is treated as no constraint
    maxwidth <- x$maxwidth
    #-- 'plotmath' text can't be wrapped:
    math <- (x$texspec == "plotmath")
    x$minwidth[math] <- pmin(-1, x$minwidth[math])
    limited_width <- (is.na(maxwidth) | (maxwidth > -1 & maxwidth < Inf))
    if (any(chk <- (math & limited_width))) {
      warning("'maxwidth' must be Inf or <= -1 for 'plotmath' entries; ", 
              "changed to Inf for: ", toString(x$id[chk], width=60))
      x$maxwidth[chk] <- Inf
    }
    #--
    horiz <- (x$angle %in% c(0, 180))
    wdpad <- ifelse(horiz, x$hpad, x$vpad)
    if (any(chk <- (!is.na(maxwidth) & maxwidth >= 0 & 
                    maxwidth < 2*wdpad)))  stop(
      "'maxwidth' is less than the requested padding for entries: ", 
      toString(x$id[chk], width=60))
    minwidth <- x$minwidth
    chk <- (!is.na(maxwidth) & ((maxwidth < 0 & minwidth < maxwidth) | 
                                (maxwidth >= 0 & minwidth > maxwidth)))
    if (any(chk))  stop(
      "'minwidth' is greater than 'maxwidth' for entries: ", 
      toString(row.names(x)[chk], width=60))
    # Style, scale checks are below.
    eltype <- "entry"
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

