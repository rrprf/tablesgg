#===== Source file: ../props.r on 2020-11-29
#-----

'propsa<-' <- function(x, arows=NULL, acols=NULL, setEnabled=TRUE, value)
{
  if (!inherits(x, c("prTable", "pltdTable")))  stop("Invalid 'x' argument")
  if (!is.null(arows) && (!is.numeric(arows) || any(is.na(arows)) || 
                          any(2*arows != round(2*arows)))) {
    stop("'arows' is not a vector of integers/half-integers without NA's")
  }
  if (!is.null(acols) && (!is.numeric(acols) || any(is.na(acols)) || 
                          any(2*acols != round(2*acols)))) {
    stop("'acols' is not a vector of integers/half-integers without NA's")
  }

  props_mod(x, value=value, arows=arows, acols=acols, setEnabled=setEnabled)
}

#-----

'props<-' <- function(x, id=NULL, regex=NULL, setEnabled=TRUE, 
                      mustMatch=TRUE, ..., value)
{
  if (!inherits(x, c("prTable", "pltdTable")))  stop("Invalid 'x' argument")
  if (!inherits(value, c("element_entry", "element_refmark")) && 
      length(regex) > 0)  stop(
    "'regex' can only be used to set properties for entries, not blocks or hvrules")
  if (length(id) == 0 && length(regex) == 0)  return(x)
  
  # If entries are being updated and 'id' includes part or block ID's, 
  # convert them to entry ID's.  Also convert 'regex' to the selected 
  # entry ID's.
  if (inherits(value, c("element_entry", "element_refmark"))) {
    xInfo <- entries(x, enabledOnly=FALSE)
    if (!is.null(id)) {
      id_ent <- xInfo[, "id"]
      id_pts <- rownames(tblParts(NULL))
      blockInfo <- blocks(x, enabledOnly=FALSE)
      id_blk <- blockInfo[, "id"]
      eids <- id[id %in% id_ent]
      wrk <- setdiff(id, eids)
      if (length(wrk) > 0) {
        # Convert part ID's to entry ID's.
        pids <- wrk[wrk %in% id_pts]
        epids <- xInfo[xInfo[, "part"] %in% pids, "id"]
        wrk <- setdiff(wrk, pids)
      } else  epids <- character(0)
      if (length(wrk) > 0) {
        # Convert block ID's to entry ID's.
        bids <- wrk[wrk %in% id_blk]
        idx <- unlist(entries_by_block(xInfo, blocks=blockInfo[bids, , drop=FALSE],
                                       strict=FALSE), use.names=FALSE)
        ebids <- xInfo[unique(idx), "id"]
        wrk <- setdiff(wrk, bids)
      } else  ebids <- character(0)
      if (length(wrk) > 0 && mustMatch)  stop(
        "'id' contains values that do not match any entry, part, or block ID: ", 
        toString(wrk, width=40))
      id <- unique(c(eids, epids, ebids))
    }
    
    if (length(regex) == 1) {
      erids <- xInfo[grepl(regex, xInfo[, "text"], ...), "id"]
      id <- { if (is.null(id))  erids
              else  intersect(id, erids) }
    } else if (length(regex) > 1) {
      stop("Argument 'regex' has length greater than 1")
    }
  } else if (inherits(value, "element_hvrule")) {
    xInfo <- hvrules(x, enabledOnly=FALSE)
    id_hvr <- xInfo[, "id"]
    blockInfo <- blocks(x, enabledOnly=FALSE)
    id_blk <- blockInfo[, "id"]
    hvrids <- id[id %in% id_hvr]
    wrk <- setdiff(id, hvrids)
    if (length(wrk) > 0) {
      # Convert block ID's to hvrule ID's.
      bids <- wrk[wrk %in% id_blk]
      hvrbids <- xInfo[xInfo[, "block"] %in% bids, "id"]
      wrk <- setdiff(wrk, bids)
    } else  hvrbids <- character(0)
    if (length(wrk) > 0 && mustMatch)  stop(
      "'id' contains values that do not match any hvrule or block ID: ", 
      toString(wrk, width=40))
    id <- unique(c(hvrids, hvrbids))
  } else if (inherits(value, "element_block")) {
    id_blk <- blocks(x, enabledOnly=FALSE)[, "id"]
    bids <- id[id %in% id_blk]
    wrk <- setdiff(id, bids)
    if (length(wrk) > 0 && mustMatch)  stop(
      "'id' contains values that do not match any block ID: ", 
      toString(wrk, width=40))
    id <- unique(bids)
  }
    
  props_mod(x, value=value, id=id, setEnabled=setEnabled) 
}

#-----

'propsd<-' <- function(x, subset=NULL, setEnabled=TRUE, value)
{
  if (!inherits(x, c("prTable", "pltdTable")))  stop("Invalid 'x' argument")
  e <- substitute(subset)
  if (is.null(e))  return(x)
  
  props_mod(x, value=value, e=e, setEnabled=setEnabled)
}

#-----

props_mod <- function(x, value, id=NULL, e=NULL, arows=NULL, acols=NULL, 
                      setEnabled=TRUE)
{
  blockInfo <- blocks(x, enabledOnly=FALSE)
  if (inherits(value, c("element_entry", "element_refmark"))) {
    xInfo <- entries(x, enabledOnly=FALSE)
    xtype <- "entry"
  } else if (inherits(value, "element_hvrule")) {
    xInfo <- hvrules(x, enabledOnly=FALSE)
    xtype <- "hvrule"
  } else if (inherits(value, "element_block")) {
    xInfo <- blockInfo
    xtype <- "block"
  } else {
    stop("Assigned value must be an 'element_entry', 'element_refmark', ", 
         "'element_hvrule', or 'element_block' object")
  }
    
  # Select rows to update.
  if (!is.null(id)) {
    idx <- match(id, xInfo[, "id"], nomatch=0)
    if (any(idx==0))  stop(
      "'id' contains values that do not match any ", xtype, " ID")
  } else if (!is.null(e)) {  # code based on base::subset.data.frame
    r <- eval(e, xInfo, parent.frame(2))  # assumes call from 'propsd<-'
    if (!is.logical(r))  stop("'subset' must be logical")
    if (length(r) == 1)  r <- rep(r, nrow(xInfo))
    idx <- which(r & !is.na(r))
  } else {  # use 'arows', 'acols'
    if (is.null(arows)) {
      rselect <- rep(TRUE, nrow(xInfo))
    } else {
      ar <- cbind(xInfo[, "arow1"], xInfo[, "arow2"])
      rselect <- apply(ar, 1, function(y) { 
                       if (anyNA(y))  FALSE  else  
                       all(seq(from=y[1], to=y[2], by=1) %in% arows) })
    }
    if (is.null(acols)) {
      cselect <- rep(TRUE, nrow(xInfo))
    } else {
      ac <- cbind(xInfo[, "acol1"], xInfo[, "acol2"])
      cselect <- apply(ac, 1, function(y) { 
                       if (anyNA(y))  FALSE  else 
                       all(seq(from=y[1], to=y[2], by=1) %in% acols) })
    }
    stopifnot(!any(is.na(rselect & cselect)))
    idx <- which(rselect & cselect)
  }  
  
  # Apply updates.
  if (setEnabled)  xInfo[idx, "enabled"] <- TRUE  # will be overridden if  
     # there is an explicit 'enabled' in 'newprops'.
  if (inherits(value, "element_refmark")) {
    raise <- value[["raise"]]
    xInfo[idx, "text"] <- add_refmark(xInfo[idx, "text"], 
                                      is_math=xInfo[idx, "math"], 
                                      mark=value[["mark"]], 
                                      side=value[["side"]], raise=raise)
    if (raise)  xInfo[idx, "math"] <- TRUE
    value <- attr(value, "extra")  # optional 'element_entry'
    stopifnot(is.null(value) || inherits(value, "element_entry"))
  }
  if (!is.null(value)) {
    # Extract new values for properties.
    newprops <- value[!sapply(value, is.null)]
    newprops$inherit.blank <- NULL
    names(newprops) <- sub("colour", "color", names(newprops))
    chk <- setdiff(names(newprops), names(xInfo))
    if (length(chk) > 0)  stop(
      "Following graphical properties (to be updated) not found in 'x': ", 
      toString(chk))
    # When modifying entry text, in the absence of an explicit 'math' setting, 
    # look for 'MATH_' prefix in the new value for entry text:
    if (xtype == "entry" && ("text" %in% names(newprops)) && 
        !("math" %in% names(newprops))) {
      if (grepl("^MATH_", newprops[["text"]])) {
        newprops[["text"]] <- sub("^MATH_", "", newprops[["text"]])
        newprops <- c(newprops, list(math=TRUE))
      }
    }
    for (nm in names(newprops)) {
      newprop <- newprops[[nm]]
      if (inherits(newprop, "rel")) {
        xInfo[idx, nm] <- unclass(newprop) * xInfo[idx, nm]
      } else {
        xInfo[idx, nm] <- newprop
      }
    }
    # 'style_row' is no longer valid for elements with updated graphical props.
    if (any(names(newprops) %in% names(grProps()[[xtype]])) && 
        "style_row" %in% names(xInfo)) {
      xInfo[idx, "style_row"] <- NA_integer_
    }
  }
  
  # Put updated entries/hvrules/blocks back in 'x'.
  if (xtype == "entry") { 
    entries(x) <- xInfo
  } else if (xtype == "hvrule") { 
    hvrules(x) <- xInfo 
  } else {
    blocks(x) <- xInfo
  }

  x
}

