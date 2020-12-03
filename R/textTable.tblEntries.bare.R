#===== Source file: ../textTable.tblEntries.r on 2020-11-29
#-----

textTable.tblEntries <- function(x, ...)
{
  chkDots(...)
  x <- as.tblEntries(x)
  if (attr(x, "rowheadInside"))  x <- undo_rowhead_inside(x)
  # Dimensions of table parts.
  partinfo <- tblParts(x)  # matrix
  pnms <- rownames(partinfo)
  rslt <- setNames(vector("list", nrow(partinfo)), pnms)
  partrow <- x[, "partrow"]
  partcol <- x[, "partcol"]
  text <- x[, "text"]
  math <- x[, "math"]
  text[math] <- paste0("MATH_", text[math])
  type <- { if ("type" %in% names(x))  x[, "type"]  
            else  rep(NA_character_, nrow(x)) }
  hjust <- { if ("hjust" %in% names(x))  x[, "hjust"]  
             else  rep(NA_character_, nrow(x)) }
  if (any(chk <- !(hjust %in% c(0, 0.5, 1, NA)))) {
    warning("Numeric values in 'hjust' will be converted to NA")
    hjust[chk] <- NA_real_
  }
  just <- as.character(c("l", "c", "r")[round(2*hjust + 1)])
  for (i in pnms) {
    nr <- partinfo[i, "nr"]
    nc <- partinfo[i, "nc"]
    part <- { if (is.na(nc))  rep(NA_character_, nr)
              else  matrix(NA_character_, nrow=nr, ncol=nc) }
    pjust <- part  # initialization
    ptype <- part
    use <- (x[, "part"] == i)
    if (any(is.na(partrow[use]) | partrow[use] < 1 | partrow[use] > nr)) stop(
      "Invalid 'partrow' value(s) for part ", i)
    if (is.na(nc)) {
      idx <- partrow[use]
    } else {
      if (any(is.na(partcol[use]) | partcol[use] < 1 | partcol[use] > nc)) stop(
        "Invalid 'partcol' value(s) for part ", i)
      idx <- cbind(partrow[use], partcol[use])
    }
    part[idx] <- text[use]
    pjust[idx] <- just[use]
    ptype[idx] <- type[use]
    attr(part, "justification") <- pjust
    if (i == "body") {
      attr(part, "type") <- ptype
    } else if (i == "rowhead") {
      attr(part, "type") <- { if (length(ptype) == 0)  rep(NA_character_, nc)
                              else  ptype[1, , drop=TRUE] }
    } else if (i == "colhead") {
      attr(part, "type") <- { if (length(ptype) == 0)  rep(NA_character_, nr)
                              else  ptype[, 1, drop=TRUE] }
    }
    rslt[[i]] <- part
  }
  
  # Fill in NA's due to merged header runs.
  if (length(rslt[["rowhead"]]) > 0) {
    part <- rslt[["rowhead"]]
    just <- attr(part, "justification")
    hier <- attr(x, "rowhier")
    for (j in seq_len(ncol(part))) {
      hierj <- hier[[j]]  # data frame
      part[, j] <- rep(part[hierj[, "start"], j], times=hierj[, "runlen"])
      just[, j] <- rep(just[hierj[, "start"], j], times=hierj[, "runlen"])
    }
    rslt[["rowhead"]] <- structure(part, justification=just)
  }
  if (length(rslt[["colhead"]]) > 0) {
    part <- rslt[["colhead"]]
    just <- attr(part, "justification")
    hier <- attr(x, "colhier")
    for (j in seq_len(nrow(part))) {
      hierj <- hier[[j]]  # data frame
      part[j, ] <- rep(part[j, hierj[, "start"]], times=hierj[, "runlen"])
      just[j, ] <- rep(just[j, hierj[, "start"]], times=hierj[, "runlen"])
    }
    rslt[["colhead"]] <- structure(part, justification=just)
  }
  
  # Final processing and validity checks.
  textTable.default(rslt)
}

