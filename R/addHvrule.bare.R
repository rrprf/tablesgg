#===== Source file: ../addHvrule.r on 2021-06-02
#-----

addHvrule <- function(x, direction, arows, acols, id, props=NULL, enabled=TRUE)
{
  if (inherits(x, c("pltdTable", "prTable"))) {
    hvrules <- hvrules(x, enabledOnly=FALSE)
  } else {
    stop("'x' is not a plotted table ('pltdTable' object)")
  }
  direction <- match.arg(direction, c("hrule", "vrule"))
  
  if (missing(id)) {
    nextnum <- nrow(hvrules) + 1
    while ((id <- paste0("hvrule", nextnum)) %in% hvrules[, "id"]) {
      nextnum <- nextnum + 1
    }
  } else if (!is.character(id) || length(id) != 1 || is.na(id))  stop(
    "'id' is not a character scalar")
  if (id %in% hvrules[, "id"])  stop(
    "An hvrule with 'id' ", id, " is already present in 'x'")

  if (direction == "hrule") {
    if (missing(acols)) {
      acols <- c(min(adim(x)[2], 1), adim(x)[2])  # might be 0's
    } else {
      acols <- range(acols)
    }
    if (length(arows) != 1 || ((2*arows) %% 2) != 1)  stop(
      "Value in 'arows' must be a single half-integer for an 'hrule'")
    if (!all(acols == round(acols))) stop(
      "Values in 'acols' must be integers for an 'hrule'")
  }
  if (direction == "vrule") {
    if (missing(arows)) {
      partinfo <- tblParts(x)
      arows <- c(min(adim(x)[1], 
                    sum(partinfo[c("title", "subtitle"), "nr"]) + 1), 
                adim(x)[1] - partinfo["foot", "nr"])  # might be 0's
    } else {
      arows <- range(arows)
    }
    if (length(acols) != 1 || ((2*acols) %% 2) != 1)  stop(
      "Value in 'acols' must be a single half-integer for a 'vrule'")
    if (!all(arows == round(arows))) stop(
      "Values in 'arows' must be integers for a 'vrule'")
  }

  # Create a new row for 'hvrules', initially filled with NAs.
  if (nrow(hvrules) > 0) {
    newrow <- hvrules[1, , drop=FALSE]
    for (i in names(newrow))  newrow[1, i] <- NA
  } else {
    newrow <- as.list(hvrules)
    for (i in names(newrow))  newrow[[i]] <- rep(newrow[[i]], length.out=1)
    newrow <- data.frame(newrow, stringsAsFactors=FALSE)
  }
  
  newrow[1, "id"] <- id
  newrow[1, "direction"] <- direction
  newrow[1, "arow1"] <- arows[1]
  newrow[1, "arow2"] <- { if (direction == "hrule")  arows[1]  else  arows[2] }
  newrow[1, "acol1"] <- acols[1]
  newrow[1, "acol2"] <- { if (direction == "vrule")  acols[1]  else  acols[2] }
  newrow[1, "enabled"] <- enabled
  propnms <- row.names(grSpecs("hvrule"))
  newrow[1, propnms] <- styles_pkg$hvruleStyle_pkg_base[1, propnms]
  newrow[1, "style_row"] <- 0  # so graphical props are not chgd by restyling

  newrules <- structure(as.prHvrules(rbind(hvrules, newrow)), 
                        current_scale=attr(hvrules, "current_scale"), 
                        style=attr(hvrules, "style"))
  hvrules(x) <- newrules
  if (!is.null(props)) {
    if (!inherits(props, "element_hvrule"))  stop(
      "'props' is not an 'element_hvrule' object")
    props(x, id=id, setEnabled=FALSE) <- props
  }
  x
}

