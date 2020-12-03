#===== Source file: ../styleObj.r on 2020-11-29
#-----

styleObj <- function(x, type, scalable_properties, match_columns=character(0))
{
  if (is.character(x))  x <- read.csv(x, stringsAsFactors=FALSE)
  if (!is.data.frame(x))  stop(
    "'x' is not a data frame, or a file containing one")
  type <- match.arg(type, c("entry", "hvrule", "block"))
  
  if (missing(scalable_properties) || is.null(scalable_properties)) {
    scalable_properties <- list("entry"=c("size", "hpad", "vpad", "border_size"), 
                                "block"=character(0), 
                                "hvrule"=c("space"))[[type]]
  }
  
  if (type == "entry") {
    selectors <- "condition"
  } else if (type == "block") {
    selectors <- "condition"
  } else if (type == "hvrule") {
    selectors <- c("block_condition", "side", "adjacent_condition")
  }
  
  if (any(chk <- !(selectors %in% names(x))))  stop(
    "Following selector column(s) are not present in 'x': ", 
    toString(selectors[chk]))
  chk <- !sapply(x[, selectors, drop=FALSE], 
                 function(y) { is.character(y) || is.logical(y) })
  if (any(chk))  stop(
    "Following selector column(s) are not of mode character: ", 
     toString(selectors[chk]))
  # Change empty strings in selector columns to NA.
  for (j in selectors) {
    chg <- (!is.na(x[, j]) & trimws(x[, j]) == "")
    x[chg, j] <- NA_character_
  }
  if (type == "hvrule") {
    chk <- setdiff(unique(x[, "side"]), c("top", "bottom", "left", "right", NA))
    if (length(chk) > 0)  stop(
      "Illegal values for 'side': ", toString(chk, width=40))
  }
  
  if (!is.character(match_columns) || anyNA(match_columns)) stop(
    "'match_columns' must be a character vector, without NA values")

  properties <- names(grProps()[[type]])
  if (any(chk <- !(properties %in% names(x)))) stop(
    "Following graphical properties are not present in 'x': ", 
    toString(properties[chk]))

  if (any(chk <- !(scalable_properties %in% properties))) stop(
    "Following scalable properties are not valid properties: ", 
    toString(scalable_properties[chk]))
  
  structure(x, element_type=type, 
            scalable_properties=scalable_properties, 
            match_columns=match_columns, 
            class=c("styleObj", "data.frame"))
}

