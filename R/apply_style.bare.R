#===== Source file: ../apply_style.r on 2021-06-02
#-----

apply_style <- function(x, style, replace, setEnabled, unstyled, base_style)
{
  stopifnot(inherits(style, "styleObj"))
  element_type <- attr(style, "element_type")
  chk <- setdiff(attr(style, "match_columns"), names(x))
  if (length(chk) > 0)  stop(
    "Following columns required by 'style' are not present in 'x': ", 
    toString(chk))
  stopifnot(row.names(x) == x[, "id"])
  unstyled <- match.arg(unstyled, c("pass", "disable", "base", "error"))
  x_original <- x

  # Determine which style row to use for each element in 'x' (i.e., matches 
  # between element and style).
  if (element_type == "hvrule") {
    # Evaluate the block and adjacent block conditions in each style row 
    # for each block in 'x'.  An NA condition matches anything.
    mtch_block_condition <- eval_conditions(x, style[, "block_condition"], 
                                            NA_condition_value=TRUE)
    mtch_block_condition[is.na(mtch_block_condition)] <- FALSE
    #if (any(chk <- apply(mtch_block_condition, 2, anyNA)))  stop(
    #  "Evaluating the 'block_condition' expression in the following rows ", 
    #  "of 'style' produced NA values: ", toString(which(chk)))
    mtch_adjacent_condition <- eval_conditions(x, style[, "adjacent_condition"], 
                                               NA_condition_value=TRUE)
    mtch_adjacent_condition[is.na(mtch_adjacent_condition)] <- FALSE
    #if (any(chk <- apply(mtch_adjacent_condition, 2, anyNA)))  stop(
    #  "Evaluating the 'adjacent_condition' expression in the following rows ", 
    #  "of 'style' produced NA values: ", toString(which(chk)))

    # Generate hvrules for each side of each block.
    hvrules <- tblHvrules(x)  # NULL 'x' returns a 0-row object
    nrules <- nrow(hvrules)
    style_row <- rep(NA_integer_, nrules)
    adj_blocks <- lapply(seq_len(nrules), function(j) {
                         unlist(strsplit(hvrules[j, "adjacent_blocks"], ";"), 
                                use.names=FALSE)  # vector of block ID's, possibly empty
                         })  # list of vectors of block ID's

    # For each style row, determine which hvrules match it.
    for (j in seq_len(nrow(style))) {
      mtch1 <- mtch_block_condition[hvrules[, "block"], j]
      mtch2 <- (is.na(style[j, "side"]) | hvrules[, "side"] == style[j, "side"])
      # For each hvrule, check whether at least one of its adjacent blocks 
      # satisfies the style's 'adjacent_condition'.  (An NA 'adjacent_condition' 
      # matches everything, even an empty set of adjacent blocks.)
      mtch3 <- (is.na(style[j, "adjacent_condition"]) |  
                vapply(adj_blocks, function(y) { 
                       any(mtch_adjacent_condition[y, j]) }, 
                       FUN.VALUE=logical(1), USE.NAMES=FALSE))
      style_row[(mtch1 & mtch2 & mtch3)] <- j
    }
    # Redefine 'x' from blocks to hvrules.
    x <- hvrules
  } else {  # entries or blocks
    # Evaluate the element selection condition in each style row 
    # for each element in 'x'.
    mtch <- eval_conditions(x, conditions=style[, "condition"], 
                            NA_condition_value=TRUE)  # element x style row matrix
    mtch[is.na(mtch)] <- FALSE
    #if (any(chk <- apply(mtch, 2, anyNA)))  stop(
    #  "Evaluating the 'condition' expression in the following rows ", 
    #  "of 'style' produced NA values: ", toString(which(chk)))
    
    style_row <- apply(mtch, 1, function(y) {
                       if (any(y))  max(which(y))  else  NA_integer_ })
  }
  
  # Add any graphical properties not already in 'x', with values set to NA.
  grspecs <- grSpecs(element_type)  # one row per available property
  properties <- row.names(grspecs)
  add_props <- setdiff(properties, names(x))
  x[, add_props] <- df_from_spec(grspecs, n=nrow(x))[, add_props, drop=FALSE]
  
  # For matched elements, copy graphical properties from the appropriate 
  # row of 'style'.
  matched <- (!is.na(style_row))
  if (!is.na(replace)) {
    repl_props <- { if (replace)  properties  else  add_props }
    x[matched, repl_props] <- style[style_row[matched], repl_props, drop=FALSE]
  } else {
    x[matched, add_props] <- style[style_row[matched], add_props, drop=FALSE]
    for (j in setdiff(properties, add_props)) {
      chg <- (matched & is.na(x[, j]))
      x[chg, j] <- style[style_row[chg], j]
    }
  }
  x[, "style_row"] <- style_row
  if (setEnabled)  x[matched, "enabled"] <- TRUE
  
  # Handle unmatched elements.
  if (any(unmatched <- !matched)) {
    if (unstyled == "error")  stop(
      sum(unmatched), " rows of 'x' were not matched by any pattern in ", 
      "'style': ", toString(x[unmatched, "id"], width=80))
    if (unstyled == "base") {
      x2 <- apply_style(x_original, style=base_style, replace=replace, 
                        setEnabled=FALSE, unstyled="error")
      id_chg <- x[unmatched, "id"]
      x[unmatched, ] <- x2[id_chg, , drop=FALSE]
      # 'style_row' remains NA for unmatched elements.
      x[unmatched, "style_row"] <- NA_integer_
    } else if (unstyled == "disable") {
      x[unmatched, "enabled"] <- FALSE
    }  # else "pass", so do nothing
  }

  row.names(x) <- x$id
  x
}

