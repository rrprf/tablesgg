#===== Source file: ../make_header_entries.r on 2020-11-29
#-----

make_header_entries <- function(x, xhier, which_head, offset, mergeRuns)
{
  which_head <- match.arg(which_head, c("row", "col"))
  part <- { if (which_head == "row")  "rowhead"  else  "colhead" }
  if (which_head == "col")  x <- t(x)
  n_head <- ncol(x)
  n_lines <- nrow(x)
  stopifnot(is.list(xhier), length(xhier) == n_head)
  nms <- colnames(x)
  if (is.null(nms))  nms <- paste0(part, seq_len(n_head))
  
  # Determine which cells to merge for each header variable.
  mergeRuns <- max(0, min(mergeRuns, n_head))
  runs <- xhier  # list, one data frame per header variable
  if (mergeRuns < n_head) {
    rnum <- seq_len(n_lines)
    for (j in (mergeRuns+1):n_head) {
      xhierj <- xhier[[j]]
      # level_in_layer is as if runs *were* being merged (reflecting actual 
      # hierarchical structure).
      layer <- rep(n_head - j + 1, n_lines)
      lvl <- rep(xhierj[, "level_in_layer"], xhierj[, "runlen"])
      stopifnot(length(lvl) == n_lines)
      runs[[j]] <- data.frame(headlayer=layer, level_in_layer=lvl, 
                              value=x[, j], start=rnum, runlen=rep(1, n_lines), 
                              stringsAsFactors=FALSE) 
    }
  }

  # Stack the data frames in 'runs' into a single data frame, which will 
  # have one row per header entry (after any merging). 
  runs <- do.call(rbind, runs)
  varnum <- n_head - runs$headlayer + 1  
  subpart <- nms[varnum]
  start <- runs$start
  end <- start + runs$runlen - 1
  text <- x[cbind(start, varnum)]

  if (which_head == "row") {
    part <- rep("rowhead", length(varnum))
    id <- paste("rowhead", start, varnum, sep=",")
    arow1 <- offset[1] + start
    arow2 <- offset[1] + end
    acol1 <- offset[2] + varnum
    acol2 <- acol1
    ij <- cbind(start, varnum)
  } else if (which_head == "col") {
    part <- rep("colhead", length(varnum))
    id <- paste("colhead", varnum, start, sep=",")
    acol1 <- offset[2] + start
    acol2 <- offset[2] + end
    arow1 <- offset[1] + varnum
    arow2 <- arow1
    ij <- cbind(varnum, start)
  }
  rslt <- data.frame(id, part, subpart, 
                     runs[, c("headlayer", "level_in_layer")], 
                     text, arow1, arow2, acol1, acol2, 
                     stringsAsFactors=FALSE)
  structure(rslt, "i,j"=ij)
}

