#===== Source file: ../headerRuns.r on 2021-06-02
#-----

headerRuns <- function(x, which_head)
{
  if (is.null(x))  return(NULL)
  stopifnot(is.matrix(x))
  which_head <- match.arg(substr(which_head, 1, 3), c("row", "col"))
  if (which_head == "col")  x <- t(x)
  nc <- ncol(x)
  nr <- nrow(x)
  rslt <- vector("list", nc)
  names(rslt) <- colnames(x)  # might be NULL
  if (nc == 0)  return(rslt)
  # Identify runs in each column separately (independently of other columns), 
  # and code them as integers.
  vidx <- array(NA_integer_, dim=dim(x))
  for (i in seq_len(nc)) {
    xx <- { if (is.factor(x[, i]))  as.numeric(x[, i])  else  x[, i] }
    temp <- rle(xx)
    vidx[, i] <- rep.int(seq_along(temp$values), times=temp$lengths)
  }
  # Identify runs cumulatively across columns.
  for (i in seq_len(nc)) {
    pasted <- { if (i == 1)  vidx[, 1]  
                else  paste(pasted, vidx[, i], sep="\r") }
    runs <- rle(pasted)
    runlen <- runs$lengths
    nrun <- length(runlen)
    end <- cumsum(runlen)
    start <- head(c(1L, end+1L), -1)
    rslt[[i]] <- data.frame(headlayer=rep(nc - i + 1, nrun), 
                            level_in_layer=seq_len(nrun), 
                            value=x[start, i], start=start, runlen=runlen, 
                            row.names=NULL, stringsAsFactors=FALSE)
  }
  rslt
}

