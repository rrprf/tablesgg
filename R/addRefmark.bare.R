#===== Source file: ../addRefmark.r on 2020-11-29
#-----

addRefmark <- function(x, mark, before=character(0), after=character(0), 
                       parts=NULL, raise, ...)
{
  if ((length(before) > 0 && !is.character(before)) || 
      length(before) > 1)  stop(
    "'before' must be a character scalar (regex)")
  if ((length(after) > 0 && !is.character(after)) || 
      length(after) > 1)  stop(
    "'after' must be a character scalar (regex)") 
  if (missing(raise) || is.null(raise))  raise <- !grepl("^\\*+$", mark)
  # Define an internal function that searches a vector/matrix of text strings 
  # for matches to 'before' or 'after' and adds the reference mark.
  add_mark <- function(txt, is_math) {
    # Defined by lexical scoping:  mark, before, after, raise
    addmark1 <- { if (length(before) == 0)  FALSE
                  else  grepl(before, txt, ...) }
    addmark2 <- { if (length(after) == 0)  FALSE 
                  else  grepl(after, txt, ...) }
    addmark <- addmark1 | addmark2
    if (is.null(is_math)) {
      is_math1 <- NULL
      is_math2 <- NULL
    } else {
      is_math1 <- is_math[addmark1]
      is_math2 <- is_math[addmark2]
    }
    txt[addmark1] <- add_refmark(txt[addmark1], is_math=is_math1, 
                                 mark=mark, side="before", raise=raise)
    txt[addmark2] <- add_refmark(txt[addmark2], is_math=is_math2, 
                                 mark=mark, side="after", raise=raise)
    list(text=txt, added_mark=addmark)
  }
    
  if (inherits(x, "textTable")) {
    partnames <- rownames(x$partdim)
    if (!is.null(parts))  partnames <- intersect(partnames, parts)
    for (i in partnames) {
      text <- x[[i]]
      if (is.null(text))  next
      wrk <- add_mark(text, is_math=NULL)  # list
      if (raise && any(chk <- (wrk$added_mark & grepl("\\n", text)))) {
        warning(sum(chk), " cells in part '", i, "' of 'x' contain newlines, ", 
                "which will not display correctly with a reference mark")
      }
      x[[i]] <- wrk$text
    }
  } else if (inherits(x, "pltdTable")) {
    etbl <- entries(x, enabledOnly=FALSE)
    if (is.null(parts)) {
      use <- rep(TRUE, nrow(etbl))
    } else if ("part" %in% names(etbl)) {
      use <- (etbl[, "part"] %in% parts)
    } else {
      stop("No 'part' information is available in 'x'")
    }
    text <- etbl[use, "text"]
    is_math <- etbl[use, "math"]
    wrk <- add_mark(text, is_math)  # list
    if (raise && any(chk <- (wrk$added_mark & grepl("\\n", text)))) {
      warning(sum(chk), " entries contain newlines, ", 
              "which will not display correctly with a reference mark")
    }
    etbl[use, "text"] <- wrk$text
    if (raise)  is_math <- is_math | wrk$added_mark
    etbl[use, "math"] <- is_math
    entries(x) <- etbl
  } else  stop("'x' is neither a 'textTable' nor 'pltdTable'")    
  x
}

