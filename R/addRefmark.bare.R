#===== Source file: ../addRefmark.r on 2021-06-02
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
  add_mark <- function(txt, txtspec) {
    # Defined by lexical scoping:  mark, before, after, raise
    addmark1 <- { if (length(before) == 0)  FALSE
                  else  grepl(before, txt, ...) }
    addmark2 <- { if (length(after) == 0)  FALSE 
                  else  grepl(after, txt, ...) }
    addmark <- addmark1 | addmark2
    wrk <- add_refmark(txt[addmark1], textspec=txtspec[addmark1], mark=mark, 
                       side="before", raise=raise)
    txt[addmark1] <- wrk$text
    txtspec[addmark1] <- wrk$textspec

    wrk <- add_refmark(txt[addmark2], textspec=txtspec[addmark2], mark=mark, 
                       side="after", raise=raise)
    txt[addmark2] <- wrk$text
    txtspec[addmark2] <- wrk$textspec
    list(text=txt, textspec=txtspec)
  }
    
  if (inherits(x, "textTable")) {
    partnames <- rownames(x$partdim)
    if (!is.null(parts))  partnames <- intersect(partnames, parts)
    for (i in partnames) {
      text <- x[[i]]
      if (is.null(text))  next
      textspec <- spec_from_text(text)
      text[] <- prefix_text(text, action="remove")
      wrk <- add_mark(text, textspec)  # list
      text[] <- prefix_text(wrk$text, spec=wrk$textspec, action="add")
      x[[i]] <- text
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
    textspec <- etbl[use, "textspec"]
    wrk <- add_mark(text, textspec)  # list
    etbl[use, "text"] <- wrk$text
    etbl[use, "textspec"] <- wrk$textspec
    entries(x) <- etbl
  } else  stop("'x' is neither a 'textTable' nor 'pltdTable'")    
  x
}

