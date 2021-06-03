#===== Source file: ../text_util.r on 2021-06-02
#-----

spec_from_text <- function(x)
{
  # Note:  'grepl' returns FALSE for NA values in 'x', so "plain" is returned.
  as.character(
    ifelse(grepl("^MATH_", x), "plotmath", 
      ifelse(grepl("^MKDN_", x), "markdown", "plain"))
  )
}

#-----

prefix_text <- function(x, spec, action)
{
  action <- match.arg(action, c("add", "remove"))
  specs <- c("plain", "plotmath", "markdown")  # valid 'textspec' values
  prefx <- c("",      "MATH_",    "MKDN_")     # associated string prefixes
  
  if (action == "add") {
    spec <- rep(spec, length.out=length(x))
    idx <- match(spec, specs)
    x <- ifelse(is.na(x), NA_character_, paste0(prefx[idx], x))
  } else {  # remove prefixes.  Currently ignores 'spec'.
    for (i in seq_along(prefx)) {
      if (prefx[i] == "")  next
      rex <- paste0("^", prefx[i])
      x <- sub(rex, "", x)  # NA remains NA
    }
  }
  
  x
}

#-----

paste_text <- function(x, ..., sep=c("plain"=" ", "plotmath"="~", 
                                     "markdown"=" "))
{
  specs <- c("plain", "plotmath", "markdown")  # valid 'textspec' values
  def_sep <- c("plain"=" ", "plotmath"="~", "markdown"=" ")
  msng <- setdiff(names(def_sep), names(sep))
  sep <- c(sep, def_sep[msng])
  x <- c(list(x), list(...))
  narg <- length(x)
  # Convert args to 3-column data frames.
  for (i in seq_len(narg)) {
    xi <- x[[i]]
    if (!is.data.frame(xi))  xi <- data.frame(text=as.character(xi), 
                                              stringsAsFactors=FALSE)
    if (!("textspec" %in% names(xi))) {
      xi$textspec <- spec_from_text(xi[, "text"])
      xi$text <- prefix_text(xi$text, action="remove")
    }
    if (!("fontface" %in% names(xi)))  xi$fontface <- rep(1, nrow(xi))
    xi[is.na(xi$fontface), "fontface"] <- 1
    x[[i]] <- xi[, c("text", "textspec", "fontface")]
  }
  ni <- sapply(x, nrow)
  nr <- max(ni)
  for (i in seq_along(x)) {
    x[[i]] <- x[[i]][rep(seq_len(ni[i]), length.out=nr), , drop=FALSE]
  }

  # Create matrices where each row corresponds to one set of strings to be 
  # pasted together:
  tmat <- do.call(cbind, lapply(x, "[[", "text"))  
  pmat <- do.call(cbind, lapply(x, "[[", "textspec"))  # no NA's
  fmat <- do.call(cbind, lapply(x, "[[", "fontface"))  # no NA's
  # Can't mix plotmath and markdown when pasting:
  chk <- apply(pmat, 1, function(y) { ("plotmath" %in% y) && 
                                      ("markdown" %in% y) })
  if (any(chk))  stop(
    "Can't paste 'plotmath' and 'markdown' strings")
  # Spec for final pasted string in each row:
  tspec <- apply(pmat, 1, function(y) { if ("plotmath" %in% y)  "plotmath" 
                                        else if ("markdown" %in% y)  "markdown" 
                                        else  "plain" })
  
  # In rows with both plain and plotmath or markdown strings, need to 
  # convert the plain strings to the appropriate type.  Also, in rows 
  # with all-plain strings but mixed fontfaces, need to convert them 
  # to plotmath or markdown.
  # -- Rows with mixed fontfaces. 
  ffset <- vector("list", nr)
  for (i in seq_len(nr))  ffset[[i]] <- unique(fmat[i, ])
  mixed_ff <- (tspec == "plain" & lengths(ffset) > 1)
  # -- Rows needing conversion of plain strings.
  convert_plain <- (mixed_ff | (tspec != "plain" & 
                                apply(pmat, 1, function(y) any(y == "plain"))))
  for (i in which(convert_plain)) {
    chg <- which(pmat[i, ] == "plain")
    # If all strings in the row are plain, can use either plotmath or markdown 
    # to create a single mixed-face string.  (But markdown does not have a 
    # way to indicate bold italic (?).)
    if (all(pmat[i, ] == "plain")) {
      tspec[i] <- { if (tablesggOpt("allowMarkdown") && !any(fmat[i, ] == 4)) 
                       "markdown"  else  "plotmath" }
    }
    if (tspec[i] == "plotmath") {
      # Enclose strings in plotmath fontface functions
      if (any(grepl('"', tmat[i, chg])))  stop(
        'Unable to handle double quote within strings to be displayed with ', 
        'plotmath')
      face <- c("plain", "bold", "italic", "bolditalic")[fmat[i, chg]]
      tmat[i, chg] <- paste0(face, '("', tmat[i, chg], '")')
    } else if (tspec[i] == "markdown") {
      if (any(bldital <- (fmat[i, chg] == 4))) {
        warning("Bold italic fontface not implemented for markdown text; ", 
                "changed to just bold")
        fmat[i, chg[bldital]] <- 2
      }
      face_mark <- c("", "**", "*")[fmat[i, chg]]
      tmat[i, chg] <- paste0(face_mark, tmat[i, chg], face_mark)
      # Change newline (\n) characters to HTML '<br>' tag.
      tmat[i, chg] <- gsub("\\n", "<br>", tmat[i, chg])
    }
  }
  
  fface <- ifelse(tspec == "plain", x[[1]][, "fontface"], 
             ifelse(tspec == "plotmath", NA_real_, 1))
  text <- rep(NA_character_, nr)
  tlst <- as.list(data.frame(tmat, stringsAsFactors=FALSE))
  for (k in specs) {
    idx <- which(tspec == k)
    text[idx] <- do.call(paste, c(lapply(tlst, "[", idx), list(sep=sep[k])))
  }
                                   
  data.frame(text, textspec=tspec, fontface=fface, stringsAsFactors=FALSE)
}

#-----

sci_fmt_pm <- function(x)
{
  stopifnot(is.character(x))
  # Regular expression matching scientific notation as produced by 'formatC'.
  re1 <- "([-+]?[[:digit:]]*\\.?[[:digit:]]*)[eE]([-+]?[[:digit:]]+)"
  has_sci <- grepl(re1, x)
  # Remove superfluous '+' and leading 0's in exponent.
  x[has_sci] <- sub("[eE]\\+0*", "e", x[has_sci])
  x[has_sci] <- sub("[eE]-0*", "e-", x[has_sci])
  paste0(ifelse(has_sci, "MATH_", ""), sub(re1, "plain('\\1') %*% 10^{\\2}", x))
}

