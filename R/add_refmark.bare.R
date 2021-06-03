#===== Source file: ../add_refmark.r on 2021-06-02
#-----

add_refmark <- function(text, textspec, mark, side, raise)
{
  stopifnot(length(textspec) == length(text))
  is_math <- (textspec == "plotmath")  # text already includes plotmath
  is_mkdn <- (textspec == "markdown")  # text already includes markdown/HTML
  is_plain <- !(is_math | is_mkdn)
  # Whether markdown should be used as the default for raised reference marks:
  default_mkdn <- tablesggOpt("allowMarkdown")
  
  if (raise && !default_mkdn && any(grepl("\\n", text[is_plain])))  stop(
    "Newlines found in entry text will not display correctly when a reference ", 
    "mark is added using 'plotmath'.  Consider enabling markdown instead.")

  if (side == "before") {
    if (raise) {
      text[is_math] <- paste0('phantom()^paste("', mark, '")*', text[is_math])
      text[is_mkdn] <- paste0('<sup>', mark, '</sup>', text[is_mkdn])
      if (default_mkdn) {
        text[is_plain] <- paste0('<sup>', mark, '</sup>', text[is_plain])
      } else {
        text[is_plain] <- paste0('phantom()^paste("', mark, '")*paste("', 
                                 text[is_plain], '")')
      }
    } else {
      text[is_math] <- paste0('paste("', mark, '")*', text[is_math])
      text[!is_math] <- paste0(mark, text[!is_math])
    }
  } else {
    if (raise) {
      text[is_math] <- paste0(text[is_math], '*phantom()^paste("', mark, '")')
      text[is_mkdn] <- paste0(text[is_mkdn], '<sup>', mark, '</sup>')
      if (default_mkdn) {
        text[is_plain] <- paste0(text[is_plain], '<sup>', mark, '</sup>')
      } else {
        text[is_plain] <- paste0('paste("', text[is_plain], 
                                 '")*phantom()^paste("', mark, '")')
      }
    } else {
      text[is_math] <- paste0(text[is_math], '*paste("', mark, '")')
      text[!is_math] <- paste0(text[!is_math], mark)
    }
  }
  if (raise) {  # need to update 'textspec' for "plain" entries
    textspec[is_plain] <- { if (default_mkdn)  "markdown"  else  "plotmath" }
  }
  dim(textspec) <- dim(text)
  list(text=text, textspec=textspec)
}

