#===== Source file: ../elements.r on 2021-06-02
#-----

elements <- function(x, type=c("entry", "block", "hvrule"), enabledOnly=TRUE)
{
  type <- match.arg(type)
  if (!inherits(x, "pltdTable"))  stop(
    "'x' is not a plotted table ('pltdTable')")

  x <- { if (type == "entry")  entries(x, enabledOnly=enabledOnly)
         else if (type == "block")  blocks(x, enabledOnly=enabledOnly)
         else if (type == "hvrule")  hvrules(x, enabledOnly=enabledOnly) }
  # Want a plain data frame, so remove extra classes and attributes.
  x <- data.frame(x, stringsAsFactors=FALSE)
  # Omit 'style_row' from data frame, since the style itself is not kept.
  x$style_row <- NULL
  x
}

#-----

element_entry <- function(text=NULL, family=NULL, fontface=NULL, colour=NULL, 
    alpha=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL, 
    color=NULL, 
    hpad=NULL, vpad=NULL, fill=NULL, fill_alpha=NULL, border_size=NULL, 
    border_colour=NULL, border_color=NULL, minwidth=NULL, maxwidth=NULL, 
    enabled=NULL, textspec=NULL, 
    inherit.blank=FALSE)
{
    if (!is.null(colour))  color <- colour
    if (!is.null(border_colour))  border_color <- border_colour
    rslt <- structure(list(text=text, family=family, fontface=fontface, 
                           color=color, alpha=alpha, size=size, hjust=hjust, 
                           vjust=vjust, 
                           angle=angle, lineheight=lineheight, hpad=hpad, 
                           vpad=vpad, fill=fill, fill_alpha=fill_alpha, 
                           border_size=border_size, border_color=border_color, 
                           minwidth=minwidth, maxwidth=maxwidth, 
                           enabled=enabled, textspec=textspec, 
                           inherit.blank=inherit.blank), 
                      class=c("element_entry", "element"))
    ok <- sapply(rslt, function(x) { is.null(x) || length(x) == 1 })
    if (!all(ok))  stop(paste0(
      "Found arguments that are neither NULL nor scalars: ", 
      toString(names(rslt)[!ok])))
    rslt
}

#-----

element_block <- function(fill=NULL, fill_alpha=NULL, border_size=NULL, 
    border_colour=NULL, border_color=NULL, enabled=NULL, 
    inherit.blank=FALSE)
{
    if (!is.null(border_colour))  border_color <- border_colour
    rslt <- structure(list(fill=fill, fill_alpha=fill_alpha, 
                           border_size=border_size, border_color=border_color, 
                           enabled=enabled, inherit.blank=inherit.blank), 
                      class=c("element_block", "element"))
    ok <- sapply(rslt, function(x) { is.null(x) || length(x) == 1 })
    if (!all(ok))  stop(paste0(
      "Found arguments that are neither NULL nor scalars: ", 
      toString(names(rslt)[!ok])))
    rslt
}

#-----

element_refmark <- function(mark=NULL, side=NULL, raise, ..., 
                            inherit.blank=FALSE)
{
  dots <- list(...)
  if (!is.null(dots[["textspec"]]) || !is.null(dots[["text"]]))  stop(
    "Do not specify 'text' or 'textspec' when setting reference marks")
  if (!is.character(mark) || length(mark) != 1 || is.na(mark))  stop(
    "'mark' must be a single character string")
  if (!is.character(side) || length(side) != 1 || 
    !(side %in% c("before", "after")))  stop(
    "'side' must be 'before' or 'after'")
  if (missing(raise) || is.null(raise))  raise <- !grepl("^\\*+$", mark)
  if (!is.logical(raise) || length(raise) != 1 || is.na(raise))  stop(
    "'raise' must be TRUE or FALSE")
  rslt <- structure(list(mark=mark, side=side, raise=raise, 
                         inherit.blank=inherit.blank), 
                    class=c("element_refmark", "element"))
  if (length(dots) > 0)  attr(rslt, "extra") <- do.call(element_entry, dots)
  rslt
}

#-----

element_hvrule <- function(colour=NULL, alpha=NULL, linetype=NULL, size=NULL, 
                           fill=NULL, fill_alpha=NULL, space=NULL, color=NULL, 
                           enabled=NULL, inherit.blank=FALSE)
{
    if (!is.null(colour))  color <- colour
    rslt <- structure(list(color=color, alpha=alpha, linetype=linetype, 
                           size=size, 
                           fill=fill, fill_alpha=fill_alpha, space=space, 
                           enabled=enabled, inherit.blank=inherit.blank), 
                      class=c("element_hvrule", "element"))
    ok <- sapply(rslt, function(x) { is.null(x) || length(x) == 1 })
    if (!all(ok))  stop(paste0(
      "Found arguments that are neither NULL nor scalars: ", 
      toString(names(rslt)[!ok])))
    rslt
}

#-----

ids <- function(x, type, enabledOnly=TRUE)
{
  if (inherits(x, "textTable")) {
    if (missing(type) || is.null(type)) {
      type <- "part"
    } else if (length(type) != 1 || type != "part")  stop(
      "'x' is a 'textTable', so 'type' must be 'part'")
    rownames(x$partdim)
  } else if (inherits(x, "pltdTable")) {
    type <- match.arg(type, c("entry", "block", "hvrule"))
    elements(x, type=type, enabledOnly=enabledOnly)[, "id"]
  } else  stop("'x' is not a 'textTable' or plotted table ('pltdTable')")
}

