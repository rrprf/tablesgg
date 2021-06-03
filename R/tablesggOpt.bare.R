#===== Source file: ../tablesggOpt.r on 2021-06-02
#-----

tablesggOpt <- function(x=NULL, reset=FALSE)
{
  if (reset) {
    opt <- .tablesggOpt
    opt[["allowMarkdown"]] <- requireNamespace("ggtext", quietly=TRUE)
    opt[["allowWrap"]] <- opt[["allowMarkdown"]] && 
                          requireNamespace("quadprog", quietly=TRUE)
    assignInMyNamespace("currentOpt", opt)
  }
  if (length(x) == 1) {
    if (!is.character(x) || !(x %in% names(currentOpt)))  stop(
      "'x' (", x, ") is not the name of an available option")
    rslt <- currentOpt[[x]]
  } else if (is.null(x)) {
    rslt <- currentOpt
  } else stop("'x' must be either a single option name, or NULL")
  if (reset)  invisible(rslt)  else  rslt
}

#-----

tablesggSetOpt <- function(...)
{
  opt <- tablesggOpt()
  dots <- list(...)
  if (length(dots) == 1 && is.null(names(dots))) {
    # A single list of options was provided, rather than a set of 'tag=value'
    # pairs.
    dots <- dots[[1]]
  } else if (length(dots) == 0)  return(invisible(list()))
  onames <- names(dots)
  if (is.null(onames))  stop("Found unnamed option settings")
  
  chk <- setdiff(onames, names(opt))
  if (length(chk) > 0)  stop(
    "Attempt to set unrecognized options: ", toString(chk))

  # Validity checks on new option values.
  for (optname in onames) {
    vi <- dots[[optname]]
    if (optname == "entryStyle") {
      valid <- (inherits(vi, "styleObj") && 
                attr(vi, "element_type") == "entry")
      msg <- "Not a 'styleObj' object with type 'entry'"
    } else  if (optname == "blockStyle") {
      valid <- (inherits(vi, "styleObj") && 
                attr(vi, "element_type") == "block")
      msg <- "Not a 'styleObj' object with type 'block'"
    } else if (optname == "hvruleStyle") {
      valid <- (inherits(vi, "styleObj") && 
                attr(vi, "element_type") == "hvrule")
      msg <- "Not a 'styleObj' object with type 'hvrule'"
    } else if (optname == "plot.margin") {
      valid <- (is.numeric(vi) && length(vi) == 4)
      msg <- "Not a numeric vector of length 4"
    } else if (optname == "allowMarkdown") {
      valid <- (isFALSE(vi) || (isTRUE(vi) && 
                                requireNamespace("ggtext", quietly=TRUE)))
      msg <- "Package 'ggtext' is required but not installed"
    } else if (optname == "allowWrap") {
      valid <- (isFALSE(vi) || (isTRUE(vi) && 
                                requireNamespace("ggtext", quietly=TRUE) && 
                                requireNamespace("quadprog", quietly=TRUE)))
      msg <- "Packages 'ggtext' and 'quadprog' are required but not installed"
    }
    if (!valid)  stop("Invalid value for option '", optname, "': ", 
                      msg)
  }
  
  oldopt <- opt[onames]
  opt[onames] <- dots
  assignInMyNamespace("currentOpt", opt)
  invisible(oldopt)
}

