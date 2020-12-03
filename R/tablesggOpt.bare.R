#===== Source file: ../tablesggOpt.r on 2020-11-29
#-----

tablesggOpt <- function(x=NULL, reset=FALSE)
{
  if (reset)  assignInMyNamespace("currentOpt", .tablesggOpt)
  if (length(x) == 1) {
    if (!is.character(x) || !(x %in% names(currentOpt)))  stop(
      "'x' (", x, ") is not the name of an available option")
    rslt <- currentOpt[[x]]
  } else if (is.null(x)) {
    rslt <- currentOpt
  } else stop("'x' must be either a single option name, or NULL")
  invisible(rslt)
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
    } else  if (optname == "blockStyle") {
      valid <- (inherits(vi, "styleObj") && 
                attr(vi, "element_type") == "block")
    } else if (optname == "hvruleStyle") {
      valid <- (inherits(vi, "styleObj") && 
                attr(vi, "element_type") == "hvrule")
    } else if (optname == "plot.margin") {
      valid <- (is.numeric(vi) && length(vi) == 4)
    }
    if (!valid)  stop("Invalid value provided for option '", optname, "'")
  }
  
  oldopt <- opt[onames]
  opt[onames] <- dots
  assignInMyNamespace("currentOpt", opt)
  invisible(oldopt)
}

