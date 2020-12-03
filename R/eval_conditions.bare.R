#===== Source file: ../eval_conditions.r on 2020-11-29
#-----

eval_conditions <- function(x, conditions, NA_condition_value=TRUE)
{
  stopifnot(is.data.frame(x))
  stopifnot(is.character(conditions) || 
            (is.logical(conditions) && all(is.na(conditions))))
  stopifnot(is.logical(NA_condition_value) && length(NA_condition_value) == 1)
  
  nx <- nrow(x)
  ncond <- length(conditions)
  rslt <- matrix(NA_condition_value, nrow=nx, ncol=ncond)
  if (nx > 0) {
    for (j in seq_len(ncond)) {
      expr_str <- conditions[j]
      if (is.na(expr_str) || trimws(expr_str) == "")  next
      expr_loc <- paste0("'conditions[", j, "]'")
      mtch <- try(eval(str2lang(expr_str), envir=x, parent.frame()), 
                  silent=TRUE)
      if (inherits(mtch, "try-error"))  stop(
        "Error evaluating the following expression from ", expr_loc, 
         " using columns of 'x': ", expr_str)
      if (!is.logical(mtch) || !(length(mtch) %in% c(1, nx)))  stop(
        "Evaluating the following expression from ", expr_loc, 
        " using columns of 'x' did not return a valid logical vector: ", 
        expr_str)
      rslt[, j] <- mtch
    }
  }
  
  rownames(rslt) <- row.names(x)
  rslt
}

