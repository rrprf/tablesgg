#===== Source file: ../apply_scale.r on 2021-06-02
#-----

apply_scale <- function(x, type, scale)
{
  type <- match.arg(type, c("entry", "block", "hvrule"))
  specs <- grSpecs(type)
  scalable_properties <- specs[specs$scalable, "name"]
  # Negative values of 'minwidth', 'maxwidth' should not be scaled.
  props1 <- setdiff(scalable_properties, c("minwidth", "maxwidth"))
  x[, props1] <- scale * x[, props1]
  props2 <- intersect(scalable_properties, c("minwidth", "maxwidth"))
  for (prop in props2) {
    idx <- which(x[, prop] > 0)
    x[idx, prop] <- scale * x[idx, prop]
  }
  x
}

