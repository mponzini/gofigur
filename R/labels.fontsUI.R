labels_and_fonts <- function(id, by = TRUE) {
  if (!is.logical(by)) {
    stop("'by' must be logical (TRUE/FALSE)")
  }
  
  htmltools::tagList(
    labels(id = id, by = by),
    fonts(id = id, by = by)
  )
}
