# Allows the ability to chain shiny validate conditions
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}
