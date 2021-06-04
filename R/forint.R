#' Formats numbers as Hungarian Forints
#' @param x number vector representing the number of forint
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(45)
#' forint(10001000.213814)

forint <- function(x) {
  assert_number(x)
  dollar(x, prefix = "", suffix = " Ft")
}
