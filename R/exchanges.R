#' Look up the values of a US dollar in Hungarian Forints
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom checkmate assert_number
#' @importFrom logger log_error log_info
#' @importFrom jsonlite fromJSON
get_usdhuf <- function(retried=0){
  tryCatch({
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number # from checkmate
  }, error=function(e){  # in case sth wrong with the function, sleep for 1s then can the fun() again, error handling mechanism
    log_error(e$message) # with error, we'll know the error time and info
    Sys.sleep(1+retried^2)
    get_usdhuf(retried = retried+1)
  })
  log_info('1 USD = {usdhuf} HUF')
  usdhuf
}
