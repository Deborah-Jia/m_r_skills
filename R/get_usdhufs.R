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

#' Look up the historical values of a US dollar in Hungarian Forints
#' @param start_date
#' @param end_date
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content

get_usdhufs <- function(start_date = Sys.Date() - 30, end_date= Sys.Date(), retried=0){
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query= list(
        base= 'USD',
        symbols='HUF',
        start_date = start_date,
        end_date= end_date
      )
    )
    exchange_rates <- content(response)$rates
    usdhufs <- data.table(
      date=as.Date(names(exchange_rates)),
      usdhuf=as.numeric(unlist(exchange_rates)))
    assert_numeric(usdhufs$usdhuf, lower = 250, upper = 400)
  }, error=function(e){
    log_error(e$message)
    Sys.sleep(1+retried^2)
    get_usdhufs(retried = retried+1)
  })
  log_info('1 USD = {usdhuf} HUF')
  usdhufs
}
