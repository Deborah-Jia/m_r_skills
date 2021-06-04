#' Look up the values of a US dollar in Hungarian Forints
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @examples
#' get_usdhuf()

get_usdhuf <- function(retried=0){
  tryCatch({
    usdhuf <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(usdhuf, lower = 220, upper = 420) # from checkmate
  }, error=function(e){  # in case sth wrong with the function, sleep for 1s then can the fun() again, error handling mechanism
    log_error(e$message) # with error, we'll know the error time and info
    Sys.sleep(1+retried^2)
    get_usdhuf(retried = retried+1)
  })
  log_info('1 USD = {usdhuf} HUF')
  usdhuf
}

#' Look up the historical values of a US dollar in Hungarian Forints
#' @param start_date the first day of the time range you would like to query
#' @param end_date the last day of the time range you would like to query
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_usdhufs()
#' get_usdhufs(start_date = "2021-04-11", end_date = "2021-05-11")

get_usdhufs <- function(start_date = Sys.Date() - 30, end_date= Sys.Date(), retried=0){
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query= list(
        base= 'USD',
        symbols='HUF',
        start_date = start_date,
        end_date = end_date
      )
    )
    exchange_rates <- content(response)$rates
    usdhufs <- data.table(
      date=as.Date(names(exchange_rates)),
      usdhuf=as.numeric(unlist(exchange_rates)))
    assert_numeric(usdhufs$usdhuf, lower = 220, upper = 420)
  }, error=function(e){
    log_error(e$message)
    Sys.sleep(1+retried^2)
    get_usdhufs(start_date = Sys.Date() - 30,
                end_date= Sys.Date(),
                retried = retried+1)
  })
  usdhufs
}
