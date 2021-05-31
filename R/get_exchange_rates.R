#' Look up the historical values of base currency in another currency.
#' @param base currency symbol
#' @param symbols currency symbol
#' @param start_date date
#' @param end_date date
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' @examples
#' get_exchange_rates()
#' get_exchange_rates(base= 'USD', symbols='EUR')
#' get_exchange_rates(base= 'USD', symbols='GBP', start_date= "2021-04-24", end_date= "2021-05-10")

get_exchange_rates <- function(base= 'USD',
                               symbols='HUF',
                               start_date = Sys.Date() - 30,
                               end_date= Sys.Date(),
                               retried=0){
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query= list(
        base= base,
        symbols=symbols,
        start_date = start_date,
        end_date= end_date
      )
    )
    exchange_rates <- content(response)$rates
    rates <- data.table(
      date=as.Date(names(exchange_rates)),
      rate=as.numeric(unlist(exchange_rates)))
    assert_numeric(rates$rate, lower = 0)
  }, error=function(e){
    log_error(e$message)
    Sys.sleep(1+retried^2)
    get_exchange_rates(base= 'USD',
                       symbols='HUF',
                       start_date = Sys.Date() - 30,
                       end_date= Sys.Date(),
                       retried = retried+1)
  })
  log_info('1 {base} = {rate} {symbols}')
  rates
}
