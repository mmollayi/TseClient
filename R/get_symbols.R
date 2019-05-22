#' Get tse data
#'
#' @param symbols
#' @param env
#' @param auto_assign
#' @param verbose
#' @param ret_class
#' @param adj_prices
#'
#' @return
#' @export
#'
#' @examples
get_symbols <- function(symbol = NULL, env = NULL, auto_assign = FALSE, verbose = FALSE,
                        return_class = c("tibble", "xts"), adj_prices = FALSE) {

    if (is.null(env)) auto_assign <- FALSE
    return_class <- match.arg(return_class)

    inscode <- symbol

    url <- httr::modify_url("http://www.tsetmc.com/tsev2/data/Export-txt.aspx",
                            query = list(t = "i", a = 2, b = 0, i = inscode))

    clnms <- c("date", "open", "high", "low", "close",
               "value", "volume", "n_trades", "last_day")

    response <- httr::GET(url)

    if ((response$status_code < 200) || (response$status_code >= 300)) {
        stop(httr::content(response, as="text"), call. = FALSE)
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    content <- readr::read_csv(content, skip = 1, col_types = "-Dddddddd-d-", col_names = clnms,
                               locale = readr::locale(date_format = "%Y%m%d"))

    content <- content %>%
        mutate(adj_ratio = lag(last_day) / close) %>%
        mutate(adj_ratio = ifelse(is.na(adj_ratio), 1, adj_ratio)) %>%
        mutate(adj_ratio = cumprod(adj_ratio)) %>%
        mutate(adjusted = close * adj_ratio)

    if (adj_prices) {
        content <- content %>%
            mutate_at(vars(open:close), funs(. * adj_ratio))
    }

    content <- content %>%
        select(date:close, adjusted, value:n_trades)

    if (return_class == "xts") content <- as.xts(zoo::read.csv.zoo(content))

    # assign(instruments[i], tse_data, env)

    return(content)
}

get_ids <- function(symbols) {
    filter_at(ids_data, vars(inscode:id_fa), any_vars(. %in% id))
}
