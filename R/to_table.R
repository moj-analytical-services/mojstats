query_athena <- function(Query) {

  con <- Rdbtools::connect_athena()
  data <- Rdbtools::dbGetQuery(con, Query)
  Rdbtools::dbDisconnect(con)

  return(data)

}

format_period <- function(data,period_type) {

  if (period_type == "Year") {

    data <- data %>%
      dplyr::select(-period_notation)

  } else if (period_type == "Quarter") {

    data <- data %>%
      dplyr::mutate(period_notation = stringr::str_sub(period_notation,1,4)) %>%
      dplyr::rename(Year = period_notation)

  }

  data <- dplyr::rename(data,!!rlang::sym(stringr::str_to_title(period_type)) := period_name)

  return(data)

}

to_table <- function(period_type,data,dimension,indicator = "count") {

  data %>%
    dplyr::filter(period_type == {{period_type}}) %>%
    dplyr::select(period_notation,period_name,{{dimension}},{{indicator}}) %>%
    dplyr::arrange(factor(!!rlang::sym(dimension),exclude = "Total"),
                   period_notation, !!rlang::sym(dimension)) %>%
    tidyr::pivot_wider(names_from = {{dimension}},
                       values_from = {{indicator}},
                       values_fn = max) %>%
    format_period(.,period_type) %>%
    replace(is.na(.), 0)

}

to_table_list <- function(data,dimensions,indicator = "count") {

  unlist(lapply(dimensions,
                function(x) {
                  lapply(unique(data$period_type),to_table,data,tidyselect::all_of(x),indicator)
                } ),
         recursive = FALSE)

}
