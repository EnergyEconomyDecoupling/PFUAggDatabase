#' Pivot an aggregation or efficiency data frame wide by years
#'
#' Converts a aggregation of efficiencies data frame (in tidy format)
#' to one that is wide by years.
#'
#' @param .df The data frame to pivot.
#' @param pivot_cols The columns to pivot.
#' @param country,year See `IEATools::iea_cols`.
#' @param quantity,.values See `IEATools::template_cols`.
#'
#' @return A version of `.df` that is pivoted wide by years.
#'
#' @export
pivot_agg_eta_wide_by_year <- function(.df,
                                       pivot_cols,
                                       country = IEATools::iea_cols$country,
                                       year = IEATools::iea_cols$year,
                                       quantity = IEATools::template_cols$quantity,
                                       .values = IEATools::template_cols$.values) {
  .df %>%
    tidyr::pivot_longer(cols = pivot_cols,
                        names_to = quantity,
                        values_to = .values) %>%
    dplyr::arrange(.data[[year]]) %>%
    tidyr::pivot_wider(names_from = year, values_from = .values) %>%
    dplyr::arrange(.data[[country]])
}

#' Retrieve a list of final demand sectors
#'
#' Retrieve a list of final demand sectors for calculation of the total final consumption
#' of final, useful, or services energy in gross or net terms.
#'
#' @return A list of final demand sectors
#' @export
#'
#' @examples
#' fd_sectors <- get_fd_sectors()
get_fd_sectors <- function(){

  fd_sectors <- IEATools::fd_sectors

  return(fd_sectors)
}


#' Create a list containing final demand sectors
#'
#' This function creates a list equal to the length of any data frame supplied.
#' It is typically used on a data frame containing Physical Supply-Use Tables (PSUT)
#' with the associated final demand sectors in the nested `Y` and `U_EIOU` matrices.
#'
#' @param fd_sectors A character vector of final demand sectors.
#' @param .sutdata A data frame containing Physical Supply-Use Table (PSUT)
#'                 matrices with associated final demand sector names
#'
#' @return A list the length of a desired data frame containing final demand vectors
#' @export
#'
#' @examples
#' library(Recca)
#' final_demand_sector_list <- create_fd_sectors_list(fd_sectors = c("Residential"),
#' .sutdata = Recca::UKEnergy2000mats)
create_fd_sectors_list <- function(fd_sectors, .sutdata) {

  fd_sectors_list <- rep(x = list(c(fd_sectors)), times = nrow(.sutdata))

  return(fd_sectors_list)

}


#' Retrieve primary industry prefixes
#'
#' Retrieve primary industry prefixes for use by `Recca::find_p_industry_names`.
#' Contains "Resources", "Imports", and "Stock changes".
#'
#' @return A list of primary industry prefixes
#' @export
#'
#' @examples
#' p_industry_prefixes <- get_p_industry_prefixes()
get_p_industry_prefixes <- function() {

  p_industry_prefixes <- IEATools::prim_agg_flows %>%
    unname() %>%
    unlist() %>%
    list()

  return(p_industry_prefixes)

}
