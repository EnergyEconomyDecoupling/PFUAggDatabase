####################################################################################################################################

#' Generate an mw graph
#'
#' Creates an mw graph from a MW PSUT.
#'
#' This function is called repeatedly from `mw_plots_df()`.
#'
#' @param .df A data frame comprised of aggregated muscle work data, usually
#'            retrieved from the `...` target.
#' @param country_val The country for which plots are to be created.
#' @param country,year,e_dot See `IEATools::iea_cols`.
#'
#' @return A `ggplot2` graph object
#'
#' @export
create_agg_graph <- function(.df,
                             country_val,
                             method_val,
                             energy_type_val,
                             gross_net_val,
                             country_colname = IEATools::iea_cols$country,
                             year = IEATools::iea_cols$year,
                             e_dot = IEATools::iea_cols$e_dot,
                             stage_colname = PFUAggDatabase::sea_cols$stage_colname,
                             agg_by_colname = PFUAggDatabase::sea_cols$agg_by_colname,
                             sector_colname = PFUAggDatabase::sea_cols$sector_colname,
                             e_product_colname = PFUAggDatabase::sea_cols$e_product_colname,
                             method_colname = IEATools::iea_cols$method,
                             energy_type_colname = IEATools::iea_cols$energy_type,
                             gross_net_colname = PFUAggDatabase::sea_cols$gross_net_colname,
                             ...) {

  stage_levels <- c("Primary", "Final", "Useful", "Services")
  .df$Stage <- factor(.df$Stage, levels = stage_levels)

  graph_data_filtered <- .df %>%
    dplyr::filter(.data[[IEATools::iea_cols$country]] == !!country_val) %>%
    dplyr::filter(.data[[IEATools::iea_cols$method]] == !!method_val) %>%
    dplyr::filter(.data[[IEATools::iea_cols$energy_type]] == !!energy_type_val) %>%
    dplyr::filter(.data[[PFUAggDatabase::sea_cols$gross_net_colname]] == !!gross_net_val)


  total_graph <- graph_data_filtered %>%
    dplyr::filter(.data[[agg_by_colname]] == "Total") %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = Year,
                                           y = E.dot,
                                           # x = .data[[year]],
                                           # y = .data[[e_dot]],
                                           # group = .data[[stage_colname]],
                                           # color = .data[[stage_colname]],
                                           group = Stage,
                                           color = Stage
                                           )) +
    ggplot2::geom_line() +
    ggplot2::ggtitle("Total Aggregations")

  sector_graph <- graph_data_filtered %>%
    dplyr::filter(.data[[agg_by_colname]] == "Sector") %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = Year,
                                           y = E.dot,
                                           # x = .data[[year]],
                                           # y = .data[[e_dot]],
                                           # group = .data[[stage_colname]],
                                           # color = .data[[stage_colname]],
                                           group = Stage,
                                           color = Stage
                                           )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data[[sector_colname]]),
                        scales = "free_y") +
    ggplot2::ggtitle("Sector Aggregations")

  product_graph <- graph_data_filtered %>%
    dplyr::filter(.data[[agg_by_colname]] == "Product") %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = Year,
                                           y = E.dot,
                                           # x = .data[[year]],
                                           # y = .data[[e_dot]],
                                           # group = .data[[stage_colname]],
                                           # color = .data[[stage_colname]],
                                           group = Stage,
                                           color = Stage
                                           )) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data[[e_product_colname]]),
                        scales = "free_y") +
    ggplot2::ggtitle("Product Aggregations")


  final_graph <- total_graph / sector_graph / product_graph

  final_graph <- final_graph +
    patchwork::plot_annotation(
      title = "PFU PSUT Aggregation Summary",
      subtitle = '',
      caption = ''
    )

  return(final_graph)

}


#' Create MW graphs in a data frame
#'
#'
#' @param .df An data frame containing aggregated PFU energy and exergy, usually
#'            produced by the `calculate_pfu_aggregations` function and stored
#'            as the target `...`.
#' @param country,year,e_dot See `IEATools::iea_cols`.
#'
#' @return A data frame containing a list column of `ggplot2` exergy-to-energy ratio graphs.
#'
#' @importFrom utils data
#'
#' @export
agg_plots_df <- function(.df,
                         country = IEATools::iea_cols$country,
                         year = IEATools::iea_cols$year,
                         e_dot = IEATools::iea_cols$e_dot,
                         country_colname = IEATools::iea_cols$country,
                         method_colname = IEATools::iea_cols$method,
                         energy_type_colname = IEATools::iea_cols$energy_type,
                         gross_net_colname = PFUAggDatabase::sea_cols$gross_net_colname){

  # Create a list containing the unique values for each of the unique metadata
  # identifiers
  vars_list <- list(z = unique(.df[[country_colname]]),
                    y = unique(.df[[method_colname]]),
                    x = unique(.df[[energy_type_colname]]),
                    w = unique(.df[[gross_net_colname]])
                    )

  # Create a data frame containing each combination of the variables
  cross_df <- purrr::cross_df(vars_list)

  # Convert the data frame to a list and remove the names
  # *Removing the names is necessary so each row can be referred to by index
  # (..1, ..2, etc) by the purrr::pmap() function.
  graph_input_variable_list <- cross_df %>% as.list() %>% unname()

  graph_df <- .df %>%

    # Nest the data in a dataframe identified by the unique metadata identifiers
    tidyr::nest(data = -c(country_colname, method_colname, energy_type_colname, gross_net_colname)) %>% # Gross.Net

    # Add a column containing a graph produced from each nested data frame
    dplyr::mutate(

      graph = purrr::pmap(

        # Supply the unnamed list of all possible combinations of variables
        .l = graph_input_variable_list,

        # Function to apply across list of variables
        .f = ~create_agg_graph(country_val = ..1,
                               method_val = ..2,
                               energy_type_val = ..3,
                               gross_net_val = ..4,
                               ...),

        # Additional static arguments to pass to function
        .df = .df)

    )

  return(graph_df)

}

