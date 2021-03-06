#' Calculate efficiencies from an aggregates data frame
#'
#' This function calculates efficiencies (etas) from the aggregates data frame.
#' Aggregate energy and exergy are retained.
#'
#' This function removes the `tar_group` column.
#'
#' @param .aggregates The data frame from which efficiencies are to be calculated.
#'                    This data frame should be the output of the
#'                    `PSUT_Re_all_St_pfu` or `PSUT_Re_all_St_pfu_by_country`
#'                    targets.
#' @param stage_colname,ex_colname,agg_by_colname,e_product_colname,gross_net_colname,sector_colname See `PFUAggDatabase::sea_cols`.
#' @param eta_pf_colname,eta_fu_colname,eta_pu_colname See `PFUAggDatabase::efficiency_cols`.
#' @param total_value See `PFUAggDatabase::agg_metadata`.
#' @param gross,net See `PFUAggDatabase::gross_net_metadata`
#' @param primary,final,useful,country,year,method,energy_type,flow See `IEATools::iea_cols`.
#' @param tar_group The name of the targets grouping column in `.aggregates`.
#'                  This column is deleted on output.
#'
#' @return A data frame of aggregates and aggregate efficiencies.
#'
#' @export
calc_agg_etas <- function(.aggregates,
                          stage_colname = PFUAggDatabase::sea_cols$stage_colname,
                          ex_colname = PFUAggDatabase::sea_cols$ex_colname,
                          agg_by_colname = PFUAggDatabase::sea_cols$agg_by_colname,
                          e_product_colname = PFUAggDatabase::sea_cols$e_product_colname,
                          gross_net_colname = PFUAggDatabase::sea_cols$gross_net_colname,
                          sector_colname = PFUAggDatabase::sea_cols$sector_colname,

                          eta_pf_colname = PFUAggDatabase::efficiency_cols$eta_pf,
                          eta_fu_colname = PFUAggDatabase::efficiency_cols$eta_fu,
                          eta_pu_colname = PFUAggDatabase::efficiency_cols$eta_pu,

                          gross = PFUAggDatabase::gross_net_metadata$gross,
                          net = PFUAggDatabase::gross_net_metadata$net,

                          total_value = PFUAggDatabase::agg_metadata$total_value,

                          primary = IEATools::all_stages$primary,
                          final = IEATools::all_stages$final,
                          useful = IEATools::all_stages$useful,
                          country = IEATools::iea_cols$country,
                          year = IEATools::iea_cols$year,
                          method = IEATools::iea_cols$method,
                          energy_type = IEATools::iea_cols$energy_type,
                          flow = IEATools::iea_cols$flow,

                          tar_group = "tar_group") {

  # Filter .aggregates to only Aggregation.by == "Total", because that's the only
  # way it makes sense to do aggregate efficiencies.


  # Duplicate the Primary stage information for both net and gross
  prim <- .aggregates %>%
    dplyr::filter(.data[[agg_by_colname]] == total_value,
                  .data[[stage_colname]] == primary)
  prim_net <- prim %>%
    dplyr::mutate(
      "{gross_net_colname}" := net
    )
  prim_gross <- prim %>%
    dplyr::mutate(
      "{gross_net_colname}" := gross
    )
  wide_primary <- dplyr::bind_rows(prim_gross, prim_net) %>%
    dplyr::mutate(
      # Eliminate unneeded columns
      "{e_product_colname}" := NULL,
      "{flow}" := NULL,
      "{agg_by_colname}" := NULL,
      "{sector_colname}" := NULL,
      "{tar_group}" := NULL
    ) %>%
    # Put the primary energy into a column.
    tidyr::pivot_wider(names_from = stage_colname, values_from = ex_colname)

  # Build the final and useful data frame.
  wide_finaluseful <- .aggregates %>%
    dplyr::filter(.data[[agg_by_colname]] == total_value,
                  .data[[stage_colname]] %in% c(final, useful)) %>%
    dplyr::mutate(
      # Eliminate unneeded columns
      "{e_product_colname}" := NULL,
      "{flow}" := NULL,
      "{agg_by_colname}" := NULL,
      "{sector_colname}" := NULL,
      "{tar_group}" := NULL
    ) %>%
    # Put the final and useful energy into a column.
    tidyr::pivot_wider(names_from = stage_colname, values_from = ex_colname)

  # Join the data frames
  dplyr::full_join(wide_primary, wide_finaluseful,
                   by = c(country, year, method, energy_type, gross_net_colname)) %>%
    dplyr::mutate(
      "{eta_pf_colname}" := .data[[final]] / .data[[primary]],
      "{eta_fu_colname}" := .data[[useful]] / .data[[final]],
      "{eta_pu_colname}" := .data[[useful]] / .data[[primary]]
    )
}


#' Pivot and write aggregate efficiencies to an Excel file
#'
#' The incoming data frame is expected to contain
#' a `year` column as well as
#' `primary`, `final`, `useful`, `eta_pf`, `eta_fu`, and `eta_pu` columns.
#'
#' @param .agg_etas A data frame created by the `eta_Re_all_St_pfu` target.
#' @param path The path where the Excel file will be saved.
#' @param aggs_tabname,etas_tabname See `PFUAggDatabase::output_file_info`.
#' @param wide_by_year If `TRUE` (the default), the incoming data frame will be pivoted to be wide by years.
#'                     If `FALSE`, data will be unchanged.
#' @param primary,final,useful See `IEATools::all_stages`.
#' @param eta_pf_colname,eta_fu_colname,eta_pu_colname See `PFUAggDatabase::efficiency_cols`.
#' @param year,country See `IEATools::iea_cols`.
#' @param quantity,.values See `IEATools::template_cols`.
#'
#' @return `TRUE` if the file was written successfully.
#'
#' @export
write_agg_etas_xlsx <- function(.agg_etas,
                                path,
                                aggs_tabname = PFUAggDatabase::output_file_info$agg_tabname,
                                etas_tabname = PFUAggDatabase::output_file_info$eta_tabname,
                                wide_by_year = TRUE,
                                primary = IEATools::all_stages$primary,
                                final = IEATools::all_stages$final,
                                useful = IEATools::all_stages$useful,
                                eta_pf_colname = PFUAggDatabase::efficiency_cols$eta_pf,
                                eta_fu_colname = PFUAggDatabase::efficiency_cols$eta_fu,
                                eta_pu_colname = PFUAggDatabase::efficiency_cols$eta_pu,
                                year = IEATools::iea_cols$year,
                                country = IEATools::iea_cols$country,
                                quantity = IEATools::template_cols$quantity,
                                .values = IEATools::template_cols$.values) {
  agg_df <- .agg_etas %>%
    dplyr::mutate(
      "{eta_pf_colname}" := NULL,
      "{eta_fu_colname}" := NULL,
      "{eta_pu_colname}" := NULL
    )
  eta_df <- .agg_etas %>%
    dplyr::mutate(
      "{primary}" := NULL,
      "{final}" := NULL,
      "{useful}" := NULL
    )
  if (wide_by_year) {
    agg_df <- agg_df %>%
      tidyr::pivot_longer(cols = c(primary, final, useful),
                          names_to = quantity,
                          values_to = .values) %>%
      dplyr::arrange(.data[[year]]) %>%
      tidyr::pivot_wider(names_from = year, values_from = .values) %>%
      dplyr::arrange(.data[[country]])
    eta_df <- eta_df %>%
      tidyr::pivot_longer(cols = c(eta_pf_colname, eta_fu_colname, eta_pu_colname),
                          names_to = quantity,
                          values_to = .values) %>%
      dplyr::arrange(.data[[year]]) %>%
      tidyr::pivot_wider(names_from = year, values_from = .values) %>%
      dplyr::arrange(.data[[country]])
  }

  writexl::write_xlsx(list(agg_df, eta_df) %>%
                        magrittr::set_names(c(aggs_tabname, etas_tabname)),
                      path = path)
  return(TRUE)
}
