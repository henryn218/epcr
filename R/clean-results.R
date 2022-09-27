#' Parses EPC API response
#'
#' @param resp A response object returned by \code{httr::GET}
#'
#' @return A list of class 'epc_api' containing a data frame holding the results of the
#' query ("contents") and a response object ("response")
#'
#' @keywords internal
#'
process_response <- function(parsed_resp) {

  if ("rows" %in% names(parsed_resp[["content"]])) {

    names(parsed_resp[["content"]][["rows"]]) <- gsub("-", "_",
                                                      names(parsed_resp[["content"]][["rows"]]),
                                                      fixed = TRUE)

    result_df <- set_data_types(parsed_resp[["content"]][["rows"]])

  } else {

    result_df <- data.frame()

  }

  structure(
    list(
      content = result_df,
      response = parsed_resp[["response"]]
    ),
    class = "epc_api"
  )

}

#' Sets data types in EPC API result data frame
#'
#' @param resp A data frame containing the results of an EPC API query
#'
#' @return A data frame containing the results of an EPC API query with data types
#' set according to the API documentation
#'
#' @keywords internal
#'
#' @importFrom purrr map imap_dfc
#'
set_data_types <- function(resp_contents) {

  data_types <- c(lmk_key = as.character, address1 = as.character, address2 = as.character,
                  address3 = as.character, postcode = as.character, building_reference_number = as.character,
                  current_energy_rating = as.character, potential_energy_rating = as.character,
                  current_energy_efficiency = as.integer, potential_energy_efficiency = as.integer,
                  property_type = as.character, built_form = as.character, inspection_date = as.Date,
                  local_authority = as.character, constituency = as.character, county = as.character,
                  lodgement_date = as.Date, transaction_type = as.character, environment_impact_current = as.integer,
                  environment_impact_potential = as.integer, energy_consumption_current = as.integer,
                  energy_consumption_potential = as.integer, co2_emissions_current = as.double,
                  co2_emiss_curr_per_floor_area = as.double, co2_emissions_potential = as.double,
                  lighting_cost_current = as.integer, lighting_cost_potential = as.integer,
                  heating_cost_current = as.integer, heating_cost_potential = as.integer,
                  hot_water_cost_current = as.integer, hot_water_cost_potential = as.integer,
                  total_floor_area = as.double, energy_tariff = as.character, mains_gas_flag = as.character,
                  floor_level = as.character, flat_top_storey = as.character, flat_storey_count = as.integer,
                  main_heating_controls = as.character, multi_glaze_proportion = as.integer,
                  glazed_type = as.character, glazed_area = as.character, extension_count = as.integer,
                  number_habitable_rooms = as.integer, number_heated_rooms = as.integer,
                  low_energy_lighting = as.integer, number_open_fireplaces = as.integer,
                  hotwater_description = as.character, hot_water_energy_eff = as.character,
                  hot_water_env_eff = as.character, floor_description = as.character,
                  floor_energy_eff = as.character, floor_env_eff = as.character,
                  windows_description = as.character, windows_energy_eff = as.character,
                  windows_env_eff = as.character, walls_description = as.character,
                  walls_energy_eff = as.character, walls_env_eff = as.character,
                  secondheat_description = as.character, sheating_energy_eff = as.character,
                  sheating_env_eff = as.character, roof_description = as.character,
                  roof_energy_eff = as.character, roof_env_eff = as.character, mainheat_description = as.character,
                  mainheat_energy_eff = as.character, mainheat_env_eff = as.character,
                  mainheatcont_description = as.character, mainheatc_energy_eff = as.character,
                  mainheatc_env_eff = as.character, lighting_description = as.character,
                  lighting_energy_eff = as.character, lighting_env_eff = as.character,
                  main_fuel = as.character, wind_turbine_count = as.integer, heat_loss_corridor = as.character,
                  unheated_corridor_length = as.double, floor_height = as.double,
                  photo_supply = as.integer, solar_water_heating_flag = as.character,
                  mechanical_ventilation = as.character, address = as.character,
                  local_authority_label = as.character, constituency_label = as.character,
                  posttown = as.character, construction_age_band = as.character,
                  lodgement_datetime = as.POSIXct, tenure = as.character, fixed_lighting_outlets_count = as.integer,
                  low_energy_fixed_light_count = as.integer, uprn = as.character, uprn_source = as.character,
                  asset_rating = as.integer, asset_rating_band = as.character, new_build_benchmark = as.character,
                  existing_stock_benchmark = as.character, building_level = as.character,
                  main_heating_fuel = as.character, other_fuel_desc = as.character,
                  special_energy_uses = as.character, renewable_sources = as.character,
                  floor_area = as.integer, standard_emissions = as.double, target_emissions = as.double,
                  typical_emissions = as.double, building_emissions = as.double,
                  aircon_present = as.character, aircon_kw_rating = as.integer, estimated_aircon_kw_rating = as.integer,
                  ac_inspection_commissioned = as.character, building_environment = as.character,
                  primary_energy_value = as.integer, current_operational_rating = as.double,
                  yr1_operational_rating = as.double, yr2_operational_rating = as.double,
                  operational_rating_band = as.character, electric_co2 = as.double,
                  heating_co2 = as.double, renewables_co2 = as.double, main_benchmark = as.character,
                  other_fuel = as.character, annual_thermal_fuel_usage = as.integer,
                  typical_thermal_fuel_usage = as.integer, annual_electrical_fuel_usage = as.integer,
                  typical_electrical_fuel_usage = as.integer, renewables_fuel_thermal = as.double,
                  renewables_electrical = as.double, yr1_electricity_co2 = as.double,
                  yr2_electricity_co2 = as.double, yr1_heating_co2 = as.double, yr2_heating_co2 = as.double,
                  yr1_renewables_co2 = as.double, yr2_renewables_co2 = as.double,
                  building_category = as.character, nominated_date = as.Date, or_assessment_end_date = as.Date,
                  occupancy_level = as.character, improvement_item = as.integer, indicative_cost = as.character,
                  improvement_summary_text  = as.character, improvement_descr_text = as.character,
                  improvement_id = as.integer, improvement_id_text = as.character)

  purrr::imap_dfc(resp_contents,
                  ~ {
                    if (any(.y == names(data_types))) {
                      type_fun <- data_types[[which(.y == names(data_types))]]
                      type_fun(.x)
                    } else {
                      warning("Column header not recognised, not setting data type")
                      .x
                    }
                  })

}

#' @importFrom utils head
#' @export
print.epc_api <- function(x, ...) {

  if (inherits(x[["response"]], "list")) {
    # For paginated searches
    do.call(cat,
            c("\nURLs:\n\n",
              lapply(x[["response"]], function(y) paste0(y[["url"]], "\n")),
              "\n\nResult:\n\n"))
  } else {
    # For single result queries (certificates or recommendations)
    cat("\nURL:\n\n ", x$response$url, "\n\nResult:\n\n")
  }

  print(utils::head(x$content))
  invisible(x)

}
