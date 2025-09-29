###_____________________________________________________________________________
### Rural Health Transformation Grant Analyses
###_____________________________________________________________________________

###
# Rural / Urban interfacility transfers
###

###_____________________________________________________________________________
# Estimate age adjusted rates of interfacility transfer
###_____________________________________________________________________________

# get transfer counts by EMS and case counts by year, county, and age group ----
# transfers
transfer_counts <- trauma_data_clean |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
    !is.na(Injury_County),
    !Injury_County %in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/Not Recorded",
        "Rock Island"
      ),
    Receiving == "Yes",
    grepl(
      pattern = "\\b(a|b)ls\\b|ambulance|\\bair\\b",
      x = Transport_To_Your_Facility_By,
      ignore.case = TRUE
    )
  ) |>
  injury_case_count(Year, Injury_County, Age_Group) |>
  tidyr::complete(Year, Injury_County, Age_Group, fill = list(n = 0L)) |>
  dplyr::arrange(Year, Injury_County, Age_Group) |>
  dplyr::left_join(
    age_group_pops_final,
    by = c("Injury_County" = "County", "Age_Group", "Year")
  ) |>
  dplyr::rename(Count = n) |>
  dplyr::filter(Age_Group != "Missing") |>
  dplyr::left_join(us_age_pops_clean, by = "Age_Group")

# get transport counts and case counts by year and age group ----
# injury counts by age group - state level
iowa_transport_counts_age <- trauma_data_clean |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = T),
    !is.na(Injury_County),
    !Injury_County %in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/Not Recorded",
        "Rock Island"
      ),
    Receiving == "Yes",
    grepl(
      pattern = "\\b(a|b)ls\\b|ambulance|\\bair\\b",
      x = Transport_To_Your_Facility_By,
      ignore.case = TRUE
    )
  ) |>
  injury_case_count(Year, Age_Group) |>
  tidyr::complete(Year, Age_Group, fill = list(n = 0L)) |>
  dplyr::arrange(Year, Age_Group) |>
  dplyr::filter(Age_Group != "Missing") |>
  dplyr::left_join(state_age_group_pops, by = c("Age_Group", "Year")) |>
  dplyr::left_join(
    us_age_pops_clean,
    by = "Age_Group"
  ) |>
  dplyr::rename(Count = n)

###_____________________________________________________________________________
# Transport data age adjustments ----
###_____________________________________________________________________________

# rates summarized by year and patient home county ----
# injuries
transport_rates <- transfer_counts |>
  calc_age_adjusted_rate(
    count = Count,
    local_population = County_Age_Population,
    standard_population_weight = Weight,
    ci_method = "gamma",
    .by = c("Year", "Injury_County")
  ) |>
  dplyr::left_join(location_data, by = c("Injury_County" = "County"))

###_____________________________________________________________________________
# rates at the state level ----
###_____________________________________________________________________________

# transports at the state level
iowa_transport_rate <- iowa_transport_counts_age |>
  calc_age_adjusted_rate(
    count = Count,
    local_population = State_Population,
    standard_population_weight = Weight,
    .by = c("Year"),
    ci_method = "gamma",
    rate = 100000
  )

###_____________________________________________________________________________
### Save the files to share with the grant team ----
###_____________________________________________________________________________

# county-level file
readr::write_csv(
  x = transport_rates,
  file = paste0(output_folder, "transport_rates.csv")
)

# state-level file
readr::write_csv(
  x = iowa_transport_rate,
  file = paste0(output_folder, "iowa_transport_rate.csv")
)
