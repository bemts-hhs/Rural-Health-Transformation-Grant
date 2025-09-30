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

# Collapse transfer counts across all years (2020–2024)
transfer_counts_overall <- transfer_counts |>
  dplyr::group_by(Injury_County, Age_Group, Weight) |>
  dplyr::summarize(
    Count = sum(Count, na.rm = TRUE),
    County_Age_Population = sum(County_Age_Population, na.rm = TRUE),
    .groups = "drop"
  )

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

# Count transfers by year × age group × urbanicity ----
transfer_counts_urb <- trauma_data_clean |>
  dplyr::filter(
    grepl(pattern = "^ia$|iowa", x = Injury_State, ignore.case = TRUE),
    !is.na(Injury_County),
    !Injury_County %in%
      c(
        "Not Applicable",
        "Not Known",
        "Not Known / Not Recorded",
        "Not Known/NotRecorded",
        "Rock Island"
      ),
    Receiving == "Yes",
    grepl(
      pattern = "\\b(a|b)ls\\b|ambulance|\\bair\\b",
      x = Transport_To_Your_Facility_By,
      ignore.case = TRUE
    )
  ) |>
  injury_case_count(Year, Age_Group, Urbanicity_Injury) |>
  tidyr::complete(Year, Age_Group, Urbanicity_Injury, fill = list(n = 0L)) |>
  dplyr::arrange(Year, Age_Group, Urbanicity_Injury) |>
  dplyr::filter(Age_Group != "Missing")

# Build denominators by year × age × urbanicity ----
pop_by_urb <- age_group_pops_final |>
  dplyr::left_join(location_data, by = c("County")) |> # adds Urbanicity category
  dplyr::group_by(Year, Age_Group, Urbanicity) |>
  dplyr::summarize(
    Urbanicity_Age_Population = sum(County_Age_Population, na.rm = TRUE),
    .groups = "drop"
  )

# Merge counts with denominators + standard population ----
transfer_counts_urb <- transfer_counts_urb |>
  dplyr::left_join(
    pop_by_urb,
    by = c("Year", "Age_Group", "Urbanicity_Injury" = "Urbanicity")
  ) |>
  dplyr::left_join(us_age_pops_clean, by = "Age_Group") |>
  dplyr::rename(Count = n)

# Collapse across years for pooled 2020–2024 counts & denominators ----
transfer_counts_urb_total <- transfer_counts_urb |>
  dplyr::group_by(Age_Group, Urbanicity_Injury, Weight) |>
  dplyr::summarize(
    Count = sum(Count, na.rm = TRUE),
    Urbanicity_Age_Population = sum(Urbanicity_Age_Population, na.rm = TRUE),
    .groups = "drop"
  )

###_____________________________________________________________________________
# Transport data age adjustments ----
###_____________________________________________________________________________

# rates summarized by year and injury county ----
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

# rates summarized by injury county ----
# Calculate overall age-adjusted rates by county
transport_rates_overall <- transfer_counts_overall |>
  calc_age_adjusted_rate(
    count = Count,
    local_population = County_Age_Population,
    standard_population_weight = Weight,
    ci_method = "gamma", # or "normal", "poisson", "none"
    .by = "Injury_County"
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

# Calculate pooled age-adjusted rates by urbanicity
transport_rates_urb <- transfer_counts_urb_total |>
  calc_age_adjusted_rate(
    count = Count,
    local_population = Urbanicity_Age_Population,
    standard_population_weight = Weight,
    ci_method = "gamma",
    .by = "Urbanicity_Injury"
  )

###_____________________________________________________________________________
### Save the files to share with the grant team ----
###_____________________________________________________________________________

# county-level by year file
readr::write_csv(
  x = transport_rates,
  file = paste0(output_folder, "transport_rates.csv")
)

# county-level only file
readr::write_csv(
  x = transport_rates_overall,
  file = paste0(output_folder, "transport_rates_overall.csv")
)

# state-level by year file
readr::write_csv(
  x = iowa_transport_rate,
  file = paste0(output_folder, "iowa_transport_rate.csv")
)

# state-level by urbanicity
readr::write_csv(
  x = transport_rates_urb,
  file = paste0(output_folder, "transport_rates_urb.csv")
)
