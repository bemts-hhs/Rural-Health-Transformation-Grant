###_____________________________________________________________________________
### Patient count overall for the state of Iowa 2024
###_____________________________________________________________________________

patient_clean <- ems_patient |>
  dplyr::filter(
    !`Agency Is Demo Service`
  ) |>
  dplyr::mutate(
    `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)` = stringr::str_squish(
      stringr::str_to_upper(
        `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`
      )
    ),
    `Patient Gender (ePatient.13)` = ifelse(
      is.na(`Patient Gender (ePatient.13)`),
      "Missing",
      `Patient Gender (ePatient.13)`
    ),
    `Patient Date Of Birth (ePatient.17)` = lubridate::mdy(
      stringr::str_remove(
        string = `Patient Date Of Birth (ePatient.17)`,
        pattern = "\\s\\d+:\\d+:\\d+\\s(?:A|P)M$"
      )
    ),
    `Patient Date Of Birth (ePatient.17)` = ifelse(
      is.na(`Patient Date Of Birth (ePatient.17)`),
      as.Date("2024-01-01"),
      `Patient Date Of Birth (ePatient.17)`
    ),
    `Patient Home Postal Code (ePatient.09)` = ifelse(
      is.na(
        `Patient Home Postal Code (ePatient.09)`
      ),
      "99999",
      `Patient Home Postal Code (ePatient.09)`
    )
  ) |>
  dplyr::mutate(
    Clean_EMS_Patient_ID = ifelse(
      is.na(`Patient EMS Patient ID (ePatient.01)`),
      stringr::str_c(
        `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`,
        `Patient Date Of Birth (ePatient.17)`,
        `Patient Gender (ePatient.13)`,
        `Patient Home Postal Code (ePatient.09)`
      ),
      `Patient EMS Patient ID (ePatient.01)`
    ),
    Unique_Patient_ID = stringr::str_c(
      `Patient Full Name (ePatient.03 - ePatient.04 - ePatient.02)`,
      `Patient Date Of Birth (ePatient.17)`,
      `Patient Gender (ePatient.13)`,
      `Patient Home Postal Code (ePatient.09)`
    ),
    .after = 1
  ) |>
  clean_ems_data(year_var = 2024)

# finish cleaning
patient_final <- patient_clean |>
  clean_county_names_1(
    county_column = new_county,
    city_column = new_city,
    zip_column = Patient_Home_Postal_Code
  ) |>
  clean_county_names_2(
    county_column = new_county,
    zip_column = Patient_Home_Postal_Code
  ) |>
  finalize_ems_data()

# get counts using the patient ID variable ----
patient_counts_ID <- patient_final |>
  dplyr::mutate(
    Designation = ifelse(is.na(Designation), "Out of State", Designation)
  ) |>
  dplyr::group_by(Designation) |>
  dplyr::distinct(Clean_EMS_Patient_ID, .keep_all = TRUE) |>
  dplyr::ungroup() |>
  dplyr::count(Designation, sort = TRUE) |>
  dplyr::mutate(percent = traumar::pretty_percent(n / sum(n), n_decimal = 2))

# get totals ----
patient_counts_totals <- patient_final |>
  dplyr::mutate(
    Designation = ifelse(is.na(Designation), "Out of State", Designation)
  ) |>
  dplyr::distinct(Clean_EMS_Patient_ID, .keep_all = TRUE) |>
  dplyr::count()
