###_____________________________________________________________________________
### Load data for the 2024 Annual Trauma report project ----
### For any analyses, these data must be loaded for the applicable section
### You must run setup.R first before utilizing this script
###_____________________________________________________________________________

### Get environment variables ----

# paths for outputs
output_folder <- Sys.getenv("output_path")
death_path <- Sys.getenv("death_folder")

# clinical data

# trauma environment variables ----
trauma_data_path_2020 <- Sys.getenv("trauma_data_2020")
trauma_data_path_2021 <- Sys.getenv("trauma_data_2021")
trauma_data_path_2022 <- Sys.getenv("trauma_data_2022")
trauma_data_path_2023 <- Sys.getenv("trauma_data_2023")
trauma_data_path_2024 <- Sys.getenv("trauma_data_2024")

# ems environment variables ----
ems_data_path <- Sys.getenv("ems_data_folder")
ems_patient_path <- Sys.getenv("ems_patient_folder_2024")

# ipop inpatient environment variables ----
ipop_ip_data_path_2020 <- Sys.getenv("ipop_ip_data_2020")
ipop_ip_data_path_2021 <- Sys.getenv("ipop_ip_data_2021")
ipop_ip_data_path_2022 <- Sys.getenv("ipop_ip_data_2022")
ipop_ip_data_path_2023 <- Sys.getenv("ipop_ip_data_2023")
ipop_ip_data_path_2024 <- Sys.getenv("ipop_ip_data_2024")

# ipop outpatient environment variables ----
ipop_op_data_path_2020 <- Sys.getenv("ipop_op_data_2020")
ipop_op_data_path_2021 <- Sys.getenv("ipop_op_data_2021")
ipop_op_data_path_2022 <- Sys.getenv("ipop_op_data_2022")
ipop_op_data_path_2023 <- Sys.getenv("ipop_op_data_2023")
ipop_op_data_path_2024 <- Sys.getenv("ipop_op_data_2024")

# files for classification environment variables ----
mech_injury_path <- Sys.getenv("mech_injury_map")
injury_matrix_path <- Sys.getenv("injury_matrix")
iowa_counties_districts_path <- Sys.getenv("iowa_counties_districts")
hospital_data_path <- Sys.getenv("hospital_data_folder")

# population files environment variables ----
iowa_county_pops_path <- Sys.getenv("IOWA_COUNTY_POPS")
iowa_county_age_pops_path <- Sys.getenv("IOWA_COUNTY_AGE_POPS")
us_standard_pops_path <- Sys.getenv("US_STANDARD_POPS")
iowa_state_age_pops_path <- Sys.getenv("IOWA_STATE_POPS")

###_____________________________________________________________________________
# Load the files used to categorize mechanism and nature of injury ----
# based on the ICD-10 injury code
# NOTICE THAT IN ORDER TO GET THE SAME COUNTS AS IN TABLEAU WITH REGARD TO THE
# CAUSE OF INJURY / NATURE OF INJURY / body region (lvl1 and lvl2) you must run
# RUN distinct(Unique_Incident_ID, [coi_ar, cc2, body region], .keep_all = TRUE)
# and then your count() function or else you will not get the same counts in R.
# Tableau does a better job of automating the grouping via AI, and in R you have
# to do that manually.
###_____________________________________________________________________________

# mechanism of injury mapping ----
mechanism_injury_mapping <- readr::read_csv(
  file = mech_injury_path
)

# select variables of interest for mappings
mechanism_injury_mapping <- mechanism_injury_mapping |>
  dplyr::select(
    UPPER_CODE,
    INTENTIONALITY,
    CUSTOM_CATEGORY2,
    CAUSE_OF_INJURY_AR
  )

# nature of injury mapping ----
nature_injury_mapping <- readxl::read_excel(path = injury_matrix_path)

# select variables of interest for mappings
nature_injury_mapping <- nature_injury_mapping |>
  dplyr::select(
    ICD_10_CODE_TRIM,
    NATURE_OF_INJURY_DESCRIPTOR,
    BODY_REGION_CATEGORY_LEVEL_1,
    BODY_REGION_CATEGORY_LEVEL_2
  )

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# select variables of interest for Iowa county classification
location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)

# classify IPOP data using hospital information ----
hospital_data <- readxl::read_excel(path = hospital_data_path) |>
  janitor::clean_names(case = "screaming_snake")

### trauma data ----
trauma_data_2020 <- readr::read_csv(file = trauma_data_path_2020)
trauma_data_2021 <- readr::read_csv(file = trauma_data_path_2021)
trauma_data_2022 <- readr::read_csv(file = trauma_data_path_2022)
trauma_data_2023 <- readr::read_csv(file = trauma_data_path_2023)
trauma_data_2024 <- readr::read_csv(file = trauma_data_path_2024)

# union the trauma data
trauma_data <- dplyr::bind_rows(
  trauma_data_2020 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_data_2021 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_data_2022 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_data_2023 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_data_2024 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$"),
      ~ as.character(.)
    ))
)

# deal with missing values in cause of injury categories
trauma_data_clean <- trauma_data |>
  dplyr::mutate(
    Age_Group = dplyr::case_when(
      Patient_Age_Years < 5 ~ "0-4",
      Patient_Age_Years >= 5 & Patient_Age_Years < 10 ~ "5-9",
      Patient_Age_Years >= 10 & Patient_Age_Years < 15 ~ "10-14",
      Patient_Age_Years >= 15 & Patient_Age_Years < 20 ~ "15-19",
      Patient_Age_Years >= 20 & Patient_Age_Years < 25 ~ "20-24",
      Patient_Age_Years >= 25 & Patient_Age_Years < 30 ~ "25-29",
      Patient_Age_Years >= 30 & Patient_Age_Years < 35 ~ "30-34",
      Patient_Age_Years >= 35 & Patient_Age_Years < 40 ~ "35-39",
      Patient_Age_Years >= 40 & Patient_Age_Years < 45 ~ "40-44",
      Patient_Age_Years >= 45 & Patient_Age_Years < 50 ~ "45-49",
      Patient_Age_Years >= 50 & Patient_Age_Years < 55 ~ "50-54",
      Patient_Age_Years >= 55 & Patient_Age_Years < 60 ~ "55-59",
      Patient_Age_Years >= 60 & Patient_Age_Years < 65 ~ "60-64",
      Patient_Age_Years >= 65 & Patient_Age_Years < 70 ~ "65-69",
      Patient_Age_Years >= 70 & Patient_Age_Years < 75 ~ "70-74",
      Patient_Age_Years >= 75 & Patient_Age_Years < 80 ~ "75-79",
      Patient_Age_Years >= 80 & Patient_Age_Years < 85 ~ "80-84",
      Patient_Age_Years >= 85 ~ "85+",
      TRUE ~ "Missing",
      .default = "Missing"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Missing"
      )
    ),
    .after = Age_Range
  ) |>
  dplyr::mutate(
    Injury_County = stringr::str_to_title(Injury_County),
    Injury_County = dplyr::if_else(
      grepl(pattern = "o'b", x = Injury_County, ignore.case = TRUE),
      "O'Brien",
      Injury_County
    ),
    NATURE_OF_INJURY_DESCRIPTOR_1 = dplyr::if_else(
      is.na(NATURE_OF_INJURY_DESCRIPTOR_1),
      NATURE_OF_INJURY_DESCRIPTOR_2,
      NATURE_OF_INJURY_DESCRIPTOR_1
    ),
    BODY_REGION_CATEGORY_LEVEL_1_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_1_1),
      BODY_REGION_CATEGORY_LEVEL_1_2,
      BODY_REGION_CATEGORY_LEVEL_1_1
    ),
    BODY_REGION_CATEGORY_LEVEL_2_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_2_1),
      BODY_REGION_CATEGORY_LEVEL_2_2,
      BODY_REGION_CATEGORY_LEVEL_2_1
    ),
    BODY_REGION_CATEGORY_LEVEL_3_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_3_1),
      BODY_REGION_CATEGORY_LEVEL_3_2,
      BODY_REGION_CATEGORY_LEVEL_3_1
    ),
    INTENTIONALITY_1 = dplyr::if_else(
      is.na(INTENTIONALITY_1),
      INTENTIONALITY_2,
      INTENTIONALITY_1
    ),
    MECHANISM_1 = dplyr::if_else(is.na(MECHANISM_1), MECHANISM_2, MECHANISM_1),
    LEVEL_FALL1_1 = dplyr::if_else(
      is.na(LEVEL_FALL1_1),
      LEVEL_FALL1_2,
      LEVEL_FALL1_1
    ),
    CAUSE_OF_INJURY_AR_1 = dplyr::if_else(
      is.na(CAUSE_OF_INJURY_AR_1),
      CAUSE_OF_INJURY_AR_2,
      CAUSE_OF_INJURY_AR_1
    )
  ) |>
  dplyr::left_join(location_data, by = c("Patient_County" = "County")) |>
  dplyr::rename(
    Designation_Patient = Designation,
    Urbanicity_Patient = Urbanicity
  ) |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation_Patient", "Urbanicity_Patient")),
    .after = Patient_County
  ) |>
  dplyr::left_join(location_data, by = c("Injury_County" = "County")) |>
  dplyr::rename(
    Designation_Injury = Designation,
    Urbanicity_Injury = Urbanicity
  ) |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation_Injury", "Urbanicity_Injury")),
    .after = Injury_County
  ) |>
  dplyr::left_join(location_data, by = "County") |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation", "Urbanicity")),
    .after = County
  )

# get the trauma data for the year of interest
trauma_2024 <- trauma_data_clean |> dplyr::filter(Year == 2024)

# check the trauma data
dplyr::glimpse(trauma_2024)

### ems data ----
ems_data <- readr::read_csv(file = ems_data_path)
ems_patient <- readr::read_csv(file = ems_patient_path)

# deal with missing injury categories
ems_data_clean <- ems_data |>
  dplyr::mutate(Injury_1 = dplyr::if_else(is.na(Injury_1), Injury_2, Injury_1))

# check the ems data
dplyr::glimpse(ems_data_clean)

### ipop data ----

# inpatient ipop data ----
ipop_ip_data_2020 <- readr::read_csv(file = ipop_ip_data_path_2020)
ipop_ip_data_2021 <- readr::read_csv(file = ipop_ip_data_path_2021)
ipop_ip_data_2022 <- readr::read_csv(file = ipop_ip_data_path_2022)
ipop_ip_data_2023 <- readr::read_csv(file = ipop_ip_data_path_2023)
ipop_ip_data_2024 <- readr::read_csv(file = ipop_ip_data_path_2024)

### union the ipop inpatient data ----
ipop_ip_data <- dplyr::bind_rows(
  ipop_ip_data_2020,
  ipop_ip_data_2021,
  ipop_ip_data_2022,
  ipop_ip_data_2023,
  ipop_ip_data_2024
)

# clean inpatient ipop data ----
ipop_ip_data_clean <- ipop_ip_data |>
  dplyr::mutate(
    Inpatient_or_Outpatient = "Inpatient",
    Census_Age_Group = factor(
      Census_Age_Group,
      levels = c(
        "0 to 4",
        "5 to 9",
        "10 to 14",
        "15 to 19",
        "20 to 24",
        "25 to 29",
        "30 to 34",
        "35 to 39",
        "40 to 44",
        "45 to 49",
        "50 to 54",
        "55 to 59",
        "60 to 64",
        "65 to 69",
        "70 to 74",
        "75 to 79",
        "80 to 84",
        "85 And Over"
      ),
      labels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      )
    )
  ) |>
  dplyr::mutate(
    ICD_10_CODE_TRIM = dplyr::if_else(
      stringr::str_detect(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}\\d{1}"
      ),
      stringr::str_extract(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}\\d{1}"
      ),
      stringr::str_extract(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}[:alpha:]{1}[\\d{1}]?"
      )
    ),
    ICD_10_CODE_TRIM = stringr::str_squish(ICD_10_CODE_TRIM),
    .before = Diagnosis_ICD10_Raw
  ) |>
  dplyr::left_join(nature_injury_mapping, by = "ICD_10_CODE_TRIM") |>
  dplyr::mutate(
    Hospital_Number = as.character(Hospital_Number)
  ) |>
  dplyr::left_join(
    hospital_data,
    by = c("Hospital_Number" = "IPOP_FACILITY_NUMBER")
  ) |>
  dplyr::filter(!is.na(FACILITY_STATE_ID)) |>
  dplyr::mutate(
    Date_of_Service = lubridate::as_date(Date_of_Service), # problematic behavior with writing this data.frame to .csv with the DOS column going down as datetime
    Year = lubridate::year(Date_of_Service),
    Month = lubridate::month(Date_of_Service),
    .after = Date_of_Service
  )

# ingest outpatient ipop data ----
ipop_op_data_2020 <- readr::read_csv(file = ipop_op_data_path_2020)
ipop_op_data_2021 <- readr::read_csv(file = ipop_op_data_path_2021)
ipop_op_data_2022 <- readr::read_csv(file = ipop_op_data_path_2022)
ipop_op_data_2023 <- readr::read_csv(file = ipop_op_data_path_2023)
ipop_op_data_2024 <- readr::read_csv(file = ipop_op_data_path_2024)

# union the outpatient ipop data ----
ipop_op_data <- dplyr::bind_rows(
  ipop_op_data_2020 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    )),
  ipop_op_data_2021 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    )),
  ipop_op_data_2022 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    )),
  ipop_op_data_2023 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    )),
  ipop_op_data_2024 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    ))
)

# clean ipop data
ipop_op_data_clean <- ipop_op_data |>
  dplyr::mutate(
    Inpatient_or_Outpatient = "Outpatient",
    Census_Age_Group = factor(
      Census_Age_Group,
      levels = c(
        "0 to 4",
        "5 to 9",
        "10 to 14",
        "15 to 19",
        "20 to 24",
        "25 to 29",
        "30 to 34",
        "35 to 39",
        "40 to 44",
        "45 to 49",
        "50 to 54",
        "55 to 59",
        "60 to 64",
        "65 to 69",
        "70 to 74",
        "75 to 79",
        "80 to 84",
        "85 And Over"
      ),
      labels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      )
    )
  ) |>
  dplyr::mutate(
    ICD_10_CODE_TRIM = dplyr::if_else(
      stringr::str_detect(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}\\d{1}"
      ),
      stringr::str_extract(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}\\d{1}"
      ),
      stringr::str_extract(
        Diagnosis_ICD10_Raw,
        pattern = "[:alpha:]\\d{2}[:alpha:]{1}[\\d{1}]?"
      )
    ),
    ICD_10_CODE_TRIM = stringr::str_squish(ICD_10_CODE_TRIM),
    .before = Diagnosis_ICD10_Raw
  ) |>
  dplyr::left_join(nature_injury_mapping, by = "ICD_10_CODE_TRIM") |>
  dplyr::mutate(
    Hospital_Number = as.character(Hospital_Number)
  ) |>
  dplyr::left_join(
    hospital_data,
    by = c("Hospital_Number" = "IPOP_FACILITY_NUMBER")
  ) |>
  dplyr::filter(!is.na(FACILITY_STATE_ID)) |>
  dplyr::mutate(
    Date_of_Service = lubridate::as_date(Date_of_Service), # problematic behavior with writing this data.frame to .csv with the DOS column going down as datetime
    Year = lubridate::year(Date_of_Service),
    Month = lubridate::month(Date_of_Service),
    .after = Date_of_Service
  )

# union the IPOP data
ipop_data_clean <- dplyr::bind_rows(
  ipop_ip_data_clean |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("zip_code"),
      ~ as.character(.)
    )),
  ipop_op_data_clean
)

# check the ipop data
dplyr::glimpse(ipop_data_clean)

###_____________________________________________________________________________
# census bureau standard pops 2020-2024 census ----
# documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
###_____________________________________________________________________________

# 2020-2024 Census Bureau County population data ----
# get years for each county population
county_pops_all <- readr::read_csv(
  file = iowa_county_pops_path
)

# get columns of interest for the county-level population data ----
county_pops_select <- county_pops_all |>
  dplyr::filter(STATE == "19", COUNTY != "000") |>
  dplyr::select(County = CTYNAME, tidyselect::matches("popestimate\\d{4}$")) |>
  tidyr::pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "County_Population"
  ) |>
  dplyr::mutate(
    Year = stringr::str_remove(string = Year, pattern = "POPESTIMATE"),
    Year = forcats::as_factor(as.numeric(Year)),
    County = stringr::str_squish(stringr::str_remove_all(
      County,
      pattern = "\\sCounty"
    )),
    County = stringr::str_to_title(County),
    County = dplyr::if_else(
      grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
      "O'Brien",
      County
    )
  )

# Iowa county pops by age group ----

# ingest data
age_group_pops <- readr::read_csv(iowa_county_age_pops_path)

# 2020-2024 data Iowa county population data by age group
age_group_pops_final <- age_group_pops |>
  dplyr::select(
    CTYNAME,
    YEAR,
    tidyselect::matches(
      "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
      ignore.case = TRUE
    ),
    POPESTIMATE
  ) |>

  # Year #1 here is the base year at 4/1/2020
  dplyr::filter(YEAR != 1) |> # 7/1/2020 - 7/1/2024 pop estimates

  # Add 2018 to each year so that 2 == 2020, 3 == 2021, 4 == 2022, 5 == 2023,
  # and 6 == 2024, which is the intended meaning
  dplyr::mutate(YEAR = 2018 + YEAR) |>
  dplyr::rename(Year = YEAR) |>
  tidyr::pivot_longer(
    cols = AGE04_TOT:AGE85PLUS_TOT,
    names_to = "Age_Group",
    values_to = "County_Age_Population"
  ) |>
  dplyr::mutate(
    Age_Group = stringr::str_extract(Age_Group, pattern = "\\d+"),
    Age_Group = dplyr::if_else(
      Age_Group == "85",
      "85+",
      dplyr::if_else(
        nchar(Age_Group) == 2,
        paste0(
          stringr::str_sub(Age_Group, 1, 1),
          "-",
          stringr::str_sub(Age_Group, 2, 2)
        ),
        dplyr::if_else(
          nchar(Age_Group) == 4,
          paste0(
            stringr::str_sub(Age_Group, 1, 2),
            "-",
            stringr::str_sub(Age_Group, 3, 4)
          ),
          "Missing"
        )
      )
    ),
    CTYNAME = stringr::str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
  ) |>
  dplyr::rename(County = CTYNAME, County_Population = POPESTIMATE) |>
  dplyr::relocate(County_Population, .after = County_Age_Population)

###_____________________________________________________________________________
# age group populations for Iowa at the state (not county) level ----
# work with the sc-est[year]-agesex-civ.csv files for this via
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/asrh/
###_____________________________________________________________________________

# Iowa age groups at the state level for 2018-2022
# clean Iowa age group populations
# these are NOT standard populations
us_state_age_pops <- readr::read_csv(file = iowa_state_age_pops_path)

# get Iowa state age populations
state_age_group_pops <- us_state_age_pops |>

  # Iowa == State #19, and SEX == 0 is for totals not sex-specific
  dplyr::filter(STATE == 19, SEX == 0) |>
  dplyr::select(
    -c(SUMLEV, REGION, DIVISION, STATE, NAME, SEX, ESTBASE2020_CIV)
  ) |>
  tidyr::pivot_longer(
    cols = -AGE,
    names_to = "Year",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Year = as.numeric(stringr::str_extract(string = Year, pattern = "\\d+")),
    Age_Group = dplyr::if_else(
      AGE < 5,
      "0-4",
      dplyr::if_else(
        AGE >= 5 & AGE < 10,
        "5-9",
        dplyr::if_else(
          AGE >= 10 & AGE < 15,
          "10-14",
          dplyr::if_else(
            AGE >= 15 & AGE < 20,
            "15-19",
            dplyr::if_else(
              AGE >= 20 & AGE < 25,
              "20-24",
              dplyr::if_else(
                AGE >= 25 & AGE < 30,
                "25-29",
                dplyr::if_else(
                  AGE >= 30 & AGE < 35,
                  "30-34",
                  dplyr::if_else(
                    AGE >= 35 & AGE < 40,
                    "35-39",
                    dplyr::if_else(
                      AGE >= 40 & AGE < 45,
                      "40-44",
                      dplyr::if_else(
                        AGE >= 45 & AGE < 50,
                        "45-49",
                        dplyr::if_else(
                          AGE >= 50 & AGE < 55,
                          "50-54",
                          dplyr::if_else(
                            AGE >= 55 & AGE < 60,
                            "55-59",
                            dplyr::if_else(
                              AGE >= 60 & AGE < 65,
                              "60-64",
                              dplyr::if_else(
                                AGE >= 65 & AGE < 70,
                                "65-69",
                                dplyr::if_else(
                                  AGE >= 70 & AGE < 75,
                                  "70-74",
                                  dplyr::if_else(
                                    AGE >= 75 & AGE < 80,
                                    "75-79",
                                    dplyr::if_else(
                                      AGE >= 80 & AGE < 85,
                                      "80-84",
                                      dplyr::if_else(
                                        AGE >= 85 & AGE < 999,
                                        "85+",
                                        dplyr::if_else(
                                          AGE >= 999,
                                          "Total",
                                          "Error",
                                          missing = "Error"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Total",
        "Error"
      )
    )
  ) |>
  dplyr::select(Year, Age_Group, Population) |>
  dplyr::summarize(
    State_Population = sum(Population, na.rm = TRUE),
    .by = c(Year, Age_Group)
  )

# standard US populations
us_age_pops <- readr::read_tsv(
  file = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  col_names = FALSE
)

# clean US standard populations
us_age_pops_clean <- us_age_pops |>
  dplyr::filter(grepl(pattern = "^204", x = X1)) |>
  dplyr::mutate(
    Age_Group = stringr::str_sub(X1, start = 4, end = 6),
    Population = stringr::str_sub(X1, start = 7, end = 14),
    Population = as.numeric(stringr::str_remove(Population, pattern = "^0")),
    Weight = round(Population / sum(Population), digits = 6)
  ) |>
  dplyr::select(-X1) |>
  dplyr::mutate(
    Age_Group = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85+"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      )
    )
  ) |>
  dplyr::rename(US_Population = Population)
