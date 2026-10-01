library(readxl)
library(tmap)
library(dplyr)
library(BSol.mapR)
library(fuzzyjoin)

school_postcodes <- read.csv(
  "data/edubasealldata20260904.csv"
) %>%
  janitor::clean_names() %>%
  filter(
    la_name == "Birmingham",
    establishment_status_name == "Open"
  ) %>%
  select(urn, establishment_name, phase_of_education_name, postcode) %>%
  mutate(
    census_join = toupper(establishment_name),
    census_join = case_when(
      census_join == "ST JOSEPH'S CATHOLIC PRIMARY SCHOOL" ~
        paste0(census_join, 
               " (", stringr::str_extract(postcode, "B\\d{1,2}"), ")"
               ),
      TRUE ~ census_join
    )
  )

healthy_schools <- read_excel(
  "data/Healthy Schools evaluation 2526 - list of engaged schools.xlsx"
  ) %>%
  janitor::clean_names() %>%
  rename(
    engagement = l_l_portal_or_newsletter
  ) %>%
  mutate(
    join_name = toupper(school),
    join_name = case_when(
      join_name == "BOA CREATIVE, DIGITAL AND PERFORMING ARTS ACADEMY" ~
        "BIRMINGHAM ORMISTON ACADEMY",
      join_name %in% c("TOWN JUNIOR SCHOOL", "HOLLAND HOUSE INFANT SCHOOL AND NURSERY") ~
        "TOWN HOUSE PRIMARY SCHOOL",
      join_name == "BEECHES INFANT SCHOOL" ~ "BEECHES JUNIOR SCHOOL",
      TRUE ~ join_name
      )
  ) %>%
  stringdist_join(
    school_postcodes,
    by=join_by("join_name" == "census_join"),
    mode='left',
    method = "jw",
    max_dist=99,
    distance_col='dist') %>%
  group_by(school) %>%
  slice_min(order_by=dist, n=1)
  
## Save lookup

lookup <- healthy_schools %>%
  mutate(
    census_name = establishment_name
    ) %>%
  select(urn, school, postcode, census_name) %>%
  distinct()
writexl::write_xlsx(lookup, "output/healthy-school-lookup.xlsx")