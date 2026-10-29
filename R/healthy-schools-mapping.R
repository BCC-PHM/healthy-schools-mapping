library(readxl)
library(tmap)
library(dplyr)
library(BSol.mapR)
library(fuzzyjoin)
library(bcctheme)

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
  ) %>%
  rbind(
    data.frame(
      urn = c(149305, 142230, 141206, 103164),
      establishment_name = c(
        "Holland House Infant School and Nursery",
        "Prince Albert Primary School",
        "Town Junior School",
        "Beeches Infant School"),
      phase_of_education_name = c("Nursery", "Primary", "Primary", "Infant"),
      postcode = c("B72 1RE", "B6 5NH",  "B72 1NX", "B42 2PY"),
      census_join = c(
        "HOLLAND HOUSE INFANT SCHOOL AND NURSERY",
        "PRINCE ALBERT PRIMARY SCHOOL",
        "TOWN JUNIOR SCHOOL",
        "BEECHES INFANT SCHOOL")
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
      # Not changing infant school as requested by CYP team
      #join_name == "BEECHES INFANT SCHOOL" ~ "BEECHES JUNIOR SCHOOL",
      TRUE ~ join_name
      ),
    engagement = case_when(
      engagement == "L&L" ~ "Lunch & Learn",
      TRUE ~ engagement
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
  slice_min(order_by=dist, n=1) %>%
  rbind(
    read_excel(
      "data/Healthy Schools evaluation - list of unengaged schools 26.10.26.xlsx"
    ) %>%
      mutate(
        establishment_name = school,
        engagement = "None"
      )
  )
  
## Save lookup

lookup <- healthy_schools %>%
  mutate(
    census_name = establishment_name
    ) %>%
  select(urn, school, postcode, census_name) %>%
  distinct()
writexl::write_xlsx(lookup, "output/healthy-school-lookup.xlsx")

## Mapping

healthy_schools <- healthy_schools %>%
  select(school, engagement, urn, school_type, postcode)

engagement_type <- unique(healthy_schools$engagement)

for (engagement_i in engagement_type) {
  healthy_schools_i <- healthy_schools %>%
    rename(Postcode = postcode) %>%
    filter(
      engagement == engagement_i
    )
  
  map <- plot_empty_map(
    map_title =  "",
    area_name = "Birmingham",
    map_type = "Ward"
  ) +
    tm_layout(title.size = 16) +
    tm_title(
      paste(
        "Schools Participating in Healthy Schools via:", 
        engagement_i),
      size = 1.0,
      position = c("top","left"),
      padding = c(-1, 0, 0, 0)
    )
  
  map <- add_points(
    map,
    healthy_schools_i,
    color = bcc_cols("orange")
  )
  
  save_name_prefix <- paste0(
    "output/maps/hs-",
    gsub("&", "and",gsub(" ", "-", tolower(engagement_i)))
    )
  
  # Save as static image
  save_map(map, paste0(save_name_prefix, ".png"))
  # Save as interactive html
  save_map(map, paste0(save_name_prefix, ".html"))
}

none_n <- sum(healthy_schools$engagement == "None", na.rm = TRUE)

# Only actual engagement themes go into the Venn diagram
x <- healthy_schools |>
  subset(engagement != "None") |>
  with(split(school, engagement))

## Venn Diagram
library(ggVennDiagram)
library(ggplot2)
venn_plt <- ggVennDiagram(x,label_alpha = 0, edge_size=0.7, label = "count") +
  scale_fill_gradient(low = "white", high = bcc_cols("purple"), guide = NULL) +
  theme_void() +
  coord_sf(
    xlim = c(-5.5, 9.5),
    ylim = c(-9, 6),
    clip = "off"
    ) +
  theme(plot.margin = unit(rep(1, 4), "cm"),
        panel.background = element_rect(fill = 'white')) +
  annotate(
    "text",
    x = 2,
    y = 5.5,
    label = paste0("n = ", none_n, "\nNot engaged"),
    size = 4.4
  ) 
venn_plt
ggsave("output/engagement-venn.png", venn_plt)


## New map

healthy_schools_agg <- healthy_schools %>%
  group_by(school, postcode) %>%
  summarise(
    `Engagement Types` = paste(
      sort(unique(na.omit(engagement))),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    `Engagement Types` = if_else(
      `Engagement Types` == "",
      "None",
      `Engagement Types`
    ),
    `Engagement Types` = factor(
      `Engagement Types`,
          levels = c(
            "Newsletter",
            "Lunch & Learn",
            "Portal",
            "Lunch & Learn; Newsletter",
            "Newsletter; Portal",
            "Lunch & Learn; Portal",
            "Lunch & Learn; Newsletter; Portal",
            "None"
            )
          )
    ) %>%
  rename(
    Postcode = postcode
  )

map <- plot_empty_map(
  map_title =  "",
  area_name = "Birmingham",
  map_type = "Ward"
)

map <- add_points(
  map,
  healthy_schools_agg,
  color = "Engagement Types",
  palette = c("#84329B", "#D00070", "#DC582A", "#FFAD00", "#75BC22", "#00A9E0", "#3c3c3b", "#FFFFFF" )
) 

map

# Save as static image
save_map(map, "output/engagement-map.png")