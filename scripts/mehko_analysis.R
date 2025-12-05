#load packages
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(rlang)
library(readxl)
library(extrafont)
library(treemapify)

#setwd "mehko-data" and load fonts as needed with font_import()
#make sure to update path to relevant xlsx file
complaint_data <- read_excel("data/mehko_data.xlsx")

#transform numeric variables to integer
complaint_data <- complaint_data %>%
  mutate(across(matches("(?i)code|health"),
                ~ parse_integer(as.character(.), na = c("", "NA"))))

### total unique complaints ###
#sum complaints
total_complaints <- complaint_data %>%
  summarise(
    unique_code_pre    = sum(unique_code_pre,    na.rm = TRUE),
    unique_code_post   = sum(unique_code_post,   na.rm = TRUE),
    unique_health_pre  = sum(unique_health_pre,  na.rm = TRUE),
    unique_health_post = sum(unique_health_post, na.rm = TRUE)
  )

print(total_complaints, width = Inf)

total <- total_complaints %>% unlist(use.names = FALSE) %>% sum()
print(total)

# plot cumulative permit growth by year and jurisdiction
cumulative_permits_by_juris <- complaint_data %>%
  filter(!is.na(permit_year), permit_year != "NA") %>%
  mutate(permit_year = as.numeric(permit_year)) %>%
  filter(!is.na(permit_year)) %>%
  group_by(permit_year, juris) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(juris, permit_year) %>%
  group_by(juris) %>%
  mutate(cumulative = cumsum(n)) %>%
  ungroup()

# Get unique jurisdictions in order they appear
unique_juris <- unique(cumulative_permits_by_juris$juris)
n_juris <- length(unique_juris)

# Create color palette
color_palette <- colorRampPalette(c(
  "#008B8B",  # Teal dark
  "#2FA9A9",  # Teal medium
  "#5FC2C2",  # Teal light
  "#CC5500",  # Burnt Orange dark
  "#E67E22",  # Burnt Orange medium
  "#F39C12",  # Burnt Orange light
  "#B91930",  # Deeper Red dark
  "#C41E3A",  # Deep Red dark
  "#D4564A",  # Deep Red medium
  "#808080",  # Gray medium
  "#B0B0B0"   # Gray light
))(n_juris)

juris_colors <- setNames(color_palette, unique_juris)

# Plot
permit_growth_by_juris_plot <- cumulative_permits_by_juris %>%
  ggplot(aes(x = permit_year, y = cumulative, fill = juris)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = juris_colors) +
  labs(
    title = "Figure 1: MEHKO Permits by Jurisdiction",
    x = "Year",
    y = "Cumulative Permits Issued",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", face = "bold", size = 14, margin = margin(b = 10)),
    axis.title = element_text(family = "Georgia", size = 11),
    axis.text = element_text(family = "Georgia", size = 10),
    legend.title = element_blank(),
    legend.text = element_text(family = "Georgia", size = 9),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "#E8E8E8", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(permit_growth_by_juris_plot)

### geocode data ###
#create address field
complaint_data <- complaint_data %>%
  unite("address", address_street, address_city, address_zip, 
        sep = ", ", remove = FALSE, na.rm = TRUE)

# census geocoding
complaint_data <- complaint_data %>%
  geocode(address = address, method = "census", full_results = TRUE) %>%
  rename(id = `id...1`) %>%
  select(-`id...87`)

census_matched <- sum(!is.na(complaint_data$lat))

# OSM fallback for failed addresses
osm_needed <- complaint_data %>%
  filter(is.na(lat) | is.na(long)) %>%
  filter(address != "")

if (nrow(osm_needed) > 0) {
  
  osm_results <- osm_needed %>%
    geocode(address = address, method = "osm", full_results = TRUE, verbose = FALSE)
  
  # get OSM lat/long columns (may be renamed)
  lat_col_osm <- names(osm_results)[grep("^lat", names(osm_results))] %>% tail(1)
  long_col_osm <- names(osm_results)[grep("^long", names(osm_results))] %>% tail(1)
  
  osm_matched <- sum(!is.na(osm_results[[lat_col_osm]]))
  cat("OSM matched:", osm_matched, "of", nrow(osm_needed), "\n")
  
  # update complaint_data with OSM results
  if (osm_matched > 0) {
    complaint_data <- complaint_data %>%
      rows_update(
        osm_results %>%
          filter(!is.na(!!sym(lat_col_osm))) %>%
          select(id, !!sym(lat_col_osm), !!sym(long_col_osm)) %>%
          rename(lat = !!sym(lat_col_osm), long = !!sym(long_col_osm)),
        by = "id"
      )
  }
}

# arcGIS fallback for still-failed addresses
arcgis_needed <- complaint_data %>%
  filter(is.na(lat) | is.na(long)) %>%
  filter(address != "")

if (nrow(arcgis_needed) > 0) {
  
  arcgis_results <- arcgis_needed %>%
    geocode(address = address, method = "arcgis", full_results = TRUE, verbose = FALSE)
  
  # get ArcGIS lat/long columns (may be renamed)
  lat_col_arcgis <- names(arcgis_results)[grep("^lat", names(arcgis_results))] %>% tail(1)
  long_col_arcgis <- names(arcgis_results)[grep("^long", names(arcgis_results))] %>% tail(1)
  
  arcgis_matched <- sum(!is.na(arcgis_results[[lat_col_arcgis]]))
  cat("ArcGIS matched:", arcgis_matched, "of", nrow(arcgis_needed), "\n")
  
  # update complaint_data with ArcGIS results
  if (arcgis_matched > 0) {
    complaint_data <- complaint_data %>%
      rows_update(
        arcgis_results %>%
          filter(!is.na(!!sym(lat_col_arcgis))) %>%
          select(id, !!sym(lat_col_arcgis), !!sym(long_col_arcgis)) %>%
          rename(lat = !!sym(lat_col_arcgis), long = !!sym(long_col_arcgis)),
        by = "id"
      )
  }
}

# get CA tract urbanity classification
ca_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "CA",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(4326) %>%
  mutate(
    area_sqmi = as.numeric(st_area(geometry)) / 2.59e6,
    pop_density = value / area_sqmi,
    urbanity = case_when(
      pop_density >= 5000 ~ "Urban",
      pop_density >= 1000 ~ "Suburban",
      pop_density >= 100 ~ "Town",
      TRUE ~ "Rural"
    )
  ) %>%
  select(GEOID, urbanity, pop_density, geometry)

#spatial join to assign urbanity
complaint_sf <- complaint_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

complaint_joined <- st_join(complaint_sf, ca_tracts) %>%
  st_drop_geometry() %>%
  select(id, urbanity, pop_density)

#add urbanity back to main dataset
complaint_data <- complaint_data %>%
  left_join(complaint_joined, by = "id")

# summary
table(complaint_data$urbanity, useNA = "ifany")

#download and read in NCES locales
nces_locale <- st_read("C:/Users/wvg1/Downloads/edge_locale24_nces_CA/edge_locale24_nces_CA.shp") %>%
  st_make_valid() %>% 
  st_transform(4326)

#spatial join complaint data to NCES locales
complaint_sf <- complaint_data %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#NCES locale code decoder
nces_decoder <- tibble(
  LOCALE = c("11", "12", "13", "21", "22", "23", "31", "32", "33", "41", "42", "43"),
  locale_name = c(
    "City-Large", "City-Midsize", "City-Small",
    "Suburb-Large", "Suburb-Midsize", "Suburb-Small",
    "Town-Fringe", "Town-Distant", "Town-Remote",
    "Rural-Fringe", "Rural-Distant", "Rural-Remote"
  )
)

complaint_nces <- st_join(complaint_sf, nces_locale) %>%
  st_drop_geometry() %>%
  select(id, LOCALE) %>%
  left_join(nces_decoder, by = "LOCALE")

#add NCES locale back to main dataset
complaint_data <- complaint_data %>%
  left_join(complaint_nces %>% select(id, LOCALE, locale_name), by = "id")

#compare Census urbanity vs NCES locale
table(complaint_data$urbanity, complaint_data$locale_name, useNA = "ifany")

head(complaint_data %>% select(id, juris, address_city, urbanity, locale_name, pop_density))

#permits by NCES locale
permit_by_locale <- complaint_data %>%
  filter(!is.na(locale_name), permit_year != "NA") %>%
  group_by(locale_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    label = paste0(locale_name, " (", n, ", ", pct, "%)")
  ) %>%
  arrange(desc(n))

print(permit_by_locale)

#summary text with percentages
total_permits <- sum(permit_by_locale$n)

cat("Total permits:", total_permits, "\n\n")
cat("Permit distribution by NCES locale:\n")
for (i in 1:nrow(permit_by_locale)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", 
              permit_by_locale$locale_name[i], 
              permit_by_locale$n[i], 
              permit_by_locale$pct[i]))
}

# permits by locale group (city, suburb, town, rural)
permit_by_group <- permit_by_locale %>%
  mutate(
    locale_group = case_when(
      str_detect(locale_name, "^City") ~ "City",
      str_detect(locale_name, "^Suburb") ~ "Suburb",
      str_detect(locale_name, "^Town") ~ "Town",
      str_detect(locale_name, "^Rural") ~ "Rural"
    )
  ) %>%
  group_by(locale_group) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  ) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

for (i in 1:nrow(permit_by_group)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", 
              permit_by_group$locale_group[i], 
              permit_by_group$n[i], 
              permit_by_group$pct[i]))
}

### plot permits by NCES locale type ###

# prepare data
permit_data_for_bars <- permit_by_locale %>%
  mutate(
    locale_group = case_when(
      str_detect(locale_name, "^City") ~ "City",
      str_detect(locale_name, "^Suburb") ~ "Suburb",
      str_detect(locale_name, "^Town") ~ "Town",
      str_detect(locale_name, "^Rural") ~ "Rural"
    )
  ) %>%
  arrange(locale_group, desc(n))

# order locale_group for x-axis
locale_group_order <- c("City", "Suburb", "Town", "Rural")

# create short labels for bars (just the size category)
permit_data_for_bars <- permit_data_for_bars %>%
  mutate(
    size_label = str_remove(locale_name, "^[A-Za-z]+-"),
    locale_group = factor(locale_group, levels = locale_group_order),
    # Create shade mapping: Large/Fringe darkest, Midsize/Distant medium, Small/Remote lightest
    shade_key = case_when(
      size_label %in% c("Large", "Fringe") ~ "dark",
      size_label %in% c("Midsize", "Distant") ~ "medium",
      size_label %in% c("Small", "Remote") ~ "light"
    )
  )

# create color palette with shades
color_palette <- data.frame(
  locale_group = rep(c("City", "Suburb", "Town", "Rural"), each = 3),
  shade_key = rep(c("dark", "medium", "light"), 4),
  color = c(
    "#008B8B", "#2FA9A9", "#5FC2C2",  # City: teal shades
    "#CC5500", "#E67E22", "#F39C12",  # Suburb: burnt orange shades
    "#36454F", "#5A6B7D", "#8B8B7A",  # Town: slate blue shades
    "#DAA520", "#E8B84B", "#F0CC76"   # Rural: gold shades
  )
)

permit_data_for_bars <- permit_data_for_bars %>%
  left_join(color_palette, by = c("locale_group", "shade_key")) %>%
  mutate(locale_group = factor(locale_group, levels = locale_group_order))

# grouped bar chart with Modern Colorful palette and shade variations
grouped_bar_plot <- permit_data_for_bars %>%
  ggplot(aes(x = locale_group, y = n, fill = color)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = size_label), position = position_dodge(width = 0.9), 
            vjust = -0.5, family = "serif", size = 3) +
  scale_fill_identity() +
  labs(
    title = "Figure 2: MEHKO Permits by Locale Type",
    x = "Locale Type",
    y = "Number of Permits"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", face = "bold", size = 14, margin = margin(b = 10)),
    axis.title = element_text(family = "Georgia", size = 11),
    axis.text = element_text(family = "Georgia", size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "#E8E8E8", size = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(grouped_bar_plot)

### total unique complaints for mehko addresses ###
#sum complaints for mehkos
mehko_complaints <- complaint_data %>%
  filter(permit_year != "NA")

sum_mehko_complaints <- mehko_complaints %>%
  summarise(
    unique_code_pre    = sum(unique_code_pre,    na.rm = TRUE),
    unique_code_post   = sum(unique_code_post,   na.rm = TRUE),
    unique_health_pre  = sum(unique_health_pre,  na.rm = TRUE),
    unique_health_post = sum(unique_health_post, na.rm = TRUE)
  )

pre_permit_mehko_complaints <- mehko_complaints %>%
  summarise(
    unique_code_pre    = sum(unique_code_pre,    na.rm = TRUE),
    unique_health_pre  = sum(unique_health_pre,  na.rm = TRUE)
  )

pre_permit <- pre_permit_mehko_complaints %>% unlist(use.names = FALSE) %>% sum()
cat("Total pre-permit complaints about MEHKos:", pre_permit, "\n")
total <- sum_mehko_complaints %>% unlist(use.names = FALSE) %>% sum()
cat("Total complaints about MEHKos (including pre-permit):", total, "\n")

#sum complaints for never permitted businesses
unpermitted_complaints <- complaint_data %>%
  filter(permit_year == "NA")

sum_unpermitted_complaints <- unpermitted_complaints %>%
  summarise(
    unique_code_pre    = sum(unique_code_pre,    na.rm = TRUE),
    unique_code_post   = sum(unique_code_post,   na.rm = TRUE),
    unique_health_pre  = sum(unique_health_pre,  na.rm = TRUE),
    unique_health_post = sum(unique_health_post, na.rm = TRUE)
  )

total <- sum_unpermitted_complaints %>% unlist(use.names = FALSE) %>% sum()
cat("Total complaints about unpermitted businesses:", total, "\n")

### number of post-permit MEHKO complaints with no substantive issues identified ###
post_permit_cols <- c(
  "post_code_traffic",
  "post_code_traffic_actions",
  "post_code_nuisance",
  "post_code_nuisance_actions",
  "post_code_noise",
  "post_code_noise_actions",
  "post_code_alcohol",
  "post_code_alcohol_actions",
  "post_code_trash",
  "post_code_trash_actions",
  "post_code_building",
  "post_code_building_actions",
  "post_code_foodborne",
  "post_code_foodborne_actions",
  "post_health_traffic",
  "post_health_traffic_actions",
  "post_health_nuisance",
  "post_health_nuisance_actions",
  "post_health_noise",
  "post_health_noise_actions",
  "post_health_alcohol",
  "post_health_alcohol_actions",
  "post_health_trash",
  "post_health_trash_actions",
  "post_health_building",
  "post_health_building_actions",
  "post_health_foodborne",
  "post_health_foodborne_actions"
)

### number of complaints with no substantive issues identified ###
all_cols <- c(
  "pre_code_traffic",
  "pre_code_traffic_actions",
  "pre_code_nuisance",
  "pre_code_nuisance_actions",
  "pre_code_noise",
  "pre_code_noise_actions",
  "pre_code_alcohol",
  "pre_code_alcohol_actions",
  "pre_code_trash",
  "pre_code_trash_actions",
  "pre_code_building",
  "pre_code_building_actions",
  "pre_code_foodborne",
  "pre_code_foodborne_actions",
  "pre_health_traffic",
  "pre_health_traffic_actions",
  "pre_health_nuisance",
  "pre_health_nuisance_actions",
  "pre_health_noise",
  "pre_health_noise_actions",
  "pre_health_alcohol",
  "pre_health_alcohol_actions",
  "pre_health_trash",
  "pre_health_trash_actions",
  "pre_health_building",
  "pre_health_building_actions",
  "pre_health_foodborne",
  "pre_health_foodborne_actions",
  "post_code_traffic",
  "post_code_traffic_actions",
  "post_code_nuisance",
  "post_code_nuisance_actions",
  "post_code_noise",
  "post_code_noise_actions",
  "post_code_alcohol",
  "post_code_alcohol_actions",
  "post_code_trash",
  "post_code_trash_actions",
  "post_code_building",
  "post_code_building_actions",
  "post_code_foodborne",
  "post_code_foodborne_actions",
  "post_health_traffic",
  "post_health_traffic_actions",
  "post_health_nuisance",
  "post_health_nuisance_actions",
  "post_health_noise",
  "post_health_noise_actions",
  "post_health_alcohol",
  "post_health_alcohol_actions",
  "post_health_trash",
  "post_health_trash_actions",
  "post_health_building",
  "post_health_building_actions",
  "post_health_foodborne",
  "post_health_foodborne_actions"
)

#print number of unique total MEHKO complaints with no substantive issues (admin)
mehko_complaints %>%
  filter(if_all(all_of(all_cols), ~is.na(.x) | .x == 0)) %>%
  summarise(
    total_unique_code_post = sum(unique_code_post, na.rm = TRUE),
    total_unique_code_pre = sum(unique_code_pre, na.rm = TRUE),
    total_unique_health_post = sum(unique_health_post, na.rm = TRUE),
    total_unique_health_pre = sum(unique_health_pre, na.rm = TRUE)
  ) %>%
  mutate(grand_total = total_unique_code_post + total_unique_code_pre + 
           total_unique_health_post + total_unique_health_pre) %>%
  print(width = Inf)

#print number of unique post-permit MEHKO complaints with no substantive issues (admin)
mehko_complaints %>%
  filter(if_all(all_of(post_permit_cols), ~is.na(.x) | .x == 0)) %>%
  summarise(
    total_unique_code_post = sum(unique_code_post, na.rm = TRUE),
    total_unique_health_post = sum(unique_health_post, na.rm = TRUE)
  )

#print number of unique total complaints with no substantive issues
complaint_data %>%
  filter(if_all(all_of(all_cols), ~is.na(.x) | .x == 0)) %>%
  summarise(
    total_unique_code_post = sum(unique_code_post, na.rm = TRUE),
    total_unique_code_pre = sum(unique_code_pre, na.rm = TRUE),
    total_unique_health_post = sum(unique_health_post, na.rm = TRUE),
    total_unique_health_pre = sum(unique_health_pre, na.rm = TRUE)
  ) %>%
  mutate(grand_total = total_unique_code_post + total_unique_code_pre + 
           total_unique_health_post + total_unique_health_pre) %>%
  print(width = Inf)

### number of complaints resulting in agency response ###
action_cols <- names(complaint_data) %>%
  str_subset("(code|health).*_actions$")
action_cols

post_permit_action_cols <- names(complaint_data) %>%
  str_subset("(post_code|post_health).*_actions$")
post_permit_action_cols

#MEHKOs (all)
mehko_complaints %>%
  filter(if_any(all_of(action_cols), ~ !is.na(.) & . != 0)) %>%
  tally(name = "rows_any_action_nonzero")

#MEHKOs (post-permit)
mehko_complaints %>%
  filter(if_any(all_of(post_permit_action_cols), ~ !is.na(.) & . != 0)) %>%
  tally(name = "rows_any_action_nonzero")

#unpermitted
unpermitted_complaints %>%
  filter(if_any(all_of(action_cols), ~ !is.na(.) & . != 0)) %>%
  tally(name = "rows_any_action_nonzero")

###breaking it down more ###

#create complaint categories
pre_code_categories <- c("pre_code_admin", "pre_code_traffic", "pre_code_nuisance", 
                         "pre_code_noise", "pre_code_alcohol", "pre_code_trash", 
                         "pre_code_building", "pre_code_foodborne")
pre_health_categories <- c("pre_health_admin", "pre_health_traffic", "pre_health_nuisance", 
                           "pre_health_noise", "pre_health_alcohol", "pre_health_trash", 
                           "pre_health_building", "pre_health_foodborne")

post_code_categories <- c("post_code_admin", "post_code_traffic", "post_code_nuisance", 
                          "post_code_noise", "post_code_alcohol", "post_code_trash", 
                          "post_code_building", "post_code_foodborne")
post_health_categories <- c("post_health_admin", "post_health_traffic", "post_health_nuisance", 
                            "post_health_noise", "post_health_alcohol", "post_health_trash", 
                            "post_health_building", "post_health_foodborne")

# Custom y-axis labels
y_axis_labels <- c(
  "admin" = "Admin",
  "traffic" = "Traffic",
  "nuisance" = "Nuisance",
  "noise" = "Noise",
  "alcohol" = "Alcohol",
  "trash" = "Trash",
  "building" = "Building",
  "foodborne" = "Foodborne"
)

# function to calculate totals
calc_totals <- function(data, categories, prefix) {
  data %>%
    select(all_of(categories)) %>%
    summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
    mutate(
      category = str_remove(category, prefix),
      total = sum(count),
      percent = (count / total) * 100
    ) %>%
    arrange(desc(percent))
}

# pre-permit
pre_permit_data <- complaint_data %>%
  filter(!is.na(permit_year) & permit_year != "Unknown" & permit_year != "NA")

pre_permit_code <- calc_totals(pre_permit_data, pre_code_categories, "pre_code_")
pre_permit_health <- calc_totals(pre_permit_data, pre_health_categories, "pre_health_")

print(pre_permit_code, n = Inf)

print(pre_permit_health, n = Inf)


# post-permit
post_permit_code <- calc_totals(pre_permit_data, post_code_categories, "post_code_")
post_permit_health <- calc_totals(pre_permit_data, post_health_categories, "post_health_")

print(post_permit_code, n = Inf)

print(post_permit_health, n = Inf)

# unpermitted
unpermitted_data <- complaint_data %>%
  filter(permit_year == "NA" | is.na(permit_year))

unpermitted_code <- calc_totals(unpermitted_data, pre_code_categories, "pre_code_")
unpermitted_health <- calc_totals(unpermitted_data, pre_health_categories, "pre_health_")

print(unpermitted_code, n = Inf)

print(unpermitted_health, n = Inf)

# create combined summary for plotting
all_summaries <- bind_rows(
  pre_permit_code %>% mutate(status = "Pre-Permit", source = "Code"),
  pre_permit_health %>% mutate(status = "Pre-Permit", source = "Health"),
  post_permit_code %>% mutate(status = "Post-Permit", source = "Code"),
  post_permit_health %>% mutate(status = "Post-Permit", source = "Health"),
  unpermitted_code %>% mutate(status = "Unpermitted", source = "Code"),
  unpermitted_health %>% mutate(status = "Unpermitted", source = "Health")
) %>%
  select(status, source, category, count, percent)

# code enforcement plot
code_plot <- all_summaries %>%
  filter(source == "Code") %>%
  mutate(category_label = y_axis_labels[category]) %>%
  ggplot(aes(x = reorder(category_label, percent), y = percent, fill = status)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Code Enforcement Complaints by Permit Status",
    x = "Complaint Category",
    y = "Percentage (%)",
    fill = "Permit Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

print(code_plot)

# health complaints plot
health_plot <- all_summaries %>%
  filter(source == "Health") %>%
  mutate(category_label = y_axis_labels[category]) %>%
  ggplot(aes(x = reorder(category_label, percent), y = percent, fill = status)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Health Complaints by Permit Status",
    x = "Complaint Category",
    y = "Percentage (%)",
    fill = "Permit Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

print(health_plot)