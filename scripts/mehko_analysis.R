#load packages
library(tidygeocoder)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(rlang)
library(readxl)
library(showtext)
library(treemapify)
library(ragg)

#setwd "mehko-data" and download Source Serif 4 font if needed
#make sure to update path to relevant xlsx file
complaint_data <- read_excel("data/mehko_data.xlsx")

#load Source Serif and Arial fonts if needed
font_add("Source Serif 4", 
         regular = "fonts/SourceSerif4-Regular.ttf")
font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf")
showtext_auto()

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

#sum of unpermitted and MEHKOs with complaints
business_counts <- complaint_data %>%
  summarise(
    businesses_with_pre_complaints  = sum((unique_code_pre > 0 | unique_health_pre > 0), na.rm = TRUE),
    businesses_with_post_complaints = sum((unique_code_post > 0 | unique_health_post > 0), na.rm = TRUE)
  )

print(business_counts, width = Inf)

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

### figure 1: Community Impact Complaints ###

# define complaint and action columns
all_complaint_cols <- c(
  "pre_code_traffic", "pre_code_nuisance", "pre_code_noise", "pre_code_alcohol",
  "pre_code_trash", "pre_code_building", "pre_code_foodborne",
  "pre_health_traffic", "pre_health_nuisance", "pre_health_noise", "pre_health_alcohol",
  "pre_health_trash", "pre_health_building", "pre_health_foodborne",
  "post_code_traffic", "post_code_nuisance", "post_code_noise", "post_code_alcohol",
  "post_code_trash", "post_code_building", "post_code_foodborne",
  "post_health_traffic", "post_health_nuisance", "post_health_noise", "post_health_alcohol",
  "post_health_trash", "post_health_building", "post_health_foodborne"
)

all_action_cols <- c(
  "pre_code_traffic_actions", "pre_code_nuisance_actions", "pre_code_noise_actions",
  "pre_code_alcohol_actions", "pre_code_trash_actions", "pre_code_building_actions",
  "pre_code_foodborne_actions",
  "pre_health_traffic_actions", "pre_health_nuisance_actions", "pre_health_noise_actions",
  "pre_health_alcohol_actions", "pre_health_trash_actions", "pre_health_building_actions",
  "pre_health_foodborne_actions",
  "post_code_traffic_actions", "post_code_nuisance_actions", "post_code_noise_actions",
  "post_code_alcohol_actions", "post_code_trash_actions", "post_code_building_actions",
  "post_code_foodborne_actions",
  "post_health_traffic_actions", "post_health_nuisance_actions", "post_health_noise_actions",
  "post_health_alcohol_actions", "post_health_trash_actions", "post_health_building_actions",
  "post_health_foodborne_actions"
)

# calculate totals for unpermitted (includes never-permitted and pre-permit)
unpermitted_complaints_total <- complaint_data %>%
  select(all_of(all_complaint_cols)) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  unlist() %>% sum()

unpermitted_actions_total <- complaint_data %>%
  select(all_of(all_action_cols)) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  unlist() %>% sum()

# MEHKOs (permitted - post-permit only)
mehko_data <- complaint_data %>%
  filter(!is.na(permit_year) & permit_year != "Unknown" & permit_year != "NA")

mehko_complaints_total <- mehko_data %>%
  select(all_of(c(
    "post_code_traffic", "post_code_nuisance", "post_code_noise", "post_code_alcohol",
    "post_code_trash", "post_code_building", "post_code_foodborne",
    "post_health_traffic", "post_health_nuisance", "post_health_noise", "post_health_alcohol",
    "post_health_trash", "post_health_building", "post_health_foodborne"
  ))) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  unlist() %>% sum()

mehko_actions_total <- mehko_data %>%
  select(all_of(c(
    "post_code_traffic_actions", "post_code_nuisance_actions", "post_code_noise_actions",
    "post_code_alcohol_actions", "post_code_trash_actions", "post_code_building_actions",
    "post_code_foodborne_actions",
    "post_health_traffic_actions", "post_health_nuisance_actions", "post_health_noise_actions",
    "post_health_alcohol_actions", "post_health_trash_actions", "post_health_building_actions",
    "post_health_foodborne_actions"
  ))) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  unlist() %>% sum()

# create dataframe for plotting
figure_1_data <- tibble(
  business_type = c("Unpermitted", "Unpermitted", 
                    "MEHKOs", "MEHKOs"),
  complaint_type = c("Complaints", "Actions",
                     "Complaints", "Actions"),
  count = c(unpermitted_complaints_total, unpermitted_actions_total,
            mehko_complaints_total, mehko_actions_total)
)

# create color palette with strong contrast between complaints and actions
color_palette_fig1 <- tibble(
  business_type = c("Unpermitted", "Unpermitted",
                    "MEHKOs", "MEHKOs"),
  complaint_type = c("Complaints", "Actions",
                     "Complaints", "Actions"),
  color = c(
    "#008B8B", "#CC5500",  # Unpermitted: Teal (complaints), Burnt Orange (actions)
    "#008B8B", "#CC5500"   # MEHKOs: Teal (complaints), Burnt Orange (actions)
  )
)

figure_1_data <- figure_1_data %>%
  left_join(color_palette_fig1, by = c("business_type", "complaint_type")) %>%
  mutate(complaint_type = factor(complaint_type, levels = c("Complaints", "Actions")))

# create grouped bar chart
figure_1 <- figure_1_data %>%
  ggplot(aes(x = factor(business_type, levels = c("Unpermitted", "MEHKOs")),
             y = count,
             fill = color,
             group = complaint_type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = complaint_type), position = position_dodge(width = 0.9),
            vjust = -0.5, family = "Source Serif 4", size = 32) +
  scale_fill_identity() +
  labs(
    title = "Figure 1: Community Impact Complaints and Agency Actions",
    x = "Business Type",
    y = "Count",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 160, margin = margin(b = 40), hjust = 0.5),
    axis.title = element_text(family = "Source Serif 4", size = 120),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text = element_text(family = "Source Serif 4", size = 90),
    axis.text.x = element_text(margin = margin(t = 20)),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure1.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(figure_1)
dev.off()

### figure 2: complaint types for MEHKOs and unpermitted businesses ###

# define complaint type categories (including admin)
complaint_categories <- c("traffic", "nuisance", "noise", "alcohol", "trash", "building", "foodborne", "admin")

# color palette
color_palette_complaints <- c(
  "#808080",  # Gray medium - admin
  "#008B8B",  # Teal dark - traffic
  "#2FA9A9",  # Teal medium - nuisance
  "#5FC2C2",  # Teal light - noise
  "#CC5500",  # Burnt Orange dark - alcohol
  "#E67E22",  # Burnt Orange medium - trash
  "#F39C12",  # Burnt Orange light - building
  "#B91930"   # Deep Red dark - foodborne
)

names(color_palette_complaints) <- complaint_categories

# function to calculate complaint totals by type
calc_complaint_totals <- function(data, pre_cols, post_cols = NULL) {
  if (!is.null(post_cols)) {
    # for permitted MEHKOs (post-permit)
    all_cols <- c(pre_cols, post_cols)
  } else {
    # for never-permitted and pre-permit (only pre columns)
    all_cols <- pre_cols
  }
  
  data %>%
    select(all_of(all_cols)) %>%
    summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "col_name", values_to = "count") %>%
    mutate(
      complaint_type = case_when(
        str_detect(col_name, "admin") ~ "admin",
        str_detect(col_name, "traffic") ~ "traffic",
        str_detect(col_name, "nuisance") ~ "nuisance",
        str_detect(col_name, "noise") ~ "noise",
        str_detect(col_name, "alcohol") ~ "alcohol",
        str_detect(col_name, "trash") ~ "trash",
        str_detect(col_name, "building") ~ "building",
        str_detect(col_name, "foodborne") ~ "foodborne"
      )
    ) %>%
    group_by(complaint_type) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    arrange(factor(complaint_type, levels = complaint_categories))
}

# MEHKOs (post-permit)
mehko_data <- complaint_data %>%
  filter(!is.na(permit_year) & permit_year != "Unknown" & permit_year != "NA")

post_cols <- c(
  "post_code_admin", "post_code_traffic", "post_code_nuisance", "post_code_noise", "post_code_alcohol",
  "post_code_trash", "post_code_building", "post_code_foodborne",
  "post_health_admin", "post_health_traffic", "post_health_nuisance", "post_health_noise", "post_health_alcohol",
  "post_health_trash", "post_health_building", "post_health_foodborne"
)

mehko_complaints <- calc_complaint_totals(mehko_data, post_cols)
mehko_complaints$business_type <- "MEHKOs"

# unpermitted (all - pre-permit and never-permitted combined)
unpermitted_data <- complaint_data %>%
  filter(permit_year == "NA" | is.na(permit_year))

pre_cols <- c(
  "pre_code_admin", "pre_code_traffic", "pre_code_nuisance", "pre_code_noise", "pre_code_alcohol",
  "pre_code_trash", "pre_code_building", "pre_code_foodborne",
  "pre_health_admin", "pre_health_traffic", "pre_health_nuisance", "pre_health_noise", "pre_health_alcohol",
  "pre_health_trash", "pre_health_building", "pre_health_foodborne"
)

unpermitted_complaints <- calc_complaint_totals(unpermitted_data, pre_cols)
unpermitted_complaints$business_type <- "Unpermitted"

# combine all data
all_complaints_data <- bind_rows(
  unpermitted_complaints,
  mehko_complaints
) %>%
  mutate(
    business_type = factor(business_type, levels = c("Unpermitted", "MEHKOs")),
    complaint_type = factor(complaint_type, levels = complaint_categories)
  )

# create stacked bar chart
figure_2 <- all_complaints_data %>%
  ggplot(aes(x = business_type, 
             y = count, fill = complaint_type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = color_palette_complaints, 
                    labels = c("traffic" = "Traffic", "nuisance" = "Nuisance", 
                               "noise" = "Noise", "alcohol" = "Alcohol",
                               "trash" = "Trash", "building" = "Building", 
                               "foodborne" = "Foodborne", "admin" = "No Substantive Complaint")) +
  labs(
    title = "Figure 2: Complaint Types",
    x = "Business Type",
    y = "Total Complaints",
    fill = "Complaint Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 180, margin = margin(b = 20), hjust = 0.5),
    axis.title = element_text(family = "Source Serif 4", size = 140),
    axis.text = element_text(family = "Source Serif 4", size = 120),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    legend.text = element_text(family = "Source Serif 4", size = 90),
    legend.title = element_text(family = "Source Serif 4", size = 120, margin = margin(b = 10)),
    legend.key.spacing.y = unit(10, "pt"),
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure2.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(figure_2)
dev.off()

### figure 3: permit growth over time ###

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

# plot
permit_growth_by_juris_plot <- cumulative_permits_by_juris %>%
  ggplot(aes(x = permit_year, y = cumulative, fill = juris)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = juris_colors) +
  labs(
    title = "Figure 3: MEHKO Permits by Jurisdiction",
    x = "Year",
    y = "Cumulative Permits Issued",
    fill = NULL
  ) +
  scale_x_continuous(limits = c(NA, 2024), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 170, margin = margin(b = 40)),
    axis.title.x = element_text(family = "Source Serif 4", size = 140, margin = margin(t = 40)),
    axis.title.y = element_text(family = "Source Serif 4", size = 140, margin = margin(r = 40)),
    axis.text = element_text(family = "Source Serif 4", size = 100),
    legend.title = element_blank(),
    legend.text = element_text(family = "Source Serif 4", size = 100),
    legend.key.spacing.y = unit(12, "pt"),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure3_permits.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(permit_growth_by_juris_plot)
dev.off()

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

#save geocoded data
saveRDS(complaint_data, "data/complaint_data_with_geocodes.rds")

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

### figure 4: permits by NCES locale type ###

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
            vjust = -0.5, family = "Source Serif 4", size = 28) +
  scale_fill_identity() +
  labs(
    title = "Figure 4: MEHKO Permits by Community Type",
    x = "Community Type (NCES Locale Classification)",
    y = "Number of Permits"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 170, margin = margin(b = 20)),
    axis.title = element_text(family = "Source Serif 4", size = 140),
    axis.text = element_text(family = "Source Serif 4", size = 120),
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure4_permits.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(grouped_bar_plot)
dev.off()


### figure 5: gender of business owners ###
# create dataframe with CA businesses and MEHKO data (race)
figure_5_data <- tibble(
  gender = c("Male", "Female"),
  `CA Businesses` = c(75.6,37.6),
  MEHKOs = c(28,69)
)

# color palette from the MEHKO code (teal and burnt orange)
colors_figure_5 <- c(
  "CA Businesses" = "#008B8B",   # Teal dark
  "MEHKOs" = "#CC5500"           # Burnt Orange dark
)

# reshape data for plotting
business_long <- figure_5_data %>%
  pivot_longer(
    cols = -gender,
    names_to = "business_type",
    values_to = "pct"
  )

figure_5 <- business_long %>%
  filter(!is.na(pct)) %>%
  ggplot(aes(x = gender, y = pct, fill = business_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors_figure_6) +
  labs(
    title = "Figure 5: Gender of Business Owners",
    x = "Gender",
    y = "% of business owners",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 170, margin = margin(b = 40), hjust = 0.5),
    axis.title = element_text(family = "Source Serif 4", size = 120),
    axis.text = element_text(family = "Source Serif 4", size = 90),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 40)),
    legend.text = element_text(family = "Source Serif 4", size = 120),
    legend.position = "right",
    legend.key.spacing.y = unit(15, "pt"),
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure5.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(figure_5)
dev.off()

### figure 6: racial comparison of CA businesses and MEHKOs ###

# create dataframe with CA businesses and MEHKO data (race)
figure_6_data <- tibble(
  ethnicity = c("Asian", "Black", "Latino / Hispanic", "White", "Something else"),
  `CA Businesses` = c(23, 2, 14, 61, 1),
  MEHKOs = c(26, 16, 28, 28, 9)
)

# color palette from the MEHKO code (teal and burnt orange)
colors_figure_6 <- c(
  "CA Businesses" = "#008B8B",   # Teal dark
  "MEHKOs" = "#CC5500"           # Burnt Orange dark
)

# reshape data for plotting
business_long <- figure_6_data %>%
  pivot_longer(
    cols = -ethnicity,
    names_to = "business_type",
    values_to = "count"
  )

# create bar chart
figure_6 <- business_long %>%
  filter(!is.na(count)) %>%
  ggplot(aes(x = ethnicity, y = count, fill = business_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors_figure_5) +
  labs(
    title = "Figure 6: Race/Ethnicity of Business Owners",
    x = "Race/Ethnicity",
    y = "% of business owners",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 170, margin = margin(b = 40), hjust = 0.5),
    axis.title = element_text(family = "Source Serif 4", size = 120),
    axis.text = element_text(family = "Source Serif 4", size = 90),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    legend.text = element_text(family = "Source Serif 4", size = 120),
    legend.position = "right",
    legend.key.spacing.y = unit(15, "pt"),
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

agg_png("plots/figure6.png", 
        width = 24, 
        height = 18, 
        units = "in", 
        res = 300,
        scaling = 2)
print(figure_6)
dev.off()

### figure 8: primary method of service ###

# data
figure_8_data <- tibble(
  method = c("Takeout", "Delivery", "Dine-in"),
  pct = c(56, 31, 11)
)

# color palette: 3 distinct tones consistent with your report style
colors_figure_8 <- c(
  "Takeout" = "#008B8B",   # teal
  "Delivery" = "#D97706",  # burnt orange
  "Dine-in"  = "#7C3AED"   # purple
)

# calculate label positions
figure_8_data <- figure_8_data %>%
  arrange(desc(method)) %>%
  mutate(
    ypos = cumsum(pct) - pct/2,
    # create a radius factor to push labels outward
    radius_factor = ifelse(pct == 11, 1.2, 1),  # 1.2 = 20% farther from center
    label = paste0(pct, "%")
  )

# create plot
figure_8 <- ggplot(figure_8_data, aes(x = 1, y = pct, fill = method)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(x = ifelse(pct == 11, 1.15, 1),
                y = ypos,
                label = label),
            color = "white",
            family = "Source Serif 4",
            size = 60) +
  scale_fill_manual(values = colors_figure_8) +
  labs(title = "Figure 8: Primary Method of Service", fill = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Black", face = "bold", size = 170, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(family = "Source Serif 4", size = 100),
    legend.spacing.y = unit(18, "pt"),
    legend.position = "right",
    plot.margin = margin(10, 40, 40, 40)
  )


# save
agg_png("plots/figure8_service_method.png",
        width = 24,
        height = 18,
        units = "in",
        res = 300,
        scaling = 2)
print(figure_8)
dev.off()

### figure 9 is the financial impact of MEHKOs graphic ###

### figure 10: survey responses ###
# create dataframe with benefits
figure_10_data <- tibble(
  survey_questions = c(
    "Community connections,\ncustomer relationships",
    "Flexibility and\nwork-life balance",
    "Low startup costs,\nfinancial benefits",
    "Passion, creative\nfulfillment",
    "Skill development,\nentrepreneurial experience"
  ),
  survey_responses = c(26, 25, 17, 12, 10)
)

# define color palette
bar_color <- "#CC5500"

# create plot
figure_10 <- figure_10_data %>%
  ggplot(aes(x = fct_reorder(survey_questions, survey_responses, .desc = TRUE),
             y = survey_responses,
             fill = survey_responses)) +  # map fill to survey_responses
  geom_col() +
  geom_text(aes(label = survey_responses), 
            vjust = -0.5, 
            family = "Source Serif 4", 
            size = 32) +
  scale_fill_gradient(low = "#FFE5CC", high = "#CC5500") +  # light-to-dark gradient
  labs(
    title = "Figure 10: Benefits of MEHKOs",
    subtitle = "'In 2-3 sentences, what have been the biggest benefits to you of your MEHKO business?'",
    x = NULL,
    y = "% of Respondents",
    fill = "Responses (%)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", face = "bold", size = 180, margin = margin(b = 40)),
    plot.subtitle = element_text(family = "Arial", size = 140, color = "gray40", margin = margin(b = 20)),
    axis.title.y = element_text(family = "Source Serif 4", size = 150),
    axis.text.y = element_text(family = "Source Serif 4", size = 120),
    axis.text.x = element_text(family = "Source Serif 4", size = 100, angle = 45, hjust = 1, lineheight = 0.8),
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40),
    legend.text = element_text(family = "Source Serif 4", size = 120),
    legend.title = element_text(family = "Source Serif 4", size = 140)
  )


# save plot
agg_png("plots/figure10.png", width = 24, height = 18, units = "in", res = 300, scaling = 2)
print(figure_10)
dev.off()

### figure 11: common challenges ###
# create dataframe with challenges
figure_11_data <- tibble(
  survey_questions = c("Finding customers",
                       "Not being able to sell at events or farmers markets",
                       "Permit limitations on catering",
                       "Building a brand",
                       "Cost of a MEHKO permit",
                       "Knowing how to get started",
                       "Daily/weekly meal caps",
                       "Annual revenue cap"),
  survey_responses = c(79, 76, 73, 60, 57, 56, 54, 50)
)

# create plot
figure_11_data %>%
  ggplot(aes(x = fct_reorder(survey_questions, survey_responses, .desc = TRUE), 
             y = survey_responses, 
             fill = survey_responses)) +
  geom_col() +
  scale_fill_gradient(low = "#FFE5CC", high = "#CC5500") +
  labs(title = "Figure 11: Common challenges",
       subtitle = "",
       x = "Challenges",
       y = "% of Respondents",
       fill = "Respondents (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, family = "Georgia"),
        axis.text.y = element_text(family = "Georgia"),
        axis.title = element_text(family = "Georgia"),
        plot.title = element_text(face = "bold", size = 14, family = "Arial"),
        plot.subtitle = element_text(size = 10, color = "gray60", family = "Arial"),
        legend.text = element_text(family = "Georgia"),
        legend.title = element_text(family = "Georgia"),
        legend.title.align = 0.5)

### figure 12: immigrant vs non immigrant challenges ###
# create dataframe with CA businesses and MEHKO data (race)
figure_12_data <- tibble(
  operator_type = c("Foreign-born", "U.S. born"),
  `Finding\ncustomers` = c(86, 73),
  `Permit costs` = c(66, 49),
  `Knowing how to \nget started` = c(65, 47)
)

# color palette from the MEHKO code (teal and burnt orange)
colors_figure_12 <- c(
  "CA Businesses" = "#008B8B",   # Teal dark
  "MEHKOs" = "#CC5500"           # Burnt Orange dark
)

# reshape data for plotting
figure_12_long <- figure_12_data %>%
  pivot_longer(
    cols = -operator_type,
    names_to = "challenge",
    values_to = "pct"
  )

colors_figure_12 <- c(
  "Foreign-born" = "#008B8B",
  "U.S. born"     = "#CC5500"
)

#create bar chart
figure_12 <- figure_x_long %>%
  ggplot(aes(x = challenge, y = pct, fill = operator_type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = pct),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            family = "Source Serif 4",
            size = 5) +
  scale_fill_manual(values = colors_figure_x) +
  labs(
    title = "Figure X: Increased Challenges for Foreign-born MEHKO Operators",
    x = "Challenge",
    y = "% of operators",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # extra space for labels
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", face = "bold", size = 24, margin = margin(b = 10)),
    axis.title = element_text(family = "Source Serif 4", size = 14),
    axis.text = element_text(family = "Source Serif 4", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(family = "Source Serif 4", size = 12),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(figure_12)
