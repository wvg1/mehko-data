#load packages
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(sf)
library(rlang)
library(readxl)

#setwd "mehko-data"
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
cat("Total unique complaints:", total, "\n")

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

#breaking it down further
#percentage of complaints by category (pre-permit)
pre_code_categories <- c("pre_code_admin", "pre_code_traffic", "pre_code_nuisance", 
                         "pre_code_noise", "pre_code_alcohol", "pre_code_trash", 
                         "pre_code_building", "pre_code_foodborne")

pre_health_categories <- c("pre_health_admin", "pre_health_traffic", "pre_health_nuisance", 
                           "pre_health_noise", "pre_health_alcohol", "pre_health_trash", 
                           "pre_health_building", "pre_health_foodborne")

#breakdown of pre-permit code enforcement complaints
code_totals <- complaint_data %>%
  select(all_of(pre_code_categories)) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
  mutate(
    category = str_remove(category, "pre_code_"),
    total = sum(count),
    percent = (count / total) * 100
  ) %>%
  arrange(desc(percent))

print(code_totals, n = Inf)
cat("\n")

#breakdown of pre-permit code enforcement complaints
health_totals <- complaint_data %>%
  select(all_of(pre_health_categories)) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
  mutate(
    category = str_remove(category, "pre_health_"),
    total = sum(count),
    percent = (count / total) * 100
  ) %>%
  arrange(desc(percent))

print(health_totals, n = Inf)
cat("\n")






