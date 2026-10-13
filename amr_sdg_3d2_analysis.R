# =====================================================================
# Title:    Antimicrobial-Resistant Bloodstream Infections: Australia
#           Compared with the World (SDG Indicator 3.d.2)
# Author:   Kushal Baraily
# Date:     October 2026
#
# Purpose:  Describe and compare the two WHO antimicrobial resistance
#           indicators used to monitor Sustainable Development Goal
#           3.d.2, with a focus on Australia:
#             1. E. coli bloodstream infections resistant to
#                third-generation cephalosporins (%)
#             2. Bloodstream infections due to methicillin-resistant
#                Staphylococcus aureus, MRSA (%)
#
# Data:     WHO data portal (data.who.int), compiled from national data
#           reported to the WHO Global Antimicrobial Resistance and Use
#           Surveillance System (GLASS). Licence: CC BY 4.0.
#             data/who_ecoli.csv  (indicator 745F475)
#             data/who_mrsa.csv   (indicator 5DD9606)
#
# Notes:    - Values are crude percentages reported by each country.
#             Numbers of patients tested are not published, so
#             confidence intervals cannot be calculated.
#           - From 2020, WHO only shows countries reporting at least 10
#             bloodstream infections with susceptibility results.
#           - The set of reporting countries changes each year, and
#             surveillance is not always nationally representative.
# =====================================================================


# ---- 1. Setup --------------------------------------------------------

# Clear the Console (same as Ctrl + L).
cat("\014")

# R must be working in this project's folder, which contains the 'data'
# folder. Opening AMR_glass.Rproj in RStudio sets this automatically, so
# the script runs on any computer without editing a file path.
if (!dir.exists("data")) {
  stop("The 'data' folder was not found in ", getwd(),
       "\nOpen AMR_glass.Rproj in RStudio (or setwd() to the project ",
       "folder) and run the script again.", call. = FALSE)
}
cat("Working folder:", getwd(), "\n")

# Install any required packages that are missing (runs only once).
#   tidyverse         - importing, cleaning, summarising and plotting
#   scales            - formatting axis labels
#   broom             - turning model results into tidy tables
#   patchwork         - combining several charts into one figure
#   countrycode       - converting UN country codes to ISO codes/regions
#   sf                - working with map (spatial) data
#   rnaturalearth(data) - world country boundaries
required_packages <- c("tidyverse", "scales", "broom", "patchwork",
                       "countrycode", "sf", "rnaturalearth",
                       "rnaturalearthdata", "cluster", "sandwich")

missing_packages <- setdiff(required_packages,
                            rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(scales)
library(broom)
library(patchwork)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cluster)
library(sandwich)

options(scipen = 999, dplyr.summarise.inform = FALSE)

# Fix the random number generator so jittered points sit in the same
# place every time the script is run.
set.seed(2026)


# ---- 1b. Publication-quality plot style --------------------------------

# A clean scientific style similar to GraphPad Prism and MATLAB figures:
# white background, solid black axis lines, outward tick marks, no
# gridlines, black sans-serif text, bold axis titles and legends without
# boxes. Defined once here so every chart follows it.

# Colours
col_main   <- "#1F3A5F"   # dark navy: main data colour
col_grey   <- "#8C8C8C"   # mid grey: reference lines, other countries
col_accent <- "#C0392B"   # strong red: highlights the focus country

# Settings that control the overall look
pub_font  <- "sans"   # Helvetica/Arial; change to "Arial" if preferred
pub_box   <- FALSE    # TRUE = full box around each panel (MATLAB style)

theme_pub <- function(base_size = 12) {
  theme_classic(base_size = base_size, base_family = pub_font) +
    theme(
      # Titles
      plot.title            = element_text(face = "bold", size = rel(1.2),
                                           colour = "black",
                                           margin = margin(b = 4)),
      plot.subtitle         = element_text(colour = "grey25",
                                           margin = margin(b = 10)),
      plot.caption          = element_text(colour = "grey40",
                                           size = rel(0.75), hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      # Axes: solid black lines with outward ticks
      axis.line             = element_line(colour = "black", linewidth = 0.6),
      axis.ticks            = element_line(colour = "black", linewidth = 0.6),
      axis.ticks.length     = unit(0.18, "cm"),
      axis.text             = element_text(colour = "black",
                                           size = rel(0.9)),
      axis.title            = element_text(colour = "black", face = "bold"),
      axis.title.x          = element_text(margin = margin(t = 8)),
      axis.title.y          = element_text(margin = margin(r = 8)),
      # Panels: no gridlines, optional full box
      panel.grid            = element_blank(),
      panel.border          = if (pub_box) {
        element_rect(colour = "black", fill = NA,
                     linewidth = 0.6)
      } else element_blank(),
      # Facet labels: bold, no grey background
      strip.background      = element_blank(),
      strip.text            = element_text(face = "bold", colour = "black",
                                           size = rel(1), hjust = 0),
      # Legend: no box, at the bottom
      legend.position       = "bottom",
      legend.title          = element_text(face = "bold", colour = "black"),
      legend.text           = element_text(colour = "black"),
      legend.key            = element_blank(),
      legend.background     = element_blank(),
      plot.margin           = margin(12, 15, 10, 12)
    )
}

# Keep the old name working, so theme_pro() calls use the new style too.
theme_pro <- theme_pub

theme_set(theme_pub())

# Save each figure as a high-resolution PNG (for Word and slides).
# PDF versions (vector format preferred by journals) are optional: set
# save_pdf to TRUE and they are all written at the very end of the script
# (Section 32). They are off by default because saving PDFs during an
# RStudio session can reset the Plots pane history.
save_pdf  <- FALSE
pdf_queue <- list()

save_figure <- function(filename, plot = last_plot(), ...) {
  ggsave(filename, plot, ...)
  if (save_pdf) {
    pdf_queue[[filename]] <<- list(plot = plot, args = list(...))
  }
}

# Panels side by side normally share one axis. Publication figures
# usually draw full axes on every panel; this needs ggplot2 3.5.0 or
# newer, so older versions fall back to the standard layout.
facet_pub <- function(...) {
  if (packageVersion("ggplot2") >= "3.5.0") {
    facet_wrap(..., axes = "all")
  } else {
    facet_wrap(...)
  }
}

source_caption <- paste("Source: WHO data portal (data.who.int), SDG indicator 3.d.2;",
                        "data reported to WHO GLASS.")


# ---- 1c. User settings ---------------------------------------------------

focus_country <- "Australia"

# File locations and the short labels used in tables and charts.
indicator_files <- c(
  "E. coli (3rd-gen. cephalosporin resistant)" = "data/who_ecoli.csv",
  "MRSA"                                       = "data/who_mrsa.csv"
)

min_years_for_trend <- 3   # countries need this many years for trend models
stopifnot(min_years_for_trend %in% 3:5)


# ---- 1d. Helper functions ------------------------------------------------

# Read one WHO data-portal indicator file and return a tidy table with
# consistent column names. Stops with a clear message if the file or an
# expected column is missing.
read_who_indicator <- function(file, label) {
  if (!file.exists(file)) {
    stop("File not found: ", file, "\nWorking folder is: ", getwd(),
         call. = FALSE)
  }
  raw <- read_csv(file, show_col_types = FALSE)
  
  needed  <- c("DIM_TIME", "DIM_GEO_CODE_M49", "DIM_GEO_CODE_TYPE",
               "GEO_NAME_SHORT", "RATE_PER_100_N")
  missing <- setdiff(needed, names(raw))
  if (length(missing) > 0) {
    stop("In ", file, " these columns are missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  
  raw |>
    transmute(indicator      = label,
              year           = as.integer(DIM_TIME),
              m49            = as.integer(DIM_GEO_CODE_M49),
              geo_type       = DIM_GEO_CODE_TYPE,
              # Shorten Kosovo's official name, which WHO lists as
              # "Kosovo (in accordance with UN Security Council
              # resolution 1244 (1999))", so it fits on chart labels.
              country        = str_remove(GEO_NAME_SHORT,
                                          "\\s*\\(in accordance with.*$"),
              resistance_pct = as.numeric(RATE_PER_100_N))
}

# Logit transformation for percentages. Resistance percentages are
# bounded by 0 and 100, so trends are modelled on the logit (log-odds)
# scale. Values of exactly 0% or 100% are moved slightly inwards because
# their logit is infinite.
logit_pct <- function(pct, epsilon = 0.5) {
  stopifnot(is.numeric(epsilon), length(epsilon) == 1,
            epsilon > 0, epsilon < 50)
  qlogis(pmin(pmax(pct, epsilon), 100 - epsilon) / 100)
}

# These are analytical boundary adjustments, not corrections derived
# from patient counts. Section 15 varies epsilon to assess sensitivity.
assert_unique <- function(data, keys, label) {
  bad <- data |> count(across(all_of(keys)), name = "records") |>
    filter(records > 1)
  if (nrow(bad) > 0) {
    print(bad, n = min(30, nrow(bad)))
    stop(label, ": duplicate keys detected. Resolve source dimensions ",
         "before analysis; records have not been silently dropped.",
         call. = FALSE)
  }
  invisible(data)
}

ecological_note <- paste(
  "Exploratory country-level associations; no causal or patient-level inference.",
  "Countries differ in reporting years, surveillance coverage and case mix.",
  "Exponentiated logit-regression coefficients are country-level odds-scale associations."
)


spearman_summary <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  if (length(x) < 3 || length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(tibble(countries = length(x), rho = NA_real_, p_value = NA_real_))
  }
  test <- cor.test(x, y, method = "spearman", exact = FALSE)
  tibble(countries = length(x), rho = unname(test$estimate), p_value = test$p.value)
}

# ---- 2. Create output folders ------------------------------------------

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)


# ---- 3. Import both indicators ------------------------------------------

# imap() reads each file, passing its label, and list_rbind() stacks the
# two indicators into one long table.
amr_all <- imap(indicator_files, read_who_indicator) |>
  list_rbind() |>
  mutate(indicator = factor(indicator, levels = names(indicator_files)))

glimpse(amr_all)


# ---- 4. Data checks ------------------------------------------------------

# Check for impossible values, duplicates and the types of geography.
data_checks <- amr_all |>
  group_by(indicator) |>
  summarise(rows            = n(),
            missing_values  = sum(is.na(resistance_pct)),
            below_0_or_100p = sum(resistance_pct < 0 | resistance_pct > 100,
                                  na.rm = TRUE),
            duplicates      = sum(duplicated(tibble(country, year))),
            first_year      = min(year),
            last_year       = max(year))

print(data_checks)
write_csv(data_checks, "tables/01_data_checks.csv")
if (anyNA(amr_all$year) || anyNA(amr_all$country) ||
    anyNA(amr_all$geo_type)) {
  stop("Missing year, geography name or geography type in source data.")
}
assert_unique(amr_all, c("indicator", "geo_type", "m49", "year"),
              "WHO source geography/year")
assert_unique(amr_all, c("indicator", "country", "year"),
              "WHO source country/year")
if (any(data_checks$below_0_or_100p > 0)) {
  stop("Resistance outside 0-100%; inspect tables/01_data_checks.csv.")
}

amr_all |> count(indicator, geo_type)


# ---- 5. Separate country data from the global estimate -------------------

# 'GLOBAL' rows are WHO's world figures; all other rows are countries,
# territories or areas. Country codes (UN M49) are converted to ISO codes
# for mapping, and to World Bank regions for regional comparison.
amr_world <- amr_all |>
  filter(geo_type == "GLOBAL") |>
  select(indicator, year, world_pct = resistance_pct)

amr <- amr_all |>
  filter(geo_type != "GLOBAL", !is.na(m49), !is.na(resistance_pct),
         between(resistance_pct, 0, 100)) |>
  mutate(iso3   = countrycode(m49, origin = "un", destination = "iso3c",
                              warn = FALSE),
         region = countrycode(iso3, origin = "iso3c", destination = "region",
                              warn = FALSE),
         region = replace_na(region, "Other"),
         is_focus = country == focus_country)

# Areas without an ISO code cannot be mapped (listed for transparency).
amr |> filter(is.na(iso3)) |> distinct(country)

# Confirm the focus country is present in each indicator.
amr |>
  filter(is_focus) |>
  select(indicator, year, resistance_pct) |>
  arrange(indicator, year) |>
  print(n = Inf)


# ---- 6. Surveillance coverage over time ----------------------------------

# Number of countries reporting each indicator each year.
coverage <- amr |>
  count(indicator, year, name = "countries")

write_csv(coverage, "tables/02_coverage_by_year.csv")

p_coverage <- ggplot(coverage, aes(x = year, y = countries,
                                   colour = indicator)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.3) +
  scale_colour_manual(values = c(col_main, col_accent)) +
  scale_x_continuous(breaks = sort(unique(coverage$year))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title    = "Countries reporting antimicrobial resistance data to WHO",
       subtitle = "Countries, territories and areas with a published value each year",
       x = NULL, y = "Number reporting", colour = NULL,
       caption  = source_caption)

print(p_coverage)
save_figure("figures/01_coverage.png", p_coverage,
            width = 9, height = 5.5, dpi = 300, bg = "white")


# ---- 7. Summary table ----------------------------------------------------

# For each indicator and year: number of countries, median and
# interquartile range (IQR, the middle 50% of countries), the WHO global
# figure and the focus country's value.
summary_by_year <- amr |>
  group_by(indicator, year) |>
  summarise(countries  = n(),
            median_pct = median(resistance_pct),
            q1_pct     = quantile(resistance_pct, 0.25),
            q3_pct     = quantile(resistance_pct, 0.75),
            focus_pct  = resistance_pct[is_focus][1]) |>
  ungroup() |>
  left_join(amr_world, by = c("indicator", "year")) |>
  mutate(across(ends_with("_pct"), ~ round(.x, 1)))

print(summary_by_year, n = Inf)
write_csv(summary_by_year, "tables/03_summary_by_year.csv")


# ---- 8. Australia compared with the world over time -----------------------

# The shaded band shows the middle 50% of reporting countries, the grey
# line their median, the dashed line WHO's global figure and the
# highlighted line the focus country.
p_compare <- ggplot(summary_by_year, aes(x = year)) +
  geom_ribbon(aes(ymin = q1_pct, ymax = q3_pct), fill = col_main,
              alpha = 0.12) +
  geom_line(aes(y = median_pct, linetype = "Median of reporting countries"),
            colour = col_main, linewidth = 0.8) +
  geom_line(aes(y = world_pct, linetype = "WHO global figure"),
            colour = col_grey, linewidth = 0.8, na.rm = TRUE) +
  geom_line(data = filter(summary_by_year, !is.na(focus_pct)),
            aes(y = focus_pct), colour = col_accent, linewidth = 1.1) +
  geom_point(data = filter(summary_by_year, !is.na(focus_pct)),
             aes(y = focus_pct), colour = col_accent, size = 2.3) +
  facet_pub(~ indicator) +
  scale_linetype_manual(values = c("Median of reporting countries" = "solid",
                                   "WHO global figure" = "dashed")) +
  guides(linetype = guide_legend(
    override.aes = list(colour = c(col_main, col_grey)))) +
  scale_x_continuous(breaks = sort(unique(summary_by_year$year))) +
  scale_y_continuous(limits = c(0, NA), labels = label_percent(scale = 1)) +
  labs(title    = paste(focus_country,
                        "compared with other countries reporting to WHO"),
       subtitle = paste0("Red line = ", focus_country,
                         "; shaded band = middle 50% of reporting countries"),
       x = NULL, y = "Bloodstream infections resistant", linetype = NULL,
       caption  = source_caption) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_compare)
save_figure("figures/02_focus_vs_world.png", p_compare,
            width = 11, height = 5.5, dpi = 300, bg = "white")


# ---- 9. Focus country ranking each year ----------------------------------

# Rank 1 = lowest resistance. The percentile shows the share of
# OTHER reporting countries with LOWER resistance than the focus country.
focus_rank <- amr |>
  group_by(indicator, year) |>
  mutate(rank       = min_rank(resistance_pct),
         countries  = n(),
         percentile = if_else(countries > 1,
                              round(100 * (rank - 1) / (countries - 1)),
                              NA_real_)) |>
  ungroup() |>
  filter(is_focus) |>
  select(indicator, year, resistance_pct, rank, countries, percentile) |>
  arrange(indicator, year)

print(focus_rank, n = Inf)
write_csv(focus_rank, "tables/04_focus_country_rank.csv")


# ---- 10. All countries in the latest year --------------------------------

# Each country's most recent value (latest year in which it reported),
# sorted from lowest to highest, with the focus country highlighted.
latest <- amr |>
  group_by(indicator, country) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup()

write_csv(latest, "tables/05_latest_value_by_country.csv")

plot_country_dots <- function(ind) {
  latest |>
    filter(indicator == ind) |>
    ggplot(aes(x = resistance_pct, y = reorder(country, resistance_pct),
               colour = is_focus)) +
    geom_segment(aes(x = 0, xend = resistance_pct,
                     yend = reorder(country, resistance_pct)),
                 linewidth = 0.3, colour = "grey85") +
    geom_point(size = 1.6) +
    scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent),
                        guide = "none") +
    scale_x_continuous(limits = c(0, 100), labels = label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title    = paste("Most recent value by country:", ind),
         subtitle = paste0("Latest year reported by each country; ",
                           focus_country, " highlighted"),
         x = "Bloodstream infections resistant", y = NULL,
         caption  = source_caption) +
    theme(axis.text.y        = element_text(size = 6),
          panel.grid.major.y = element_blank())
}

walk2(levels(amr$indicator), c("03a", "03b"), function(ind, num) {
  p <- plot_country_dots(ind)
  print(p)
  n_countries <- sum(latest$indicator == ind)
  save_figure(paste0("figures/", num, "_country_values_",
                     if (num == "03a") "ecoli" else "mrsa", ".png"),
              p, width = 8, height = max(6, n_countries * 0.13),
              dpi = 300, bg = "white", limitsize = FALSE)
})


# ---- 11. Regional comparison ---------------------------------------------

# Median of each country's latest value, by World Bank region.
# (The WHO files do not include WHO regions.)
regional <- latest |>
  group_by(indicator, region) |>
  summarise(countries  = n(),
            median_pct = median(resistance_pct)) |>
  ungroup()

print(regional, n = Inf)
write_csv(regional, "tables/06_regional_summary.csv")

p_regional <- ggplot(latest, aes(x = resistance_pct,
                                 y = fct_reorder(region, resistance_pct,
                                                 .fun = median))) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55",
               width = 0.6) +
  geom_jitter(aes(colour = is_focus, size = is_focus),
              height = 0.15, alpha = 0.8) +
  scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent),
                      labels = c("Other countries", focus_country),
                      name = NULL) +
  scale_size_manual(values = c(`FALSE` = 1.4, `TRUE` = 3), guide = "none") +
  # facet_wrap() (not facet_pub) so region names are shown once, on the
  # left; long panel titles and region names wrap onto two lines.
  facet_wrap(~ indicator, labeller = label_wrap_gen(width = 30)) +
  scale_y_discrete(labels = label_wrap(25)) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
                     labels = label_percent(scale = 1)) +
  labs(title    = "Antimicrobial resistance by world region",
       subtitle = "Each point is one country's most recent value; boxes show the median and middle 50%",
       x = "Bloodstream infections resistant", y = NULL,
       caption  = paste("Regions follow the World Bank classification.",
                        "\n", source_caption, sep = "")) +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing      = unit(2, "lines"))

print(p_regional)
save_figure("figures/04_regional_comparison.png", p_regional,
            width = 12, height = 6.5, dpi = 300, bg = "white")


# ---- 12. Are the two types of resistance related? ------------------------

# Describe whether E. coli and MRSA percentages are associated across
# countries. This correlation does not identify shared causes. Each country's average across matched years is used.
# Spearman's correlation is used because it does not assume a straight-
# line relationship and is robust to outliers.
matched_pairs <- amr |>
  mutate(indicator = if_else(str_detect(as.character(indicator), "MRSA"),
                             "mrsa", "ecoli")) |>
  select(country, region, year, indicator, resistance_pct) |>
  pivot_wider(names_from = indicator, values_from = resistance_pct) |>
  drop_na(ecoli, mrsa)
country_means <- matched_pairs |>
  group_by(country, region) |>
  summarise(ecoli = mean(ecoli), mrsa = mean(mrsa),
            matched_years = n_distinct(year), .groups = "drop")
if (nrow(country_means) < 4 || sd(country_means$ecoli) == 0 ||
    sd(country_means$mrsa) == 0) {
  stop("Insufficient matched country data for correlation and clustering.")
}
write_csv(matched_pairs, "tables/05b_matched_indicator_years.csv")

correlation <- cor.test(country_means$ecoli, country_means$mrsa,
                        method = "spearman", exact = FALSE)
print(correlation)

p_relation <- ggplot(country_means, aes(x = ecoli, y = mrsa)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = col_grey,
              fill = "grey90", linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_main, size = 2, alpha = 0.8) +
  geom_point(data = filter(country_means, country == focus_country),
             colour = col_accent, size = 3.5) +
  geom_text(data = filter(country_means, country == focus_country),
            aes(label = country), colour = col_accent,
            vjust = -1, size = 3.5) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "E. coli and MRSA resistance in the same countries",
       subtitle = paste0("Each point is one country's average across matched years (n = ",
                         nrow(country_means), "); Spearman's rho = ",
                         round(correlation$estimate, 2)),
       x = "E. coli resistant to 3rd-generation cephalosporins",
       y = "MRSA",
       caption  = source_caption)

print(p_relation)
save_figure("figures/05_ecoli_vs_mrsa.png", p_relation,
            width = 8, height = 6, dpi = 300, bg = "white")


# ---- 13. World maps ------------------------------------------------------

# Maps in the style used by WHO and the World Bank: Equal Earth projection
# (shows countries at their true relative size), a light grid of latitude
# and longitude lines inside an outline of the globe, and countries
# coloured by category rather than a continuous scale, which is easier to
# read. Countries without data are shown in a neutral sand colour.
# 'iso_a3_eh' is used for matching because the standard 'iso_a3' column is
# missing for some countries (e.g. France and Norway). Base R subsetting
# and match() are used on the map object instead of dplyr's filter() and
# left_join(), which avoids version clashes between sf and dplyr.
map_crs <- "+proj=eqearth"

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$admin != "Antarctica", ]

# Outline of the globe: a rectangle in longitude/latitude that becomes
# the rounded map edge once projected.
globe_outline <- st_sfc(
  st_polygon(list(cbind(c(rep(-179.99, 181), rep(179.99, 181), -179.99),
                        c(-90:90, 90:-90, -90)))),
  crs = 4326)

# Resistance categories and their colours (light to dark blue).
map_breaks <- c(0, 10, 25, 50, 100.01)
map_labels <- c("<10%", "10% to <25%", "25% to <50%", "\u226550%")
map_colours <- c(setNames(c("#BDD7EE", "#6BAED6", "#3A7DBF", "#173F7A"),
                          map_labels),
                 "No data" = "#E7C99A")

make_world_map <- function(ind) {
  values   <- latest[latest$indicator == ind, ]
  map_data <- world
  map_data$resistance_pct <- values$resistance_pct[
    match(map_data$iso_a3_eh, values$iso3)]
  map_data$category <- cut(map_data$resistance_pct, breaks = map_breaks,
                           labels = map_labels, right = FALSE)
  map_data$category <- factor(
    ifelse(is.na(map_data$category), "No data",
           as.character(map_data$category)),
    levels = c(map_labels, "No data"))
  
  ggplot() +
    geom_sf(data = globe_outline, fill = NA, colour = "grey55",
            linewidth = 0.3) +
    geom_sf(data = map_data, aes(fill = category), colour = "white",
            linewidth = 0.1) +
    scale_fill_manual(values = map_colours, drop = FALSE, name = NULL) +
    scale_x_continuous(breaks = seq(-180, 180, by = 15)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 15)) +
    coord_sf(crs = map_crs) +
    labs(title = ind) +
    theme_void(base_size = 11, base_family = pub_font) +
    theme(plot.title         = element_text(face = "bold", hjust = 0),
          panel.grid.major   = element_line(colour = "grey88",
                                            linewidth = 0.2),
          legend.position    = "bottom",
          legend.text        = element_text(colour = "black"),
          legend.key.size    = unit(0.45, "cm")) +
    guides(fill = guide_legend(nrow = 1))
}

p_maps <- (make_world_map(levels(amr$indicator)[1]) /
             make_world_map(levels(amr$indicator)[2])) +
  plot_annotation(
    title    = "Antimicrobial-resistant bloodstream infections worldwide",
    subtitle = "Most recent value reported by each country, 2016-2022",
    caption  = source_caption,
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(colour = "grey35"),
                     plot.caption  = element_text(colour = "grey45", hjust = 0))
  )

print(p_maps)
save_figure("figures/06_world_maps.png", p_maps,
            width = 10, height = 10, dpi = 300, bg = "white")


# ---- 14. Trend analysis --------------------------------------------------

# Is resistance changing over time WITHIN countries? Simply comparing
# global averages across years is misleading because different countries
# report each year. This model compares each country with itself
# ('country fixed effects'), using countries with at least
# 'min_years_for_trend' years of data.
#   Outcome: logit(resistance %)   Predictors: year + country
# The year coefficient is converted to an odds ratio per year:
# OR > 1 = resistance rising, OR < 1 = falling.
# CAUTION: no denominators are available, so all countries are weighted
# equally regardless of how many patients were tested.
trend_data <- amr |>
  group_by(indicator, country) |>
  filter(n_distinct(year) >= min_years_for_trend) |>
  ungroup() |>
  mutate(logit_resistance = logit_pct(resistance_pct))

fit_country_trend <- function(data) {
  empty <- tibble(odds_ratio_per_year = NA_real_, conf_low = NA_real_,
                  conf_high = NA_real_, p_value = NA_real_,
                  status = "Insufficient data")
  g <- n_distinct(data$country)
  if (g < 3 || n_distinct(data$year) < 3) return(empty)
  model <- lm(logit_resistance ~ year + country, data = data)
  if (df.residual(model) < 1 || is.na(coef(model)["year"])) return(empty)
  # Cluster by country to allow dependence among its annual observations.
  # Small numbers of countries still make these intervals unreliable.
  V <- sandwich::vcovCL(model, cluster = data$country, type = "HC1")
  est <- unname(coef(model)["year"])
  se <- sqrt(V["year", "year"])
  critical <- qt(0.975, df = g - 1)
  tibble(odds_ratio_per_year = exp(est),
         conf_low = exp(est - critical * se),
         conf_high = exp(est + critical * se),
         p_value = 2 * pt(abs(est / se), df = g - 1, lower.tail = FALSE),
         status = if_else(g < 20, "Few country clusters: interpret cautiously", "OK"))
}

trend_results <- trend_data |>
  group_by(indicator) |>
  group_modify(~ fit_country_trend(.x) |>
                 mutate(countries    = n_distinct(.x$country),
                        observations = nrow(.x))) |>
  ungroup()

print(trend_results)
write_csv(trend_results, "tables/07_trend_models.csv")

# Show each country's trajectory (grey) with the focus country
# highlighted, to see the variation behind the overall trend.
p_trajectories <- ggplot(trend_data, aes(x = year, y = resistance_pct,
                                         group = country)) +
  geom_line(colour = col_grey, alpha = 0.35, linewidth = 0.4) +
  geom_line(data = filter(trend_data, is_focus),
            colour = col_accent, linewidth = 1.2) +
  geom_point(data = filter(trend_data, is_focus),
             colour = col_accent, size = 2.2) +
  facet_pub(~ indicator) +
  scale_x_continuous(breaks = sort(unique(trend_data$year))) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title    = "Resistance trajectories of individual countries",
       subtitle = paste0("Countries with at least ", min_years_for_trend,
                         " years of data; ", focus_country, " highlighted"),
       x = NULL, y = "Bloodstream infections resistant",
       caption  = source_caption) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_trajectories)
save_figure("figures/07_country_trajectories.png", p_trajectories,
            width = 11, height = 5.5, dpi = 300, bg = "white")


# ---- 15. Composition and boundary-adjustment sensitivity -----------------

# Fixed cohorts report in every calendar year of each selected window.
# This removes changing composition within the window, but selects more
# consistent reporters. It does not make surveillance representative.
last_amr_year <- max(amr$year)
composition_windows <- list(
  full = seq(min(amr$year), last_amr_year),
  recent = seq(max(min(amr$year), last_amr_year - 3), last_amr_year)
)
composition_windows <- composition_windows[
  !duplicated(map_chr(composition_windows, ~ paste(.x, collapse = ",")))]

fixed_members <- imap(composition_windows, function(years, window) {
  amr |> filter(year %in% years) |>
    group_by(indicator, country) |>
    summarise(years_reported = n_distinct(year), .groups = "drop") |>
    filter(years_reported == length(years)) |>
    mutate(window = window, start_year = min(years), end_year = max(years))
}) |> list_rbind()
write_csv(fixed_members, "tables/08a_fixed_cohort_members.csv")

composition_summary <- imap(composition_windows, function(years, window) {
  d <- amr |> filter(year %in% years)
  fixed <- d |> semi_join(filter(fixed_members, .data$window == .env$window),
                          by = c("indicator", "country"))
  bind_rows(
    d |> group_by(indicator, year) |>
      summarise(countries = n(), median_pct = median(resistance_pct),
                .groups = "drop") |> mutate(cohort = "All reporting countries"),
    fixed |> group_by(indicator, year) |>
      summarise(countries = n(), median_pct = median(resistance_pct),
                .groups = "drop") |> mutate(cohort = "Fixed reporting cohort"),
    d |> filter(is_focus) |>
      transmute(indicator, year, countries = 1L,
                median_pct = resistance_pct, cohort = focus_country)
  ) |> mutate(window = window)
}) |> list_rbind()
write_csv(composition_summary, "tables/08b_composition_comparison.csv")
p_composition <- ggplot(composition_summary,
                        aes(year, median_pct, colour = cohort)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  facet_grid(indicator ~ window) +
  scale_colour_manual(values = setNames(c(col_grey, col_main, col_accent),
                                        c("All reporting countries", "Fixed reporting cohort", focus_country))) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Does the trend persist with the same reporting countries?",
       subtitle = "Fixed cohort: a published value in every year of the window",
       x = NULL, y = "Resistance (%)", colour = NULL,
       caption = paste("Country medians, not patient-weighted estimates.",
                       "See tables for cohort sizes.", source_caption))
print(p_composition)
save_figure("figures/08a_composition_comparison.png", p_composition,
            width = 12, height = 7, dpi = 300, bg = "white")

# Vary record length, composition and logit boundary adjustment together.
# "Exclude endpoints" drops only exact 0% / 100% observations and uses
# the exact logit of the remaining values. Eligibility is recomputed
# after exclusions; cohort membership can therefore differ.
sensitivity_grid <- crossing(
  min_years = 3:5,
  adjustment = c("Clamp 0.1%", "Clamp 0.5%", "Clamp 1%", "Exclude endpoints"),
  cohort = c("All eligible countries", paste0("Fixed: ", names(composition_windows)))
)
sensitivity <- pmap(sensitivity_grid, function(min_years, adjustment, cohort) {
  d <- amr
  if (cohort != "All eligible countries") {
    window <- sub("^Fixed: ", "", cohort)
    d <- d |> filter(year %in% composition_windows[[window]]) |>
      semi_join(filter(fixed_members, .data$window == .env$window),
                by = c("indicator", "country"))
  }
  if (adjustment == "Exclude endpoints") {
    d <- d |> filter(resistance_pct > 0, resistance_pct < 100) |>
      mutate(logit_resistance = qlogis(resistance_pct / 100))
  } else {
    eps <- c("Clamp 0.1%" = 0.1, "Clamp 0.5%" = 0.5, "Clamp 1%" = 1)[[adjustment]]
    d <- d |> mutate(logit_resistance = logit_pct(resistance_pct, eps))
  }
  d <- d |> group_by(indicator, country) |>
    filter(n_distinct(year) >= min_years) |> ungroup()
  map(levels(amr$indicator), function(ind) {
    di <- d |> filter(indicator == ind)
    fit_country_trend(di) |>
      mutate(indicator = ind, countries = n_distinct(di$country),
             observations = nrow(di), min_years = min_years,
             adjustment = adjustment, cohort = cohort)
  }) |> list_rbind()
}) |> list_rbind()
print(sensitivity, n = 30)
write_csv(sensitivity, "tables/08_trend_sensitivity.csv")
p_sensitivity <- sensitivity |> filter(min_years == min_years_for_trend) |>
  ggplot(aes(odds_ratio_per_year, adjustment, colour = cohort)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = col_grey) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                position = position_dodge(width = 0.6), width = 0.15,
                orientation = "y", na.rm = TRUE) +
  geom_point(position = position_dodge(width = 0.6), na.rm = TRUE) +
  facet_pub(~ indicator) +
  labs(title = "Sensitivity to reporting composition and logit adjustment",
       subtitle = paste("Minimum", min_years_for_trend,
                        "years; country-clustered 95% intervals"),
       x = "Odds ratio per year", y = NULL, colour = NULL,
       caption = "All record-length settings and sample sizes are saved in the table.")
print(p_sensitivity)
save_figure("figures/08_trend_sensitivity.png", p_sensitivity,
            width = 12, height = 6, dpi = 300, bg = "white")

# ---- 16. Text summary and session information ---------------------------

focus_latest <- latest |> filter(is_focus)

summary_lines <- c(
  "SDG 3.d.2 ANTIMICROBIAL RESISTANCE ANALYSIS",
  "",
  map_chr(levels(amr$indicator), function(ind) {
    d   <- amr |> filter(indicator == ind)
    fl  <- focus_latest |> filter(indicator == ind)
    tr  <- trend_results |> filter(indicator == ind)
    paste0(ind, ": ", n_distinct(d$country), " countries, ",
           min(d$year), "-", max(d$year),
           if (nrow(fl) > 0) paste0("; ", focus_country, " ",
                                    round(fl$resistance_pct, 1), "% (",
                                    fl$year, ")") else "",
           "; within-country OR per year ",
           round(tr$odds_ratio_per_year, 3), " (95% CI ",
           round(tr$conf_low, 3), "-", round(tr$conf_high, 3), ")")
  }),
  "",
  paste0("Spearman correlation between E. coli and MRSA (country averages): ",
         round(correlation$estimate, 2), " (n = ", nrow(country_means), ")")
)

writeLines(summary_lines, "tables/09_analysis_summary.txt")
cat(summary_lines, sep = "\n")

capture.output(sessionInfo(), file = "tables/10_session_info.txt")

cat("\nANALYSIS COMPLETE: figures in 'figures/', tables in 'tables/'\n")


# =====================================================================
# PART 2: FURTHER ANALYSES
# =====================================================================

# Additional packages for Part 2 (installed automatically if missing):
#   WDI   - downloads World Bank development indicators
#   lme4  - mixed-effects (multilevel) models
for (pkg in c("WDI", "lme4")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(WDI)
library(lme4)

# Short, readable country names for charts (e.g. 'United Kingdom'
# instead of 'United Kingdom of Great Britain and Northern Ireland').
amr <- amr |>
  mutate(country_short = coalesce(
    countrycode(iso3, origin = "iso3c", destination = "country.name",
                warn = FALSE),
    country))

# Helper for charts that sort countries separately within each panel:
# the panel name is attached to the country name for sorting, then
# removed again from the axis labels.
sort_within <- function(name, panel, value) {
  reorder(paste(name, panel, sep = "___"), value)
}
drop_panel_suffix <- function(x) sub("___.*$", "", x)


# ---- 17. Data coverage grid ----------------------------------------------

# Which countries reported in which years? Each row is a country and
# each filled square a year with data. Countries are sorted by how many
# years they reported, revealing how patchy surveillance is.
coverage_grid <- amr |>
  group_by(country_short) |>
  mutate(years_reported = n_distinct(year)) |>
  ungroup()

p_grid <- ggplot(coverage_grid,
                 aes(x = factor(year),
                     y = fct_reorder(country_short, years_reported),
                     fill = is_focus)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  facet_wrap(~ indicator) +
  scale_fill_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent),
                    guide = "none") +
  labs(title    = "Which countries reported, and when?",
       subtitle = paste0("Each square is a year with data; countries sorted by ",
                         "number of years reported; ", focus_country,
                         " highlighted"),
       x = NULL, y = NULL,
       caption  = source_caption) +
  theme(axis.text.y        = element_text(size = 5.5),
        axis.text.x        = element_text(angle = 45, hjust = 1),
        axis.line.x        = element_blank(),
        panel.grid.major.y = element_blank())

print(p_grid)
save_figure("figures/09_coverage_grid.png", p_grid,
            width = 10, height = max(8, n_distinct(amr$country_short) * 0.13),
            dpi = 300, bg = "white", limitsize = FALSE)


# ---- 18. Peer comparison: similar high-income countries --------------------

# Comparing with all reporting countries mixes very different health
# systems. Here the focus country is compared with high-income peers,
# listed by ISO code; peers without data are simply left out.
peer_iso3 <- c("AUS", "NZL", "GBR", "IRL", "CAN", "USA", "JPN", "KOR",
               "SGP", "NOR", "SWE", "DNK", "FIN", "NLD", "DEU", "FRA",
               "CHE", "AUT", "BEL")

# Each peer's most recent value, ranked within each indicator
# (rank 1 = lowest resistance).
peer_latest <- amr |>
  filter(iso3 %in% peer_iso3) |>
  group_by(indicator, country_short) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  group_by(indicator) |>
  mutate(rank_among_peers = min_rank(resistance_pct),
         peers_reporting  = n()) |>
  ungroup() |>
  arrange(indicator, resistance_pct)

peer_latest |>
  select(indicator, country_short, year, resistance_pct,
         rank_among_peers, peers_reporting) |>
  print(n = Inf)

write_csv(peer_latest, "tables/11_peer_comparison.csv")

peer_medians <- peer_latest |>
  group_by(indicator) |>
  summarise(median_pct = median(resistance_pct))

p_peers <- ggplot(peer_latest,
                  aes(x = resistance_pct,
                      y = sort_within(country_short, indicator, resistance_pct),
                      colour = is_focus)) +
  geom_vline(data = peer_medians, aes(xintercept = median_pct),
             linetype = "dashed", colour = col_grey) +
  geom_segment(aes(x = 0, xend = resistance_pct,
                   yend = sort_within(country_short, indicator,
                                      resistance_pct)),
               colour = "grey85", linewidth = 0.4) +
  geom_point(size = 2.6) +
  geom_text(aes(label = sprintf("%.1f%%", resistance_pct)),
            hjust = -0.35, size = 3, colour = "grey30") +
  facet_wrap(~ indicator, scales = "free_y",
             labeller = label_wrap_gen(width = 30)) +
  scale_y_discrete(labels = drop_panel_suffix) +
  scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent),
                      guide = "none") +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = paste(focus_country, "compared with high-income peers"),
       subtitle = "Most recent value reported by each country; dashed line = peer median",
       x = "Bloodstream infections resistant", y = NULL,
       caption  = source_caption) +
  theme(panel.grid.major.y = element_blank())

print(p_peers)
save_figure("figures/10_peer_comparison.png", p_peers,
            width = 11, height = 6, dpi = 300, bg = "white")


# ---- 19. Did resistance change during COVID-19? ----------------------------

# For countries with data both before and during the pandemic, compare
# the 2019 value ('before') with the average of 2021-2022 ('during').
# The Wilcoxon signed-rank test checks whether the changes are
# consistently in one direction. Changes may reflect shifts in antibiotic
# use, infection control and which patients were tested, not only
# changes in resistance itself.
covid_change <- amr |>
  mutate(period = case_when(year == 2019          ~ "before",
                            year %in% 2021:2022   ~ "during")) |>
  filter(!is.na(period)) |>
  group_by(indicator, country_short, is_focus, period) |>
  summarise(resistance_pct = mean(resistance_pct)) |>
  ungroup() |>
  pivot_wider(names_from = period, values_from = resistance_pct) |>
  drop_na(before, during) |>
  mutate(change = during - before)

covid_tests <- covid_change |>
  group_by(indicator) |>
  summarise(
    countries        = n(),
    median_before    = median(before),
    median_during    = median(during),
    median_change    = median(change),
    countries_rising = sum(change > 0),
    p_value          = wilcox.test(during, before, paired = TRUE,
                                   exact = FALSE)$p.value
  )

print(covid_tests)
write_csv(covid_tests, "tables/12_covid_period_change.csv")

p_covid <- covid_change |>
  pivot_longer(c(before, during), names_to = "period",
               values_to = "resistance_pct") |>
  mutate(period = factor(period, levels = c("before", "during"),
                         labels = c("2019\n(before)", "2021-22\n(during)"))) |>
  ggplot(aes(x = period, y = resistance_pct, group = country_short)) +
  geom_line(colour = col_grey, alpha = 0.4, linewidth = 0.4) +
  geom_point(colour = col_grey, alpha = 0.5, size = 1) +
  geom_line(data = ~ filter(.x, is_focus), colour = col_accent,
            linewidth = 1.2) +
  geom_point(data = ~ filter(.x, is_focus), colour = col_accent, size = 2.5) +
  facet_pub(~ indicator) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title    = "Resistance before and during the COVID-19 pandemic",
       subtitle = paste0("Each line is one country with data in both periods; ",
                         focus_country, " highlighted"),
       x = NULL, y = "Bloodstream infections resistant",
       caption  = source_caption)

print(p_covid)
save_figure("figures/11_covid_period_change.png", p_covid,
            width = 10, height = 6, dpi = 300, bg = "white")


# ---- 20. Country characteristics from the World Bank ----------------------

# Download national indicators for exploratory associations with
# resistance. A copy is saved so the download only happens once.
#   NY.GDP.PCAP.PP.KD - GDP per person (constant international $, PPP)
#   SH.XPD.CHEX.GD.ZS - current health expenditure (% of GDP)
#   SH_UHC_SCI        - universal health coverage service index (0-100),
#                       current World Bank code
#   SH.UHC.SRVS.CV.XD - the same index under its older code (used if the
#                       current code returns no data)
wdi_file <- "data/world_bank_indicators.csv"

wdi_codes <- c(gdp_pc_ppp     = "NY.GDP.PCAP.PP.KD",
               health_exp_pct = "SH.XPD.CHEX.GD.ZS",
               uhc_new        = "SH_UHC_SCI",
               uhc_old        = "SH.UHC.SRVS.CV.XD")

if (file.exists(wdi_file)) {
  wdi <- read_csv(wdi_file, show_col_types = FALSE)
} else {
  wdi <- WDI(country = "all", indicator = wdi_codes,
             start = 2016, end = 2022, extra = TRUE) |>
    as_tibble()
  write_csv(wdi, wdi_file)
}

# WDI() silently drops any indicator for which the World Bank returns no
# data. Add those back as empty columns so the code below still runs,
# and report which ones are missing.
for (v in names(wdi_codes)) {
  if (!v %in% names(wdi)) {
    message("No World Bank data for '", v, "' (", wdi_codes[[v]],
            "); it will be left out of the analysis.")
    wdi[[v]] <- NA_real_
  }
}

# Use the current UHC code where available, otherwise the older one.
wdi <- wdi |> mutate(uhc_index = coalesce(uhc_new, uhc_old))

# Average each indicator over 2016-2022 for each country (the UHC index
# is only published every few years). Regional and income aggregates
# are removed, keeping individual countries only.
wdi_country <- wdi |>
  filter(!is.na(iso3c), region != "Aggregates") |>
  group_by(iso3 = iso3c) |>
  summarise(gdp_pc_ppp     = mean(gdp_pc_ppp,     na.rm = TRUE),
            health_exp_pct = mean(health_exp_pct, na.rm = TRUE),
            uhc_index      = mean(uhc_index,      na.rm = TRUE),
            income_group   = first(income)) |>
  mutate(across(where(is.numeric), ~ if_else(is.nan(.x), NA_real_, .x)),
         income_group = factor(income_group,
                               levels = c("Low income", "Lower middle income",
                                          "Upper middle income",
                                          "High income")))

# Each country's average resistance across all reported years, joined
# to the World Bank data.
assert_unique(wdi_country, "iso3", "World Bank country lookup")

determinants <- amr |>
  group_by(indicator, iso3, country_short, is_focus) |>
  summarise(mean_pct = mean(resistance_pct)) |>
  ungroup() |>
  inner_join(wdi_country, by = "iso3")

determinants |> count(indicator, name = "countries_matched")


# ---- 21. Exploratory country-level associations ------------------------------------------

# (a) Resistance by World Bank income group, with a Kruskal-Wallis test
#     (a non-parametric test comparing several groups).
income_tests <- determinants |>
  filter(!is.na(income_group)) |>
  group_by(indicator) |>
  summarise(countries = n(),
            p_value   = if (n_distinct(income_group) >= 2)
              kruskal.test(mean_pct ~ income_group)$p.value else NA_real_)

print(income_tests)

p_income <- determinants |>
  filter(!is.na(income_group)) |>
  ggplot(aes(x = income_group, y = mean_pct)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55",
               width = 0.6) +
  geom_jitter(aes(colour = is_focus, size = is_focus),
              width = 0.15, alpha = 0.8) +
  scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent),
                      labels = c("Other countries", focus_country),
                      name = NULL) +
  scale_size_manual(values = c(`FALSE` = 1.4, `TRUE` = 3), guide = "none") +
  facet_pub(~ indicator) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title    = "Antimicrobial resistance by country income group",
       subtitle = "Each point is one country's average across reported years",
       x = NULL, y = "Bloodstream infections resistant",
       caption  = paste("Income groups: World Bank classification.",
                        "\n", source_caption, sep = "")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_income <- p_income + labs(caption = paste(p_income$labels$caption,
                                            ecological_note, sep = "\n"))
print(p_income)
save_figure("figures/12_income_groups.png", p_income,
            width = 11, height = 6, dpi = 300, bg = "white")

# (b) Relationship with each country characteristic, using Spearman's
#     correlation (one factor at a time). GDP is shown on a log10 scale
#     because it is highly skewed (a few very rich countries).
determinants_long <- determinants |>
  mutate(log10_gdp = log10(gdp_pc_ppp)) |>
  pivot_longer(c(log10_gdp, health_exp_pct, uhc_index),
               names_to = "factor", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(factor = recode(factor,
                         log10_gdp      = "GDP per person (log10 PPP $)",
                         health_exp_pct = "Health spending (% of GDP)",
                         uhc_index      = "Universal health coverage index"))

correlations <- determinants_long |>
  group_by(indicator, factor) |>
  group_modify(~ spearman_summary(.x$value, .x$mean_pct)) |>
  ungroup()

print(correlations)
write_csv(correlations, "tables/13_determinant_correlations.csv")

# Correlation labels for each panel.
rho_labels <- correlations |>
  mutate(label = paste0("rho = ", round(rho, 2)))

p_determinants <- ggplot(determinants_long, aes(x = value, y = mean_pct)) +
  geom_smooth(method = "loess", formula = y ~ x, colour = col_grey,
              fill = "grey90", linewidth = 0.6) +
  geom_point(colour = col_main, alpha = 0.7, size = 1.4) +
  geom_point(data = ~ filter(.x, is_focus), colour = col_accent, size = 3) +
  geom_text(data = rho_labels, aes(x = Inf, y = 97, label = label),
            hjust = 1.1, size = 3.2, colour = "grey30") +
  facet_grid(indicator ~ factor, scales = "free_x",
             labeller = label_wrap_gen(width = 28)) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "Country characteristics and antimicrobial resistance",
       subtitle = paste0("Each point is one country (", focus_country,
                         " highlighted); curve = smoothed trend; rho = Spearman correlation"),
       x = NULL, y = "Bloodstream infections resistant",
       caption  = paste("Country characteristics: World Bank, averaged 2016-2022.",
                        "\n", source_caption, sep = ""))

p_determinants <- p_determinants + labs(caption = paste(p_determinants$labels$caption,
                                                        ecological_note, sep = "\n"))
print(p_determinants)
save_figure("figures/13_country_characteristics.png", p_determinants,
            width = 12, height = 7.5, dpi = 300, bg = "white")

# (c) Multivariable model: all three factors together, so each effect is
#     adjusted for the others. Outcome on the logit scale, so effects are
#     odds ratios (OR):
#       GDP    - OR for a 10-fold higher GDP per person
#       Health - OR per 1 percentage point more of GDP spent on health
#       UHC    - OR per 10-point higher UHC index
#     CAUTION: an ecological analysis of country averages; it shows
#     associations between countries, not effects on individual patients.
#     The three factors are strongly related to each other, which makes
#     their separate effects hard to disentangle.
# Only factors with data for at least 10 countries are included.
predictor_terms <- c(gdp_pc_ppp     = "log10(gdp_pc_ppp)",
                     health_exp_pct = "health_exp_pct",
                     uhc_index      = "I(uhc_index/10)")

available_predictors <- names(predictor_terms)[
  map_lgl(names(predictor_terms), ~ sum(!is.na(determinants[[.x]])) >= 10)]

cat("\nFactors included in the multivariable model:",
    paste(available_predictors, collapse = ", "), "\n")

determinant_formula <- if (length(available_predictors) > 0)
  reformulate(predictor_terms[available_predictors], response = "logit_pct(mean_pct)")
else logit_pct(mean_pct) ~ 1

fit_determinants <- function(data) {
  if (nrow(data) <= length(available_predictors) + 2) {
    return(tibble(term = character(), odds_ratio = double(), conf_low = double(),
                  conf_high = double(), p_value = double(), countries = integer()))
  }
  lm(determinant_formula, data = data) |>
    tidy(conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    transmute(term = recode(term,
                            "log10(gdp_pc_ppp)" = "GDP per person (10-fold higher)",
                            "health_exp_pct"    = "Health spending (+1% of GDP)",
                            "I(uhc_index/10)"   = "UHC index (+10 points)"),
              odds_ratio = exp(estimate),
              conf_low   = exp(conf.low),
              conf_high  = exp(conf.high),
              p_value    = p.value,
              countries  = nrow(data))
}

determinant_models <- determinants |>
  drop_na(all_of(available_predictors)) |>
  group_by(indicator) |>
  group_modify(~ fit_determinants(.x)) |>
  ungroup()

print(determinant_models)
write_csv(determinant_models, "tables/14_determinant_models.csv")


# ---- 22. Mixed-effects model selection and diagnostics -------------------

# Try correlated random slopes, then independent random slopes, then
# random intercepts. Accept the first converged, nonsingular fit.
# This is a prespecified simplification rule, not proof of model adequacy.
# Stop if no supported model fits; do not use unreliable projections.
year_origin <- floor(median(unique(amr$year)))
mixed_data <- trend_data |>
  mutate(year_c = year - year_origin, country_short = country)

fit_mixed_checked <- function(data) {
  forms <- list(
    correlated_slopes = logit_resistance ~ year_c + (1 + year_c | country_short),
    independent_slopes = logit_resistance ~ year_c + (1 + year_c || country_short),
    random_intercept = logit_resistance ~ year_c + (1 | country_short)
  )
  attempts <- list()
  selected <- NULL
  for (name in names(forms)) {
    warnings <- character()
    model <- tryCatch(withCallingHandlers(
      lmer(forms[[name]], data = data, REML = TRUE,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000))),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        # Keep warnings visible in the console.
      }), error = function(e) e)
    if (inherits(model, "error")) {
      attempts[[name]] <- tibble(structure = name, singular = NA,
                                 converged = FALSE, selected = FALSE, messages = conditionMessage(model))
      next
    }
    msgs <- model@optinfo$conv$lme4$messages
    opt <- model@optinfo$conv$opt
    ok <- (is.null(opt) || all(opt == 0)) && length(msgs) == 0
    singular <- isSingular(model, tol = 1e-4)
    accept <- ok && !singular
    attempts[[name]] <- tibble(structure = name, singular = singular,
                               converged = ok, selected = accept,
                               messages = paste(unique(c(warnings, msgs)), collapse = " | "))
    if (accept) { selected <- model; break }
  }
  list(model = selected, diagnostics = list_rbind(attempts))
}

mixed_fits <- map(set_names(levels(amr$indicator)), function(ind) {
  fit_mixed_checked(filter(mixed_data, indicator == ind))
})
mixed_diagnostics <- imap(mixed_fits, ~ mutate(.x$diagnostics, indicator = .y)) |>
  list_rbind()
print(mixed_diagnostics)
write_csv(mixed_diagnostics, "tables/15a_mixed_fit_diagnostics.csv")
if (any(map_lgl(mixed_fits, ~ is.null(.x$model)))) {
  stop("No converged nonsingular mixed model for one or more indicators. ",
       "Inspect tables/15a_mixed_fit_diagnostics.csv; later models were not run.")
}
mixed_models <- map(mixed_fits, "model")

mixed_overall <- imap(mixed_models, function(model, ind) {
  est <- unname(fixef(model)["year_c"])
  se <- sqrt(vcov(model)["year_c", "year_c"])
  tibble(indicator = ind, odds_ratio_per_year = exp(est),
         conf_low = exp(est - 1.96 * se), conf_high = exp(est + 1.96 * se),
         countries = ngrps(model)[["country_short"]], singular_fit = isSingular(model),
         structure = mixed_diagnostics$structure[
           mixed_diagnostics$indicator == ind & mixed_diagnostics$selected])
}) |> list_rbind()
print(mixed_overall)
write_csv(mixed_overall, "tables/15_mixed_model_overall.csv")

# Residual plots are checks, not pass/fail certification. Normality,
# nonlinearity and temporal dependence may invalidate simple intervals.
mixed_residuals <- imap(mixed_models, function(model, ind) {
  d <- model.frame(model)
  tibble(indicator = ind, country = as.character(d$country_short),
         year = d$year_c + year_origin, fitted = as.numeric(fitted(model)),
         residual = as.numeric(residuals(model)),
         std_residual = as.numeric(residuals(model)) / sigma(model))
}) |> list_rbind()
write_csv(mixed_residuals, "tables/15b_mixed_residuals.csv")
p_residual <- ggplot(mixed_residuals, aes(fitted, std_residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = col_grey) +
  geom_point(alpha = 0.45, size = 1.4, colour = col_main) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = col_accent) +
  facet_pub(~ indicator) +
  labs(title = "Mixed-model residuals", x = "Fitted logit resistance",
       y = "Residual / model residual SD")
p_qq <- ggplot(mixed_residuals, aes(sample = std_residual)) +
  stat_qq(alpha = 0.45, size = 1.4, colour = col_main) + stat_qq_line() +
  facet_pub(~ indicator) +
  labs(title = "Residual normal Q-Q plots", x = "Theoretical quantile",
       y = "Observed quantile")
print(p_residual); print(p_qq)
save_figure("figures/14a_residual_diagnostics.png", p_residual / p_qq,
            width = 12, height = 9, dpi = 300, bg = "white")

# Leave one country out, keeping the selected structure fixed, to
# assess influence on the overall year coefficient. Failures are logged.
influence <- imap(mixed_models, function(model, ind) {
  d <- filter(mixed_data, indicator == ind)
  full <- unname(fixef(model)["year_c"])
  map(sort(unique(d$country_short)), function(excluded) {
    fit <- tryCatch(update(model, data = filter(d, country_short != excluded)),
                    error = function(e) NULL)
    if (is.null(fit)) return(tibble(country = excluded, slope = NA_real_,
                                    change_from_full = NA_real_, singular = NA, converged = FALSE))
    slope <- unname(fixef(fit)["year_c"])
    tibble(country = excluded, slope = slope, change_from_full = slope - full,
           singular = isSingular(fit),
           converged = length(fit@optinfo$conv$lme4$messages) == 0 &&
             all(fit@optinfo$conv$opt == 0))
  }) |> list_rbind() |> mutate(indicator = ind)
}) |> list_rbind()
write_csv(influence, "tables/15c_leave_one_country_out.csv")

# Country slopes are descriptive point estimates. Conditional random-
# effect variances alone are not valid uncertainty for total slopes.
# Do not classify significance using incomplete country intervals.
country_trends <- imap(mixed_models, function(model, ind) {
  cf <- coef(model)$country_short
  tibble(indicator = ind, country_short = rownames(cf),
         odds_ratio = exp(cf$year_c),
         slope_structure = if_else("year_c" %in% names(ranef(model)$country_short),
                                   "Country-specific slope", "Shared slope"))
}) |> list_rbind() |>
  mutate(is_focus = country_short == focus_country)
write_csv(country_trends, "tables/16_country_specific_trends.csv")
p_country_trends <- ggplot(country_trends,
                           aes(odds_ratio, sort_within(country_short, indicator, odds_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = col_grey) +
  geom_point(aes(colour = is_focus, shape = is_focus), size = 1.7) +
  scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  facet_wrap(~ indicator, scales = "free_y", labeller = label_wrap_gen(30)) +
  scale_y_discrete(labels = drop_panel_suffix) +
  labs(title = "Estimated country resistance trends",
       subtitle = "Descriptive point estimates; intercept-only models share a slope",
       x = "Odds ratio per year", y = NULL,
       caption = paste("No country-level significance claims.", source_caption)) +
  theme(axis.text.y = element_text(size = 5.5))
print(p_country_trends)
save_figure("figures/14_country_specific_trends.png", p_country_trends,
            width = 11, height = max(7, n_distinct(country_trends$country_short) * 0.14),
            dpi = 300, bg = "white", limitsize = FALSE)

# ---- 23. Summary of Part 2 ---------------------------------------------------

part2_lines <- c(
  "",
  "PART 2 RESULTS",
  "",
  "Change from 2019 to 2021-22 (median percentage-point change):",
  paste0("  ", covid_tests$indicator, ": ", round(covid_tests$median_change, 1),
         " (", covid_tests$countries, " countries; Wilcoxon p = ",
         signif(covid_tests$p_value, 2), ")"),
  "",
  "Mixed-effects overall trend (OR per year):",
  paste0("  ", mixed_overall$indicator, ": ",
         round(mixed_overall$odds_ratio_per_year, 3), " (95% CI ",
         round(mixed_overall$conf_low, 3), "-",
         round(mixed_overall$conf_high, 3), ")"),
  "",
  "Spearman correlations with country characteristics:",
  paste0("  ", correlations$indicator, " / ", correlations$factor, ": rho = ",
         round(correlations$rho, 2), " (n = ", correlations$countries, ")")
)

write(part2_lines, "tables/09_analysis_summary.txt", append = TRUE)
cat(part2_lines, sep = "\n")

cat("\nPART 2 COMPLETE: new figures 09-14 and tables 11-16 saved.\n")


# =====================================================================
# PART 3: EXTENDED ANALYSES
# =====================================================================





# ---- 24. Out-of-pocket health spending and sanitation ----------------------

# Two country-level characteristics for exploratory association:
#   SH.XPD.OOPC.CH.ZS - out-of-pocket spending (% of health spending)
#   SH.STA.BASS.ZS    - population using at least basic sanitation (%)
# These data cannot test mechanisms, causation or patient-level effects.
# A copy is saved so the download only happens once.
extra_file  <- "data/world_bank_extra.csv"
extra_codes <- c(oop_pct = "SH.XPD.OOPC.CH.ZS",
                 sanitation_pct = "SH.STA.BASS.ZS")

if (file.exists(extra_file)) {
  wdi_extra <- read_csv(extra_file, show_col_types = FALSE)
} else {
  wdi_extra <- WDI(country = "all", indicator = extra_codes,
                   start = 2016, end = 2022, extra = TRUE) |>
    as_tibble()
  write_csv(wdi_extra, extra_file)
}

for (v in names(extra_codes)) {
  if (!v %in% names(wdi_extra)) {
    message("No World Bank data for '", v, "'; it will be left out.")
    wdi_extra[[v]] <- NA_real_
  }
}

# Average 2016-2022 for each country, then join to the resistance data
# (determinants, from Section 20, already contains GDP per person).
extra_country <- wdi_extra |>
  filter(!is.na(iso3c), region != "Aggregates") |>
  group_by(iso3 = iso3c) |>
  summarise(oop_pct        = mean(oop_pct,        na.rm = TRUE),
            sanitation_pct = mean(sanitation_pct, na.rm = TRUE)) |>
  mutate(across(where(is.numeric), ~ if_else(is.nan(.x), NA_real_, .x)))

assert_unique(extra_country, "iso3", "Extra World Bank country lookup")

determinants_extra <- determinants |>
  left_join(extra_country, by = "iso3")

# (a) Spearman correlations, one factor at a time.
extra_long <- determinants_extra |>
  pivot_longer(c(oop_pct, sanitation_pct), names_to = "factor",
               values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(factor = recode(factor,
                         oop_pct        = "Out-of-pocket spending (% of health spending)",
                         sanitation_pct = "Basic sanitation (% of population)"),
         panel  = str_wrap(paste0(indicator, ": ", factor), width = 40)) |>
  arrange(indicator, factor) |>
  mutate(panel = fct_inorder(panel))

extra_correlations <- extra_long |>
  group_by(indicator, factor, panel) |>
  group_modify(~ spearman_summary(.x$value, .x$mean_pct)) |>
  ungroup()

print(extra_correlations)
write_csv(extra_correlations, "tables/17_oop_sanitation_correlations.csv")

p_extra <- ggplot(extra_long, aes(x = value, y = mean_pct)) +
  geom_smooth(method = "loess", formula = y ~ x, colour = col_grey,
              fill = "grey90", linewidth = 0.6) +
  geom_point(colour = col_main, alpha = 0.7, size = 1.5) +
  geom_point(data = ~ filter(.x, is_focus), colour = col_accent, size = 3) +
  geom_text(data = extra_correlations,
            aes(x = Inf, y = 98, label = paste0("rho = ", round(rho, 2))),
            hjust = 1.1, size = 3.3, colour = "grey30") +
  facet_wrap(~ panel, ncol = 2, scales = "free_x") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title    = "Out-of-pocket health spending, sanitation and resistance",
       subtitle = paste0("Each point is one country (", focus_country,
                         " in red); curve = smoothed trend; rho = Spearman correlation"),
       x = NULL, y = "Bloodstream infections resistant",
       caption  = paste("Country characteristics: World Bank, averaged 2016-2022.",
                        "\n", source_caption, sep = "")) +
  theme(panel.spacing = unit(2, "lines"))

p_extra <- p_extra + labs(caption = paste(p_extra$labels$caption,
                                          ecological_note, sep = "\n"))
print(p_extra)
save_figure("figures/15_oop_sanitation.png", p_extra,
            width = 11, height = 8.5, dpi = 300, bg = "white")

# (b) Adjusted model: are out-of-pocket spending and sanitation linked
#     to resistance AFTER accounting for national wealth (GDP)?
#     Odds ratios: OOP and sanitation per 10 percentage points; GDP per
#     10-fold higher. Ecological analysis of country averages.
extra_terms <- c(gdp_pc_ppp     = "log10(gdp_pc_ppp)",
                 oop_pct        = "I(oop_pct/10)",
                 sanitation_pct = "I(sanitation_pct/10)")

extra_available <- names(extra_terms)[
  map_lgl(names(extra_terms), ~ sum(!is.na(determinants_extra[[.x]])) >= 10)]

extra_formula <- if (length(extra_available) > 0)
  reformulate(extra_terms[extra_available], response = "logit_pct(mean_pct)")
else logit_pct(mean_pct) ~ 1

extra_models <- determinants_extra |>
  drop_na(all_of(extra_available)) |>
  group_by(indicator) |>
  group_modify(function(.x, .y) {
    if (nrow(.x) <= length(extra_available) + 2) {
      return(tibble(term = character(), estimate = double(), conf.low = double(),
                    conf.high = double(), p.value = double(), countries = integer()))
    }
    lm(extra_formula, data = .x) |> tidy(conf.int = TRUE) |>
      filter(term != "(Intercept)") |> mutate(countries = nrow(.x))
  }) |>
  ungroup() |>
  transmute(indicator,
            term = recode(term,
                          "log10(gdp_pc_ppp)"    = "GDP per person (10-fold higher)",
                          "I(oop_pct/10)"        = "Out-of-pocket spending (+10 points)",
                          "I(sanitation_pct/10)" = "Basic sanitation (+10 points)"),
            odds_ratio = exp(estimate), conf_low = exp(conf.low),
            conf_high = exp(conf.high), p_value = p.value, countries)

print(extra_models)
write_csv(extra_models, "tables/18_oop_sanitation_models.csv")

p_extra_models <- ggplot(extra_models,
                         aes(x = odds_ratio, y = fct_rev(fct_inorder(term)))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = col_grey) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                width = 0.2, linewidth = 0.5, colour = col_main) +
  geom_point(size = 2.8, colour = col_main) +
  facet_wrap(~ indicator, labeller = label_wrap_gen(width = 30)) +
  scale_x_log10() +
  labs(title    = "Country characteristics associated with resistance",
       subtitle = "Exploratory adjusted associations (95% model CI); dashed line = no association",
       x = "Odds ratio (log scale)", y = NULL,
       caption  = paste0("Ecological analysis of country averages; associations, ",
                         "not proof of cause.\n", source_caption)) +
  theme(panel.spacing = unit(2, "lines"))

p_extra_models <- p_extra_models + labs(caption = paste(p_extra_models$labels$caption,
                                                        ecological_note, sep = "\n"))
print(p_extra_models)
save_figure("figures/16_oop_sanitation_models.png", p_extra_models,
            width = 11, height = 5, dpi = 300, bg = "white")


# ---- 25. Antibiotic use and resistance -------------------------------------

# Antibiotic consumption is measured in defined daily doses (DDD) per
# 1,000 people per day. The data are not in the WHO resistance files, so
# they need to be downloaded separately (see the instructions given with
# this script) and saved as 'data/antibiotic_use.csv'. The file needs a
# 3-letter country code column (e.g. 'Code'), a year column ('Year') and
# one numeric column of consumption. If the file is missing, this section
# is skipped.
abx_file <- "data/antibiotic_use.csv"

if (!file.exists(abx_file)) {
  message("Section 25 skipped: '", abx_file, "' not found.")
} else {
  abx_raw   <- read_csv(abx_file, show_col_types = FALSE)
  code_col  <- intersect(c("Code", "code", "iso3", "ISO3", "iso3c"),
                         names(abx_raw))[1]
  year_col  <- intersect(c("Year", "year"), names(abx_raw))[1]
  # Set the column explicitly if the file contains several numeric fields.
  abx_value_column <- NULL  # e.g. "ddd_per_1000_per_day"
  candidates <- setdiff(names(abx_raw)[map_lgl(abx_raw, is.numeric)],
                        c(year_col, code_col))
  value_col <- if (!is.null(abx_value_column)) abx_value_column else
    if (length(candidates) == 1) candidates[[1]] else NA_character_
  if (is.na(code_col) || is.na(year_col) || is.na(value_col) ||
      !value_col %in% names(abx_raw)) {
    stop("Antibiotic-use file needs country code, year and one explicit DDD ",
         "column. Set abx_value_column; do not guess among numeric columns.")
  }
  assert_unique(abx_raw, c(code_col, year_col), "Antibiotic-use input")
  cat("Antibiotic use file: country code =", code_col, "| year =", year_col,
      "| consumption =", value_col, "\n")
  
  # Average consumption per country over 2016-2022.
  abx <- abx_raw |>
    transmute(iso3 = .data[[code_col]],
              year = if (is.na(year_col)) NA_real_ else .data[[year_col]],
              ddd  = .data[[value_col]]) |>
    filter(is.finite(ddd), ddd > 0, str_length(iso3) == 3,
           is.na(year) | between(year, 2016, 2022)) |>
    group_by(iso3) |>
    summarise(ddd = mean(ddd))
  
  abx_data <- determinants |> inner_join(abx, by = "iso3")
  
  abx_correlations <- abx_data |>
    group_by(indicator) |>
    group_modify(~ spearman_summary(.x$ddd, .x$mean_pct)) |>
    ungroup()
  print(abx_correlations)
  write_csv(abx_correlations, "tables/19_antibiotic_use_correlations.csv")
  
  # Adjusted for GDP: odds ratio for a doubling of antibiotic consumption.
  abx_models <- abx_data |>
    drop_na(gdp_pc_ppp) |>
    group_by(indicator) |>
    group_modify(~ lm(logit_pct(mean_pct) ~ log2(ddd) + log10(gdp_pc_ppp),
                      data = .x) |>
                   tidy(conf.int = TRUE) |>
                   filter(term == "log2(ddd)") |>
                   mutate(countries = nrow(.x))) |>
    ungroup() |>
    transmute(indicator, term = "Antibiotic use (doubling), adjusted for GDP",
              odds_ratio = exp(estimate), conf_low = exp(conf.low),
              conf_high = exp(conf.high), p_value = p.value, countries)
  print(abx_models)
  write_csv(abx_models, "tables/20_antibiotic_use_models.csv")
  
  p_abx <- ggplot(abx_data, aes(x = ddd, y = mean_pct)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = col_grey,
                fill = "grey90", linetype = "dashed", linewidth = 0.5) +
    geom_point(colour = col_main, alpha = 0.7, size = 1.8) +
    geom_point(data = ~ filter(.x, is_focus), colour = col_accent, size = 3.2) +
    geom_text(data = abx_correlations,
              aes(x = Inf, y = 98, label = paste0("rho = ", round(rho, 2))),
              hjust = 1.1, size = 3.3, colour = "grey30") +
    facet_wrap(~ indicator, labeller = label_wrap_gen(width = 30)) +
    scale_x_log10() +
    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(title    = "Antibiotic use and antimicrobial resistance",
         subtitle = paste0("Each point is one country (", focus_country,
                           " in red); antibiotic use averaged 2016-2022"),
         x = "Antibiotic use (DDD per 1,000 people per day, log scale)",
         y = "Bloodstream infections resistant",
         caption = paste0("DDD = defined daily doses.\n", source_caption)) +
    theme(panel.spacing = unit(2, "lines"))
  
  p_abx <- p_abx + labs(caption = paste(p_abx$labels$caption,
                                        ecological_note, sep = "\n"))
  print(p_abx)
  save_figure("figures/17_antibiotic_use.png", p_abx,
              width = 11, height = 5.5, dpi = 300, bg = "white")
}


# ---- 26. Exploratory AMR profiles: cluster number and stability -----------

# Countries are compared over matched E. coli/MRSA years (Section 12).
# K-means is exploratory: clusters are not established clinical groups.
profile_data <- country_means |>
  mutate(ecoli_logit = logit_pct(ecoli), mrsa_logit = logit_pct(mrsa))
Z <- scale(as.matrix(profile_data[, c("ecoli_logit", "mrsa_logit")]))
if (any(!is.finite(Z))) stop("Clustering variables cannot be scaled.")
k_max <- min(6L, nrow(Z) - 1L, nrow(unique(as.data.frame(Z))) - 1L)
if (k_max < 2) stop("Too few distinct profiles for cluster comparison.")
set.seed(2026)
k_candidates <- set_names(map(2:k_max, ~ kmeans(Z, centers = .x, nstart = 50)),
                          as.character(2:k_max))
cluster_comparison <- imap(k_candidates, function(fit, k) {
  si <- cluster::silhouette(fit$cluster, dist(Z))
  tibble(k = as.integer(k), mean_silhouette = mean(si[, "sil_width"]),
         smallest_cluster = min(fit$size), within_ss = fit$tot.withinss)
}) |> list_rbind()
# Use the best silhouette among solutions without singleton clusters.
eligible_k <- filter(cluster_comparison, smallest_cluster >= 2)
if (nrow(eligible_k) == 0) stop("All candidate solutions contain singleton clusters.")
selected_k <- eligible_k |> arrange(desc(mean_silhouette), k) |> slice(1) |> pull(k)
k_fit <- k_candidates[[as.character(selected_k)]]
cluster_comparison <- cluster_comparison |> mutate(selected = k == selected_k)
write_csv(cluster_comparison, "tables/21a_cluster_number_comparison.csv")

# Stability: refit on 80% of countries, assign all countries to nearest
# centres, and compare with the full-data partition using adjusted Rand
# index. Fixed scaling isolates sampling changes; this does not measure
# uncertainty in surveillance percentages or selected k.
adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  choose2 <- function(x) x * (x - 1) / 2
  observed <- sum(choose2(tab))
  row_pairs <- sum(choose2(rowSums(tab)))
  col_pairs <- sum(choose2(colSums(tab)))
  total <- choose2(sum(tab))
  expected <- row_pairs * col_pairs / total
  maximum <- (row_pairs + col_pairs) / 2
  if (abs(maximum - expected) < 1e-12) return(NA_real_)
  (observed - expected) / (maximum - expected)
}
set.seed(2026)
cluster_stability <- map(seq_len(200), function(iteration) {
  n_sub <- max(selected_k + 1L, floor(0.8 * nrow(Z)))
  idx <- sample(seq_len(nrow(Z)), min(n_sub, nrow(Z)), replace = FALSE)
  fit <- tryCatch(kmeans(Z[idx, , drop = FALSE], centers = selected_k,
                         nstart = 30), error = function(e) NULL)
  if (is.null(fit)) return(tibble(iteration, ari = NA_real_))
  distances <- sapply(seq_len(selected_k), function(j)
    rowSums(sweep(Z, 2, fit$centers[j, ], "-")^2))
  assigned <- max.col(-distances, ties.method = "first")
  tibble(iteration, ari = adjusted_rand(k_fit$cluster, assigned))
}) |> list_rbind()
write_csv(cluster_stability, "tables/21b_cluster_stability_runs.csv")
stability_summary <- cluster_stability |>
  summarise(k = selected_k, successful_runs = sum(is.finite(ari)),
            median_ari = median(ari, na.rm = TRUE),
            q025_ari = quantile(ari, 0.025, na.rm = TRUE),
            q975_ari = quantile(ari, 0.975, na.rm = TRUE))
print(cluster_comparison); print(stability_summary)
write_csv(stability_summary, "tables/21c_cluster_stability_summary.csv")

# Number groups by their mean raw resistance. Numbering is descriptive;
# two indicators can trade off, so avoid calling every group high/low.
cluster_order <- tibble(cluster = k_fit$cluster,
                        total = profile_data$ecoli + profile_data$mrsa) |>
  group_by(cluster) |> summarise(total = mean(total), .groups = "drop") |>
  arrange(total) |> mutate(profile = paste("Profile", row_number()))
profile_data <- profile_data |> mutate(cluster = k_fit$cluster) |>
  left_join(select(cluster_order, cluster, profile), by = "cluster") |>
  mutate(profile = factor(profile, levels = cluster_order$profile))
profile_summary <- profile_data |> group_by(profile) |>
  summarise(countries = n(), mean_ecoli = round(mean(ecoli), 1),
            mean_mrsa = round(mean(mrsa), 1),
            examples = paste(head(sort(country), 6), collapse = ", "),
            .groups = "drop")
write_csv(profile_summary, "tables/21_amr_profiles.csv")
write_csv(profile_data |> select(country, region, matched_years, ecoli, mrsa, profile),
          "tables/22_amr_profile_by_country.csv")

mismatch_model <- lm(mrsa_logit ~ ecoli_logit, data = profile_data)
profile_data <- profile_data |> mutate(residual = residuals(mismatch_model))
unusual <- bind_rows(
  profile_data |> slice_max(residual, n = 4, with_ties = FALSE) |>
    mutate(pattern = "MRSA above fitted relationship"),
  profile_data |> slice_min(residual, n = 4, with_ties = FALSE) |>
    mutate(pattern = "MRSA below fitted relationship")) |>
  select(pattern, country, ecoli, mrsa, residual)
write_csv(unusual, "tables/23_unusual_countries.csv")
p_profiles <- ggplot(profile_data, aes(ecoli, mrsa, colour = profile)) +
  geom_point(size = 2.3, alpha = 0.85) +
  geom_point(data = filter(profile_data, country == focus_country),
             shape = 21, size = 5, colour = "black", stroke = 1) +
  geom_text(data = filter(profile_data, country %in% c(focus_country, unusual$country)),
            aes(label = country), colour = "grey20", size = 3, vjust = -1) +
  scale_colour_brewer(palette = "Blues", name = NULL) +
  scale_x_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title = "Exploratory country AMR profiles",
       subtitle = paste("Silhouette-selected k =", selected_k,
                        "; median subsampling ARI =", round(stability_summary$median_ari, 2)),
       x = "E. coli resistant to 3rd-generation cephalosporins", y = "MRSA",
       caption = paste("Country averages across matched indicator years.",
                       "Profiles are descriptive, not validated clinical categories.", source_caption))
print(p_profiles)
save_figure("figures/18_amr_profiles.png", p_profiles,
            width = 11, height = 7.5, dpi = 300, bg = "white")
p_k <- ggplot(cluster_comparison, aes(k, mean_silhouette)) +
  geom_line(colour = col_main) + geom_point(aes(colour = selected), size = 3) +
  scale_colour_manual(values = c(`FALSE` = col_main, `TRUE` = col_accent), guide = "none") +
  scale_x_continuous(breaks = 2:k_max) +
  labs(title = "Comparison of candidate cluster counts",
       x = "Number of clusters", y = "Mean silhouette width",
       caption = "Selection excludes solutions with singleton clusters; clustering is exploratory.")
print(p_k)
save_figure("figures/18a_cluster_selection.png", p_k,
            width = 8, height = 5, dpi = 300, bg = "white")

# ---- 27. Do neighbouring countries have similar resistance? ------------------

# Moran's I measures geographic clustering: values above 0 mean
# neighbouring countries (sharing a border) tend to have similar
# resistance. Significance is tested by randomly shuffling values between
# countries 999 times (a permutation test). Island countries without land
# neighbours, including Australia, cannot be included.
morans_i <- function(values, neighbours) {
  z  <- values - mean(values)
  lag_sum <- map_dbl(seq_along(values),
                     function(i) mean(z[neighbours[[i]]]))
  sum(z * lag_sum) / sum(z^2)
}

world_valid <- st_make_valid(world)

spatial_results <- map(levels(amr$indicator), function(ind) {
  values  <- latest[latest$indicator == ind, ]
  map_dat <- world_valid
  map_dat$value <- values$resistance_pct[match(map_dat$iso_a3_eh, values$iso3)]
  map_dat <- map_dat[!is.na(map_dat$value), ]
  
  touches    <- st_touches(map_dat)
  has_border <- lengths(touches) > 0
  map_dat    <- map_dat[has_border, ]
  touches    <- st_touches(map_dat)
  keep       <- lengths(touches) > 0
  map_dat    <- map_dat[keep, ]
  touches    <- st_touches(map_dat)
  
  observed <- morans_i(map_dat$value, touches)
  permuted <- replicate(999, morans_i(sample(map_dat$value), touches))
  
  tibble(indicator = ind,
         countries = nrow(map_dat),
         morans_i  = observed,
         p_value   = (sum(permuted >= observed) + 1) / (999 + 1))
}) |>
  list_rbind()

print(spatial_results)
write_csv(spatial_results, "tables/24_spatial_clustering.csv")


# ---- 28. Is reporting related to resistance? ---------------------------------

# If countries with high resistance report less often, or only started
# reporting recently, the global picture could be biased.
# (a) Correlation between years of reporting and average resistance.
# (b) Early reporters (first report 2016-2017) compared with recent
#     joiners (first report 2020 or later), Wilcoxon rank-sum test.
reporting <- amr |>
  group_by(indicator, country_short) |>
  summarise(years_reported = n_distinct(year),
            first_year     = min(year),
            mean_pct       = mean(resistance_pct)) |>
  ungroup() |>
  mutate(joined = case_when(first_year <= 2017 ~ "Early (2016-2017)",
                            first_year >= 2020 ~ "Recent (2020 or later)",
                            TRUE               ~ "Middle (2018-2019)"),
         joined = factor(joined, levels = c("Early (2016-2017)",
                                            "Middle (2018-2019)",
                                            "Recent (2020 or later)")))

reporting_tests <- reporting |>
  group_by(indicator) |>
  group_modify(function(d, key) {
    rho  <- spearman_summary(d$years_reported, d$mean_pct)
    two  <- d |> filter(joined != "Middle (2018-2019)")
    wilc <- wilcox.test(mean_pct ~ joined, data = droplevels(two),
                        exact = FALSE)
    tibble(rho_years_vs_resistance = rho$rho,
           p_rho = rho$p_value,
           median_early  = median(d$mean_pct[d$joined == "Early (2016-2017)"]),
           median_recent = median(d$mean_pct[d$joined == "Recent (2020 or later)"]),
           p_early_vs_recent = wilc$p.value)
  }) |>
  ungroup()

print(reporting_tests)
write_csv(reporting_tests, "tables/25_reporting_bias.csv")

p_reporting <- ggplot(reporting, aes(x = joined, y = mean_pct)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55",
               width = 0.6) +
  geom_jitter(width = 0.15, colour = col_main, alpha = 0.7, size = 1.5) +
  facet_wrap(~ indicator, labeller = label_wrap_gen(width = 30)) +
  scale_x_discrete(labels = label_wrap(12)) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title    = "Does resistance differ by when countries started reporting?",
       subtitle = "Each point is one country's average resistance, grouped by first year of reporting",
       x = "First year of reporting", y = "Bloodstream infections resistant",
       caption  = source_caption) +
  theme(panel.spacing = unit(2, "lines"))

print(p_reporting)
save_figure("figures/19_reporting_bias.png", p_reporting,
            width = 11, height = 6, dpi = 300, bg = "white")


# ---- 29. Australia: WHO figures compared with national data -----------------

# Australia's own surveillance (the AURA program of the Australian
# Commission on Safety and Quality in Health Care, and the Australian
# Group on Antimicrobial Resistance, AGAR, bloodstream infection
# programs) publishes comparable figures. Enter the national values
# below from those reports (percentage resistant, by year), keeping NA
# where no value is available. The section is skipped while all are NA.
national_data <- tribble(
  ~indicator,               ~year, ~national_pct,
  levels(amr$indicator)[1],  2019, NA_real_,
  levels(amr$indicator)[1],  2020, NA_real_,
  levels(amr$indicator)[1],  2021, NA_real_,
  levels(amr$indicator)[1],  2022, NA_real_,
  levels(amr$indicator)[2],  2019, NA_real_,
  levels(amr$indicator)[2],  2020, NA_real_,
  levels(amr$indicator)[2],  2021, NA_real_,
  levels(amr$indicator)[2],  2022, NA_real_
)

assert_unique(national_data, c("indicator", "year"), "Australian national input")

if (all(is.na(national_data$national_pct))) {
  message("Section 29 skipped: enter Australian national figures in ",
          "'national_data' to compare them with the WHO values.")
} else {
  comparison <- amr |>
    filter(is_focus) |>
    select(indicator, year, who_pct = resistance_pct) |>
    mutate(indicator = as.character(indicator)) |>
    full_join(national_data, by = c("indicator", "year")) |>
    mutate(difference = who_pct - national_pct)
  print(comparison)
  write_csv(comparison, "tables/26_who_vs_national.csv")
  
  p_national <- comparison |>
    pivot_longer(c(who_pct, national_pct), names_to = "source",
                 values_to = "pct") |>
    filter(!is.na(pct)) |>
    mutate(source = recode(source, who_pct = "WHO (GLASS)",
                           national_pct = "Australian national data")) |>
    ggplot(aes(x = year, y = pct, colour = source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    facet_wrap(~ indicator, labeller = label_wrap_gen(width = 30)) +
    scale_colour_manual(values = c("WHO (GLASS)" = col_accent,
                                   "Australian national data" = col_main),
                        name = NULL) +
    scale_y_continuous(limits = c(0, NA), labels = label_percent(scale = 1)) +
    labs(title = paste(focus_country, ": WHO figures compared with national data"),
         x = NULL, y = "Bloodstream infections resistant",
         caption = "National data: AURA / AGAR reports.") +
    theme(panel.spacing = unit(2, "lines"))
  print(p_national)
  save_figure("figures/20_who_vs_national.png", p_national,
              width = 10, height = 5.5, dpi = 300, bg = "white")
}


# ---- 30. Short-term backtesting and illustrative scenarios ---------------

# Rolling-origin evaluation: train only on years <= cutoff, predict
# cutoff+1 and cutoff+2. Eligibility and structure selection use training
# data alone. Compare the same country-year records with persistence
# (last available country value). Persistence may be older than 1 year.
# Repeated fold records are not independent; scores are descriptive.
backtest_records <- tibble()
backtest_fit_log <- tibble()
for (ind in levels(amr$indicator)) {
  d <- amr |> filter(indicator == ind)
  cutoffs <- sort(unique(d$year))
  cutoffs <- cutoffs[cutoffs >= min(d$year) + 2 & cutoffs < max(d$year)]
  for (cutoff in cutoffs) {
    train <- d |> filter(year <= cutoff) |> group_by(country) |>
      filter(n_distinct(year) >= min_years_for_trend) |> ungroup() |>
      mutate(country_short = country, year_c = year - year_origin,
             logit_resistance = logit_pct(resistance_pct))
    test <- d |> filter(year > cutoff, year <= cutoff + 2) |>
      semi_join(train, by = "country") |>
      mutate(country_short = country, year_c = year - year_origin)
    if (n_distinct(train$country) < 5 || nrow(test) == 0) next
    result <- fit_mixed_checked(train)
    backtest_fit_log <- bind_rows(backtest_fit_log,
                                  mutate(result$diagnostics, indicator = ind, cutoff = cutoff))
    if (is.null(result$model)) next
    baseline <- train |> group_by(country) |>
      slice_max(year, n = 1, with_ties = FALSE) |> ungroup() |>
      select(country, baseline_year = year, persistence_pct = resistance_pct)
    test <- test |> left_join(baseline, by = "country") |>
      mutate(model_pct = 100 * plogis(predict(result$model, newdata = test,
                                              re.form = NULL, allow.new.levels = FALSE)),
             cutoff = cutoff, horizon = year - cutoff,
             baseline_age = year - baseline_year,
             structure = result$diagnostics$structure[result$diagnostics$selected])
    backtest_records <- bind_rows(backtest_records,
                                  select(test, indicator, country, year, cutoff, horizon, baseline_age,
                                         resistance_pct, model_pct, persistence_pct, structure))
  }
}
write_csv(backtest_fit_log, "tables/27a_backtest_fit_diagnostics.csv")
if (nrow(backtest_records) > 0) {
  write_csv(backtest_records, "tables/27b_backtest_predictions.csv")
  backtest_scores <- backtest_records |>
    pivot_longer(c(model_pct, persistence_pct), names_to = "method", values_to = "predicted") |>
    group_by(indicator, horizon, method) |>
    summarise(predictions = n(), countries = n_distinct(country),
              folds = n_distinct(cutoff),
              mae_pp = mean(abs(predicted - resistance_pct)),
              rmse_pp = sqrt(mean((predicted - resistance_pct)^2)),
              bias_pp = mean(predicted - resistance_pct), .groups = "drop")
  print(backtest_scores)
  write_csv(backtest_scores, "tables/27c_backtest_scores.csv")
  p_backtest <- ggplot(backtest_scores,
                       aes(factor(horizon), mae_pp, fill = method)) +
    geom_col(position = "dodge") + facet_pub(~ indicator) +
    labs(title = "Short-term prediction errors on withheld years",
         subtitle = "Lower error is better; mixed model compared with last-value persistence",
         x = "Years beyond training cutoff", y = "Mean absolute error (percentage points)",
         fill = NULL, caption = "Scores describe observed historical folds; future performance may differ.")
  print(p_backtest)
  save_figure("figures/21a_backtest_errors.png", p_backtest,
              width = 11, height = 5.5, dpi = 300, bg = "white")
} else {
  message("Backtesting skipped: insufficient eligible training/test records.")
}

# Default: a short illustrative scenario, not a validated forecast.
# Set projection_end <- 2050 explicitly only for long-range scenarios.
# Fixed-effect intervals describe the fitted logit trend at zero country
# random effects. They are NOT future-observation prediction intervals,
# global estimates, or uncertainty in surveillance/system changes.
projection_end <- last_amr_year + 3L
projection_years <- seq(min(amr$year), projection_end)
projections <- imap(mixed_models, function(model, ind) {
  b <- fixef(model)
  V <- as.matrix(vcov(model))
  X <- model.matrix(~ year_c, data = tibble(year_c = projection_years - year_origin))
  X <- X[, names(b), drop = FALSE]
  fit <- as.vector(X %*% b)
  se <- sqrt(rowSums((X %*% V) * X))
  typical <- tibble(indicator = ind, year = projection_years,
                    who = "Zero-random-effect trend",
                    pct = 100 * plogis(fit),
                    low = 100 * plogis(fit - 1.96 * se),
                    high = 100 * plogis(fit + 1.96 * se))
  if (focus_country %in% rownames(ranef(model)$country_short)) {
    nd <- tibble(year_c = projection_years - year_origin,
                 country_short = focus_country)
    focus_line <- tibble(indicator = ind, year = projection_years,
                         who = focus_country,
                         pct = 100 * plogis(predict(model, newdata = nd, re.form = NULL)),
                         low = NA_real_, high = NA_real_)
    bind_rows(typical, focus_line)
  } else typical
}) |> list_rbind() |>
  mutate(indicator = factor(indicator, levels = levels(amr$indicator)),
         period = if_else(year > last_amr_year, "Scenario", "Fitted"))
write_csv(projections, "tables/27_projections.csv")
observed_focus <- amr |> filter(is_focus)
p_projection <- ggplot(projections, aes(year, pct, colour = who)) +
  geom_ribbon(data = filter(projections, !is.na(low)),
              aes(ymin = low, ymax = high), fill = col_main, alpha = 0.12, colour = NA) +
  geom_vline(xintercept = last_amr_year + 0.5, linetype = "dotted", colour = col_grey) +
  geom_line(aes(linetype = period), linewidth = 1) +
  geom_point(data = observed_focus, aes(year, resistance_pct),
             colour = col_accent, size = 2.4, inherit.aes = FALSE) +
  facet_pub(~ indicator) +
  scale_colour_manual(values = setNames(c(col_main, col_accent),
                                        c("Zero-random-effect trend", focus_country)), name = NULL) +
  scale_linetype_manual(values = c(Fitted = "solid", Scenario = "dashed"), name = NULL) +
  scale_x_continuous(breaks = seq(min(amr$year), projection_end, 2)) +
  scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  labs(title = paste("Illustrative continuation of trends to", projection_end),
       subtitle = "Band: 95% interval for fixed-effect trend only; country line has no uncertainty band",
       x = NULL, y = "Bloodstream infections resistant",
       caption = paste("These are scenarios, not validated forecasts or global estimates.",
                       "Intervals exclude future observations and surveillance changes.", source_caption))
print(p_projection)
save_figure("figures/21_projection.png", p_projection,
            width = 12, height = 6.5, dpi = 300, bg = "white")

# ---- 31. Summary of Part 3 ---------------------------------------------------

part3_lines <- c(
  "",
  "PART 3 RESULTS",
  "",
  "Out-of-pocket spending and sanitation (Spearman rho):",
  paste0("  ", extra_correlations$indicator, " / ", extra_correlations$factor,
         ": rho = ", round(extra_correlations$rho, 2),
         " (n = ", extra_correlations$countries, ")"),
  "",
  "Geographic clustering (Moran's I):",
  paste0("  ", spatial_results$indicator, ": I = ",
         round(spatial_results$morans_i, 2), ", p = ",
         signif(spatial_results$p_value, 2)),
  "",
  "Exploratory AMR profiles (silhouette-selected k-means):",
  paste0("  ", profile_summary$profile, ": ", profile_summary$countries,
         " countries"),
  "",
  paste0("Illustrative fixed-effect trend (95% mean-trend interval) in ", projection_end, ":"),
  with(filter(projections, year == projection_end,
              who == "Zero-random-effect trend"),
       paste0("  ", indicator, ": ", round(pct, 1), "% (",
              round(low, 1), "-", round(high, 1), "%)"))
)

write(part3_lines, "tables/09_analysis_summary.txt", append = TRUE)
cat(part3_lines, sep = "\n")

cat("\nPART 3 COMPLETE.\n")

# ---- 32. Save PDF versions of all figures (optional) -----------------------

# Runs only when save_pdf is TRUE in Section 1b. Vector PDFs stay sharp
# at any size and are preferred by most journals. Cairo is used where
# available because it handles accented country names correctly.
# Tip: browse the charts in the Plots pane first, then run the script
# with save_pdf = TRUE when you need the final PDFs.
if (save_pdf) {
  iwalk(pdf_queue, function(item, filename) {
    do.call(ggsave, c(list(filename = sub("\\.png$", ".pdf", filename),
                           plot     = item$plot,
                           device   = if (capabilities("cairo")) cairo_pdf
                           else "pdf"),
                      item$args))
  })
  cat("PDF versions saved for", length(pdf_queue), "figures.\n")
}

# ---- 33. Interpretation and reproducibility record ------------------------
interpretation <- c(
  "INTERPRETATION LIMITS",
  ecological_note,
  "Percentages have no published patient denominators here; no binomial sampling CIs are calculated.",
  "Regression intervals reflect model assumptions, not all surveillance uncertainty.",
  "Fixed cohorts select consistent reporters and are not nationally representative by construction.",
  "Backtesting compares historical prediction errors on identical held-out records with persistence.",
  "Cluster stability reflects country subsampling at selected k, not clinical validity.",
  "Scenario bands cover only fixed-effect mean-trend estimation, not future-observation uncertainty.",
  "Country trend point estimates are descriptive; no incomplete significance intervals are shown."
)
writeLines(interpretation, "tables/28_interpretation_limits.txt")
cat(interpretation, sep = "\n")
capture.output(sessionInfo(), file = "tables/10_session_info.txt")
saveRDS(list(mixed_models = mixed_models, year_origin = year_origin,
             selected_k = selected_k, settings = sensitivity_grid),
        "tables/29_analysis_objects.rds")
cat("\nREVISED ANALYSIS COMPLETE.\n")
