Worked R Code: Season–Year & Stretch–Year Connected Trend Plots (PNG + SVG)
1) Season–Year Connected Line Plots (PNG + SVG)
# ==========================================================
#  Seasonal patterns across years (Season–Year combined)
#  x-axis = Pre-2022 → Mon-2022 → Post-2024
#  Colour-coded by Season (Okabe–Ito palette)
#  No legends; PNG + SVG saved
# ==========================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

if (!exists("sig_by_season")) {
  sig_by_season <- map(category_list, ~ find_significant(dat, "Season", .x))
}

season_levels  <- c("Pre-monsoon", "Monsoon", "Post-monsoon")
year_levels    <- c("2022","2023","2024")
time_levels <- as.vector(outer(season_levels, year_levels, paste, sep = "-"))
time_levels <- factor(time_levels, levels = time_levels)

make_time <- function(Season, Year) {
  factor(paste(Season, as.character(Year), sep = "-"), levels = levels(time_levels))
}

# Optional: interpolate missing values for smooth connection
.fill_missing <- function(df) {
  df %>%
    complete(Time = factor(levels(time_levels), levels = time_levels)) %>%
    arrange(Time) %>%
    mutate(idx = as.integer(Time)) %>%
    group_modify(~{
      d <- .x
      if (all(is.na(d$value))) return(d)
      keep <- !is.na(d$value)
      if (sum(keep) >= 2) {
        d$value <- approx(x = d$idx[keep], y = d$value[keep],
                          xout = d$idx, method = "linear", rule = 2)$y
      } else if (sum(keep) == 1) {
        d$value <- rep(d$value[keep][1], length(d$idx))
      }
      d
    }) %>%
    ungroup()
}

plot_season_year_coloured <- function(df, param, out_dir) {
  summ <- df %>%
    group_by(Season, Year) %>%
    summarise(value = median(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Time = make_time(Season, Year),
      Season = factor(Season, levels = season_levels)
    ) %>%
    .fill_missing()
  
  if (nrow(summ) == 0) return(invisible(NULL))
  ylab <- if (!is.null(param_labels[[param]])) param_labels[[param]] else param
  
  g <- ggplot(summ, aes(x = Time, y = value, group = 1, color = Season)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.6) +
    scale_color_manual(values = okabe_ito[1:3]) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.y = element_text(face = "bold"),
      text = element_text(family = "Arial"),
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # --- Save both PNG and SVG ---
  ggsave(file.path(out_dir, paste0(param, "_SeasonYear_Colored.png")),
         g, width = 7, height = 5, dpi = 600)
  ggsave(file.path(out_dir, paste0(param, "_SeasonYear_Colored.svg")),
         g, width = 7, height = 5, device = "svg")
}

for (cat in names(sig_by_season)) {
  sig_params <- sig_by_season[[cat]]
  if (!length(sig_params)) next
  message("(Season-Year coloured | ", cat, ") -> ", paste(sig_params, collapse = ", "))
  out_dir <- file.path("SeasonYear_Colored", cat)
  for (p in sig_params) plot_season_year_coloured(dat, p, out_dir)
}

message("\n✅ Saved coloured season-year connected line plots in 'SeasonYear_Colored/' (PNG + SVG).")

2) Stretch–Year Connected Line Plots (PNG + SVG)
# ==========================================================
#  Stretch patterns across years (Stretch–Year combined)
#  x-axis = Up-2022 → Mid-2022 → Down-2024
#  Colour-coded by Stretch (Okabe–Ito palette)
#  No legends; PNG + SVG saved
# ==========================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

if (!exists("sig_by_stretch")) {
  sig_by_stretch <- map(category_list, ~ find_significant(dat, "Stretch", .x))
}

stretch_levels <- c("Upstream", "Midstream", "Downstream")
year_levels    <- c("2022", "2023", "2024")
time_levels_stretch <- as.vector(outer(stretch_levels, year_levels, paste, sep = "-"))
time_levels_stretch <- factor(time_levels_stretch, levels = time_levels_stretch)

make_time_stretch <- function(Stretch, Year) {
  factor(paste(Stretch, as.character(Year), sep = "-"), levels = levels(time_levels_stretch))
}

.fill_missing_stretch <- function(df) {
  df %>%
    complete(Time = factor(levels(time_levels_stretch), levels = time_levels_stretch)) %>%
    arrange(Time) %>%
    mutate(idx = as.integer(Time)) %>%
    group_modify(~{
      d <- .x
      if (all(is.na(d$value))) return(d)
      keep <- !is.na(d$value)
      if (sum(keep) >= 2) {
        d$value <- approx(x = d$idx[keep], y = d$value[keep],
                          xout = d$idx, method = "linear", rule = 2)$y
      } else if (sum(keep) == 1) {
        d$value <- rep(d$value[keep][1], length(d$idx))
      }
      d
    }) %>% ungroup()
}

plot_stretch_year_coloured <- function(df, param, out_dir) {
  summ <- df %>%
    group_by(Stretch, Year) %>%
    summarise(value = median(.data[[param]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Time = make_time_stretch(Stretch, Year),
      Stretch = factor(Stretch, levels = stretch_levels)
    ) %>%
    .fill_missing_stretch()
  
  if (nrow(summ) == 0) return(invisible(NULL))
  ylab <- if (!is.null(param_labels[[param]])) param_labels[[param]] else param
  
  g <- ggplot(summ, aes(x = Time, y = value, group = 1, color = Stretch)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.6) +
    scale_color_manual(values = okabe_ito[1:3]) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.y = element_text(face = "bold"),
      text = element_text(family = "Arial"),
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # --- Save both PNG and SVG ---
  ggsave(file.path(out_dir, paste0(param, "_StretchYear_Colored.png")),
         g, width = 7, height = 5, dpi = 600)
  ggsave(file.path(out_dir, paste0(param, "_StretchYear_Colored.svg")),
         g, width = 7, height = 5, device = "svg")
}

for (cat in names(sig_by_stretch)) {
  sig_params <- sig_by_stretch[[cat]]
  if (!length(sig_params)) next
  message("(Stretch-Year coloured | ", cat, ") -> ", paste(sig_params, collapse = ", "))
  out_dir <- file.path("StretchYear_Colored", cat)
  for (p in sig_params) plot_stretch_year_coloured(dat, p, out_dir)
}

message("\n✅ Saved coloured stretch-year connected line plots in 'StretchYear_Colored/' (PNG + SVG).")

