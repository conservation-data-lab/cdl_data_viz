

library(tidyverse)
library(lubridate)
library(scales)
library(ggridges)



# read in fire occurrence data
fod_data <- suppressWarnings(
  read_csv(
    "ridgeline/short_fod_1992_2020.csv",
    show_col_types = FALSE
  )
)


# pull out fire size and date
# useful for looking at the distribution of fire sizes later
fire_sizes <- fod_data |>
  select(FIRE_SIZE, DISCOVERY_DATE)


# clean up dates and add date parts
fire_date <- fod_data |>
  select(DISCOVERY_DATE, FIRE_SIZE) |>
  mutate(
    DISCOVERY_DATE = mdy(DISCOVERY_DATE),
    month = month(DISCOVERY_DATE),
    day = day(DISCOVERY_DATE),
    year = year(DISCOVERY_DATE)
  )


# fire counts by month and year
# this is the base for the annual and monthly summaries
month_year_count <- fire_date |>
  group_by(month, year) |>
  summarize(
    count = n(),
    .groups = "drop"
  )


# total fires by year
year_count <- month_year_count |>
  group_by(year) |>
  summarize(
    count = sum(count),
    .groups = "drop"
  )


# mean number of fires per month across years
monthly_mean <- month_year_count |>
  group_by(month) |>
  summarize(
    mean = round(mean(count)),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Ridgeline plot
# ------------------------------------------------------------

# count fires for each day
daily_counts <- fire_date |>
  mutate(
    date = as.Date(DISCOVERY_DATE),
    month = as.integer(month)
  ) |>
  group_by(date, month) |>
  summarize(
    count = n(),
    .groups = "drop"
  )


# add year and day of year for plotting
daily_counts_faceted <- daily_counts |>
  mutate(
    year = year(date),
    doy = yday(date)
  )


# first day of each month in a non-leap year
# used to put month labels on the x-axis
month_breaks <- c(
  1, 32, 60, 91, 121, 152,
  182, 213, 244, 274, 305, 335
)

month_labels <- month.abb


# add missing days within each year
# days with no fires get a count of zero
# keeping counts raw here rather than scaling them
df_full <- daily_counts_faceted |>
  mutate(year = as.integer(year)) |>
  group_by(year) |>
  tidyr::complete(
    doy = 1:365,
    fill = list(count = 0)
  ) |>
  ungroup()


# order years for the ridgeline plot
years_ord <- sort(
  unique(df_full$year),
  decreasing = TRUE
)


# spacing between years
# increase this if the ridges feel too crowded
gap <- 20

year_positions <- tibble(
  year = years_ord,
  y_pos = seq_along(years_ord) * gap
)


# add plotting position for each year
df_plot <- df_full |>
  left_join(year_positions, by = "year")


# plot daily fire counts by year

ridgeline_plot <-
ggplot(
  df_plot,
  aes(
    x = doy,
    y = y_pos,
    height = count,
    group = year
  )
) +
  geom_ridgeline(
    fill = "#4e79a7",
    color = "#2f4e7a",
    linewidth = 0.10,
    scale = 1
  ) +
  geom_ridgeline(
    color = "grey",
    linewidth = 0.10,
    fill = NA
  ) +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = year_positions$y_pos,
    labels = year_positions$year
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Annual density of fire occurances for the Northwoods ",
    subtitle ="Generalized over each year.  Fire size not a factor.",
    caption = "Data from https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0009.6, by Karen Short."
  ) +
  theme_minimal(base_size = 12)


# show plot
ridgeline_plot


# export
ggsave(
  "ridgeline/fire_ridgeline.png",
  plot = ridgeline_plot,
  width = 10,
  height = 8,
  dpi = 300
)


