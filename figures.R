# Script to generate figures for leading indicator recommendations paper

# generate epidemic for modelling using the `epidemics` package getting started example.
# Then do transformations to it, to demonstrate principles discussed in paper.

# Note you will need tidyverse packages and the below
library(epidemics)
library(ggplot2)
library(patchwork)
# used for ccf_boot
library(funtimes)
library(socialmixr)

theme_set(theme_bw())

set.seed(07734)


output_dir <- fs::dir_create(here::here("methods", "leading-indicator", "output"))
# Generate epidemic ####

polymod <- socialmixr::polymod
contact_data <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  # add in another group for more differentiation
  age.limits = c(0, 20, 40),
  symmetric = TRUE
)

# prepare contact matrix
contact_matrix <- t(contact_data$matrix)

# prepare the demography vector
demography_vector <- contact_data$demography$population
names(demography_vector) <- rownames(contact_matrix)

initial_i <- 1e-6
initial_conditions <- c(
  S = 1 - initial_i,
  E = 0,
  I = initial_i,
  R = 0,
  V = 0
)

# build for all age groups
initial_conditions <- rbind(
  initial_conditions,
  initial_conditions,
  initial_conditions
)

# assign rownames for clarity
rownames(initial_conditions) <- rownames(contact_matrix)

uk_population <- population(
  name = "UK",
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  initial_conditions = initial_conditions
)


max_time <- 600

# run an epidemic model using `epidemic`
output <- model_default(
  population = uk_population,
  time_end = max_time,
  increment = 1.0,
  # adjusted from getting started script to make epidemic shorter
  transmission_rate = 1.8 / 7
) |>
  # remove vaccinated as irrelevant to this work
  dplyr::filter(compartment != "vaccinated") |>
  # round for ease later
  dplyr::mutate(value = round(value))

output |>
  ggplot() +
  geom_line(aes(x = time, y = value, group = compartment, color = compartment)) +
  facet_grid(rows = vars(demography_group))


raw_incidence <- epidemics::new_infections(output) |>
  tibble::tibble() |>
  dplyr::mutate(value = new_infections, compartment = "new_infections", .keep = "unused")

incidence <- raw_incidence |>
  dplyr::summarise(value = sum(value), compartment = unique(compartment), .by = "time") |>
  dplyr::mutate(demography_group = "all") |>
  dplyr::bind_rows(raw_incidence)

# QA check the incidence curves
incidence |>
  ggplot() +
  geom_line(aes(x = time, y = value, group = demography_group, color = demography_group))


# Reporting delays and backfilling ####

# Lets assume cases are 20% of infections, and identified a on average 5 days after exposure

icr <- 0.2
case_identification_delay_shape <- 5


cases <- incidence |>
  # lets assume all cases reported are in the most elderly age group
  dplyr::filter(demography_group == "40+") |>
  # we need round numbers to work at the individual level later,
  # consider moving earlier in processing.
  dplyr::mutate(value = rpois(n = dplyr::n(), lambda = icr * value)) |>
  tidyr::uncount(weights = value, .id = "id") |>
  dplyr::mutate(
    delay = floor(rgamma(n = dplyr::n(), shape = case_identification_delay_shape, rate = 1)),
    time = time + delay
  ) |>
  dplyr::summarise(value = dplyr::n(), .by = c("time", "demography_group")) |>
  dplyr::mutate(compartment = "new_cases")
cases

## Generate revisions triangle #####

# use a gamma distribution for reporting delay
report_delay_shape <- 7
report_delay_rate <- 1

# The 'time' that the real-time analysis is conducted in the simulation
cut_off_time <- 150

reporting_rectangle <- cases |>
  tidyr::uncount(weights = value, .id = "id") |>
  # generate a delay according to our defined distribution
  dplyr::mutate(
    reporting_delay = floor(rgamma(n = dplyr::n(), shape = report_delay_shape, rate = report_delay_rate))
  ) |>
  dplyr::summarise(value = dplyr::n(), .by = c("time", "reporting_delay", "demography_group", "compartment"))

reported_cases <- reporting_rectangle |>
  dplyr::filter(time + reporting_delay <= cut_off_time) |>
  dplyr::summarise(value = sum(value), .by = c("time", "demography_group")) |>
  dplyr::mutate(compartment = "new_reported_cases")

reported_cases_report_date <- reporting_rectangle |>
  dplyr::mutate(time = time + reporting_delay) |>
  dplyr::summarise(value = sum(value), .by = c("time", "demography_group")) |>
  dplyr::mutate(compartment = "new_reported_cases_report_date") |>
  dplyr::filter(time <= cut_off_time)

combined_delay_cases <- cases |>
  dplyr::filter(demography_group == "40+") |>
  dplyr::bind_rows(reported_cases, reported_cases_report_date) |>
  tidyr::pivot_wider(values_from = value, names_from = compartment)


text_width <- 80

backfill_plot <- combined_delay_cases |>
  dplyr::filter(time < cut_off_time + 15) |>
  ggplot(aes(x = time)) +
  geom_vline(aes(xintercept = cut_off_time), linetype = 3) +
  geom_line(aes(y = new_cases, color = "final reported cases"), linetype = 1) +
  geom_line(aes(y = new_reported_cases, color = "real-time reported cases"), linetype = 2) +
  annotate("text", x = cut_off_time, label = "\ntime of analysis", y = 2e4, colour = "black", angle = 90) +
  labs(
    title = "A.",
    subtitle = stringr::str_wrap(
      paste(
        "The delay between specimen collection and report leaves",
        "recent data partially complete when counting cases by specimen detection time."
      ),
      width = text_width
    ),
    x = "specimen day",
    y = "count"
  ) +
  coord_cartesian(xlim = c(100, NA)) +
  scale_color_manual(
    name = NULL,
    values = c("final reported cases" = "royalblue", "real-time reported cases" = "firebrick")
  ) +
  theme(legend.position = "bottom")

backfill_plot

report_date_plot <- combined_delay_cases |>
  dplyr::filter(time < cut_off_time + 15) |>
  ggplot(aes(x = time)) +
  geom_vline(aes(xintercept = cut_off_time), linetype = 3) +
  geom_line(aes(y = new_cases, color = "final reported cases by specimen time"), linetype = 1) +
  geom_line(aes(y = new_reported_cases_report_date, color = "real-time reported cases by report time"), linetype = 2) +
  annotate("text", x = cut_off_time, label = "\ntime of analysis", y = 2e4, colour = "black", angle = 90) +
  labs(
    title = "B.",
    subtitle = stringr::str_wrap(
      paste(
        "Counting cases by date of report gives more complete data,",
        " but is delayed compared to the specimen detection time."
      ),
      width = text_width
    ),
    x = "Day",
    y = "Count"
  ) +
  coord_cartesian(xlim = c(100, NA)) +
  scale_color_manual(
    name = NULL,
    values = c(
      "final reported cases by specimen time" = "royalblue",
      "real-time reported cases by report time" = "forestgreen"
    )
  ) +
  theme(legend.position = "bottom")

report_date_plot

delay_plot <- backfill_plot / report_date_plot

delay_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "reporting_delay.png"),
  plot = delay_plot,
  width = 7,
  height = 9
)


# Transformations
# compare incident infections with reported cases on different scales

# add noise for the signal -> proxy, to make the proxy less reliable
noise_sd <- 0.0003

# generate a proxy signal that skews young
proxy <- incidence |>
  dplyr::filter(demography_group != "all") |>
  dplyr::mutate(
    weight = dplyr::case_match(
      demography_group,
      "[0,20)" ~ 0.7,
      "[20,40)" ~ 0.2,
      "40+" ~ 0.03
    )
  ) |>
  # scale and add some noise (because it's a proxy)
  dplyr::summarise(value = sum(rnorm(n = dplyr::n(), mean = 0.001, sd = noise_sd) * value * weight), .by = c("time")) |>
  # noise approach may add negative values
  dplyr::mutate(value = dplyr::if_else(value < 0, 0, value)) |>
  dplyr::mutate(demography_group = "all", compartment = "proxy")

# generate reported cases again without cut off
reported_cases_all <- reporting_rectangle |>
  dplyr::mutate(time = time + reporting_delay) |>
  dplyr::summarise(value = sum(value), .by = c("time", "demography_group")) |>
  dplyr::mutate(compartment = "cases")

transform_data_raw <- dplyr::bind_rows(proxy, reported_cases_all) |>
  dplyr::mutate(
    compartment_name = dplyr::case_match(
      compartment,
      "cases" ~ "Signal",
      "proxy" ~ "Indicator"
    )
  ) |>
  dplyr::mutate(demography_group = "combined")

# the proxy indicator is aligned with incidence (with some weighting across ages)
# and the reported cases are mean(time to report) + mean(reporting delay) days delayed from incidence.

proxy_plot <- transform_data_raw |>
  ggplot() +
  coord_cartesian(xlim = c(100, 250)) +
  geom_line(aes(x = time, y = value, color = compartment)) +
  facet_grid(rows = vars(compartment_name), scales = "free_y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("cases" = "maroon4", "proxy" = "darkorange"), ) +
  guides(color = "none") +
  labs(
    y = NULL,
    title = "A.",
    x = "Day",
    subtitle = stringr::str_wrap(
      paste(
        "The indicator and signal are generated from the same underlying epidemic",
        "process with different population coverage and delays."
      ),
      width = 80
    )
  )

proxy_plot

transform_data <- transform_data_raw |>
  dplyr::select(-compartment_name) |>
  tidyr::pivot_wider(values_from = value, names_from = compartment) |>
  # cases can be NA because of the time shift from incidence to case
  tidyr::replace_na(list(cases = 0)) |>
  dplyr::arrange(time) |>
  # add in transformations
  dplyr::mutate(
    # calculate smoothed signals for later use in growth rate
    proxy_smooth = zoo::rollmean(x = proxy, k = 21, align = "right", na.pad = TRUE),
    cases_smooth = zoo::rollmean(x = cases, k = 21, align = "right", na.pad = TRUE),
    # there is some differential impact for the fixed +0.1 value here as
    # the cases and proxy are on very different scales
    proxy_gr = log(proxy_smooth + 0.1) - dplyr::lag(log(proxy_smooth + 0.1)),
    cases_gr = log(cases_smooth + 0.1) - dplyr::lag(log(cases_smooth + 0.1)),
    proxy_log = log(proxy + 0.1),
    cases_log = log(cases + 0.1)
  ) |>
  # things are odd at the beginning and end of the simulation due to small numbers
  dplyr::filter(time <= 300, time > 50)

# QA the transformations
transform_data |>
  ggplot() +
  geom_line(aes(x = time, y = proxy_log)) +
  geom_line(aes(x = time, y = cases_log))

transform_data |>
  ggplot() +
  geom_line(aes(x = time, y = proxy_gr, color = "proxy")) +
  geom_line(aes(x = time, y = cases_gr, color = "cases"))


# calculate the ccfs with bootstrap.
# Set a maximum order of zero so tha the AR process is only on the
# 'natural' scale of the data passed in.
ccf_natural_results <- funtimes::ccf_boot(
  x = transform_data$proxy,
  y = transform_data$cases,
  ar.order = 0,
  lag.max = 30,
  plot = "none"
) |>
  dplyr::mutate(scale = "natural")

ccf_log_results <- funtimes::ccf_boot(
  x = transform_data$proxy_log,
  y = transform_data$cases_log,
  ar.order = 0,
  lag.max = 30,
  plot = "none"
) |>
  dplyr::mutate(scale = "log")

ccf_gr_results <- funtimes::ccf_boot(
  x = transform_data$proxy_gr,
  y = transform_data$cases_gr,
  ar.order = 0,
  lag.max = 30,
  plot = "none"
) |>
  dplyr::mutate(scale = "growth rate")

ccf_results <- dplyr::bind_rows(
  ccf_natural_results,
  ccf_log_results,
  ccf_gr_results
) |>
  dplyr::mutate(is_max = r_P == max(r_P), .by = scale) |>
  dplyr::mutate(scale = factor(scale, levels = c("natural", "log", "growth rate")))

ccf_plot <- ccf_results |>
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 5) +
  geom_ribbon(aes(x = Lag, ymin = lower_P, ymax = upper_P, fill = "CI"), alpha = 0.2) +
  geom_linerange(aes(x = Lag, ymin = 0, ymax = r_P, color = is_max), alpha = 0.5) +
  geom_point(aes(x = Lag, y = r_P, color = is_max)) +

  coord_cartesian(ylim = c(-0.5, 1), xlim = c(-30, 15)) +
  scale_x_continuous(breaks = seq(-30, 30, 5)) +
  labs(
    y = "Pearson correlation",
    x = "Lag (days)",
    title = "B.",
    subtitle = "The cross correlation estimated varies depending on the transformation applied."
  ) +
  scale_color_manual(
    name = NULL,
    values = c("TRUE" = "red", "FALSE" = "darkblue"),
    labels = c("TRUE" = "Maximum correlation"),
    breaks = c(TRUE)
  ) +
  scale_fill_manual(name = NULL, values = c("CI" = "black"), labels = c("CI" = "95% significance threshold")) +
  theme(legend.position = "bottom") +
  facet_grid(rows = vars(scale))

ccf_plot

transformation_plot <- proxy_plot / ccf_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "transformation.png"),
  plot = transformation_plot,
  width = 8,
  height = 10
)


# Smoothing & Denoising ####
# lets take the indicator variable and apply a range of smoothing methods then visualise.

smooth_data <- transform_data_raw |>
  dplyr::select(-compartment_name) |>
  tidyr::pivot_wider(values_from = value, names_from = compartment) |>
  # cases can be NA because of the time shift from incidence to case
  tidyr::replace_na(list(cases = 0)) |>
  dplyr::arrange(time) |>
  dplyr::mutate(
    # we want methods that will produce a clear visual difference
    proxy_smooth_7_right = zoo::rollmean(x = proxy, k = 7, align = "right", na.pad = TRUE),
    proxy_smooth_21_right = zoo::rollmean(x = proxy, k = 21, align = "right", na.pad = TRUE),
    proxy_loess = stats::loess(proxy ~ time, span = 0.1) |>
      stats::predict(data.frame(year = seq(1, max_time + 1, 1)))
  ) |>
  tidyr::pivot_longer(cols = dplyr::contains("proxy"))

# create plot that emphasises the smooth methods not the raw
smooth_plot <- smooth_data |>
  ggplot() +
  geom_line(aes(x = time, y = value, group = name, color = name), linewidth = 0.8) +
  coord_cartesian(xlim = c(90, 220)) +
  scale_color_manual(
    name = "Smoothing method",
    labels = c(
      "proxy" = "Raw data",
      "proxy_loess" = "LOESS",
      "proxy_smooth_7_right" = "Right aligned 7 day rolling average",
      "proxy_smooth_21_right" = "Right aligned 21 day rolling average"
    ),
    values = c(
      # take colours from Brewer Set1
      "proxy" = "grey70",
      "proxy_loess" = "#E41A1C",
      "proxy_smooth_7_right" = "#377EB8",
      "proxy_smooth_21_right" = "#984EA3"
    )
  ) +
  labs(y = "Indicator value", x = "Day") +
  theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = fs::path(output_dir, "smooth.png"),
  plot = smooth_plot,
  width = 10,
  height = 8
)
