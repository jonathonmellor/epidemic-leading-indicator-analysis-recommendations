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
library(fs)
library(zoo)

theme_set(theme_bw())

set.seed(07734)


output_dir <- fs::dir_create(here::here("outputs"))
output_dir_tiff <- fs::dir_create(here::here("outputs", "tiff"))
output_dir_supp <- fs::dir_create(here::here("outputs", "supp"))

# Generate epidemic ####

polymod <- socialmixr::polymod
contact_data <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  # add in another group for more differentiation
  age_limits = c(0, 20, 40),
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


max_time <- 350

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
incidence_plot <- incidence |>
  ggplot() +
  geom_line(aes(x = time, y = value/100000, group = demography_group, color = demography_group)) +
  labs(y="New infections (per 100k)",
       x="Days") +
  theme(legend.position = "bottom") +
  scale_color_brewer(name="Demography group",
                     palette="Set1")


# Reporting delays and backfilling ####

# Lets assume cases are 20% of infections, and identified a on average 5 days after exposure

icr <- 0.2
case_identification_delay_shape <- 4


cases <- incidence |>
  # lets assume all cases reported are in the most elderly age group
  dplyr::filter(demography_group == "[40,Inf)") |>
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
report_delay_shape <- 5
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
  dplyr::filter(demography_group == "[40,Inf)") |>
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

ggplot2::ggsave(
  filename = fs::path(output_dir_tiff, "reporting_delay.tiff"),
  plot = delay_plot,
  width = 7,
  height = 9
)


# Transformations
# compare incident infections with reported cases on different scales

# add noise for the signal -> proxy, to make the proxy less reliable
noise_rate <- 10
noise_shape <- 10

# generate a proxy signal that skews young
proxy <- incidence |>
  dplyr::filter(demography_group != "all") |>
  dplyr::mutate(
    weight = dplyr::recode_values(
      demography_group,
      "[0,20)" ~ 0.7,
      "[20,40)" ~ 0.2,
      "[40,Inf)" ~ 0.1
    )
  ) |>
  # scale and add some noise (because it's a proxy)
  dplyr::summarise(value = sum((rgamma(n = dplyr::n(), shape = noise_shape, rate = noise_rate)) * value * weight/1000), .by = c("time")) |>
  # noise approach may add negative values
  dplyr::mutate(demography_group = "all", compartment = "proxy")

# generate reported cases again without cut off
reported_cases_all <- reporting_rectangle |>
  dplyr::mutate(time = time + reporting_delay) |>
  dplyr::summarise(value = sum(value), .by = c("time", "demography_group")) |>
  dplyr::mutate(compartment = "cases")

transform_data_raw <- dplyr::bind_rows(proxy, reported_cases_all) |>
  dplyr::mutate(
    compartment_name = dplyr::recode_values(
      compartment,
      "cases" ~ "Signal",
      "proxy" ~ "Indicator"
    )
  ) |>
  dplyr::mutate(demography_group = "combined")


# Smoothing & Denoising ####
# lets take the indicator variable and apply a range of smoothing methods then visualise.


# fit two different gams to demonstrate statistical modelling approaches
gam_2nd_order <- mgcv::gam(
  formula = as.formula(value ~ s(time, bs="tp", m=1, k=round(max_time/20))),
  data = transform_data_raw |>
    dplyr::filter(compartment == "proxy",
                  value !=0),
  family=Gamma(link="log")
)

gam_2nd_order_results <- gratia::add_fitted_samples(object = transform_data_raw |>
                                                      dplyr::filter(compartment == "proxy"),
                                                    model=gam_2nd_order,
                                                    scale = "response",
                                                    method="mh",
                                                    n=2000) |>
  dplyr::summarise(
    q50=(quantile(.fitted, 0.5)),
    q95=(quantile(.fitted, 0.95)),
    q5=(quantile(.fitted, 0.05)),
    .by=c(time, compartment, demography_group)) |>
  dplyr::mutate(model = "GAM 2nd order TP")

gam_1st_order <- mgcv::gam(
  formula = as.formula(value ~ s(time, bs="tp", m=2, k=round(max_time/20))),
  data = transform_data_raw |>
    dplyr::filter(compartment == "proxy",
                  value !=0),
  family=Gamma(link="log")
)

gam_1st_order_results <- gratia::add_fitted_samples(object = transform_data_raw |>
                                                      dplyr::filter(compartment == "proxy"),
                                                    model=gam_1st_order,
                                                    scale="response",
                                                    method="mh",
                                                    n=2000) |>
  dplyr::summarise(
    q50=(quantile(.fitted, 0.5)),
    q95=(quantile(.fitted, 0.95)),
    q5=(quantile(.fitted, 0.05)),
    .by=c(time, compartment, demography_group)) |>
  dplyr::mutate(model = "GAM 1st order TP")

gam_signal <- mgcv::gam(
  formula = as.formula(value ~ s(time, bs="tp", m=1, k=round(max_time/20))),
  data = transform_data_raw |>
    dplyr::filter(compartment == "cases", demography_group=="combined"),
  family="nb"
)

gam_signal_results <- gratia::add_fitted_samples(object = transform_data_raw |>
                                                      dplyr::filter(compartment == "cases",
                                                                    demography_group == "combined"),
                                                    model=gam_signal,
                                                    scale="response",
                                                    method="mh",
                                                    n=2000) |>
  dplyr::summarise(
    q50=(quantile(.fitted, 0.5)),
    q95=(quantile(.fitted, 0.95)),
    q5=(quantile(.fitted, 0.05)),
    .by=c(time, compartment, demography_group)) |>
  dplyr::mutate(model = "Signal") |>
  dplyr::left_join(transform_data_raw |>
                     dplyr::filter(compartment == "cases",
                                   demography_group == "combined") |>
                     dplyr::select(-c(compartment, compartment_name)) |>
                     dplyr::rename(cases=value),
                   by=c("time", "demography_group"))


gam_signal_results |>
  ggplot() +
  geom_line(aes(x=time, y=q50, color="Median estimate")) +
  geom_line(aes(x=time, y=cases, color="Cases"), linewidth=0.8) +
  geom_ribbon(aes(x=time, ymin=q5, ymax=q95, fill="90% confidence interval"), alpha=0.5) +
  coord_cartesian(xlim = c(90, 220)) +
  scale_fill_brewer(palette="Set1") +
  scale_color_brewer(palette="Set1") +
  labs(y = "Cases", x = "Day") +
  theme(legend.position = "bottom")

gam_indicator_results <- dplyr::bind_rows(
  gam_1st_order_results,
  gam_2nd_order_results
) |>
  dplyr::select(-compartment) |>
  dplyr::left_join(transform_data_raw |>
                     dplyr::filter(compartment == "proxy") |>
                     dplyr::select(-compartment) |>
                     dplyr::rename(proxy=value),
                   by=c("time", "demography_group"))

gam_indicator_plot <- gam_indicator_results |>
  ggplot() +
  geom_line(aes(x=time, y=proxy), linewidth=0.8, color="black") +
  geom_line(aes(x=time, y=q50, group=model, color=model)) +
  geom_ribbon(aes(x=time, ymin=q5, ymax=q95, group=model, fill=model), alpha=0.5) +
  coord_cartesian(xlim = c(90, 220)) +
  scale_fill_brewer(palette="Set1") +
  labs(y = "Indicator value", x = "Day",
       title="B.") +
  theme(legend.position = "bottom")

gam_indicator_plot

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
      stats::predict(data.frame(time = seq(1, dplyr::n(), 1)))
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
      "proxy" = "black",
      "proxy_loess" = "#E41A1C",
      "proxy_smooth_7_right" = "#377EB8",
      "proxy_smooth_21_right" = "#984EA3"
    )
  ) +
  labs(y = "Indicator value", x = "Day",
       title = "A.") +
  theme(legend.position = "bottom")

smooth_plot


final_smooth_plot <- smooth_plot / gam_indicator_plot

final_smooth_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "smooth.png"),
  plot = final_smooth_plot,
  width = 10,
  height = 14
)

ggplot2::ggsave(
  filename = fs::path(output_dir_tiff, "smooth.tiff"),
  plot = final_smooth_plot,
  width = 10,
  height = 8
)


# the proxy indicator is aligned with incidence (with some weighting across ages)
# and the reported cases are mean(time to report) + mean(reporting delay) days delayed from incidence.

proxy_plot <- transform_data_raw |>
  ggplot() +
  coord_cartesian(xlim = c(50, 250)) +
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

# Estimate growth rates
gam_indicator_gr <- gratia::derivative_samples(data = transform_data_raw |>
                                                      dplyr::filter(compartment == "proxy"),
                                                    focal="time",
                                                    object=gam_1st_order,
                                                    scale="linear_predictor",
                                                    method="mh",
                                                    n=2000) |>
  dplyr::summarise(
    q50=(quantile(.derivative, 0.5)),
    q95=(quantile(.derivative, 0.95)),
    q5=(quantile(.derivative, 0.05)),
    .by=c(time)) |>
  dplyr::mutate(model = "Indicator")

gam_signal_gr <- gratia::derivative_samples(data = transform_data_raw |>
                                                 dplyr::filter(compartment == "cases",
                                                               demography_group=="combined"),
                                               focal="time",
                                               object=gam_signal,
                                               scale="linear_predictor",
                                               method="mh",
                                               n=2000) |>
  dplyr::summarise(
    q50=(quantile(.derivative, 0.5)),
    q95=(quantile(.derivative, 0.95)),
    q5=(quantile(.derivative, 0.05)),
    .by=c(time)) |>
  dplyr::mutate(model = "Signal")

gam_gr_results <- dplyr::bind_rows(
  gam_signal_gr,
  gam_indicator_gr
)

gr_plot <- gam_gr_results |>
  ggplot() +
  geom_hline(aes(yintercept=0), linetype=2) +
  geom_ribbon(aes(x=time, ymin=q5, ymax=q95, group=model, fill=model), alpha=0.5) +
  geom_line(aes(x=time, y=q50, group=model, color=model)) +
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(ylim=c(-0.1, 0.1),
                  xlim = c(50, 250)) +
  labs(y="Daily growth rate",
       x="Day",
       subtitle="The estimated growth rate varies across indicator and signal over time.",
       title="B.") +
  scale_color_manual(name=NULL, values = c("Signal" = "maroon4", "Indicator" = "darkorange"), ) +
  scale_fill_manual(name=NULL,values = c("Signal" = "maroon4", "Indicator" = "darkorange"), ) +
  theme(legend.position = "bottom")

gr_plot

gam_gr_results_wide <- gam_gr_results |>
  dplyr::mutate(model = stringr::str_to_lower(model)) |>
  tidyr::pivot_wider(names_from=model, values_from = dplyr::starts_with("q")) |>
  dplyr::arrange(time) |>
  dplyr::filter(time >= 50,
                time <= 250)





# calculate the ccfs with bootstrap.
# Set a maximum order of zero so tha the AR process is only on the
# raw natural scale

transform_data_clipped <- transform_data |>
  dplyr::filter(time >= 50,
                time <= 250)

# bring signal and indicator together from modelled estimate
smooth_results <- dplyr::bind_rows(
  gam_signal_results,
  gam_indicator_results |>
    dplyr::filter(model == "GAM 1st order TP") |>
    dplyr::mutate(model= "Indicator")
) |>
  dplyr::select(-c(cases, proxy, compartment_name, compartment, demography_group)) |>
  dplyr::mutate(model = stringr::str_to_lower(model)) |>
  tidyr::pivot_wider(names_from=model, values_from= dplyr::starts_with("q")) |>
  dplyr::arrange(time) |>
  dplyr::filter(time >= 50,
                time <= 250)

ccf_raw_results <- funtimes::ccf_boot(
  x = transform_data_clipped$proxy,
  y = transform_data_clipped$cases,
  ar.order = 0,
  lag.max = 40,
  plot = "none"
) |>
  dplyr::mutate(scale = "raw")

# smoothed natural scale
ccf_smooth_results <- funtimes::ccf_boot(
  x = smooth_results$q50_indicator,
  y = smooth_results$q50_signal,
  ar.order = 0,
  lag.max = 40,
  ic="none",
  plot = "none"
) |>
  dplyr::mutate(scale = "smooth")

# log scaled from smooth
ccf_log_results <- funtimes::ccf_boot(
  x = log(smooth_results$q50_indicator),
  y = log(smooth_results$q50_signal),
  ar.order = 0,
  lag.max = 40,
  ic="none",
  plot = "none"
) |>
  dplyr::mutate(scale = "smooth log")

# growth rate from smooth
ccf_gr_results <- funtimes::ccf_boot(
  x = gam_gr_results_wide$q50_indicator,
  y = gam_gr_results_wide$q50_signal,
  ar.order = 0,
  lag.max = 40,
  ic="none",
  plot = "none"
) |>
  dplyr::mutate(scale = "growth rate")




ccf_results <- dplyr::bind_rows(
  ccf_raw_results,
  ccf_smooth_results,
  ccf_log_results,
  ccf_gr_results
) |>
  # choose spearman or pearson statistic
  dplyr::mutate(scale = factor(stringr::str_wrap(scale, width = 8), levels = c("raw", "smooth", "smooth\nlog", "growth\nrate")))

ccf_plot <- ccf_results |>
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 5) +
  geom_ribbon(aes(x = Lag, ymin = lower_S, ymax = upper_S, fill = "CI"), alpha = 0.2) +
  geom_linerange(aes(x = Lag, ymin = 0, ymax = r_S), alpha = 0.5) +
  geom_point(aes(x = Lag, y = r_S), size=0.75) +
  coord_cartesian(ylim = c(-0.3, 1), xlim = c(-40, 40)) +
  scale_x_continuous(breaks = seq(-40, 40, 5)) +
  labs(
    y = "Spearman correlation",
    x = "Lag (days)",
    title = "C.",
    subtitle = "The cross correlation estimated varies depending on the transformation applied."
  ) +
  scale_fill_manual(name = NULL, values = c("CI" = "black"), labels = c("CI" = "95% significance threshold")) +
  theme(legend.position = "bottom") +
  facet_grid(rows = vars(scale))

ccf_plot

transformation_plot <- (((proxy_plot / gr_plot) +
                           patchwork::plot_layout(axes="collect")) / ccf_plot) +
  patchwork::plot_layout(height=c(1.3, 1.3, 2))


transformation_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "transformation.png"),
  plot = transformation_plot,
  width = 8,
  height = 10
)

ggplot2::ggsave(
  filename = fs::path(output_dir_tiff, "transformation.tiff"),
  plot = transformation_plot,
  width = 8,
  height = 10
)


# Uncertainty ######
# Demonstrate uncertainty by comparing different peak timings

signal_samples <- gratia::add_fitted_samples(object = transform_data_raw |>
                             dplyr::filter(compartment == "cases",
                                           demography_group == "combined"),
                           model=gam_signal,
                           scale="response",
                           method="mh",
                           n=500) |>
  dplyr::select(time, .fitted, .draw) |>
  dplyr::mutate(metric="Signal")

indicator_1st_order_samples <- gratia::add_fitted_samples(object = transform_data_raw |>
                                                      dplyr::filter(compartment == "proxy"),
                                                    model=gam_1st_order,
                                                    scale="response",
                                                    method="mh",
                                                    n=500) |>
  dplyr::select(time, .fitted, .draw) |>
  dplyr::mutate(metric="indicator_model_1")

indicator_2nd_order_samples <- gratia::add_fitted_samples(object = transform_data_raw |>
                                                            dplyr::filter(compartment == "proxy"),
                                                          model=gam_2nd_order,
                                                          scale="response",
                                                          method="mh",
                                                          n=500) |>
  dplyr::select(time, .fitted, .draw) |>
  dplyr::mutate(metric="indicator_model_2")

sample_results <- dplyr::bind_rows(
  signal_samples,
  indicator_1st_order_samples,
  indicator_2nd_order_samples
)

peak_samples <- sample_results |>
  # This assumes no ties in max value
  dplyr::filter(.fitted == max(.fitted, na.rm=TRUE), .by=c(metric, .draw))

peak_estimate <- peak_samples |>
  dplyr::mutate(metric = dplyr::recode(
    metric,
    "indicator_model_1" = "Indicator\n(GAM 1st order TP)",
    "indicator_model_2" = "Indicator\n(GAM 2nd order TP)",
  ))|>
  dplyr::mutate(metric = factor(metric, levels = c("Signal", "Indicator\n(GAM 1st order TP)", "Indicator\n(GAM 2nd order TP)")))


difference_samples <- peak_samples |>
  dplyr::mutate(metric = stringr::str_to_lower(metric)) |>
  tidyr::pivot_wider(id_cols = .draw, names_from = metric, values_from = time) |>
  dplyr::mutate(diff_model_1 = signal - indicator_model_1,
                diff_model_2 = signal - indicator_model_2) |>

  tidyr::pivot_longer(cols = dplyr::starts_with("diff")) |>
  dplyr::mutate(metric = stringr::str_remove(name, "diff_")) |>
  dplyr::select(-name) |>
  dplyr::mutate(metric = dplyr::recode(
    metric,
    "model_1" = "Indicator\n(GAM 1st order TP)",
    "model_2" = "Indicator\n(GAM 2nd order TP)",
  )) |>
  dplyr::mutate(metric = factor(metric, levels = c("Signal", "Indicator\n(GAM 1st order TP)", "Indicator\n(GAM 2nd order TP)")))

signal_col <- "#E41A1C"
indication1_col <- "#377EB8"
indication2_col <- "#984EA3"


wave_plot <- sample_results |>
  dplyr::mutate(metric = dplyr::recode(
    metric,
    "indicator_model_1" = "Indicator\n(GAM 1st order TP)",
    "indicator_model_2" = "Indicator\n(GAM 2nd order TP)",
  )) |>
  dplyr::mutate(metric = factor(metric, levels = c("Signal", "Indicator\n(GAM 1st order TP)", "Indicator\n(GAM 2nd order TP)")))|>
  ggplot() +
  geom_line(aes(x=time, y=.fitted, group=.draw, color=metric), alpha=0.01) +
  facet_grid(rows=vars(metric), scales="free_y") +
  coord_cartesian(xlim=c(50,250)) +
  labs(title = "A.",
       x="Day",
       y=NULL) +
  scale_color_manual(values = c(
    "Signal"=signal_col,
    "Indicator\n(GAM 1st order TP)" = indication1_col,
    "Indicator\n(GAM 2nd order TP)" = indication2_col
  )) +
  guides(color="none")

wave_plot


peak_timing_plot <- peak_estimate |>
  ggplot() +
  ggdist::stat_pointinterval(aes(x=time, y=metric, color=metric)) +
  labs(title = "B.",
       x="Estimated peak day",
       y=NULL) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(
    "Signal"=signal_col,
    "Indicator\n(GAM 1st order TP)" = indication1_col,
    "Indicator\n(GAM 2nd order TP)" = indication2_col
  )) +
  guides(color="none") +
  scale_y_discrete(limits=rev)

peak_timing_plot

lead_time_plot <- difference_samples |>
  ggplot() +
  ggdist::stat_slabinterval(aes(x=value, y=metric, color=metric),
                            density="histogram",
                            breaks=seq(10,26, 1)) +
  scale_x_continuous(breaks=seq(10, 30, 2)) +
  labs(title = "C.",
       x="Estimated peak difference (days)",
       y=NULL) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(
    "Signal"=signal_col,
    "Indicator\n(GAM 1st order TP)" = indication1_col,
    "Indicator\n(GAM 2nd order TP)" = indication2_col
  )) +
  guides(color="none")+
  scale_y_discrete(limits=rev)

lead_time_plot

uncertainty_plot <- wave_plot / peak_timing_plot / lead_time_plot + patchwork::plot_layout(heights=c(1, 0.5, 0.5))

uncertainty_plot

ggplot2::ggsave(
  filename = fs::path(output_dir, "uncertainty.png"),
  plot = uncertainty_plot,
  width = 8,
  height = 10
)

ggplot2::ggsave(
  filename = fs::path(output_dir_tiff, "uncertainty.tiff"),
  plot = uncertainty_plot,
  width = 8,
  height = 10
)
