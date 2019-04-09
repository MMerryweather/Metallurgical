pacman::p_load(tidyverse, hrbrthemes, ggthemr)

# Washability Example
# -------------------
# Observations: 252
# Variables: 9
# Groups: unit, size_fraction [6]
# $ unit            <chr> "HF", "HF", "HF", "HF", "HF", "HF", "HF", "HF", "HF...
# $ stream          <chr> "Feed", "Feed", "Feed", "Feed", "Feed", "Feed", "Fe...
# $ size_fraction   <chr> "+1.4ww", "+1.4ww", "+1.4ww", "+1.4ww", "+1.4ww", "...
# $ sinks           <dbl> NA, 1.28, 1.30, 1.35, 1.40, 1.45, 1.50, 1.55, 1.60,...
# $ floats          <dbl> 1.28, 1.30, 1.35, 1.40, 1.45, 1.50, 1.55, 1.60, 1.7...
# $ fractional_mass <dbl> 42.2, 11.8, 18.4, 8.0, 5.2, 3.0, 1.8, 1.1, 1.2, 0.6...
# $ fractional_ash  <dbl> 3.1, 6.8, 9.6, 13.5, 17.7, 22.6, 28.0, 32.4, 37.9, ...
# $ cumulative_mass <dbl> 42.2, 54.0, 72.4, 80.4, 85.6, 88.6, 90.4, 91.5, 92....
# $ cumulative_ash  <dbl> 3.1, 3.9, 5.3, 6.1, 6.8, 7.3, 7.7, 8.0, 8.4, 8.6, 8...

Washability = read_csv(
  here::here("data/Head to Head/Washability.csv"),
  col_types = cols(
    unit = col_character(),
    stream = col_character(),
    size_fraction = col_character(),
    sinks = col_double(),
    floats = col_double(),
    fractional_mass = col_double(),
    fractional_ash = col_double(),
    cumulative_mass = col_double(),
    cumulative_ash = col_double()
  )
) %>%
  mutate(size_fraction = str_remove_all(size_fraction, " Washability")) %>%
  group_by(unit, size_fraction)

df = Washability %>% select(unit, stream, size_fraction, sinks, floats, fractional_mass) %>%
  spread(stream, fractional_mass) %>%
  mutate(floats = ifelse(is.na(floats), 2.4, floats))

partition_number = function(df) {
  # Based off Mass Fractions
  df %>% mutate(
    FT = Feed - Tails,
    PT = Con - Tails,
    FTPT = FT * PT,
    sum_FTPT = sum(FTPT),
    PTPT = PT * PT,
    sum_PTPT = sum(PTPT),
    yield = sum_FTPT / sum_PTPT,
    P = Con * yield,
    `T` = Tails * (1 - yield),
    `F` = P + `T`,
    PN = (P / `F`) * 100
  )
}
fit_partition_curve = function(df) {
  min.rss = function(data, par) {
    sum(((100 - (
      par[3] + (par[4] - par[3]) / (1 + exp((
        1.0986 * (par[1] - data$floats) / par[2]
      )))
    )) - data$PN) ^ 2)
  }

  results = optim(
    par = c(1.6, 0.2, 0, 100),
    fn = min.rss,
    data = df,
    lower = c(1, 0.005, 0, 0),
    upper = c(2.5, 0.5, 100, 100),
    method = "L-BFGS-B"
  )
  results
}
apply_partition_curve = function(float_sink_table, ep = 0.005, cutpoint = 1.5, t_0 = 0, t_100 = 100) {
  float_sink_table %>%
    mutate(
      recovery_to_product = (100 - (t_0 + (t_100 - t_0) / (1 + exp((1.0986 * (cutpoint - floats) / ep)
      )))) / 100,
      recovery_to_reject = 1 - recovery_to_product
    )
}

df_partition = df %>%
  group_by(unit, size_fraction) %>%
  nest() %>%
  mutate(
    partition = map(data, partition_number),
    coefs = map(partition, fit_partition_curve)
  )

df_coefficients = df_partition %>% select(unit, size_fraction, coefs) %>%
  mutate(
    convergence = map_dbl(coefs, "convergence"),
    d50 = map_dbl(coefs, ~ .$par[1]),
    ep = map_dbl(coefs, ~ .$par[2]),
    t_0 = map_dbl(coefs, ~ .$par[3]),
    t_100 = map_dbl(coefs, ~ .$par[4])
  ) %>%
  select(-coefs)

df_partition_results = df_partition %>%
  select(unit, size_fraction, partition) %>%
  unnest()

yields = df_partition_results %>% select(unit, size_fraction, yield) %>% unique()

yields %>%
  mutate(size_fraction = factor(size_fraction, levels = rev(
    c("+1.4ww", "-1.4ww +0.71mm", "-0.71mm +0.25mm")
  ), ordered = T)) %>%
  ggplot(aes(
    x = size_fraction,
    y = yield,
    colour = unit,
    group = unit
  )) +
  geom_line() +
  geom_point(
    shape = 21,
    size = 3,
    fill = "white",
    show.legend = F
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  labs(
    title = "",
    subtitle = "",
    x = "Size Fraction",
    y = "Yield",
    colour = "Method"
  ) +
  theme_ipsum_rc() +
  no_vertical_gridlines() +
  no_minor_gridlines()
ggsave(filename = "outputs/_______.png",
       device = "png",
       width = 4, height = 2, units = "in", dpi = 320,
       scale = 3)

sgs = floats_sink_table = data.frame(floats = seq(from = 1.2,
                                                  to = 2.4,
                                                  by = 0.01))

df_fitted = df_coefficients %>%
  mutate(fits = pmap(
    list(
      cutpoint = d50,
      ep = ep,
      t_0 = t_0,
      t_100 = t_100
    ),
    apply_partition_curve,
    sgs
  )) %>%
  unnest()

df_fitted %>%
  ggplot(aes(x = floats,
             y = recovery_to_product,
             colour = unit)) +
  geom_line() +
  facet_wrap( ~ size_fraction) +
  geom_point(data = df_partition_results %>% transmute(unit, size_fraction, floats, recovery_to_product = PN /
                                                         100)) +
  scale_x_continuous(limits = c(1.2, 2.4), breaks = seq(1.2, 2.4, 0.2)) +
  theme_ipsum_rc() +
  no_minor_gridlines() +
  scale_y_percent() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    colour = "Method"
  )
ggsave(filename = "outputs/___________.png",
       device = "png",
       width = 4, height = 2, units = "in", dpi = 320,
       scale = 3)

df_fitted %>%
  ggplot(aes(x = floats,
             y = recovery_to_product,
             colour = size_fraction)) +
  geom_line() +
  facet_wrap( ~ unit) +
  geom_point(
    data = df_partition_results %>% transmute(unit, size_fraction, floats, recovery_to_product = PN /
                                                100),
    show.legend = F
  ) +
  scale_x_continuous(limits = c(1.2, 2.4), breaks = seq(1.2, 2.4, 0.2)) +
  theme_ipsum_rc() +
  no_minor_gridlines() +
  scale_y_percent() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    colour = "Method"
  )
ggsave(filename = "outputs/_________________.png",
       device = "png",
       width = 4, height = 2, units = "in", dpi = 320,
       scale = 3)

df_coefficients %>%
  ggplot(aes(
    y = size_fraction,
    x = d50,
    colour = unit,
    group = unit
  )) +
  geom_line() +
  geom_point(
    shape = 21,
    size = 3,
    fill = "white",
    show.legend = F
  ) +
  theme_ipsum_rc() +
  no_horizontal_gridlines() +
  no_minor_gridlines() +
  scale_x_continuous(limits = c(1.2, 2.2), breaks = seq(1.2, 2.4, 0.1)) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    colour = ""
  )
ggsave(filename = "outputs/_____________.png",
       device = "png",
       width = 4, height = 2, units = "in", dpi = 320,
       scale = 3)
