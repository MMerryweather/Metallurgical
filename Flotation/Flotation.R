Flotation = function(condition){

    # circuit_feed_percentage is another scalar
    rate = condition$rate
    circuit_feed_percentage = condition$Floats
    flotation_feed_tonnes = rate * circuit_feed_percentage
    quiet_read = quietly(read_csv)
    source("functions/gaudin_parameter_estimation.R")
    source("functions/gaudin_parameter_estimation.R")
    source("functions/carrying_capacity.R")
    source("functions/jameson_cell_carrying_limit.R")

    # Setup cells
    primary_cells = list(cell_model = "J5000/10",
                         N_cells = 6,
                         air_flow_rate = 700, #m3/h
                         target_ash = 4.5, #%
                         feed_SG = 1.388)

    secondary_cells = list(cell_model = "J6500/24",
                           N_cells = 2,
                           air_flow_rate = 1700, #m3/h
                           target_ash = 6.1, #%
                           feed_SG = 1.388)

    primary_feed = quiet_read("data/flotation_primary_feed_sizing.csv")[["result"]] %>%
        mutate(tonnes = fraction_mass * flotation_feed_tonnes)

    primary_con_tonnes = jameson_cell_carrying_limit(primary_cells$cell_model,primary_cells$air_flow_rate,primary_cells$N_cells,primary_cells$target_ash, primary_feed)$total_carrying_capacity

    primary_con = quiet_read("data/flotation_primary_con_sizing.csv")[["result"]] %>%
        mutate(tonnes = fraction_mass * primary_con_tonnes)

    primary_tail = primary_feed %>%
        inner_join(primary_con,
                   by = c("size_fraction"="size_fraction",
                          "bottom_size" = "bottom_size",
                          "top_size" = "top_size",
                          "geom_mean" = "geom_mean"),
                   suffix = c("_feed","_tail")) %>%
        select(-contains("fraction_")) %>%
        mutate(tonnes = tonnes_feed - tonnes_tail,
               fraction_mass = tonnes / sum(tonnes)) %>%
        select(-contains("tonnes_"))

    secondary_results = jameson_cell_carrying_limit(secondary_cells$cell_model,secondary_cells$air_flow_rate,secondary_cells$N_cells,secondary_cells$target_ash, primary_tail)

    mass_balance = data.frame(rate = rate,
                              pri_feed = flotation_feed_tonnes,
                              pri_con = primary_con_tonnes) %>%
        mutate(
            pri_tail = sum(primary_tail$tonnes),
            sec_feed = sum(primary_tail$tonnes),
            sec_con = secondary_results$total_carrying_capacity,
            sec_tail = sum(primary_tail$tonnes) - secondary_results$total_carrying_capacity
        ) %>%
        mutate(
            pri_yield = pri_con / pri_feed,
            sec_yield = sec_con / sec_feed,
            sec_yield_overall = sec_con / pri_feed,
            total_con = pri_con + sec_con,
            total_yield = sec_yield_overall + pri_yield,
            plant_yield = (sec_con + pri_con) / rate
        )

    mass_balance %>% mutate(RR_k = condition$k) %>% select(rate, RR_k, plant_yield)
}
#
# conditions = list(rates = seq(2000, 2500, by=50),
#                   percentages = seq(0.1, 0.25, by =0.025)) %>%
#     cross()
#
# impact = conditions %>%
#     map(~Flotation(rate = .x$rates, circuit_feed_percentage = .x$percentages )) %>%
#     map2_df(conditions, ~ .x %>% mutate(flotation_percentage = .y$percentages)) %>%
#     group_by(flotation_percentage) %>%
#     mutate(base_yield = max(total_yield),
#            yield_delta_circuit = total_yield - base_yield,
#            yield_delta_plant = yield_delta_circuit * flotation_percentage)
#
# impact %>%
#     ggplot(aes(rate, yield_delta_plant, colour = flotation_percentage, group = flotation_percentage)) %>%
#     add(geom_line()) %>%
#     add(scale_y_continuous(labels = scales::percent_format()))





