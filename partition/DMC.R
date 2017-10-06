DMC = function(SG = 1.43, circuit_feed_percentage = 0.403, rates){
    # SG is a single number,
    # circuit_feed_percentage is another scalar
    # Rates is a sequence e.g seq(2000, 2500, by = 50)

    pacman::p_load(tidyverse, magrittr, ggthemr)
    source("functions/apply_partition_curve.R")
    source("functions/predict_DMC_ep_from_rate.R")
    source("functions/partition_yields.R")

    quiet_read = quietly(read_csv)
    DMC_float_sink = quiet_read("data/DMC_float_sink.csv")[["result"]]

    rates = rates %>% as.list()
    eps = rates %>% map(predict_DMC_ep_from_rate)

    recoveries = eps %>%
        map(~ apply_partition_curve(DMC_float_sink, ep=., cutpoint = SG))

    yields = recoveries %>% map(partition_yields) %>%
        map2(rates, ~ .x %>% mutate(rate=.y)) %>%
        map2_df(eps, ~ .x %>% mutate(ep=.y))

    base_yield = yields %>% filter(rate == 2000) %>% pluck("product_mass")
    yields %>% mutate(yield_delta_circuit = (product_mass - base_yield) / 100,
                               yield_delta_plant = yield_delta_circuit * circuit_feed_percentage) %>%
        select(rate, ep, yield_delta_plant, yield_delta_circuit, everything())
}
