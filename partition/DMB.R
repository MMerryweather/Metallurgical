# rm(list=ls())

DMB = function(condition){

    DMC_circuit_percentage = condition$DMC
    circuit_feed_percentage = condition$DMB
    rate = condition$rate

    quiet_read = quietly(read_csv)
    source("functions/extract_size_fractions.R")
    source("functions/rosin_rammler_coefficients.R")
    source("functions/cumulative_mass.R")
    source("functions/apply_rosin_rammler.R")
    source("functions/apply_partition_curve.R")
    source("functions/predict_DMC_ep_from_rate.R")
    source("functions/partition_yields.R")

    quiet_read = quietly(read_csv)
    misplaced_material = function(rate){
        rate*(0.04333/2)-24.667
    }
    # % of undersize going to oversize, assume all DMC material

    DMB_float_sink = quiet_read("data/DMC_float_sink.csv")[["result"]] %>%
        apply_partition_curve(ep=0.024,cutpoint = 1.5, t_0 = 8.38, t_100 = 99) %>%
        partition_yields() %>%
        mutate(circuit="DMB",
               circuit_split = condition$DMB)

    DMC_float_sink = quiet_read("data/DMC_float_sink.csv")[["result"]] %>%
        apply_partition_curve(ep=predict_DMC_ep_from_rate(rate), cutpoint = 1.5, t_0 = 0, t_100 = 100) %>%
        partition_yields()%>%
        mutate(circuit="DMC",
               circuit_split = condition$DMC)

    DM = DMB_float_sink %>% bind_rows(DMC_float_sink) %>%
        select(circuit, product_mass) %>% bind_cols(data.frame(circuit_split = c(condition$DMB,condition$DMC)))

    underflow_misplaced = misplaced_material(rate) / 100
    plant_feed_misplaced = (1 - condition$DMB) * underflow_misplaced
    DMC_feed_misplaced = plant_feed_misplaced / condition$DMC
    DMC_misplaced_of_plant_feed = DMC_feed_misplaced * condition$DMC

    DMB_actual_of_plant_feed = DMC_misplaced_of_plant_feed + condition$DMB
    DMC_actual_of_plant_feed = condition$DMC - DMC_misplaced_of_plant_feed

    DMB_yield = DMB_float_sink$product_mass / 100
    DMC_yield = DMC_float_sink$product_mass / 100

    output = data.frame(rate = rate,
                        RR_k = condition$k,
                        DMB_yield = DMB_yield * DMB_actual_of_plant_feed,
                        DMC_yield = DMC_yield * DMC_actual_of_plant_feed)
