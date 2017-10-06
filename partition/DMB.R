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

    # DMB_circuit_percentage = function(RR_k){
    #     RR = data.frame(RR_n = 0.580756, RR_k = RR_k)
    #     (100 - apply_rosin_rammler(20, RR))/100
    # }
    # find_RR_k = function(target_circuit_split, lower, upper){
    #     optimize(function(y) abs(DMB_circuit_percentage(y)-target_circuit_split), lower=lower, upper=upper)$minimum
    # }
    # df = quiet_read("data/raw_plant_feed_sizing.csv")[["result"]] %>%
    #     extract_size_fractions() %>%
    #     cumulative_mass()
    #
    # RR = df %>% rosin_rammler_coefficients()

    # hold spread constant, change P80
    # Given the known RR spread, find RR P80 that satisfied target circuit split
    #fitted_RR = data.frame(RR_n = RR$RR_n, RR_k=find_RR_k(circuit_feed_percentage,1,50))
    # Now we know the RR k / P80, find DMC split aka difference between passing at 20mm and 2mm
    #DMC_circuit_percentage = (apply_rosin_rammler(20, fitted_RR) - apply_rosin_rammler(2, fitted_RR))/100
#------------------------------------------------------------------------------
#     DMB_yield = 79.28
#     DMC_yield = 86.38
#
# output = data.frame(rate = rate,
#               DMB_circuit_split = condition$DMB,
#               DMC_circuit_split = condition$DMC,
#               misplaced_into_DMB = misplaced_material(rate) / 100) %>%
#     mutate(misplaced_from_DMC = (condition$DMB * ( 1 - misplaced_into_DMB))/ condition$DMC,
#            DMC_misplaced_of_plant_feed = misplaced_from_DMC * condition$DMC,
#            feed_percent_to_DMC = condition$DMC-misplaced_from_DMC,
#            DMC_fraction_yield= ((misplaced_from_DMC * DMB_yield + feed_percent_to_DMC * DMC_yield)/condition$DMC)/100,
#            DMC_loss = DMC_misplaced_of_plant_feed *(DMC_yield - DMB_yield)/100,
#            DMB_contribution_to_plant_yield = condition$DMB + misplaced_from_DMC*condition$DMC)
#     output
}



# conditions = list(rates = seq(2000, 2500, by=50),
#                   percentages = seq(0.1, 0.25, by =0.025)) %>%
#     cross()
#
# impact = conditions %>%
#     map_df(~DMB(rate = .x$rates, circuit_feed_percentage = .x$percentages )) %>%
#     group_by(DMB_circuit_split) %>%
#     mutate(base_yield = max(DMC_fraction_yield),
#            yield_delta_circuit = DMC_fraction_yield - base_yield,
#            yield_delta_plant = yield_delta_circuit * DMC_circuit_split)
#
# impact %>%
#     ggplot(aes(rate, yield_delta_plant, colour = DMB_circuit_split, group = DMB_circuit_split)) %>%
#     add(geom_line()) %>%
#     add(scale_y_continuous(labels = scales::percent_format()))
