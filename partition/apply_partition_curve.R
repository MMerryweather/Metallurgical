apply_partition_curve = function(float_sink_table, ep = 0.005, cutpoint = 1.5, t_0 = 0, t_100 = 100){
    float_sink_table %>%
        mutate(recovery_to_product = (100 - (t_0 + (t_100 - t_0) / (1 + exp((1.0986 * (cutpoint - floats) / ep))))) / 100,
               recovery_to_reject = 1 - recovery_to_product)
}
