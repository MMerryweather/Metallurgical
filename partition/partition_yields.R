partition_yields = function(tbl){
    tbl %>% mutate(product_mass = fraction_mass * recovery_to_product,
                   product_ash = fraction_ash * product_mass,
                   reject_mass = fraction_mass * recovery_to_reject,
                   reject_ash = fraction_ash * reject_mass) %>%
        select(product_mass, product_ash, reject_mass, reject_ash) %>%
        summarise_all(sum) %>%
        mutate(product_ash = product_ash / product_mass,
               reject_ash = reject_ash / reject_mass)
}