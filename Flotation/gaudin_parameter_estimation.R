gaudin_parameter_estimation = function(feed_sizing){
    #P50 in micron
    tbl = feed_sizing %>%
        mutate(cuml_retained_mass = cumsum(fraction_mass) * 100,
               cuml_passing_mass  = 100 - cuml_retained_mass)

    lm_model = lm = tbl %>%
        filter(top_size != max(top_size) & bottom_size != min(bottom_size)) %>%
        lm(cuml_passing_mass ~ log(geom_mean), data=.)
    P50 = exp((50-lm_model$coefficients[1])/lm_model$coefficients[2])
    nls_model = nls(cuml_passing_mass ~ 100 * ((geom_mean / P50)^modulus), data = tbl, start=list(modulus=0.05))
    modulus = nls_model$m$getAllPars()

    output = list()
    output$tbl = tbl %>% mutate(gaudin_cuml_passing = 100 * (geom_mean/P50)^modulus)
    output$lm = lm_model
    output$nls = nls_model
    output$P50 = P50[[1]]
    output$modulus = modulus[[1]]
    output
}