rosin_rammler_coefficients = function(tbl){
    pacman::p_load(tidyverse)
    RR = tbl %>% mutate(x = log(bottom_size),
                        y = log(-log(1-mass_passing/100)))
    RR %>%
        filter(y != Inf & x != -Inf) %>%
        lm(y ~ x, data = .) %>%
        broom::tidy() %>%
        select(term, estimate) %>%
        spread(term,estimate) %>%
        set_names(c("Intercept","RR_n")) %>%
        transmute(RR_n, RR_k = exp(-Intercept/RR_n))
}