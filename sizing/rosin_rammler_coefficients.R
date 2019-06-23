rosin_rammler_coefficients = function(tbl, bottom_size, mass_passing){
  pacman::p_load(tidyverse)
  size_enq = enquo(bottom_size)
  mass_enq = enquo(mass_passing)
  RR = tbl %>%
    mutate(x = log(!!size_enq),
           y = log(-log(1 - !!mass_enq / 100)))
  RR %>%
    filter(y != Inf & x != -Inf) %>%
    lm(y ~ x, data = .) %>%
    broom::tidy() %>%
    select(term, estimate) %>%
    spread(term,estimate) %>%
    set_names(c("Intercept","RR_n")) %>%
    transmute(RR_n, RR_k = exp(-Intercept/RR_n))
}
