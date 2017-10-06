plant_splits = function(n = 0.580756, k = 9.987429){
    source("functions/apply_rosin_rammler.R")
    source("functions/circuit_split.R")
    pacman::p_load(tidyverse)
    RR = data.frame(RR_n = n, RR_k=k)
    list(RR = RR,
         DMB = (100 - apply_rosin_rammler(20, RR))/100,
         DMC = RR %>% circuit_split(20,2),
         TBS = RR %>% circuit_split(2,0.25)) %>%
        append(list(Floats = 1 - .$DMB - .$DMC - .$TBS))
}
