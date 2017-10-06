circuit_split = function(RR, upper, lower){
  
    source("functions/apply_rosin_rammler.R")
    (apply_rosin_rammler(upper, RR) - apply_rosin_rammler(lower, RR))/100
}
