apply_rosin_rammler = function(size, RR){
  # size in mm
  # RR is assumed to be a data frame with 1 observation of two values:
  # RR_k is the Rosin-Rammler P80 parameter and
  # RR_n is the Rosin-Rammler spread parameter
    100*(1-exp(-((size/RR$RR_k)^RR$RR_n)))
}
