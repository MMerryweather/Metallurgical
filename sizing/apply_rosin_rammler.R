apply_rosin_rammler = function(size, k, n ){
  # size in mm
  # k is the Rosin-Rammler P80 parameter and
  # n is the Rosin-Rammler spread parameter
    100*(1-exp(-((size/k)^n)))
}
