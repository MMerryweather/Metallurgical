inverse_rosin_rammler = function(percentile, n, k){
  # result in mm
  # k is the Rosin-Rammler P80 parameter and
  # n is the Rosin-Rammler spread parameter
  k *(-log((100 - percentile)/100))^(1/n)
}
