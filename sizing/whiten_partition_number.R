whiten_partition_number = function(diameter, d50, alpha, bypass=98.5, units="mm"){
    diameter = if_else(units=="um", diameter/1000, diameter)
    d_d50 = diameter / d50
    100-bypass*(exp(alpha)-1)/(exp(alpha*d_d50)+exp(alpha)-2)
}