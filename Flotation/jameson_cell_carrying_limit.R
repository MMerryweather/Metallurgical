jameson_cell_carrying_limit = function(cell_model,air_flow_rate,N_cells, target_ash, feed_sizing){
    bc = 5.796 # Bubble Constant

    quiet_read = quietly(read_csv)
    jameson_cell_data = quiet_read("data/jameson_cell_data.csv")[["result"]] %>%
        filter(model == cell_model)

    cell_area = jameson_cell_data %>% pluck("area")
    total_area = cell_area * N_cells #m2
    superficial_air_velocity =(air_flow_rate/cell_area)*(100/3600) # cm/s
    bubble_diameter = 1000 * bc * superficial_air_velocity^0.888 #um
    gaudin_parameters = feed_sizing %>% gaudin_parameter_estimation()

    P50 = gaudin_parameters$P50
    modulus = gaudin_parameters$modulus

    bubbles_per_unit_time =(2*bubble_diameter)/(bubble_diameter-((bubble_diameter^2)-(if_else(cell_model == "J6500/24", 73.61, gaudin_parameters$P50)^2))^0.5)
    product_sg =0.016*target_ash+1.125
    carrying_capacity = carrying_capacity(P50, modulus, bubbles_per_unit_time, product_sg, superficial_air_velocity, bubble_diameter)

    output = list()
    output$P50 = P50
    output$total_area = total_area
    output$superficial_air_velocity = superficial_air_velocity
    output$bubble_diameter = bubble_diameter
    output$gaudin_parameters = gaudin_parameters
    output$bubbles_per_unit_time = bubbles_per_unit_time
    output$product_sg = product_sg
    output$carrying_capacity = carrying_capacity
    output$total_carrying_capacity = (carrying_capacity * total_area)[[1]]  # t/h
    output
}
