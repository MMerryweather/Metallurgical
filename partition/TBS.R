TBS = function(condition){
    rate = condition$rate
    source("functions/whiten_partition_number.R")
    source("functions/geometric_mean.R")
    source("functions/circuit_split.R")
    source("functions/plant_splits.R")
    source("functions/apply_partition_curve.R")
    source("functions/partition_yields.R")

    predict_alpha_from_rate = function(rate){
        # From plant data
        # 0.98 @ 2000 t/h
        # 0.81 @ 2200 t/h
        -0.00085 * rate + 2.68
    }
    predict_loading_ep_increase = function(solids_loading){
        # Assume 50% feed solids
        0.002* solids_loading -0.04
    }
    predict_ep_from_size = function(geomean){
        # TBS Handbook, 4:1 top:bottom ratio
        -0.025512741 * log(geomean) + 0.120590218
    }
    select_float_sink_table = function(bottom_size){
        quiet_read = quietly(read_csv)
        tbl = if(bottom_size >= 0.71){
            quiet_read("data/coarse_TBS_float_sink.csv")[["result"]]
        } else{
            quiet_read("data/fine_TBS_float_sink.csv")[["result"]]
        }
        list(tbl)
    }

    splits = plant_splits()
    sieve_bends = data.frame(bottom_size = c(1, 0.71,0.5,0.25),
                         top_size = c(2, 1, 0.71, 0.5)) %>%
    rowwise() %>%
    mutate(geomean = geometric_mean(bottom_size, top_size),
           overflow = whiten_partition_number(diameter = geomean,
                                              d50 = 0.71,
                                              alpha = predict_alpha_from_rate(rate)
                                              ) / 100,
           underflow = 1 - overflow) %>%
    gather(stream,recovery, -(1:3)) %>%
    mutate(plant_feed_split = circuit_split(condition$RR, top_size, bottom_size),
           circuit_feed_split = plant_feed_split * recovery,
           feed_tonnes = rate * circuit_feed_split,
           type = if_else((stream=="overflow" & bottom_size>=0.71)|(stream == "underflow" & bottom_size < 0.71),"Correct","Misplaced"),
           misplaced_tonnes = if_else(type=="Misplaced",(feed_tonnes),0),
           baseline_ep = predict_ep_from_size(geomean)) %>%
    ungroup()

loadings = sieve_bends %>%
    group_by(stream) %>%
    summarise(feed_tonnes = sum(feed_tonnes),
              misplaced_feed_tonnes = sum(feed_tonnes)) %>%
    rowwise() %>%
    mutate(N_TBS = if_else(stream=="overflow",4,2),
           TBS_diameter = if_else(stream=="overflow",2.1,2.4),
           total_area = N_TBS * pi * (TBS_diameter/2)^2,
           loading = feed_tonnes / total_area,
           ep_increase = predict_loading_ep_increase(loading))

sieve_bends = sieve_bends %>%
    inner_join(loadings %>% select(stream, ep_increase), by = "stream") %>%
    rowwise() %>%
    mutate(ep = baseline_ep + ep_increase,
           float_sinks = select_float_sink_table(bottom_size))

# list_partition_curve = function(...){
#     list(apply_partition_curve(...))
# }
fractional_yields = sieve_bends %>% pluck("float_sinks") %>%
    map2(sieve_bends %>% split(1:nrow(.)), ~apply_partition_curve(.x,ep=.y$ep, cutpoint = 1.7, t_100 = 93)) %>%
    map(partition_yields) %>%
    map_dbl("product_mass")/100

sieve_bends = sieve_bends %>%
    bind_cols(data.frame(fractional_yields = fractional_yields)) %>%
    mutate(product_tonnes = feed_tonnes * fractional_yields,
           rate = rate)

product = sieve_bends %>% select(rate, feed_tonnes, product_tonnes) %>%
    ungroup() %>%
    group_by(rate) %>%
    summarise_all(sum) %>%
    mutate(TBS_yield = (product_tonnes / feed_tonnes)*condition$TBS,
           RR_k = condition$k) %>%
    select(rate, RR_k, plant_yield = TBS_yield)
product
}

