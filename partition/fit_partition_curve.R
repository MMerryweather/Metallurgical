df = clipr::read_clip_tbl()

min.rss = function(data, par){
    sum(( (100 - ( par[3] + (par[4] - par[3]) / (1 + exp(( 1.0986 * (par[1] - data$SG) / par[2]))))) - data$PN) ^ 2)
    }

results = optim(par = c(1.8,0.2,0,100), min.rss,
                data = df,
                lower = c(1,0.005,0,0),
                upper = c(2.5,0.5,100,100),
                method = "L-BFGS-B")

results$par %>% clipr::write_clip()
