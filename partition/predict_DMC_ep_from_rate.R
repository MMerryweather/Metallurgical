predict_DMC_ep_from_rate = function(rate){
    df = data.frame(rate = c(2000,  2200,  2300,  2500),
                    ep   = c(0.005, 0.007, 0.009, 0.012))
    model = lm(ep ~ rate, data = df)
    predict.lm(model, newdata = data.frame(rate = rate)) %>% as.numeric()
}
