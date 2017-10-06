extract_size_fractions = function(df){
    names(df) = c("size_fraction","fraction_mass")
    df %>% mutate(size_fraction = stringr::str_replace(size_fraction,"\\+ ","+"),
                      bottom_size = stringr::str_extract(size_fraction,"(?<=\\+)\\d\\.*\\d*"),
                  bottom_size = ifelse(is.na(bottom_size),0,as.numeric(bottom_size)))
}