df_to_tsne = function(df, perplexity = 30, dims = 2, pca = TRUE, ...)
{
tsne = Rtsne::Rtsne(df, dims = dims, perplexity = perplexity, pca = pca, ...)

nm = seq_len(dims) %>% paste0("TSNE",.)

tsne$Y %>%
  as.data.frame() %>% 
  set_names(nm) %>%
  bind_cols(df) 
}
