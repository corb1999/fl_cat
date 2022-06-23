
# add functions to execute analysis with ::::::::::::::::::

fun_agg1 <- function(arg_df = dfplt, arg_grouper = grouper) {
  df_fun <- arg_df %>% 
    rename(gvar = !!arg_grouper) %>% 
    group_by(gvar) %>% 
    summarise(recs = n(), 
              uni_zips = n_distinct(zip_code_group), 
              min_rate = min(rate_per_1k), 
              med_rate = median(rate_per_1k), 
              max_rate = max(rate_per_1k))
  return(df_fun)
}
# fun_agg1()

fun_scatter_1yr <- function(arg_df = dfplt, 
                            arg_pltname = pltname) {
  plt1 <- arg_df %>% 
    ggplot(aes(x = zip_code_group, y = rate_per_1k)) + 
    geom_point(aes(color = construct_type), 
               alpha = 0.85, size = 1) + 
    facet_wrap(vars(cov_lvl)) +
    theme(legend.position = 'top') + 
    labs(title = arg_pltname, x = 'Zip Grouping', 
         y = 'Rate per 1K TIV', color = '', 
         subtitle = 'Facets = Coverage %s')
  plt1 <- my_gg(plt1)
  return(plt1)
}
# fun_scatter_1yr()
