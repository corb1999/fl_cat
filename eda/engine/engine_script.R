
# add functions to execute analysis with ::::::::::::::::::

fun_agg1 <- function(arg_df = dfplt) {
  df_fun <- arg_df %>% 
    # rename(gvar = !!arg_grouper) %>% 
    # group_by(gvar) %>% 
    summarise(recs = n(), 
              # uni_zips = n_distinct(zip_code_group), 
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

fun_yrtrend1 <- function(arg_df = dfplt, 
                         arg_grouper = grouper, 
                         arg_pltname = pltname) {
  df1 <- arg_df %>% rename(gvar = !!arg_grouper) %>% 
    mutate(rate_yr = as.factor(rate_yr), 
           gvar = as.factor(gvar)) %>% 
    group_by(rate_yr, gvar) %>% fun_agg1() 
  plt1 <- df1 %>% 
    ggplot(aes(x = rate_yr, color = gvar, group = gvar)) + 
    geom_point(aes(y = med_rate), size = 1.1) + 
    geom_line(aes(y = med_rate)) + 
    scale_color_brewer(palette = 'Set1') + 
    labs(title = arg_pltname, 
         x = 'Median Rate per 1k TIV', 
         y = '', color = arg_grouper)
  plt1 <- my_gg(plt1)
  return(plt1)
}
# fun_yrtrend1()
