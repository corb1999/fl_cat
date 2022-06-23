# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO =================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "eda", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# source the script that will load the data and prep it for analysis
sourcerpath <- paste0(getwd(), '/eda/remodel/remodel_script.R')
clockin()
source(file = sourcerpath)
clockout()

# source another script with analysis functions to run
sourcerpath <- paste0(getwd(), '/eda/engine/engine_script.R')
clockin()
source(file = sourcerpath)
clockout()

# cleanup
ls()
trash()
mem_used()

# ^ ====================================
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::

# viz prep ---------------------------------------------------

dt_filters <- fun_dater('2021-01-01', 
                        '2021-01-01')

(pltname <- 'FL CAT Rates; ' %ps% 
    # 'Cov-Lvl = 45%; ' %ps% 
    'Ded = 0-2500; ' %ps% 
    dt_filters$date_text_str %ps% 
    '')

dfplt <- dfa %>% 
  # filter(cov_lvl == 0.45) %>% 
  filter(ded_lvl == 'DED 0000-2500') %>% 
  filter(rate_yr >= dt_filters$start_date, 
         rate_yr <= dt_filters$end_date)

grouper <- 'rate_yr'

# ^ -----

# run plots and visuals ------------------------------------




# ^ -----
