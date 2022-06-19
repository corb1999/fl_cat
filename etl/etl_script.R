# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("1999-01-01"), 
                                    author = "", 
                                    proj_name = "", 
                                    script_type = "etl", 
                                    notepad = paste0("")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
# library(gt)
library(janitor)
library(readxl)
library(tidycensus)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5, 
        stringsAsFactors = FALSE)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

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

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# unionizer script --------------------------------------------
# a script to load and UNION ALL a number of datasets together
# require(purrr)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# identify dropzone where files are stored and vector the filenames
filepath_prefix_payload <- paste0(getwd(), "/etl/ore/cat_fund_rates")

# put the files list into a dataframe
payload <- data.frame(file_nm = list.files(path = filepath_prefix_payload)) %>% 
  mutate(file_nm_full = paste0(filepath_prefix_payload, "/", 
                               file_nm)) %>% 
  mutate(rate_yr = str_sub(file_nm, start = 1L, end = 2))

# test +++++++++++++++++++++++++++
dim(payload)
# payload
# payload[1, 1]
# payload[1, 2]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# write a function to read each file the same way into r
fun_readfiles <- function(filepaths, arg_rate_yr) {
  sub_arg_sheets <- c('Com 90', 'Com 75', 'Com 45')
  sub_arg_range <- rep('A9:H34', length(sub_arg_sheets))
  # set up a fun to read each sheet
  fun_sub_read <- function(sub_arg_filepaths, sub_arg_rate_yr, 
                           arg_sheet, arg_range) {
    xx <- read_excel(path = sub_arg_filepaths, 
                     sheet = arg_sheet, 
                     range = arg_range)
    xx <- xx %>% 
      mutate(rate_yr = as.integer(sub_arg_rate_yr) + 2000, 
             lob_cov_lvl = arg_sheet, 
             ded_lvl = 'DED 0000-2500') 
    names(xx)[1] <- 'zip_code_group'
    names(xx)[8] <- 'UNK Construction'
    return(xx)
  }
  # now read each sheet in each year's workbook
  xx <- pmap_dfr(list(filepaths, arg_rate_yr, 
                      sub_arg_sheets, sub_arg_range), 
                 fun_sub_read)
  return(xx)}

# test +++++++++++++++++++++++++++
# fun_readfiles(payload[2, 2], payload[2, 3])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute the file reading purrr, and time the execution
clockin()
payload_list <- pmap(list(payload[, 2], payload[, 3]), 
                     .f = fun_readfiles)
clockout()

# test +++++++++++++++++++++++++++
# payload_list[[1]]
# dim(payload_list[[1]])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run checks and validation on the loaded files
payload_stats <- data.frame(col_num_chk = map_dbl(payload_list, 
                                                  ncol), 
                            row_num_chk = map_dbl(payload_list, 
                                                  nrow))

fun_colnames_chk <- function(x) {
  aa <- colnames(x)
  bb <- reduce(aa, paste0)
  return(bb)}

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
payload_stats
length(unique(payload_stats[, 1])) == 1
length(unique(map_chr(payload_list, fun_colnames_chk))) == 1

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combine into a single dataframe, as long as checks pass
clockin()
df <- map_dfr(payload_list, rbind)
clockout()

# test +++++++++++++++++++++++++++
dim(df)

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sum(payload_stats$row_num_chk) == nrow(df)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute any additional filter and manipulation before writing

df <- df %>% 
  pivot_longer(cols = c(2:8), 
               names_to = 'construct_type', 
               values_to = 'rate_per_1k') %>% 
  mutate(lob = str_sub(lob_cov_lvl, start = 1L, end = 3), 
         cov_lvl = str_sub(lob_cov_lvl, start = -2, end = -1), 
         cov_lvl = as.double(cov_lvl) / 100) %>% 
  select(-lob_cov_lvl)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create and append a metadata tag
metadata_tag <- paste0("unionizer script metadata; ", 
                       "files consumed = ", 
                       nrow(payload_stats), 
                       "; runtime = ", Sys.time(), 
                       "; nrow = ", nrow(df), 
                       "; ncol = ", ncol(df))
metadata_tag
df <- df %>% mutate(metadata_tag = "NA")
df[1, "metadata_tag"] <- metadata_tag

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# write the cleaned dataset ---------------------------------------

# write to rds
filename <- paste0(getwd(), "/etl/ingot/dataframe.rds")
clockin()
saveRDS(dfa, file = filename)
clockout()

# ^ ----- 

# summarize and record etl -------------------------------------

(interim <- list(a = Sys.info(), 
                 b = nrow(dfa), 
                 c = ncol(dfa), 
                 d = sizer(dfa)))

# create an etl summary object
etl_metadata <- data.frame(etl_runtime = metadatar$script_starttime, 
                           etl_user = interim$a[[8]], 
                           data_rows = interim$b, 
                           data_cols = interim$c, 
                           data_size = interim$d, 
                           etl_note = 'no notes')
etl_metadata
rm(interim)

# write to csv
filename <- paste0(getwd(), "/etl/etl_metadata.csv")
clockin()
write.csv(etl_metadata, file = filename, row.names = FALSE)
clockout()

# ^ ----- 