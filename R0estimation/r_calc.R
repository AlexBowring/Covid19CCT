#' Combine dataframes together by column
#'
#' @param region
#' @param country
#' @details 
#'
#' @examples
#' r_calc_epinow('Scotland')
#'
#' @export r_calc_epinow 
r_calc_epinow <- function(region, country = 'all countries'){
  require(EpiNow, quietly = TRUE)
  require(NCoVUtils, quietly = TRUE)
  require(furrr, quietly = TRUE)
  require(future, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(tidyr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(forecastHybrid, quietly = TRUE)
  require(data.table, quietly = TRUE)
  
  epinow_codeloc = 'C:/Users/12SDa/davenpor/davenpor/Private_Projects/Covid/EpiNowForecast/'
  save_dir <- paste0(epinow_codeloc, region)   
  delay_loc = paste0(epinow_codeloc, 'Delay_Files/delays_', region, '.rds')
  delay_defs <- readRDS(delay_loc)
  
  NCoVUtils::reset_cache()
  cases <- NCoVUtils::get_uk_regional_cases(geography = country)
  
  regional_cases = as.data.table(filter(cases, region == region))
  regional_cases = data.table::setDT(regional_cases)
  regional_cases <- regional_cases[, `:=`(confirm = as.integer(cases), import_status = "local")][,
                                                                                             cases := NULL]
  rt_pipeline(cases = london_cases,
              delay_defs = delay_defs,
              target_date = max(regional_cases$date),
              target_folder = save_dir)
  
  knitr::include_graphics(paste0(target_dir, "/latest/rt_cases_plot.png"))
}

#' Combine dataframes together by column
#'
#' @param cases
#' @details this function can combine dataframes which have different numbers of rows,
#' replacing undefined rows with NAs.
#'
#' @examples
#' h1 = data.frame(matrix(1:6,2))
#' h2 = data.frame(matrix(1:3,1))
#' cbinddf(h1,h2)
#'
#' @export county_r
county_r <- function(county){
  require(EpiNow, quietly = TRUE)
  require(NCoVUtils, quietly = TRUE)
  require(furrr, quietly = TRUE)
  require(future, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(tidyr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(forecastHybrid, quietly = TRUE)
  require(data.table, quietly = TRUE)
  
  gitrep_loc = 'C:/Users/12SDa/davenpor/davenpor/Other_Toolboxes/COVID19CCT/'
  code_loc = paste0(gitrep_loc,'R0calcutations/')
  save_dir <- paste0(code_loc, 'Counties/',region)   
  delay_loc = paste0(code_loc, 'delays.rds')
  delay_defs <- readRDS(delay_loc)
  
  cases = load(paste0(code_loc, 'covid_data.csv'))
  names(us_cases)[1] = c('County')
  ndates = length(us_cases) - 1;
  names(us_cases)[2:length(us_cases)] = as.character(seq(1,ndates))
  
  county_cases = as.data.table(filter(cases, region == region))
  county_cases = data.table::setDT(county_cases)
  county_cases <- county_cases[, `:=`(confirm = as.integer(cases), import_status = "local")][,cases := NULL]
  
  rt_pipeline(cases = london_cases,
              delay_defs = delay_defs,
              target_date = max(regional_cases$date),
              target_folder = save_dir)
  
  knitr::include_graphics(paste0(target_dir, "/latest/rt_cases_plot.png"))
}
