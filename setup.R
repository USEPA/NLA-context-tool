library(shiny)
library(ggplot2)
library(shinyjs)
library(htmltools)
library(stringr)
library(stringi)
library(readr)
library(dplyr)
library(glue)
library(scales)
library(patchwork)
library(magrittr)
library(ggpubr)
library(grid)
library(shinyWidgets)


# Load the shared functions
source("functions.R")

# Config ------------------------------------------------------------------

plot_height <- 56
header_color <- "#0097DC"
subheader_color <- "#005DA9"
lake_name_limit <- 17

# Text Templates ----------------------------------------------------------

# Main Section Title
title_text <- "How Does {title_lake_name} Compare to Other U.S. Lakes?"

# First Sentence text
header_text <- "You reported that {lake_name} in {state_name} ({state_abbrs[[state_name]]}) had an observed value of <span class='user_data'>{comma_format(accuracy = 0.1)(round2(value,1))} {indi_measure} </span> for {indi_english} in {year}. The graphs below show how your lake ranks at the state, regional and national levels compared to representative data collected by the U.S. National Lakes Assessment in <span class='user_data'>{nla_year}</span>. {indi_text}"

# Bottom Disclaimer Text
disclaimer_text <- "These population estimates are based on a weighted analysis of lake data from the U.S. EPA’s {nla_year} U.S. National Lakes Assessment (NLA). {indi_text} was measured once at an open water location from {survey_timeframe} {nla_year}. Sampled lakes were selected using a statistically representative approach that balances lake size with their distribution across the continental U.S. Results shown are weighted based on those factors. Percentiles are rounded to the nearest whole number. Estimated max. margin of error for {state_abbr} percentile ranking, based upon limited observations: ±{margin_of_error}. To learn about the NLA, please visit the "

# Default landing page text
intro_text <- function(window_inner_width) {

  if (window_inner_width < 1200) {
    tagList(
      div("This tool was produced by the National Aquatic Resource Surveys (NARS) program of the U.S. Environmental Protection Agency (EPA). The NARS program conducts large-scale studies of the quality of the nation’s waters. One such study is the National Lakes Assessment (NLA)."),
      
      div(tags$strong("What Can the Tool Do?"),"This tool allows you to input water quality data for a lake you care about, then see it compared to statistically representative data collected by the NLA.  You’ll view comparisons to the national, regional and state level. This is currently possible using 2012 and 2017 NLA data for any of four important and common indicators of water quality:",
          tags$ul(tags$li(tags$i("Secchi Depth")," (a measure of water clarity)"),
                  tags$li(tags$i("Total Phosphorus")," (a nutrient that can trigger problematic algal blooms)"),
                  tags$li(tags$i("Total Nitrogen"), " (another such nutrient)"),
                  tags$li(tags$i("Chlorophyll")," a (a measure of algal population)"))),
      
      div(tags$strong("How Do I Get Started?")," Complete the form above, then your output will appear in place of these instructions. (Entering the name of your lake in the form is optional.)"),
      
      div(tags$strong("How Do I Interpret Results?")," For help interpreting the results, click the “?” icon above."),
      
      div(tags$strong("What Devices Are Best?")," For best results, use this tool in Chrome or Edge using a laptop or desktop computer. It may not display properly on mobile or other devices with smaller screens."),
      
      div(HTML(paste(tags$strong("How Do I Learn More?"), " Visit the EPA's ", tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla", target="_blank", "NLA website"), ".", sep = "")))
    )   
  }
  else {
    tagList(
      div("This tool was produced by the National Aquatic Resource Surveys (NARS) program of the U.S. Environmental Protection Agency (EPA). The NARS program conducts large-scale studies of the quality of the nation’s waters. One such study is the National Lakes Assessment (NLA)."),
      
      div(tags$strong("What Can the Tool Do?"),"This tool allows you to input water quality data for a lake you care about, then see it compared to statistically representative data collected by the NLA.  You’ll view comparisons to the national, regional and state level. This is currently possible using 2012 and 2017 NLA data for any of four important and common indicators of water quality:",
          tags$ul(tags$li(tags$i("Secchi Depth")," (a measure of water clarity)"),
                  tags$li(tags$i("Total Phosphorus")," (a nutrient that can trigger problematic algal blooms)"),
                  tags$li(tags$i("Total Nitrogen"), " (another such nutrient)"),
                  tags$li(tags$i("Chlorophyll")," a (a measure of algal population)"))),
      
      div(tags$strong("How Do I Get Started?")," Complete the form on the left, then your output will appear in place of these instructions. (Entering the name of your lake in the form is optional.)"),
      
      div(tags$strong("How Do I Interpret Results?")," For help interpreting the results, click the “?” icon on the left."),
      
      div(tags$strong("What Devices Are Best?")," For best results, use this tool in Chrome or Edge using a laptop or desktop computer. It may not display properly on mobile or other devices with smaller screens."),
      
      div(HTML(paste(tags$strong("How Do I Learn More?"), " Visit the EPA's ", tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla", target="_blank", "NLA website"), ".", sep = "")))
    ) 
  }
}



# Lookups -----------------------------------------------------------------
# This section is all the various look up lists
# To use any lookup use this syntax:
# lookup_table["value to look up"]
# An example would be:
# measurements["SECCHI"] would return "meters

# Indicator abbr -> Measurement
measurements <- list("PTL" = "μg/L",
                     "NTL" = "μg/L",
                     "SECCHI" = "meters",
                     "CHL" = "μg/L")

# Indicator abbr -> Ranking Text
indicator_text <- list("CHL" = "For Chlorophyll-a, a lower percentile ranking is generally preferable.",
                      "NTL" = "For Total Nitrogen, a lower percentile ranking is generally preferable.",
                      "PTL" = "For Total Phosphorus, a lower percentile ranking is generally preferable.",
                      "SECCHI" = "For Secchi Depth, an upper percentile ranking is generally preferable.")

# Indicator abbr -> English Name
indicator_english <- list("CHL" = "Chlorophyll-a",
                          "NTL" = "Total Nitrogen",
                          "PTL" = "Total Phosphorus",
                          "SECCHI" = "Secchi Depth")

# Data Import -------------------------------------------------------------

## Create the state lookup table. We'll need EPA Region,
# State Name and State Abbr
region_lookup_table <-
  read_csv("data/epa_region_states.csv") %>%
  filter(!(state_name %in% c("Alaska","Puerto Rico","Hawaii"))) %>% # We have no data for these locations so remove them.
  mutate(state_abbr = setNames(state.abb, state.name)[state_name])

## State Input List
state_names <-
  setNames(region_lookup_table$state_name,
           region_lookup_table$state_abbr) %>%
  sort()

state_abbrs <-
  setNames(region_lookup_table$state_abbr,
           region_lookup_table$state_name)

## Indcators
indicators_2012 <- read_csv("data/combined_indicators_2012.csv") %>% tibble::add_column(year = 2012)
indicators_2017 <- read_csv("data/combined_indicators_2017_v03.csv") %>% tibble::add_column(year = 2017)
indicators <- dplyr::bind_rows(indicators_2012, indicators_2017)

#indicators <- indicators_2012

indicator_names <-
  tibble(indi_abbr = c("SECCHI","NTL","PTL","CHL"),
         names = c("Secchi Depth","Total Nitrogen","Total Phosphorus","Chlorophyll-a"))

bp_stats <- c("5Pct","25Pct","50Pct","75Pct","95Pct")

indicator_list <- list("Secchi Depth" = "SECCHI",
                  "Total Phosphorus" = "PTL",
                  "Total Nitrogen" = "NTL",
                  "Chlorophyll-a" = "CHL")

scale_max <- list("PTL" = 2000,
                  'SECCHI' = 30,
                  'CHL' = 310,
                  'NTL' = 5400)


## Estimates
estimates_2012 <- read_csv("data/combined_estimates_2012.csv") %>% tibble::add_column(year = 2012)
estimates_2017 <- read_csv("data/combined_estimates_2017_v03.csv") %>% tibble::add_column(year = 2017)
estimates <- dplyr::bind_rows(estimates_2012, estimates_2017)
