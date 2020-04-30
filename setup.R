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
library(ggpubr)
library(grid)
library(gridExtra)
library(shinyWidgets)
library(lattice)

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
header_text <- "You reported that {lake_name} in {state_name} ({state_abbrs[[state_name]]}) had an observed value of <span class='user_data'>{comma_format(accuracy = 0.1)(round2(value,1))} {indi_measure} </span> for {indi_english} in {year}. The graphs below show how your lake ranks at the state, regional and national levels compared to representative data collected by the U.S. National Lakes Assessment in 2012. {indi_text}" 

# Bottom Disclaimer Text
disclaimer_text <- "Population estimates presented above are based on a weighted analysis of lake data from the U.S. EPA’s 2012 U.S. National Lakes Assessment (NLA). {indi_text} was measured once at an open water location from June to September 2012. Sampled lakes were selected using a statistically representative approach that balances lake size with their distribution across the continental U.S. Results shown are weighted based on those factors. Maximum margin of error for your percentile ranking in {state_name}: ±{margin_of_error}. To learn about the NLA, please visit the "

# Default landing page text
intro_text <- function() {
  
  tagList(
    div("This tool was produced by National Aquatic Resource Surveys (NARS) program of the U.S. Environmental Protection Agency (EPA). The NARS program conducts large-scale studies of the quality of the nation’s waters. One such study is the National Lakes Assessment (NLA).", style = "font-size:110%;"),
  
    div(tags$strong("What Can the Tool Do?"),"This tool allows you to input water quality data for a lake you care about, then see it compared to statistically representative data collected by the NLA.  You’ll view comparisons to the national, regional and state level. This is currently possible using 2012 NLA data for any of four important and common indicators of water quality:",
        tags$ul(tags$li(tags$i("Secchi Depth")," (a measure of water clarity)"),
                tags$li(tags$i("Total Phosphorus")," (a nutrient that can trigger problematic algal blooms)"),
                tags$li(tags$i("Total Nitrogen"), " (another such nutrient)"),
                tags$li(tags$i("Chlorophyll")," a (a measure of algal population)"))),
  
    div(tags$strong("How Do I Get Started?")," Please read the instructions on the left then enter data.  Output will appear once you have entered data into all required fields (marked with an *). Entering the lake name is optional."),

    div(tags$strong("How Do I Interpret Results?")," For help interpreting the results, hover over the “?” icon on the left."),
  
    div(tags$strong("What Devices Are Best?")," For best results, use this tool in Chrome or Safari using a laptop or desktop computer. It may not display properly on mobile or other devices with smaller screens"),
  
    div(tags$strong("How Do I Learn More?")," Visit the NLA’s site at", tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla", "epa.gov/national-aquatic-resource-surveys/nla."))
  )
}



# Lookups -----------------------------------------------------------------
# This section is all the various look up lists
# To use any lookup use this syntax:
# lookup_table["value to look up"]
# An example would be:
# measurements["SECCHI"] would return "meters

# Indicator abbr -> Measurement
measurements <- list("PTL" = "μg/L",
                     "NTL" = "mg/L",
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

indicators <- read_csv("data/combined_indicators.csv")

indicator_names <- 
  tibble(indi_abbr = c("SECCHI","NTL","PTL","CHL"),
         names = c("Secchi Depth","Total Nitrogren","Total Phosphorus","Chlorophyll-a"))

bp_stats <- c("5Pct","25Pct","50Pct","75Pct","95Pct")

indicator_list <- list("Secchi Depth" = "SECCHI",
                  "Total Phosphorus" = "PTL",
                  "Total Nitrogen" = "NTL",
                  "Chlorophyll-a" = "CHL")

scale_max <- list("PTL" = 4100,
                  'SECCHI' = 31,
                  'CHL' = 250,
                  'NTL' = 12001)

## Estimates 
estimates <- read_csv("data/combined_estimates.csv")

