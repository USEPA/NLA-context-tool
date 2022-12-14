
# For generating the x-axis ticks
base_breaks <- function(n = 20){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }}

# Custom rounding function.
# R's built in rounding function will round 1.45 -> 1.40
# We wanted to have 1.45 round to 1.5
# Arguments: 
# x: A float
# n: Number of places to round
#
# returns: The rounded #
round2 = function(x, n=1) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

getStat <- function(df, stat) {
  stat_val <- df %>%
    filter(statistic == stat) %>%
    pull(estimate) %>% 
    max()
  
  return(stat_val)
}

# Boxplot Scale Max
# This function takes the following arguments:
# sub_pop: The state to plot
# indi: The indicator to plot
#
# returns: the maximum value for that indicator across state, epa region, and allsites values

getScaleMax <- function(sub_pop, indi) {
  
  epa_region_row <- region_lookup_table %>%
    filter(state_abbr == sub_pop)
  
  epa_region <- epa_region_row[1, "epa_region"] %>% toString()
  
  subpop_max <- indicators %>%
    filter(indicator == indi) %>%
    filter(subpopulation == sub_pop) %>%
    filter(statistic == "95Pct") %>%
    pull(estimate) %>% 
    max()
  
  region_max <- indicators %>%
    filter(indicator == indi) %>%
    filter(subpopulation == epa_region) %>%
    filter(statistic == "95Pct") %>%
    pull(estimate) %>% 
    max()
  
  allsites_max <- indicators %>%
    filter(indicator == indi) %>%
    filter(subpopulation == "All_Sites") %>%
    filter(statistic == "95Pct") %>%
    pull(estimate) %>% 
    max()
  
  indicator_state_allsites_max <- max(subpop_max, region_max, allsites_max)
  
  if (indi == "PTL" && epa_region == "Region_9") {
    indicator_state_allsites_max <- min(1000, indicator_state_allsites_max)
  }
  else if (indi == "CHL" && epa_region == "Region_9") {
    indicator_state_allsites_max <- min(300, indicator_state_allsites_max)
  }
  else if (indi == "CHL" && epa_region == "Region_8") {
    indicator_state_allsites_max <- min(307, indicator_state_allsites_max)
  }
  else if (indi == "NTL" && epa_region == "Region_9") {
    indicator_state_allsites_max <- min(5000, indicator_state_allsites_max)
  }
  else if (indi == "NTL" && epa_region == "Region_7") {
    indicator_state_allsites_max <- min(6000, indicator_state_allsites_max)
  }
  
  return(indicator_state_allsites_max)
}

# Indicator Plot Function
# This function takes the following arguments:
# df: The indicators data frame
# sub_pop: The state to plot
# indi: The indicator to plot
# measure_unit: This is the measurement unit label in the scale
# compared_value: The value to be plotted.
#
# returns: A ggplot boxplot 

indicator_plot <- function(df,
                           sub_pop,
                           indi,
                           measure_unit,
                           compared_value = 0.0,
                           upper_limit,
                           window_inner_width = 1300) {
  

  max_scale <- max(upper_limit, compared_value)

  scale_limits <- c(0.0, max_scale * 1.03)
  
  lower_expansion_multiplier <- 0.04
  if (window_inner_width < 1200) {
    lower_expansion_multiplier <- 0.08
  }

  # Generates the box plot for displaying in the site
  
  df <- df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi,
           statistic %in% bp_stats)
  
  formatted_df <- data.frame(
    x = 0,
    y5 = if (getStat(df, "5Pct") >= scale_limits[1]) getStat(df, "5Pct") else scale_limits[1],
    y25 = getStat(df, "25Pct"),
    y50 = getStat(df, "50Pct"),
    y75 = getStat(df, "75Pct"),
    y95 = if (getStat(df, "95Pct") <= scale_limits[2]) getStat(df, "95Pct") else scale_limits[2]
  )
  
  formatted_df_without_limits <- data.frame(
    x = 0,
    y5 = getStat(df, "5Pct"),
    y25 = getStat(df, "25Pct"),
    y50 = getStat(df, "50Pct"),
    y75 = getStat(df, "75Pct"),
    y95 = getStat(df, "95Pct")
  )
  
  plot <- formatted_df %>%
    ggplot(aes(x)) +
    geom_boxplot(width = .15,
                 size = .5,
                 outlier.shape = NA,
                 color = "dimgray",
                 mapping = aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95),
                 stat = "identity")
  
  # Add 5Pct cap if within limits
  if (getStat(df, "5Pct") >= scale_limits[1]) {
    plot <- plot + geom_segment(
      size = 0.5,
      color = "dimgray",
      aes(x = -.05, y = y5, xend = .05, yend = y5),
      data = formatted_df_without_limits
    ) 
  }
  
  # Add 95Pct cap if within limits
  if (getStat(df, "95Pct") <= scale_limits[2]) {
    plot <- plot + geom_segment(
      size = 0.5,
      color = "dimgray",
      aes(x = -.05, y = y95, xend = .05, yend = y95),
      data = formatted_df_without_limits
    )
  } 
  
  plot <- plot + theme_minimal() +
    # Places the measurement on the plot
    geom_hline(yintercept = compared_value,
               size = 2,
               color = "#005DA9",
               alpha = 0.90) +
    scale_x_continuous(breaks = NULL,
                       limits = c(-.11,.11)) +
    scale_y_continuous(
                       breaks = axisTicks(c(0.0, max_scale), FALSE, NULL, 6),
                       limits = scale_limits,
                       expand = expansion(mult = c(lower_expansion_multiplier, 0)),
                       oob = oob_keep,
                       labels = function(x) {
                         # This function generates and formats the label
                         formatted_labels <-
                           comma_format(big.mark = ",",
                                        decimal.mark = ".",
                                        accuracy = .1)(x)

                         # this appends the measurement unit to the first label on
                         # the x-axis
                         formatted_labels[1] <- paste(formatted_labels[1],
                                                      measure_unit)
                         formatted_labels
                       }) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = margin(t = 0, r = 0.5, b = -9, l = 0, unit = "pt"),
          axis.text.x = element_text(size = 10),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "dimgray", fill = NA, size = 0.25),
          panel.grid.major = element_line(color = "dimgray", size = 0.25, linetype = "dashed"),
          plot.background = element_rect(
            fill = NA,
            colour = "white",
            size = 1
          )) +
    coord_flip() +
    labs(y = "")
  
  return(plot)
}


# Percentile Value
# Gets the percentile that a measurement falls into for an indicator
# Arguments
# df: the estimates data frame
# sub_pop: The sub population of the indicator, in this case the state
# indi: The indicator to use
# comp_value: The value to compare
#
# returns: a float of the percentile
percentile_value <- function(df,sub_pop,indi,comp_value) {
  filtered_df <- 
    df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi)
  
  # this finds the the upper threshold of the indicator's
  # estimates
  top_threshold <- 
    df %>% 
    select(subpopulation,indicator,value,estimate_p) %>% 
    filter(subpopulation == sub_pop,
           indicator == indi,
           estimate_p == 100) %>% 
    pull(value) %>%
    min()
  
  # If the comp value is above the top threshhold
  # then the percentile is 100 so return that
  if (comp_value >= top_threshold) {
    return(100)
  }
  
  filtered_df %>% 
    filter(value <= comp_value) %>% 
    pull(estimate_p) %>% 
    round2(0) %>% 
    max()
}

# Returns the maximum value for an indicator
# Arguments:
# df: A dataframe containing the estimates
# sub_pop: The subpopulation to filter by. It'll be a state, region or all sites
# indi: the indicator to filter by. 
indicator_max <- function(df,sub_pop,indi) {
  df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi)  %>%
    pull(value)  %>% 
    max()
}

# Returns the minimum value for an indicator
indicator_min <- function(df,sub_pop,indi) {
  df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi,
           n_resp > 0)  %>%
    pull(value)  %>% 
    min()
}

# Return plain text description of the time frame (e.g., 'June to September')
get_survey_timeframe <- function(nla_year) {
  if (nla_year == "2017" || nla_year == 2017) {
    return('May to October')
  }
  else {
    return('June to September')
  }
}

# Calculates the margin of error for an indicator compared to
# a comparison value
# Arguments:
# df: the indicator data frame
# sub_pop: The subpopulation to get. In this case the sate
# indi: The indicator to calculate for
# comp_value: the value to compare
#
# returns: a float margin of error
margin_calculator <- function(df,sub_pop,indi,comp_value) {
  
  filtered_df <- 
    df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi) %>% 
    filter(value <= comp_value)
  
  if (nrow(filtered_df) == 0) {
    return(get_non_zero_margin_of_error(df,sub_pop,indi))
  }
  
  percentile_row <- filtered_df[which.max(filtered_df$estimate_p),]
  
  ucb <- percentile_row[1, "ucb95pct_p"] %>% as.numeric()
  lcb <- percentile_row[1, "lcb95pct_p"] %>% as.numeric()
  est <- percentile_row[1, "estimate_p"] %>% as.numeric()
  
  ucb_diff <- ucb - est
  lcb_diff <- est - lcb
  
  margin_of_error <- round2(max(ucb_diff, lcb_diff), 1)
  
  if (margin_of_error == 0) {
    return(get_non_zero_margin_of_error(df,sub_pop,indi))
  }
  
  ifelse(identical(margin_of_error, numeric(0)),0,margin_of_error)
}

get_non_zero_margin_of_error <- function(df, sub_pop, indi) {
  
  moe <- 
    df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi) %>% 
    mutate(moe = ifelse(ucb95pct_p - estimate_p > estimate_p - lcb95pct_p, ucb95pct_p - estimate_p, estimate_p - lcb95pct_p)) %>% 
    filter(moe > 0) %>%
    pull(moe) %>%
    min()
  
  return(moe)
}


# This function generates a header
# sub_pop: the sub_population to use. In this case the state, region or all_sites
# indicator: the indicator to use
# value: the lake measurement value
# lake_name: The name of the lake
# area_name: The EPA region
#
# returns: A text string of the header
generate_header <- function(sub_pop,indicator,value,lake_name,area_name,nla_year) {
  
  max_indi_val <- indicator_max(dplyr::filter(estimates, year == nla_year),sub_pop,indicator)
  min_indi_val <- indicator_min(dplyr::filter(estimates, year == nla_year),sub_pop,indicator)
  
  if (value > max_indi_val) {
    pct_value <- "'s value is above the highest NLA result."
  } else if (value < min_indi_val) {
    pct_value <- "'s value is at or below the lowest NLA result."
  } else {
    pct_num <- ordinal_format()(percentile_value(dplyr::filter(estimates, year == nla_year),sub_pop,indicator,value))
    pct_value <- glue(" is in the {pct_num} percentile.")
  }
  
  if (area_name == "Nationally") {
    glue("{area_name}, {lake_name}{pct_value}")
  } else {
    glue("In {area_name}, {lake_name}{pct_value}")
  }

  
}

# This function generates an HTML header
# sub_pop: the sub_population to use. In this case the state, region or all_sites
# indicator: the indicator to use
# value: the lake measurement value
# lake_name: The name of the lake
# area_name: The EPA region
#
# returns: A text string of the header
generate_html_header <- function(sub_pop,indicator,value,lake_name,area_name,nla_year) {
  
  # HTML(paste(tags$strong("How Do I Learn More?"), " Visit the EPA's ", tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla", target="_blank", "NLA website"), ".", sep = ""))
  
  max_indi_val <- indicator_max(dplyr::filter(estimates, year == nla_year),sub_pop,indicator)
  min_indi_val <- indicator_min(dplyr::filter(estimates, year == nla_year),sub_pop,indicator)
  
  if (value > max_indi_val) {
    pct_value <- HTML(paste("'s value is above the highest NLA", HTML("&nbsp;"), "result.", sep=""))
  } else if (value < min_indi_val) {
    pct_value <- HTML(paste("'s value is at or below the lowest NLA", HTML("&nbsp;"), "result.", sep=""))
  } else {
    pct_num <- ordinal_format()(percentile_value(dplyr::filter(estimates, year == nla_year),sub_pop,indicator,value))
    pct_value <- HTML(paste(" is in the ", pct_num, HTML("&nbsp"), "percentile.", sep=""))
  }
  
  if (area_name == "Nationally") {
    #glue("{area_name}, {lake_name}{pct_value}")
    HTML(paste(area_name, ", ", lake_name, pct_value, "*", sep=""))
  } else {
    # glue("In {area_name}, {lake_name}{pct_value}")
    HTML(paste("In ", area_name, ", ", lake_name, pct_value, "*", sep=""))
  }
  
  
}

# Creates a section title for exported PNG.
# Arguments:
# text: The text for the seciton
# text_color: Hex color for the text color
# background_color: Hex color for the background color
# size: the size of the text
# 
# returns: a graphics object for the section.
section_title <- function(text,text_color,background_color,size) {
  grobTree(rectGrob(gp = gpar(fill = background_color,col = NA)),
           textGrob(text, x = 0.5, hjust = 0.5,
                    gp = gpar(col = text_color, cex = size,fontface = "bold"))) %>% as_ggplot()
}

# A small filler object for the vertical alignment of the exported PNG
filler <- rectGrob(gp = gpar(fill = "#FFFFFF",col = NA)) %>% as_ggplot()

# A function to create a small 'invalid' that is used when the user hasn't
# entered required values on the left hand side of the web-page.
invalid_image_file <- function() {
  invalid_title <- section_title("RESULTS EXPORT FAILED.","white","#0097DC",1)
  
  invalid_title +
    filler +
    text_grob("Please provide required inputs before exporting results image.") +
    filler +
    plot_layout(ncol = 1, heights = c(.1,.1,.1,1))
}

# Formats capitalization of the measurement unit
# Arguments
# measurement_unit: What type of measurement to put on the x-axis
format_measure_unit <- function(measure_unit) {
  
  if (measure_unit == "??g/L") {
    return("??g/L")
  }
  else {
    return("METERS")
  }
  

}

# Generates a PNG for the user to download
# Arguments
# df: the indicator data frame
# sub_pop: The subpopulation to filter by, in this case the state, region or all_sites
# measurement_unit: What type of measurement to put on the x-axis
# compared_value: the value to plot
# lake_name: A string of the lake name. Defaults to your lake
# year: a user inputed year
# indi_text: The indicator text to display.
# name: The state name to use
# session_url: The URL where the app is hosted
# margin_of_error: The margin of error
png_creator <-  function(df,sub_pop,indi,measure_unit,compared_value,lake_name = "your lake", 
                         year = 2018, indi_text = "NA", name = "Alabama",
                         session_url = "", margin_of_error, nla_year = 2012, survey_timeframe = 'June to September') {
  
  # Looks up the english version of the indicator name
  indi_english <- indicator_english[indi]
  
  # Bottom lake name is used for the arguments text at the bottom
  if (lake_name == "Your Lake") {
    bottom_lake_name <- "[None Provided]"
  } else {
    bottom_lake_name <- lake_name
  }
  
  # This is so if the lake name is blank in the title it'll be 'Your Lake'
  # but in the body it'll be 'your lake'
  if (lake_name == "your lake") {
    title_lake_name <- "Your Lake"
  } else {
    title_lake_name <- lake_name
  }
  
  # The summary paragraph at the top of the image
  top_text <- "You reported that {lake_name} in {name} ({state_abbr}) had an observed value of {comma_format(accuracy = 0.1)(round2(compared_value,2))} {measure_unit} for {indi_english} in {year}. The graphs below show how your lake ranks at the state, regional and national levels compared to representative data collected by the U.S. National Lakes Assessment in {nla_year}. {indi_text}"
  
  bottom_text <- "*IMPORTANT: These population estimates are based on a weighted analysis of lake data from the U.S. EPA???s {nla_year} U.S. National Lakes Assessment (NLA). {indi_english} was measured once at an open water location from {survey_timeframe} {nla_year}. Sampled lakes were selected using a statistically representative approach that balances lake size with their distribution across the continental U.S. Results shown are weighted based on those factors. Percentiles are rounded to the nearest whole number. Estimated max. margin of error for {state_abbr} percentile ranking, based upon limited observations: ??{margin_of_error}."
  
  values_text <- "Box-and-whisker plots above use the 5th and 95th percentile as the whisker endpoints. Plots are based on the following user inputs: INDICATOR: {indi_english}; OBSERVED DATA IN {format_measure_unit(measure_unit)}: {comma_format(accuracy = 0.1)(compared_value)}; YEAR DATA COLLECTED: {year}; LAKE NAME: {bottom_lake_name}; STATE NAME: {name}."

  url_text <- "Image exported from {session_url} on {date()}"
  
  # Looks up the region name
  area_name <- 
    region_lookup_table %>% 
    filter(state_name == name) %>% 
    pull(region_name)
  
  # looks up the EPA region
  epa_region <- 
    region_lookup_table %>% 
    filter(state_name == name) %>% 
    pull(epa_region)
  
  state_abbr <- state_abbrs[name][[1]]
  name_length <- nchar(lake_name)
  length_ratio <- min(lake_name_limit/name_length,1)

  # Generate Title Sections
  header_title <- section_title(glue(title_text),"white","#0097DC",1.7 * length_ratio)
  local_title <- section_title(paste0(generate_header(sub_pop,indi,compared_value,lake_name,state_abbr,nla_year),"*"),"white","#005DA9",1.375 * length_ratio)
  local <- indicator_plot(df,sub_pop,indi,measure_unit,compared_value, getScaleMax(sub_pop, indi))
  regional <- indicator_plot(df,epa_region,indi,measure_unit,compared_value, getScaleMax(sub_pop, indi))
  regional_title <- section_title(paste0(generate_header(epa_region,indi,compared_value,lake_name,area_name,nla_year),"*"),"white","#005DA9",1.375 * length_ratio)
  national <- indicator_plot(df,"All_Sites",indi,measure_unit,compared_value, getScaleMax(sub_pop, indi))
  national_title <- section_title(paste0(generate_header("All_Sites",indi,compared_value,lake_name,"Nationally",nla_year),"*"),"white","#005DA9",1.375 * length_ratio)
  
  # Plot sizing config.
  plot_height <- .95
  header_height <- 0.705882353
  top_paragraph <- 1.4
  
  header_title +
    filler +
    ggparagraph(glue(top_text), family = "Arial", size = 15, lineheight = 1.1, color = "black") +
    filler +
    local_title + filler + local + 
    regional_title + filler + regional +
    national_title + filler  + national +
    filler +
    ggparagraph(glue(bottom_text), family = "Arial", size = 12.5, color = "black") +
    ggparagraph(glue(values_text), family = "Arial", size = 12.5, color = "black") +
    ggparagraph(glue(url_text), family = "Arial", size = 12.5, color = "black") +
    plot_layout(ncol = 1, heights = c(header_height,
                                      .21,
                                      top_paragraph,.2,
                                      header_height, .1, plot_height,
                                      header_height, .1, plot_height,
                                      header_height, .1, plot_height,
                                      0.2,
                                      1.8,
                                      1.2,
                                      .5))
}

# Uncomment this after sourcing setup.R to be able to test changes to
# the png_creator function.
# ggsave("test_png.png",png_creator(indicators,
#                         sub_pop = "AR",
#                         indi = "PTL",
#                         measure_unit = "??g/L",
#                         compared_value = 87.4,
#                         lake_name = "WWWWWWWWWWWWw",
#                         year = 2002,
#                         indi_text = "Secchi Disk",
#                         name = "Arkansas",
#                         session_url = "http://www.testurl.test/",
#                         margin_of_error = 0.44),
#        width = 10.4,height = 9.69)
# 
