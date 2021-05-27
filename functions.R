
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

getStat <- function(df, stat, max_scale) {
  stat_val <- df %>%
    filter(statistic == stat) %>%
    pull(estimate) %>% 
    max()
  
  # if (stat_val > max_scale) {
  #   stat_val <- max_scale
  # }
  
  # if (stat_val < 0.01) {
  #   stat_val <- 0.01
  # }
  
  
  return(stat_val)
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
                           compared_value = 0.0) {
  
  # Set the maximum value for the scale as the greatest 
  # between the compared value and the maximum value for the
  # indicator in the data set.
  max_scale <- max(scale_max[indi][[1]],compared_value)

  # Generates the box plot for displaying in the site
  
  df <- df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi,
           statistic %in% bp_stats)
  
  formatted_df <- data.frame(
    x = 0,
    y5 = getStat(df, "5Pct", max_scale),
    y25 = getStat(df, "25Pct", max_scale),
    y50 = getStat(df, "50Pct", max_scale),
    y75 = getStat(df, "75Pct", max_scale),
    y95 = getStat(df, "95Pct", max_scale)
  )

  formatted_df %>%
    ggplot(aes(x)) +
    geom_boxplot(width = .2,
                 outlier.shape = NA,
                 color = "darkgray",
                 mapping = aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95),
                 stat = "identity") +
    # # Comment out the line the below to hide the whiskers
    # stat_boxplot(geom = 'errorbar',
    #              coef = 6, # you can adjust this to adjust the whisker size
    #              width = 0.085,
    #              color = "darkgray") +
    geom_segment(
      colour = "dark gray",
      aes(x = -.05, y = y5, xend = .05, yend = y5),
      data = formatted_df
    ) +
    geom_segment(
      colour = "dark gray",
      aes(x = -.05, y = y95, xend = .05, yend = y95),
      data = formatted_df
    ) +
    theme_minimal() +
    # Places the measurement on the plot
    geom_hline(yintercept = compared_value,
               size = 2,
               color = "#005DA9",
               alpha = 0.90) +
    scale_x_continuous(breaks = NULL,
                       limits = c(-.11,.11)) +
    scale_y_continuous(
                       # trans = log_trans(),
                       # breaks = base_breaks(),
                       limits = c(0.00,max_scale),
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
          plot.margin = margin(0,0,-9,0,unit = "pt"),
          axis.text.x = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "#efefef", fill = NA, size = 1),
          plot.background = element_rect(
            fill = NA,
            colour = "#efefef",
            size = 1
          )) +
    coord_flip() +
    labs(y = "")
}


# Percentile Value
# Gets the percentile that a measurement falls into for an indicator
# Arguments
# df: the indicators data frame
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
    round() %>% 
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
  
  margin_of_error <- 
    df %>% 
    filter(subpopulation == sub_pop,
           indicator == indi) %>% 
    filter(value <= comp_value) %>% 
    filter(value == max(value)) %>% 
    mutate(lcb = case_when(
      round(value) == comp_value ~ lcb95pct_p,
      round(value) < comp_value ~ lcb95pct_p,
      round(value) > comp_value ~ 0.0),
      ucb = case_when(
        round(value) == comp_value ~ ucb95pct_p,
        round(value) < comp_value ~ ucb95pct_p,
        round(value) > comp_value ~ 0.0),
      percentile = case_when(
        value == comp_value ~ estimate_p,
        value < comp_value ~ estimate_p,
        value > comp_value ~ 0.0),
      margin = if_else(percentile - lcb > percentile - ucb,
                       percentile - lcb,
                       percentile - ucb)) %>% 
    pull(margin)
  
  ifelse(identical(margin_of_error, numeric(0)),0,margin_of_error)
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
    # ggparagraph("Please provide required inputs before exporting results image.") +
    text_grob("Please provide required inputs before exporting results image.") +
    filler +
    plot_layout(ncol = 1, heights = c(.1,.1,.1,1))
}

# Formats capitalization of the measurement unit
# Arguments
# measurement_unit: What type of measurement to put on the x-axis
format_measure_unit <- function(measure_unit) {
  
  if (measure_unit == "μg/L") {
    return("μg/L")
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
  
  bottom_text <- "†IMPORTANT: Population estimates presented above are based on a weighted analysis of lake data from the U.S. EPA’s {nla_year} U.S. National Lakes Assessment (NLA). {indi_english} was measured once at an open water location from {survey_timeframe} {nla_year}. Sampled lakes were selected using a statistically representative approach that balances lake size with their distribution across the continental U.S. Results shown are weighted based on those factors. Maximum margin of error for your percentile ranking in {name}: ±{margin_of_error}."
  
  values_text <- "Box-and-whisker plots above use the 5th and 95th percentile as the whisker endpoints. A logarithmic scale is used to accommodate extreme values. Plots are based on the following user inputs: INDICATOR: {indi_english}; OBSERVED DATA IN {format_measure_unit(measure_unit)}: {comma_format(accuracy = 0.1)(compared_value)}; YEAR DATA COLLECTED: {year}; LAKE NAME: {bottom_lake_name}; STATE NAME: {name}."

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
  local_title <- section_title(paste0(generate_header(sub_pop,indi,compared_value,lake_name,state_abbr,nla_year),"†"),"white","#005DA9",1.75 * length_ratio)
  local <- indicator_plot(df,sub_pop,indi,measure_unit,compared_value)
  regional <- indicator_plot(df,epa_region,indi,measure_unit,compared_value)
  regional_title <- section_title(paste0(generate_header(epa_region,indi,compared_value,lake_name,area_name,nla_year),"†"),"white","#005DA9",1.75 * length_ratio)
  national <- indicator_plot(df,"All_Sites",indi,measure_unit,compared_value)
  national_title <- section_title(paste0(generate_header("All_Sites",indi,compared_value,lake_name,"Nationally",nla_year),"†"),"white","#005DA9",1.75 * length_ratio)
  
  # Plot sizing config.
  plot_height <- .95
  header_height <- 0.705882353
  top_paragraph <- 2.8
  
  header_title +
    filler +
    ggparagraph(glue(top_text), family = "Arial", size = 18.5) +
    filler +
    local_title + filler + local + 
    regional_title + filler + regional +
    national_title + filler  + national +
    filler +
    ggparagraph(glue(bottom_text), family = "Arial", size = 11) +
    ggparagraph(glue(values_text), family = "Arial", size = 11) +
    ggparagraph(glue(url_text), family = "Arial", size = 11) +
    plot_layout(ncol = 1, heights = c(header_height,
                                      .21,
                                      top_paragraph,.2,
                                      header_height, .1, plot_height,
                                      header_height, .1, plot_height,
                                      header_height, 0.1, plot_height,
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
#                         measure_unit = "μg/L",
#                         compared_value = 87.4,
#                         lake_name = "WWWWWWWWWWWWw",
#                         year = 2002,
#                         indi_text = "Secchi Disk",
#                         name = "Arkansas",
#                         session_url = "http://www.testurl.test/",
#                         margin_of_error = 0.44),
#        width = 10.4,height = 9.69)
# 
