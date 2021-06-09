source("setup.R")

# addResourcePath(prefix = 'static', directoryPath = '~/www')
options(bitmapType = 'cairo', device = 'png')

ui <- fixedPage(
  # This is required for the bit of javascript used on line 79
  tags$head(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  sidebarLayout(
    # Side bar layout lives here
    sidebarPanel(
      tags$div("Instructions",class = "sidebar_header",
               dropdownButton(tags$img(src = "help_tooltip.png",align = "right",
                                       height = 650,
                                       width = 1033),
                              size = "xs",
                              inputId = "help_button",
                              tooltip = "Help",
                              icon = icon("question-circle"))),
      tags$div(class = "instructions",
               'Input data for one lake below to see how it
               compares to a nationwide representative
               sample. After inputting data, you can ',downloadLink("png_export","click here to export an image of the results.")),
      div(style = "color: #0097DC",
          tags$strong("Select an Indicator*", class = "input_header"),
          selectInput("indicator_selector",
                      NULL,
                      choices =  c("Choose an indicator",indicator_list)),
          uiOutput("value_header",inline = TRUE),
          numericInput("indicator_value",
                       NULL,
                       NA),
          div(
            id="flex-row",
            div(id="year_collected_wrapper",
              div(tags$strong("Year Data Collected*",class = "input_header"),class = "value_header_fix"),
              numericInput("year_input",
                           NULL,
                           NA)
            ),
            div(id="year_selector_wrapper",
              div(tags$strong("NLA Year",class = "input_header"),class = "value_header_fix"),
              selectInput("year_selector",
                          NULL,
                          width="100%",
                          choices =  c(2012, 2017))
            )
          ),
          div(tags$strong("Lake Name",class = "input_header"),class = "value_header_fix"),
          textInput("lake_name_input",
                    NULL),
          div(tags$strong("Select State*",class = "input_header"),class = "value_header_fix"),
          selectInput("state_input",
                      NULL,
                      c("",state_names[order(names(state_names))]),
                      ""),
          tags$h5(textOutput("region_title"), id = "map_title"),
          imageOutput("region_map",
                      inline = TRUE))
    ),
    
    mainPanel(
      fixedRow(column(width = 12,
                      htmlOutput("title_bar"))),
      fixedRow(column(width = 12,
                      id = "default_message",
                      tags$div(id = "default_container",
                               intro_text()))),
      fixedRow(column(width = 12,
                      htmlOutput("overview_text")),
               style = "min-height: 100px;"),
      fixedRow(column(width = 12,
                      htmlOutput("state_header"))),
      fixedRow(column(width = 12,
                      plotOutput("state_plot", width = "100%", height = "65px"))),
      fixedRow(column(width = 12,
                      htmlOutput("region_header"))),
      fixedRow(column(width = 12,
                      plotOutput("region_plot", width = "100%",height = "65px"))),
      fixedRow(column(width = 12,
                      htmlOutput("national_header"))),
      fixedRow(column(width = 12,
                      plotOutput("national_plot", width = "100%",height = "65px"))),
      fixedRow(column(width = 12,
                      htmlOutput("disclaimer_text")))
    )))
server <- function(input, output,session) {

  options("Set-Cookie" = paste0("JSESSIONID=", session$token)) 
  
  # Attaches a max length attribute to the lake name input. Default is 17.
  runjs(glue("$('#lake_name_input').attr('maxlength', {lake_name_limit});$('#lake_name_input').attr('spellcheck', 'FALSE');"))
  
  lake_name <- reactive({
    input$state_input
    if (input$lake_name_input == "") {
      lake_name = "your lake"
    } else {
      lake_name = input$lake_name_input
    }
  })
  
  output$value_header <- renderUI({
    
    indi_measure <- measurements[input$indicator_selector]
    
    if (input$indicator_selector == "Choose an indicator") {
      indicator_text  <- ""
    } else {
      indicator_text <- glue(" (in {indi_measure})")
    }
    
    if (is.null(input$indicator_value)) {
      default_value <- NULL
    } else {
      default_value <- input$indicator_value
    }
    tags$strong(glue("Input Your Data{indicator_text}*"),class = "input_header")
    
  })
  
  output$title_bar <- renderUI({
    if (valid_inputs())  {
      
      lake_name <- lake_name()
      
      if (lake_name == "your lake") {
        title_lake_name <- "Your Lake"
      } else {
        title_lake_name <- lake_name
      }
      
    tags$h3(class = "main_header",
            glue(title_text)) }
    else {
      title_text <- "Welcome to EPA’s Lake Comparison Tool"
      
      tags$h3(class = "main_header",
              style = "background-color:#005da4;text-align:left;padding-left:10px;",
              glue(title_text),
              tags$img(src = "epa_logo.png",
                       align = "right",
                       width = 73,
                       height = 24,
                       style = "margin-top: 1px;margin-right: 4px"))
    }
  })
  
  
  input_value <- reactive({
    input$indicator_value
  })
  
  input_value_d <- input_value
  
  output$overview_text <- renderUI({
    req(valid_inputs())
    
    state_name <- input$state_input
    indi_english <- indicator_english[input$indicator_selector]
    indi_measure <- measurements[input$indicator_selector]
    indi_text <- indicator_text[input$indicator_selector]
    lake_name <- lake_name()
    year <- input$year_input
    nla_year <- input$year_selector
    
    if (is.null(input_value_d())) {
      value <- 0.85
    } else {
      value <- input_value_d()
    }
    
    HTML(glue(header_text))
    
  })
  
  output$disclaimer_text <- renderUI({
    req(valid_inputs())
    
    state_abbr <- state_abbrs[input$state_input][[1]]
    lake_value <- input_value_d()
    margin_of_error <- 
      margin_calculator(dplyr::filter(estimates, year == input$year_selector),state_abbr,input$indicator_selector,lake_value) %>%
      round()
    
    state_name <- input$state_input
    indi_text <- 
      indicator_names %>% 
      filter(indi_abbr == input$indicator_selector) %>% 
      pull(names)
    
    nla_year <- input$year_selector
    survey_timeframe <- get_survey_timeframe(nla_year)
    
    tags$div(class = "disclaimer",
             tags$sup("†"),
             tags$strong("Important:"),
             glue(disclaimer_text),tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla","EPA's website.", 
                                          target = "_blank"))
  })
  
  output$state_header <- renderUI({
    req(valid_inputs())
    
    value <- input_value_d()
    indicator <- input$indicator_selector
    state_abbr <- state_abbrs[input$state_input][[1]]
    nla_year <- input$year_selector
    
    text <- generate_header(state_abbr,indicator,value,lake_name(),state_abbr, nla_year)
    
    tags$div(class = "plot_header",
             HTML(glue("{text}<sup>†</sup>")))
  })
  
  output$region_header <- renderUI({
    req(valid_inputs())
    
    area_name <- 
      region_lookup_table %>% 
      filter(state_name == input$state_input) %>% 
      pull(region_name)
    
    epa_region <- 
      region_lookup_table %>% 
      filter(state_name == input$state_input) %>% 
      pull(epa_region)
    
    value <- input_value_d()
    indicator <- input$indicator_selector
    nla_year <- input$year_selector
    
    text <- generate_header(epa_region,indicator,value,lake_name(),area_name, nla_year)
    
    tags$div(class = "plot_header",
             HTML(glue("{text}<sup>†</sup>")))
  })
  
  output$national_header <- renderUI({
    req(valid_inputs())
    
    value <- input_value_d()
    indicator <- input$indicator_selector
    nla_year <- input$year_selector
    
    text <- generate_header("All_Sites",indicator,value,lake_name(),"Nationally", nla_year)
    
    tags$div(class = "plot_header",
             HTML(glue("{text}<sup>†</sup>")))
  })
  
  output$region_title <- renderText({
    
    region_name <- 
      region_lookup_table %>% 
      filter(state_name == input$state_input) %>% 
      pull(region_name)
    
    state_name <- input$state_input
    
    if (state_name == "") {
      "This tool provides data for your state's EPA region,\nin addition to data at the national and state levels."
    } else {
      glue("{state_name} is in EPA {region_name}.")
    }
    
    
  })
  
  output$region_map <- renderImage({
    
    images_loc <- "www/images/region_maps/"
    region_file <- 
      region_lookup_table %>% 
      filter(state_name == input$state_input) %>% 
      pull(epa_region)
    
    if (input$state_input == "") {
      images_url <- "www/images/region_maps/all_regions.png"
    } else {
      images_url <- paste0(images_loc,region_file,".png")
    }
    
    
    list(src = images_url,
         width = "100%",
         id = "region_map_img")},
    deleteFile = FALSE)
  
  
  output$state_plot <- renderPlot({
    req(valid_inputs())
    
    measure_unit <- measurements[input$indicator_selector][[1]]
    state_abbr <- state_abbrs[input$state_input][[1]]
    indicators %>% 
      dplyr::filter(year == input$year_selector) %>%
      indicator_plot(state_abbr,
                     input$indicator_selector,
                     measure_unit,
                     input_value_d(),
                     getScaleMax(state_abbr, input$indicator_selector))},
    height = plot_height)
  
  valid_inputs <- reactive({
    not(any(input$indicator_selector == "Choose an indicator",
            input$state_input == "",
            is.na(input$year_input),
            is.na(input_value_d())))
  })
  
  output$region_plot <- renderPlot({
    req(valid_inputs())
    
    state_abbr <- state_abbrs[input$state_input][[1]]
    
    region_name <- 
      region_lookup_table %>% 
      filter(state_name == input$state_input) %>% 
      pull(epa_region)
    measure_unit <- measurements[input$indicator_selector][[1]]
    state_abbr <- state_abbrs[input$state_input][[1]]
    
    indicators %>% 
      dplyr::filter(year == input$year_selector) %>%
      indicator_plot(region_name,
                     input$indicator_selector,
                     measure_unit,
                     input_value_d(),
                     getScaleMax(state_abbr, input$indicator_selector))},
    height = plot_height)
  
  output$national_plot <- renderPlot({
    req(valid_inputs())
    
    state_abbr <- state_abbrs[input$state_input][[1]]
    measure_unit <- measurements[input$indicator_selector][[1]]
    
    indicators %>% 
      dplyr::filter(year == input$year_selector) %>%
      indicator_plot("All_Sites",
                     input$indicator_selector,
                     measure_unit,
                     input_value_d(),
                     getScaleMax(state_abbr, input$indicator_selector))},
    height = plot_height)
  
  output$png_export <- downloadHandler(
    filename = function() {
      # browser()
      indicator_name <- indicator_english[input$indicator_selector][[1]] %>% str_replace_all(" ","-")
      value <- input_value_d()
      state_abbr <- state_abbr <- state_abbrs[input$state_input][[1]]
      nla_year <- input$year_selector
      
      
      if (valid_inputs()) {
        glue("NLA-{nla_year}-Context_{indicator_name}_{value}_{state_abbr}.png") 
      } else {
        "invalid_inputs.png"
      }
    },
    content =  function(file) {
      
      if (valid_inputs()) {
        state_abbr <- state_abbrs[input$state_input][[1]]
        lake_value <- input_value_d()
        
        session_url <- paste0(session$clientData$url_protocol,"//",session$clientData$url_hostname,session$clientData$url_pathname)
        ggsave(file,png_creator(dplyr::filter(indicators, year == input$year_selector),
                                sub_pop = state_abbrs[input$state_input][[1]],
                                indi = input$indicator_selector,
                                measure_unit = measurements[input$indicator_selector],
                                compared_value = input_value_d(),
                                lake_name = lake_name(),
                                year = input$year_input, 
                                indi_text = indicator_text[input$indicator_selector],
                                name = input$state_input,
                                session_url = session_url,
                                margin_of_error = round(margin_calculator(dplyr::filter(estimates, year == input$year_selector),state_abbr,input$indicator_selector,lake_value)),
                                nla_year = input$year_selector,
                                survey_timeframe = get_survey_timeframe(input$year_selector)
                                ),
               width = 10.4,height = 9.69, type="cairo", device="png") }
      else {
        ggsave(file,invalid_image_file())
      }
    }
  )
  
  observe({
    if (!valid_inputs()) {
      shinyjs::show("default_message")
    } else {
      shinyjs::hide("default_message")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
# shiny::runApp(list(ui = ui, server = server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))

