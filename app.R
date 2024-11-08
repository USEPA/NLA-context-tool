source("setup.R")

options(bitmapType = 'cairo', device = 'png')

ui <- fixedPage(
  tags$html(class = "no-js", lang="en"),
  tags$head(
    tags$title('Lake Context Tool | US EPA'),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    includeHTML("www/header.html")
  ),
  useShinyjs(),
  # Script for calculated the browser width
  tags$script('
        var dimension = [0, 0];
        
        var delay = 250;
        var timeout = false;
        
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        
        $(window).resize(function(e) {
        
          // clear the timeout
          clearTimeout(timeout);
          // start timing for event "completion"
          timeout = setTimeout(function() {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
          }, delay);
        });
    '),
  br(),
  sidebarLayout(
    # Side bar layout lives here
    sidebarPanel(
      tags$div("Instructions",
               class = "sidebar_header",
               dropdownButton(
                 tags$div(
                   class = "tip",
                   tags$img(src = "help-tooltip.png", 
                            align = "right")),
                 size = "xs",
                 inputId = "help_button",
                 tooltip = "Help",
                 icon = icon("question-circle"))),
      tags$div(class = "instructions",
               'Input data for one lake below to see how it
               compares to a nationwide representative
               sample. Afterward, ',
               actionButton("go", "Click Here", icon("camera"), style="color: #fff; background-color: #5cb85c; border-color: #4cae4c;"), 
               "to export an image of the results."),
      
      div(style = "color: #0097DC",
          tags$strong("Select an Indicator", class = "input_header"),
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
                div(tags$strong("Year Data Collected",class = "input_header"),class = "value_header_fix"),
                numericInput("year_input",
                             NULL,
                             NA)
            ),
            div(id="year_selector_wrapper",
                div(tags$strong("NLA Year",class = "input_header"),class = "value_header_fix"),
                selectInput("year_selector",
                            NULL,
                            width="100%",
                            selected = 2022,
                            choices =  c(2012, 2017, 2022))
            )
          ),
          div(tags$strong("Lake Name (optional)",class = "input_header"),class = "value_header_fix"),
          textInput("lake_name_input",
                    NULL),
          div(tags$strong("Select State",class = "input_header"),class = "value_header_fix"),
          selectInput("state_input",
                      NULL,
                      c("",state_names[order(names(state_names))]),
                      ""),
          tags$h5(htmlOutput("region_title"), id = "map_title"),
          imageOutput("region_map",
                      inline = TRUE)), br()
    ),
    mainPanel(
      div(id="context_plot",
          fixedRow(column(width = 12,
                          htmlOutput("title_bar"))),
          conditionalPanel(
            condition = "input.indicator_selector == 'Choose an indicator' |
                     input.indicator_value == null |
                     input.year_input == null |
                     input.state_input == ''",
            fixedRow(column(width = 12,
                            id = "default_message",
                            tags$div(id = "default_container",
                                     uiOutput("intro_text"))
            )
            )
          ),
          fixedRow(column(width = 12,
                          htmlOutput("overview_text") %>% withSpinner(color="#0275d8")),
                   style = "min-height: 100px;"),
          fixedRow(column(width = 12,
                          htmlOutput("state_header"))),
          fixedRow(column(width = 12,
                          plotOutput("state_plot", width = "100%", height = "65px"))),
          fixedRow(column(width = 12,
                          uiOutput("ecoregion_output"))),
          fixedRow(column(width = 12,
                          htmlOutput("national_header"))),
          fixedRow(column(width = 12,
                          plotOutput("national_plot", width = "100%",height = "65px"))),
          fixedRow(column(width = 12,
                          htmlOutput("disclaimer_text")))
      )))
  ,includeHTML("www/footer.html")
)

server <- function(input, output, session) {
  
  observeEvent(input$go, {
    req(valid_inputs())
    indicator_name <- indicator_english[input$indicator_selector][[1]] %>% str_replace_all(" ","-")
    value <- input_value_d()
    state_abbr <- state_abbr <- state_abbrs[input$state_input][[1]]
    nla_year <- input$year_selector
    
    screenshot(
      selector = "#context_plot",
      scale = 1,
      filename = glue("NLA-{nla_year}-Context_{indicator_name}_{value}_{state_abbr}.png")
    )
  })
  
  
  options("Set-Cookie" = paste0("JSESSIONID=", session$token))
  
  # Attaches a max length attribute to the lake name input. Default is 17.
  runjs(glue("$('#lake_name_input').attr('maxlength', {lake_name_limit});$('#lake_name_input').attr('spellcheck', 'FALSE');"))
  
  valid_inputs <- reactive({
    !any(input$indicator_selector == "Choose an indicator",
         input$state_input == "",
         is.na(input$year_input),
         is.na(input_value_d()))
  })
  
  Estimate_Data <- reactive({
    req(valid_inputs())
    estimates %>%
      filter(year == input$year_selector,
             indicator == input$indicator_selector)
  })
  
  Indicator_Data <- reactive({
    req(valid_inputs())
    indicators %>%
      filter(year == input$year_selector,
             indicator == input$indicator_selector)
  })
  
  output$region_title <- renderUI({
    
    ecoregion_name <-
      ecoregion_lookup_table %>%
      filter(state_name == input$state_input) %>%
      pull(ecoregion_name)
    
    state_name <- input$state_input
    
    if (state_name == "") {
      tags$p("This tool provides data for your state's", tags$a(href="https://www.epa.gov/national-aquatic-resource-surveys/ecoregions-used-national-aquatic-resource-surveys", "NARS Ecoregion(s)"),"\nin addition to data at the national and state levels.")
    } else {
      tags$p(paste0(input$state_input), " is in the following", tags$a(href="https://www.epa.gov/national-aquatic-resource-surveys/ecoregions-used-national-aquatic-resource-surveys", "NARS Ecoregion(s):"), fPaste(ecoregion_name))
      #tags$p(tags$br(), paste(input$state_input, "is in the following NARS Ecoregions: (", paste0(ecoregion_name, collapse = ", "), ")."))
    }
  })
  
  output$region_map <- renderImage({
    
    images_url <- "www/images/region_maps/ecoregions.jpg"
    
    list(src = images_url,
         width = "100%",
         id = "region_map_img")},
    deleteFile = FALSE)
  
  
  output$state_plot <- renderPlot({
    req(valid_inputs())
    
    window_inner_width <- input$dimension[1]
    
    measure_unit <- measurements[input$indicator_selector][[1]]
    state_abbr <- state_abbrs[input$state_input][[1]]
    
    Indicator_Data() %>%
      indicator_plot(state_abbr,
                     input$indicator_selector,
                     measure_unit,
                     input_value_d(),
                     getScaleMax(Indicator_Data(), state_abbr, input$indicator_selector),
                     window_inner_width)}, height = plot_height)
  
  
  lake_name <- reactive({
    input$state_input
    if (input$lake_name_input == "") {
      lake_name = "your lake"
    } else {
      lake_name = input$lake_name_input
    }
  })
  
  output$intro_text <- renderUI({
    
    window_inner_width <- input$dimension[1]
    
    if (is.null(window_inner_width)) {
      window_inner_width <- 1300
    }
    
    intro_text(window_inner_width)
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
    tags$strong(glue("Input Your Data{indicator_text}"),class = "input_header")
    
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
              HTML(paste(
                "How Does ", title_lake_name, " Compare to Other",HTML('&nbsp;'), "U.S.", HTML('&nbsp;'), "Lakes?", sep=""
              )
              )
      )
    }
    else {
      title_text <- "Welcome to EPA’s Lake Context Tool"
      
      tags$h3(class = "main_header",
              style = "background-color:#005da4;text-align:left;padding-left:10px;display:flex;justify-content:space-between;",
              # glue(title_text),
              HTML(
                paste("Welcome to EPA's Lake Context", HTML('&nbsp;'), "Tool", sep="")
              ),
              tags$img(src = "epa_logo.png",
                       align = "right",
                       width = 73,
                       height = 24,
                       style = "margin-top: 1px;margin-right: 4px;margin-left: 10px;"))
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
      margin_calculator(Estimate_Data(),state_abbr,input$indicator_selector,lake_value) %>%
      round2(1)
    
    state_name <- input$state_input
    indi_text <-
      indicator_names %>%
      filter(indi_abbr == input$indicator_selector) %>%
      pull(names)
    
    nla_year <- input$year_selector
    survey_timeframe <- get_survey_timeframe(nla_year)
    
    tags$div(class = "disclaimer",
             tags$strong("*Important:"),
             glue(disclaimer_text),tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/nla","EPA's website.",
                                          target = "_blank"))
  })
  
  output$state_header <- renderUI({
    req(valid_inputs())
    
    value <- input_value_d()
    indicator <- input$indicator_selector
    state_abbr <- state_abbrs[input$state_input][[1]]
    state_name <- input$state_input
    nla_year <- input$year_selector
    
    header_html <- generate_html_header(Estimate_Data(), state_abbr,indicator,value,lake_name(),state_name)
    
    tags$div(class = "plot_header", header_html)
  })
  
  observe({    
    req(valid_inputs())
    
    area_name <-
      ecoregion_lookup_table %>%
      filter(state_name == input$state_input) %>%
      pull(ecoregion_name) %>% unique()
    
    
    window_inner_width <- input$dimension[1]
    state_abbr <- state_abbrs[input$state_input][[1]]
    
    ecoregion_name <-
      ecoregion_lookup_table %>%
      filter(state_name == input$state_input) %>%
      pull(ecoregion_name) %>% unique()
    
    measure_unit <- measurements[input$indicator_selector][[1]]
    window_inner_width <- input$dimension[1]
    value <- input_value_d()
    indicator <- input$indicator_selector
    nla_year <- input$year_selector
    
    lapply(ecoregion_name, function(model) {
      output[[paste0("header_", model)]] <- renderUI({
        
        if(model == "Xeric") {
          html_header <- generate_html_header(Estimate_Data(), paste(model),indicator,value,lake_name(),paste('the',model,'ecoregion'))  
        } else {
          html_header <- generate_html_header(Estimate_Data(), paste(model),indicator,value,lake_name(),paste('the',model))
        }
        tags$div(class = "plot_header", html_header)
      })
      
      output[[paste0("plot_", model)]] <- renderPlot({
        Indicator_Data() %>%
          indicator_plot(paste(model),
                         input$indicator_selector,
                         measure_unit,
                         input_value_d(),
                         getScaleMax(Indicator_Data(), state_abbr, input$indicator_selector),
                         window_inner_width)},
        height = plot_height)
    })
  })
  
  output$ecoregion_output <- renderUI({
    req(valid_inputs())
    ecoregion_name <-
      ecoregion_lookup_table %>%
      filter(state_name == input$state_input) %>%
      pull(ecoregion_name) %>% unique()
    
    lapply(ecoregion_name, function(model) {
      fixedRow(column(width = 12,
                      htmlOutput(paste0("header_", model))),
               column(width = 12,
                      plotOutput(paste0("plot_", model), width = "100%",height = "65px")))
    })
  })
  
  output$national_header <- renderUI({
    req(valid_inputs())
    
    value <- input_value_d()
    indicator <- input$indicator_selector
    nla_year <- input$year_selector
    
    html_header <- generate_html_header(Estimate_Data(), "All Sites",indicator,value,lake_name(),"Nationally")
    
    tags$div(class = "plot_header", html_header)
  })
  
  output$national_plot <- renderPlot({
    req(valid_inputs())
    
    window_inner_width <- input$dimension[1]
    
    state_abbr <- state_abbrs[input$state_input][[1]]
    measure_unit <- measurements[input$indicator_selector][[1]]
    
    Indicator_Data() %>%
      indicator_plot("All Sites",
                     input$indicator_selector,
                     measure_unit,
                     input_value_d(),
                     getScaleMax(Indicator_Data(), state_abbr, input$indicator_selector),
                     window_inner_width)},
    height = plot_height)
  
  
  
  # output$png_export <- downloadHandler(
  #   filename = function() {
  #     # browser()
  #     indicator_name <- indicator_english[input$indicator_selector][[1]] %>% str_replace_all(" ","-")
  #     value <- input_value_d()
  #     state_abbr <- state_abbr <- state_abbrs[input$state_input][[1]]
  #     nla_year <- input$year_selector
  #     
  #     
  #     if (valid_inputs()) {
  #       glue("NLA-{nla_year}-Context_{indicator_name}_{value}_{state_abbr}.png")
  #     } else {
  #       "invalid_inputs.png"
  #     }
  #   },
  #   content =  function(file) {
  #     
  #     ggsave_scale <- 1.2
  #     
  #     if (valid_inputs()) {
  #       state_abbr <- state_abbrs[input$state_input][[1]]
  #       lake_value <- input_value_d()
  #       
  #       session_url <- paste0(session$clientData$url_protocol,"//",session$clientData$url_hostname,session$clientData$url_pathname)
  #       ggsave(file,png_creator(dplyr::filter(indicators, year == input$year_selector),
  #                               sub_pop = state_abbrs[input$state_input][[1]],
  #                               indi = input$indicator_selector,
  #                               measure_unit = measurements[input$indicator_selector],
  #                               compared_value = input_value_d(),
  #                               lake_name = lake_name(),
  #                               year = input$year_input,
  #                               indi_text = indicator_text[input$indicator_selector],
  #                               name = input$state_input,
  #                               session_url = session_url,
  #                               margin_of_error = round2(margin_calculator(dplyr::filter(estimates, year == input$year_selector),state_abbr,input$indicator_selector,lake_value), 1),
  #                               nla_year = input$year_selector,
  #                               survey_timeframe = get_survey_timeframe(input$year_selector)
  #       ),
  #       width = 10.4 * ggsave_scale, height = 9.69 * ggsave_scale, type="cairo", device="png", units="in", dpi = 300) }
  #     else {
  #       ggsave(file,invalid_image_file())
  #     }
  #   }
  # )
  
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

