# Shiny app to read data and generate data visualization for exploration
# --------------------------------------------------------------------------

# Install & load required libraries
# --------------------------------------------------------------------------
# packages <- c("tidyverse","here","shiny","tigris","sf","plotly","ggiraph")
# install.packages(setdiff(packages, rownames(installed.packages())))
# invisible(lapply(packages, library, character.only = TRUE))
library(tidyverse)
library(here)
library(shiny)
library(tigris)
library(sf)
library(plotly)
library(ggiraph)
library(cowplot)
library(gridExtra)
library(grid)
library(magick)
library(svglite)
library(gridtext)

# Set file location relative to current project
# --------------------------------------------------------------------------
suppressMessages(here::i_am("app.R"))
shiny::addResourcePath("www", here::here("img"))

# Load the data

# Get model output data from the vax_impact_map_model_output RDS in the `data` folder
read_path_rds <- here("data/vax_impact_map_model_output_curated.rds")
data <- readRDS(read_path_rds)

# Get tigris state data from the RDS in the `data` folder
read_path_tigris_states_rds <- here("data-raw/tigris_states.rds")
tigris_states <- readRDS(read_path_tigris_states_rds)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .plotly {
        min-height: 500px !important;
      }

      @media (min-width: 769px) {
        .js-plotly-plot {
          height: 650px !important;
        }
      }

      @media (max-width: 768px) {
        .sidebar-layout {
          flex-direction: column !important;
        }
        .sidebar-panel {
          width: 100% !important;
          order: 2;
        }
        .main-panel {
          width: 100% !important;
          order: 1;
        }
        .js-plotly-plot {
          height: 400px !important;
        }
      }
      
      .download-panel {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      
      .download-panel h4 {
        color: #495057;
        font-size: 14px;
        margin-bottom: 8px;
      }
      
      .download-panel p {
        color: #6c757d;
        font-size: 12px;
        margin-bottom: 12px;
      }
      
      .download-btn {
        background-color: white !important;
        color: #495057 !important;
        border: 1px solid #ced4da !important;
        font-weight: 500 !important;
        margin: 5px !important;
        font-size: 13px !important;
      }
      
      .download-btn:hover {
        background-color: #495057 !important;
        color: white !important;
        border-color: #495057 !important;
      }
      
      .download-btn i {
        margin-right: 5px;
      }
      
    "))
  ),

  titlePanel(
    title = div(
      style = "display: flex; align-items: center;",
      img(src = "www/logo.png", height = 50, style = "margin-right: 15px;"),
      h2("Quantifying the Health and Economic Costs of Declining Childhood Vaccination",
         style = "margin: 0; color: #000; flex: 1; text-align: center; font-weight: bold; font-size: 20px;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "margin-top: 74px;",

      selectInput("disease",
                  "Disease:",
                  choices = unique(data$disease),
                  selected = unique(data$disease)[1]),
      
      uiOutput("age_group_info"),
      
      br(),

      sliderInput("percent_decline",
                  HTML("Coverage Decline <br> among Infants:"),
                  min = 0,
                  max = 20,
                  value = 10,
                  step = 1,
                  post = "%"),

      selectInput("accrual_label",
                  "Years of Lower Coverage:",
                  choices = unique(data$accrual_label),
                  selected = unique(data$accrual_label)[2]),

      radioButtons("burden_type",
                   "Burden Type:",
                   choices = c("Additional" = "additional",
                               "Total" = "total"),
                   selected = "additional"),

      radioButtons("rate_or_count",
                   "Display:",
                   choices = c("Counts" = "count",
                               "Rates (per 100k Population)" = "rate"),
                   selected = "count"),

      hr(),
      actionButton("show_info", "About", icon = icon("info-circle"))
    ),

    mainPanel(
      width = 10,
      h3(textOutput("map_title_1"),
         textOutput("map_title_2"),
         style = "text-align: center; margin-bottom: 15px; color: #2C3E50; font-size: 18px; font-weight: 500;"),
      
      # Summary Statistics Panel
      div(
        style = "margin-bottom: 20px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
        h4("National Impact Summary",
           style = "color: white; margin-top: 0; margin-bottom: 15px; font-size: 16px; font-weight: bold;"),
        fluidRow(
          column(3,
                 div(style = "background-color: rgba(255,255,255,0.95); padding: 12px; border-radius: 6px; text-align: center; height: 100%;",
                     div(style = "font-size: 11px; color: #666; font-weight: 600; margin-bottom: 5px;",
                         textOutput("summary_label_cases", inline = TRUE)),
                     div(style = "font-size: 22px; font-weight: bold; color: #2C3E50;",
                         textOutput("summary_cases", inline = TRUE))
                 )
          ),
          column(3,
                 div(style = "background-color: rgba(255,255,255,0.95); padding: 12px; border-radius: 6px; text-align: center; height: 100%;",
                     div(style = "font-size: 11px; color: #666; font-weight: 600; margin-bottom: 5px;",
                         textOutput("summary_label_hosp", inline = TRUE)),
                     div(style = "font-size: 22px; font-weight: bold; color: #2C3E50;",
                         textOutput("summary_hosp", inline = TRUE))
                 )
          ),
          column(3,
                 div(style = "background-color: rgba(255,255,255,0.95); padding: 12px; border-radius: 6px; text-align: center; height: 100%;",
                     div(style = "font-size: 11px; color: #666; font-weight: 600; margin-bottom: 5px;",
                         textOutput("summary_label_deaths", inline = TRUE)),
                     div(style = "font-size: 22px; font-weight: bold; color: #2C3E50;",
                         textOutput("summary_deaths", inline = TRUE))
                 )
          ),
          column(3,
                 div(style = "background-color: rgba(255,255,255,0.95); padding: 12px; border-radius: 6px; text-align: center; height: 100%;",
                     div(style = "font-size: 11px; color: #666; font-weight: 600; margin-bottom: 5px;",
                         textOutput("summary_label_costs", inline = TRUE)),
                     div(style = "font-size: 22px; font-weight: bold; color: #2C3E50;",
                         textOutput("summary_costs", inline = TRUE))
                 )
          )
        )
      ),
      
      div(
        class = "download-panel",
        fluidRow(
          column(4,
                 downloadButton("download_png", 
                                label = tagList(icon("image"), "PNG"), 
                                class = "download-btn btn-block")),
          column(4,
                 downloadButton("download_pdf", 
                                label = tagList(icon("file-pdf"), "PDF"), 
                                class = "download-btn btn-block")),
          column(4,
                 downloadButton("download_svg", 
                                label = tagList(icon("vector-square"), "SVG"), 
                                class = "download-btn btn-block"))
        )
      ),

      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; background-color: white;",
        girafeOutput("map", height = "700px")
      ),


      # Mobile hint
      tags$p("Hover over any state for details. Best viewed on desktop.",
             style = "text-align: center; color: #888; font-size: 12px; margin-top: 0px;"),
    
    # Footer
    tags$footer(
      style = "text-align: left; padding: 20px; margin-top: 50px; border-top: 1px solid #eee;",
      HTML("Except where otherwise indicated, the content on this app is licensed under the 
         <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/' target='_blank'>
         Creative Commons Attribution–NonCommercial–NoDerivatives 4.0 International License (CC BY-NC-ND 4.0)</a>.")
    )
    
    )
  )
)

server <- function(input, output, session) {
  
  # Disease images from CDC
  disease_images <- list(
    "Rotavirus" = here("img/rotavirus.jpg"),
    "Pertussis" = here("img/pertussis.jpg"),
    "Pneumococcal" = here("img/pneumococcal.jpg")
  )
  
  # Disease image captions
  disease_image_captions <- list(
    "Rotavirus" = "Rotavirus virions\nIllustrator: Alissa Eckert, MS\nCDC, 2016",
    "Pertussis" = "Bordetella pertussis bacteria\nIllustrator: Meredith Newlove\nCDC, 2016",
    "Pneumococcal" = "Drug-resistant Streptococcus pneumoniae\nIllustrator: Meredith Newlove\nCDC, 2019"
  )

  # Show modal dialog on startup
  showModal(modalDialog(
    title = "Welcome to VaxImpactMap",
    HTML("
      <p>This interactive tool visualizes the potential health and economic impacts of declining childhood vaccination coverage in the United States.</p>
      
      <h4>How to use this tool:</h4>
      <ol>
        <li><strong>Select a disease</strong> from the dropdown menu</li>
        <li><strong>Adjust the coverage decline</strong> using the slider (0-20%)</li>
        <li><strong>Choose the time period</strong> for lower coverage</li>
        <li><strong>Select burden type:</strong> Additional (attributable to decline) or Total (overall burden)</li>
        <li><strong>Choose display format:</strong> Counts or Rates per 100k population</li>
        <li><strong>Download your map</strong> with full attribution and parameters for sharing</li>
      </ol>
      
      <p><strong>Hover over any state</strong> to see detailed statistics.</p>
      
      <p style='font-size: 12px; color: #666; margin-top: 20px;'>
      This tool is for educational and advocacy purposes. 
      Data is based on epidemiological models and should be interpreted appropriately.
      </p>
    "),
    easyClose = TRUE,
    footer = modalButton("Get Started")
  ))
  
  # Show info modal
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "About VaxImpactMap",
      HTML("
        <h4>Purpose</h4>
        <p>This visualization tool demonstrates the potential public health and economic consequences 
        of declining childhood vaccination coverage across the United States.</p>
        
        <h4>Methodology</h4>
        <p>The estimates are based on epidemiological models that calculate disease burden under 
        different vaccination coverage scenarios.</p>
        
        <h4>License</h4>
        <p>Content is licensed under CC BY-NC-ND 4.0. When sharing downloaded maps, 
        please maintain the attribution included in the image.</p>
        
        <h4>Contact</h4>
        <p>For questions or more information, please visit VaxImpactMap.org.</p>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Reactive expression to determine which column to use
  metric_column <- reactive({
    prefix <- if(input$burden_type == "additional") "additional_" else ""
    base_metric <- paste0(prefix, "hospitalizations")

    if (input$burden_type == "total") {
      base_metric <- "hospitalizations"
    }

    if (input$rate_or_count == "rate") {
      base_metric <- if(input$burden_type == "additional") {
        "additional_hospitalizations_per_100k"
      } else {
        "hospitalizations_per_100k"
      }
    }

    return(base_metric)
  })

  # Filter data based on inputs
  filtered_data <- reactive({
    data %>%
      filter(
        disease == input$disease,
        percent_decline == as.character(input$percent_decline),
        accrual_label == input$accrual_label
      ) %>%
      mutate(state_lower = tolower(state_name))
  })
  
  # Reactive text based on disease selection
  output$age_group_info <- renderUI({
    age_groups <- unique(filtered_data()$age_group)
    
    tagList(
      tags$label("Population:", style = "font-weight: bold;"),
      tags$div(paste("Children Ages ",tools::toTitleCase(age_groups)))
    )
  })

  # Add map title 1
  output$map_title_1 <- renderText({
    
    age_groups <- unique(filtered_data()$age_group)
    
    current_date <- Sys.Date()
    
    current_year_char <- format(current_date, "%Y")
    
    example_year <- ifelse(input$accrual_label=='5 Years',
                           as.character(as.numeric(current_year_char)+5),
                           as.character(as.numeric(current_year_char)+1))
    
    paste0("Annual ", input$disease, " Burden Among Children Ages ", 
          tools::toTitleCase(age_groups), " in ", example_year, sep="")
  })
  
  # Add map title 2
  output$map_title_2 <- renderText({
    
    current_date <- Sys.Date()
    
    current_year_char <- format(current_date, "%Y")
    
    paste0(" after ", input$accrual_label, " of ", 
           as.character(input$percent_decline), 
           "% Decline From Current Coverage per Year among Infants Beginning in ", 
           current_year_char, sep="")
  })

  # Get US-level summary data
  us_summary <- reactive({
    data %>%
      filter(
        disease == input$disease,
        percent_decline == as.character(input$percent_decline),
        accrual_label == input$accrual_label,
        state_name == "United States"
      )
  })

  # Summary statistics labels
  output$summary_label_cases <- renderText({
    if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") "ADDITIONAL CASES" else "ADDITIONAL CASES PER 100K"
    } else {
      if(input$rate_or_count == "count") "TOTAL CASES" else "TOTAL CASES PER 100K"
    }
  })

  output$summary_label_hosp <- renderText({
    if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") "ADDITIONAL HOSPITALIZATIONS" else "ADDITIONAL HOSP. PER 100K"
    } else {
      if(input$rate_or_count == "count") "TOTAL HOSPITALIZATIONS" else "TOTAL HOSP. PER 100K"
    }
  })

  output$summary_label_deaths <- renderText({
    if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") "ADDITIONAL DEATHS" else "ADDITIONAL DEATHS PER 100K"
    } else {
      if(input$rate_or_count == "count") "TOTAL DEATHS" else "TOTAL DEATHS PER 100K"
    }
  })

  output$summary_label_costs <- renderText({
    if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") "ADDITIONAL COSTS" else "ADDITIONAL COSTS PER 100K"
    } else {
      if(input$rate_or_count == "count") "TOTAL COSTS" else "TOTAL COSTS PER 100K"
    }
  })

  # Summary statistics values
  output$summary_cases <- renderText({
    us_data <- us_summary()
    if(nrow(us_data) == 0) return("N/A")

    value <- if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") us_data$additional_cases else us_data$additional_cases_per_100k
    } else {
      if(input$rate_or_count == "count") us_data$cases else us_data$cases_per_100k
    }

    scales::comma(round(value))
  })

  output$summary_hosp <- renderText({
    us_data <- us_summary()
    if(nrow(us_data) == 0) return("N/A")

    value <- if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") us_data$additional_hospitalizations else us_data$additional_hospitalizations_per_100k
    } else {
      if(input$rate_or_count == "count") us_data$hospitalizations else us_data$hospitalizations_per_100k
    }

    scales::comma(round(value))
  })

  output$summary_deaths <- renderText({
    us_data <- us_summary()
    if(nrow(us_data) == 0) return("N/A")

    value <- if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") us_data$additional_deaths else us_data$additional_deaths_per_100k
    } else {
      if(input$rate_or_count == "count") us_data$deaths else us_data$deaths_per_100k
    }

    scales::comma(round(value, 1))
  })

  output$summary_costs <- renderText({
    us_data <- us_summary()
    if(nrow(us_data) == 0) return("N/A")

    value <- if(input$burden_type == "additional") {
      if(input$rate_or_count == "count") us_data$additional_total_cost else us_data$additional_total_cost_per_100k
    } else {
      if(input$rate_or_count == "count") us_data$total_cost else us_data$total_cost_per_100k
    }

    scales::dollar(round(value / 1000000) * 1000000)
  })
  
  create_static_map <- reactive({
    # Get map data
    us_states <- tigris_states
    state_data <- filtered_data()
    
    plot_data <- us_states %>%
      left_join(state_data, by = c("NAME" = "state_name"))
    
    metric_col <- metric_column()
    
    global_max <- data %>%
      filter(
        disease == input$disease,
        accrual_label == input$accrual_label,
        state_name != 'United States'
      ) %>%
      pull(!!sym(metric_col)) %>%
      max(na.rm = TRUE)
    
    burden_label <- if(input$burden_type == "additional") "Additional " else "Total "
    rate_label <- if(input$rate_or_count == "rate") " per 100k" else ""
    legend_name <- paste0(burden_label, "Hospitalizations", rate_label)
    
    # Create static map without interactive features
    p <- ggplot(plot_data) +
      geom_sf(
        aes(fill = .data[[metric_col]]),
        color = "black",
        linewidth = 0.3
      ) +
      scale_fill_gradient(
        low = "#ffffcc",
        high = "#800026",
        name = legend_name,
        labels = scales::comma,
        na.value = "grey90",
        limits = c(0, global_max)
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 30, 10, 10)
      ) +
      guides(fill = guide_colorbar(
        title.position = "top", 
        title.hjust = 0.5,
        barwidth = 1.5,
        barheight = 15
      ))
    
    return(p)
  })
  
  # FUNCTION TO CREATE FULL DOWNLOADABLE PLOT WITH METADATA
  create_download_plot <- function() {
    # Get the base map
    base_map <- ggplotGrob(create_static_map())
    
    base_map_with_margins <- arrangeGrob(
      base_map,
      padding = unit(c(0, 0.3, 0, 0.3), "inches")  # top, right, bottom, left
    )
    
    # Create header
    age_groups <- unique(filtered_data()$age_group)
    current_date <- Sys.Date()
    current_year_char <- format(current_date, "%Y")
    example_year <- ifelse(input$accrual_label=='5 Years',
                           as.character(as.numeric(current_year_char)+5),
                           as.character(as.numeric(current_year_char)+1))
    
    title_line_1 <- paste0("Annual ", input$disease, " Burden Among Children Ages ", 
                           tools::toTitleCase(age_groups), " in ", example_year)
    title_line_2 <- paste0(" after ", input$accrual_label, " of ", 
                           as.character(input$percent_decline), 
                           "% Decline From Current Coverage per Year among Infants Beginning in ", 
                           current_year_char)
    
    header_text <- paste0(title_line_1, "\n", title_line_2)
    
    # Create parameter details box
    param_text <- paste0(
      "PARAMETERS\n",
      "Disease: ", tools::toTitleCase(input$disease), "\n",
      "Coverage Decline: ", input$percent_decline, "%\n",
      "Years of Lower Coverage: ", input$accrual_label, "\n",
      "Burden Type: ", tools::toTitleCase(input$burden_type), "\n",
      "Display Format: ", ifelse(input$rate_or_count == "count", "Counts", "Rates per 100k"), "\n",
      "Age Group: ", tools::toTitleCase(filtered_data()$age_group[1]), "\n",
      "Generated: ", format(Sys.Date(), "%B %d, %Y")
    )
    
    # Create attribution text
    attribution_base <- paste0(
      "ATTRIBUTION & LICENSE\n",
      "Source: VaxImpactMap\n",
      "Website: VaxImpactMap.org\n"
    )
    
    # Add disease image credit if applicable
    if (!is.null(disease_images[[input$disease]])) {
      attribution_base <- paste0(
        attribution_base,
        "Disease image: Centers for Disease Control and Prevention (CDC)\n"
      )
    }
    
    attribution_text <- paste0(
      attribution_base,
      "\nThis work is licensed under CC BY-NC-ND 4.0\n",
      "(Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International)\n",
      "You may share this image with attribution for non-commercial purposes.\n",
      "https://creativecommons.org/licenses/by-nc-nd/4.0/"
    )
    
    # Create national summary text with formatted components
    us_data <- us_summary()
    if(nrow(us_data) > 0) {
      # Create title
      if(input$rate_or_count == "rate") {
        summary_title <- paste0("NATIONAL IMPACT (PER 100K CHILDREN AGES ", 
        toupper(tools::toTitleCase(age_groups)), ")")
      } else {
        summary_title <- "NATIONAL IMPACT"
      }
      
      # Create title grob (size 14, bold)
      summary_title_grob <- textGrob(
        summary_title,
        x = 0.5, just = "center",
        gp = gpar(fontsize = 14, fontface = "bold")
      )
      
      # Create metrics grob with mixed formatting (bold labels, regular values)
      # Using richtext_grob to support HTML formatting
      if(input$burden_type == "additional" && input$rate_or_count == "count") {
        metrics_html <- paste0(
          "<b>Additional Cases:</b> ", scales::comma(round(us_data$additional_cases)), " | ",
          "<b>Additional Hospitalizations:</b> ", scales::comma(round(us_data$additional_hospitalizations)), " | ",
          "<b>Additional Deaths:</b> ", round(us_data$additional_deaths, 1), " | ",
          "<b>Additional Costs:</b> ", scales::dollar(round(us_data$additional_total_cost / 1000000) * 1000000)
        )
      } else if(input$burden_type == "total" && input$rate_or_count == "count") {
        metrics_html <- paste0(
          "<b>Total Cases:</b> ", scales::comma(round(us_data$cases)), " | ",
          "<b>Total Hospitalizations:</b> ", scales::comma(round(us_data$hospitalizations)), " | ",
          "<b>Total Deaths:</b> ", round(us_data$deaths, 1), " | ",
          "<b>Total Costs:</b> ", scales::dollar(round(us_data$total_cost / 1000000) * 1000000)
        )
      } else if(input$burden_type == "additional" && input$rate_or_count == "rate") {
        metrics_html <- paste0(
          "<b>Additional Cases:</b> ", scales::comma(round(us_data$additional_cases_per_100k, 1)), " | ",
          "<b>Additional Hospitalizations:</b> ", scales::comma(round(us_data$additional_hospitalizations_per_100k, 1)), " | ",
          "<b>Additional Deaths:</b> ", round(us_data$additional_deaths_per_100k, 2), " | ",
          "<b>Additional Costs:</b> ", scales::dollar(round(us_data$additional_total_cost_per_100k))
        )
      } else {
        metrics_html <- paste0(
          "<b>Total Cases:</b> ", scales::comma(round(us_data$cases_per_100k, 1)), " | ",
          "<b>Total Hospitalizations:</b> ", scales::comma(round(us_data$hospitalizations_per_100k, 1)), " | ",
          "<b>Total Deaths:</b> ", round(us_data$deaths_per_100k, 2), " | ",
          "<b>Total Costs:</b> ", scales::dollar(round(us_data$total_cost_per_100k))
        )
      }
      
      summary_metrics_grob <- gridtext::textbox_grob(
        metrics_html,
        x = 0.5, y = 0.5,
        hjust = 0.5, vjust = 0.5,
        halign = 0.5,
        width = unit(0.9, "npc"),  # Use 90% of available width
        gp = gpar(fontsize = 10),
        box_gp = gpar(col = NA, fill = NA),
        r = unit(0, "pt"),
        padding = unit(c(0, 0, 0, 0), "pt")
      )
      
      # Combine title and metrics
      summary_grob <- arrangeGrob(
        summary_title_grob,
        summary_metrics_grob,
        ncol = 1,
        heights = c(0.4, 0.6)
      )
    } else {
      summary_grob <- textGrob(
        "NATIONAL IMPACT\nData not available",
        x = 0.5, just = "center",
        gp = gpar(fontsize = 14, fontface = "bold")
      )
    }
    
    # Create text grobs
    title_grob <- textGrob(
      header_text,
      gp = gpar(fontsize = 16, fontface = "bold"),
      just = "center"
    )
    
    # Add logo positioned in upper left with margins
    logo_path <- here("img/logo.png")
    logo_img <- magick::image_read(logo_path)
    logo_img <- magick::image_scale(logo_img, "x100")  # Scale to 100px height, width auto
    logo_img_raster <- as.raster(logo_img)
    logo_grob <- rasterGrob(
      logo_img_raster,
      x = unit(0.2, "inches"),  # 0.2 inches from left edge
      y = unit(1, "npc") - unit(0.2, "inches"),  # 0.2 inches from top edge
      just = c("left", "top")  # Anchor at top-left of the logo
    )
    
    # Add disease image for bottom section
    disease_img <- magick::image_read(disease_images[[input$disease]])
    disease_img <- magick::image_scale(disease_img, "200x200")
    disease_img_raster <- as.raster(disease_img)
    disease_img_grob <- rasterGrob(disease_img_raster, 
                                   width = unit(2, "inches"),
                                   height = unit(2, "inches"))
    
    # Add caption from lookup table
    caption_text <- disease_image_captions[[input$disease]]
    disease_caption_grob <- textGrob(
      caption_text,
      x = 0.5, just = "center",
      gp = gpar(fontsize = 7, col = "gray40", lineheight = 1.2)
    )
    
    param_grob <- textGrob(
      param_text,
      x = 0.05, just = "left",
      gp = gpar(fontsize = 9, fontfamily = "mono")
    )
    
    attribution_grob <- textGrob(
      attribution_text,
      x = 0.05, just = "left",
      gp = gpar(fontsize = 8, col = "gray30")
    )
    
    # Create bottom section with parameters and disease image side by side
    # Combine disease image and its caption
    disease_with_caption <- arrangeGrob(
      disease_img_grob,
      disease_caption_grob,
      ncol = 1,
      heights = c(2, 0.5)
    )
    
    bottom_section <- arrangeGrob(
      nullGrob(),
      disease_with_caption,
      nullGrob(),
      param_grob,
      nullGrob(),
      attribution_grob,
      nullGrob(),
      ncol = 7,
      widths = c(1, 2, 1, 2, 1, 4, 1)
    )
    
    # Build the final plot - logo at top, title below, then rest of content
    final_plot <- grid.arrange(
      logo_grob,
      nullGrob(),
      title_grob,
      nullGrob(),
      summary_grob,
      base_map_with_margins,
      bottom_section,
      nullGrob(),
      ncol = 1,
      heights = c(0.3, 0.3, 0.6, 0.2, 0.5, 4, 1.5, 0.3)
    )
    
    return(final_plot)
  }
  
  # Download handlers
  output$download_png <- downloadHandler(
    filename = function() {
      paste0(
        "vaximpactmap_",
        tolower(gsub(" ", "_", input$disease)), "_",
        input$percent_decline, "pct_decline_",
        tolower(gsub(" ", "_", input$accrual_label)), "_",
        input$burden_type, "_",
        input$rate_or_count, "_",
        format(Sys.Date(), "%Y%m%d"),
        ".png"
      )
    },
    content = function(file) {
      # Create the plot
      plot_to_save <- create_download_plot()
      
      # Save as high-resolution PNG
      ggsave(
        file,
        plot = plot_to_save,
        width = 12,
        height = 14,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(
        "vaximpactmap_",
        tolower(gsub(" ", "_", input$disease)), "_",
        input$percent_decline, "pct_decline_",
        tolower(gsub(" ", "_", input$accrual_label)), "_",
        input$burden_type, "_",
        input$rate_or_count, "_",
        format(Sys.Date(), "%Y%m%d"),
        ".pdf"
      )
    },
    content = function(file) {
      plot_to_save <- create_download_plot()
      
      ggsave(
        file,
        plot = plot_to_save,
        width = 12,
        height = 14,
        device = "pdf",
        bg = "white"
      )
    }
  )
  
  output$download_svg <- downloadHandler(
    filename = function() {
      paste0(
        "vaximpactmap_",
        tolower(gsub(" ", "_", input$disease)), "_",
        input$percent_decline, "pct_decline_",
        tolower(gsub(" ", "_", input$accrual_label)), "_",
        input$burden_type, "_",
        input$rate_or_count, "_",
        format(Sys.Date(), "%Y%m%d"),
        ".svg"
      )
    },
    content = function(file) {
      plot_to_save <- create_download_plot()
      
      ggsave(
        file,
        plot = plot_to_save,
        width = 12,
        height = 14,
        device = "svg",
        bg = "white"
      )
    }
  )

  # Render the map
  output$map <- renderGirafe({
    # Get map data
    us_states <- tigris_states

    # Get filtered state data
    state_data <- filtered_data()

    # Join map with data
    plot_data <- us_states %>%
      left_join(state_data, by = c("NAME" = "state_name"))

    # Get the metric value for coloring
    metric_col <- metric_column()

    # Calculate global max for this metric across ALL percent_decline values
    # but filtering by disease, accrual_label
    global_max <- data %>%
      filter(
        disease == input$disease,
        accrual_label == input$accrual_label,
        state_name != 'United States'
      ) %>%
      pull(!!sym(metric_col)) %>%
      max(na.rm = TRUE)

    # Create tooltip text
    plot_data <- plot_data %>%
      group_by(NAME) %>%
      mutate(
        tooltip_text = if(input$burden_type == "additional" & input$rate_or_count == "count") {
          paste0(
            "<b style='font-size:16px;'>", toupper(tools::toTitleCase(NAME)), "</b><br>",
            "Baseline coverage: ", scales::percent(baseline_coverage, accuracy = 0.1), "<br>",
            "Population Age ",tools::toTitleCase(age_group),": ", scales::comma(age_group_population), "<br>",
            "<br>",
            "<b>HEALTH BURDEN</b>", "<br>",
            "Additional Cases: ", scales::comma(additional_cases), "<br>",
            "Additional Hospitalizations: ", scales::comma(additional_hospitalizations), "<br>",
            "Additional Deaths: ", round(additional_deaths, 1), "<br>",
            "<br>",
            "<b>ECONOMIC BURDEN</b>", "<br>",
            "Additional Workdays Lost: ", scales::comma(additional_workdays_lost), "<br>",
            "Additional Productivity Costs: ", scales::dollar(round(additional_productivity_cost / 100000) * 100000), "<br>",
            "Additional Hospitalization Costs: ", scales::dollar(round(additional_hospitalization_cost / 100000) * 100000), "<br>",
            "Additional Total Costs: ", scales::dollar(round(additional_total_cost / 100000) * 100000)
          )
        } else if(input$burden_type == "total" & input$rate_or_count == "count") {
          paste0(
            "<b style='font-size:16px;'>", toupper(tools::toTitleCase(NAME)), "</b><br>",
            "Baseline coverage: ", scales::percent(baseline_coverage, accuracy = 0.1), "<br>",
            "Population Age ",tools::toTitleCase(age_group),": ", scales::comma(age_group_population), "<br>",
            "<br>",
            "<b>HEALTH BURDEN</b>", "<br>",
            "Total Cases: ", scales::comma(cases), "<br>",
            "Total Hospitalizations: ", scales::comma(hospitalizations), "<br>",
            "Total Deaths: ", round(deaths, 1), "<br>",
            "<br>",
            "<b>ECONOMIC BURDEN</b>", "<br>",
            "Total Workdays Lost: ", scales::comma(workdays_lost), "<br>",
            "Total Productivity Costs: ", scales::dollar(round(productivity_cost / 100000) * 100000), "<br>",
            "Total Hospitalization Costs: ", scales::dollar(round(hospitalization_cost / 100000) * 100000), "<br>",
            "Total Costs: ", scales::dollar(round(total_cost / 100000) * 100000)
          )
        } else if(input$burden_type == "additional" & input$rate_or_count == "rate") {
          paste0(
            "<b style='font-size:16px;'>", toupper(tools::toTitleCase(NAME)), "</b><br>",
            "Baseline coverage: ", scales::percent(baseline_coverage, accuracy = 0.1), "<br>",
            "Population Age ",tools::toTitleCase(age_group),": ", scales::comma(age_group_population), "<br>",
            "<br>",
            "<b>HEALTH BURDEN</b>", "<br>",
            "Additional Cases per 100k: ", scales::comma(additional_cases_per_100k), "<br>",
            "Additional Hospitalizations per 100k: ", scales::comma(additional_hospitalizations_per_100k), "<br>",
            "Additional Deaths per 100k: ", round(additional_deaths_per_100k, 1), "<br>",
            "<br>",
            "<b>ECONOMIC BURDEN</b>", "<br>",
            "Additional Workdays Lost per 100k: ", scales::comma(additional_workdays_lost_per_100k), "<br>",
            "Additional Productivity Costs per 100k: ", scales::dollar(round(additional_productivity_cost_per_100k / 100000) * 100000), "<br>",
            "Additional Hospitalization Costs per 100k: ", scales::dollar(round(additional_hospitalization_cost_per_100k / 100000) * 100000), "<br>",
            "Additional Total Costs per 100k: ", scales::dollar(round(additional_total_cost_per_100k / 100000) * 100000)
          )
        } else if(input$burden_type == "total" & input$rate_or_count == "rate"){
          paste0(
            "<b style='font-size:16px;'>", toupper(tools::toTitleCase(NAME)), "</b><br>",
            "Baseline coverage: ", scales::percent(baseline_coverage, accuracy = 0.1), "<br>",
            "Population Age ",tools::toTitleCase(age_group),": ", scales::comma(age_group_population), "<br>",
            "<br>",
            "<b>HEALTH BURDEN</b>", "<br>",
            "Total Cases per 100k: ", scales::comma(cases_per_100k), "<br>",
            "Total Hospitalizations per 100k: ", scales::comma(hospitalizations_per_100k), "<br>",
            "Total Deaths per 100k: ", round(deaths_per_100k, 1), "<br>",
            "<br>",
            "<b>ECONOMIC BURDEN</b>", "<br>",
            "Total Workdays Lost per 100k: ", scales::comma(workdays_lost_per_100k), "<br>",
            "Total Productivity Costs per 100k: ", scales::dollar(round(productivity_cost_per_100k / 100000) * 100000), "<br>",
            "Total Hospitalization Costs per 100k: ", scales::dollar(round(hospitalization_cost_per_100k / 100000) * 100000), "<br>",
            "Total Costs per 100k: ", scales::dollar(round(total_cost_per_100k / 100000) * 100000)
          )
        }
      ) %>%
      ungroup()

    # Build legend title
    burden_label <- if(input$burden_type == "additional") "Additional " else "Total "
    rate_label <- if(input$rate_or_count == "rate") " per 100k" else ""
    legend_name <- paste0(burden_label, "Hospitalizations", rate_label)

    # Create the plot with interactive geom
    p <- ggplot(plot_data) +
      geom_sf_interactive(
        aes(fill = .data[[metric_col]],
            tooltip = tooltip_text,
            data_id = NAME),
        color = "black",
        linewidth = 0.2
      ) +
      scale_fill_gradient(
        low = "#ffffcc",
        high = "#800026",
        name = legend_name,
        labels = scales::comma,
        na.value = "grey90",
        limits = c(0, global_max)
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.background = element_blank(),
        plot.background = element_blank()
      ) +
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

    # Render as interactive girafe object
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6,
      options = list(
        opts_hover(css = "stroke:black;stroke-width:3;"),
        opts_tooltip(
          css = "background-color:white;color:black;padding:10px;border-radius:5px;box-shadow:0 0 10px rgba(0,0,0,0.5);",
          opacity = 0.95
        ),
        opts_toolbar(hidden = c('selection', 'zoom', 'misc')),
        opts_sizing(rescale = TRUE)
      )
    )
  })
}

shinyApp(ui, server)
