# intro -------
source("cleaning.R")

library(shiny)


# ui ----------
ui <- fluidPage(
  titlePanel("Quality of FATE exams"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter"),
      
      # 1. Filter: Examiner
      selectInput(
        inputId = "examiner",
        label   = "Examiner",
        choices = sort(unique(linelist_long$examiner)),
        multiple = TRUE,
        selected = sort(unique(linelist_long$examiner))
      ),
      
      # 2. Filter: Pathology group
      selectInput(
        inputId = "pathology_fate",
        label   = "Pathology",
        choices = levels(linelist_long$pathology_fate),
        multiple = TRUE,
        selected = sort(unique(linelist_long$pathology_fate))
      ),
      
      # 3. Filter: time to advanced echo
      sliderTextInput(
        inputId = "echo_window",
        label   = "Maximum time to TTE/TEE",
        choices = c("< 24h", "24h", "48h", "72h", "> 72h"),
        selected = "> 72h",
        grid = TRUE
      )
      
    ),
    
    
    
    mainPanel(
     ### h3("Quality of FATE results"),
     ###  plotOutput("plot_quality", height = "450px"),
      h3("FATE correctness heatmap"),
      plotOutput("heatmap_correct", height = "500px")
    
    )
  )
)






# SERVER --------
server <- function(input, output, session) {


  ### filtered data, depending Examiner & Pathology ----------
  filtered_data <- reactive({
    dat <- linelist_long
    
    #### Examiner-Filter
    if (!is.null(input$examiner) && length(input$examiner) > 0) {
      dat <- dat %>% filter(examiner %in% input$examiner)
    }
    
    #### Pathology-Filter
    if (!is.null(input$pathology_fate) && length(input$pathology_fate) > 0) {
      dat <- dat %>% filter(pathology_fate %in% input$pathology_fate)
    }
    
    #### time to echo filter
    if (!is.null(input$echo_window)) {
      echo_map <- c(
        "< 24h" = 1,
        "24h" = 2,
        "48h" = 3,
        "72h" = 4, 
        "> 72h" = 5
        )
      max_cat <- echo_map[[input$echo_window]]
      
      dat <-  dat %>% 
         filter(
           !is.na(hours_to_adv_echo_cat),
           hours_to_adv_echo_cat <= max_cat
         )
    }
      
    dat
  })
  
  ### aggregation for barchart: per Examiner x Pathology x correct_label
  summary_data <- reactive({
    filtered_data() %>%
      group_by(examiner, pathology_fate, correct_label) %>%
      summarise(
        n = n(),
        .groups = "drop_last"
      ) %>%
      mutate(
        total = sum(n),
        prop  = n / total
      ) %>%
      ungroup()
  })
  
  ## plot barchart ---------
  output$plot_quality <- renderPlot({
    dat <- summary_data()
    req(nrow(dat) > 0)   # plot only, if data
  
  ### radiobutton proportion vs abs count
  #### proportion
  if (input$prop_abs == "proportion") {
    ggplot(dat, aes(x = pathology_fate, y = prop, fill = correct_label)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c(
          "correct"   = "#2ECC71",
          "incorrect" = "#E74C3C"
        )
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        x = "Pathology group",
        y = "Proportion",
        fill = "Result",
        title = "Share of correct vs incorrect findings per pathology group"
      ) +
      facet_wrap(~ examiner) +
      coord_flip() +
      theme_minimal()
  } 
  
  #### count
  else {
    ggplot(dat, aes(x = pathology_fate, y = n, fill = correct_label)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c(
          "correct"   = "#2ECC71",
          "incorrect" = "#E74C3C" 
        )
      ) +
      labs(
        x = "Pathology group",
        y = "Number of cases",
        fill = "Result",
        title = "Counts of correct vs incorrect findings per pathology group"
      ) +
      facet_wrap(~ examiner) +
      coord_flip() +
      theme_minimal()
  }
})


  
  ## heatmap -------
output$heatmap_correct <- renderPlot({
  ### get filtered data
  dat <- filtered_data()
  req(nrow(dat) > 0)
      
  ### proportion incorrect results
  heat_df <- dat %>% 
    group_by(examiner, pathology_fate) %>% 
    summarise(
      n_total = n(),
      n_incorrect = sum(correct_label == "incorrect"),
      prop_incorrect = n_incorrect / n_total,
      .groups = "drop"
    )
  
  ### pivot wide
  heat_wide <- heat_df %>%
    tidyr::pivot_wider(
      id_cols = examiner,
      names_from  = pathology_fate,
      values_from = prop_incorrect,
      values_fill = NA_real_  
    ) %>%
    dplyr::distinct(examiner, .keep_all = TRUE) %>% 
    as.data.frame()
  
  ### as.numeric
  rownames(heat_wide) <- heat_wide$examiner
  heat_wide$examiner  <- NULL
  heat_wide[] <- lapply(heat_wide, as.numeric)

  heat_mat <- as.matrix(heat_wide)
  

  
  
  ### color filling
  col_fun <- circlize::colorRamp2(
    c(0, 0.000001, 1),
    colors = c("#2ECC71", "#F8DBD4", "#E74C3C")   # green → red
  )
 
  ### heatmap with ComplexHeatmap
  
 hm <- ComplexHeatmap::Heatmap(
      heat_mat,
      name = "prop_incorrect",
      col = col_fun,
      na_col    = "white",  
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      row_title = "Examiner",
      column_title = "Pathology (FATE)"
    )
 
 ComplexHeatmap::draw(hm)
  })
  
}


  
 
shinyApp(ui = ui, server = server)
 