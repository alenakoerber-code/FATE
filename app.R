# intro -------
source("cleaning.R")

library(shiny)


# ui ----------
ui <- fluidPage(
  titlePanel("FATE data quality dashboard"),
  
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
      
      # 3. switch proportion vs absolute
      radioButtons(
        inputId = "prop_abs",
        label   = "",
        choices = c("proportion", "count"),
        selected = "proportion"
      )
    ),
    
    mainPanel(
     ### h3("Quality of FATE results"),
     ###  plotOutput("plot_quality", height = "450px"),
     
      h3("FATE error heatmap"),
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
  
  ### color for smallest positive
  min_pos <- suppressWarnings(
    min(heat_df$prop_incorrect[heat_df$prop_incorrect > 0], na.rm = TRUE)
  )
  
  if (!is.finite(min_pos)) {
    min_pos <- 1  
  }
  
  ### heatmap with ggplot
  ggplot(
    heat_df,
    aes(x = pathology_fate, y = examiner, fill = prop_incorrect)
  ) + 
    geom_tile(color = "grey80") + 
    scale_fill_gradientn(
      colours = "#2ECC71", "#FADBD8", "#E74C3C",
      values = scales::rescale(c(0, min_pos, 1)),
      limits = c(0,1),
      na.value = "white",
      name = "prop_n_incorrect"
    ) +
    labs( 
      x = "Pathology (FATE)",
      y = "Examiner",
      title = "Proportion of incorrect findings per examiner and pathology"
      ) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid   = element_blank()
    )
  
 
})

}

 
shinyApp(ui, server)
 