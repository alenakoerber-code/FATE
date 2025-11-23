# intro -------

library("renv")
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization
  skimr,
  readxl,      # reads excel datasets
  units,
  kableExtra,  # pivoting
  shiny
)

# read dataset
linelist <- readr::read_csv("linelist_FATE.csv", show_col_types = FALSE) %>%
  mutate(
    correct_label = if_else(correct == 1, "correct", "incorrect"),
    clinical_diagn_grp = forcats::fct_explicit_na(clinical_diagn_grp, na_level = "unknown")
  )

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
        choices = sort(unique(linelist$examiner)),
        multiple = TRUE,
        selected = sort(unique(linelist$examiner))
      ),
      
      # 2. Filter: Pathology group
      selectInput(
        inputId = "diagn_grp",
        label   = "Pathology group (clinical_diagn_grp)",
        choices = sort(unique(linelist$clinical_diagn_grp)),
        multiple = TRUE,
        selected = sort(unique(linelist$clinical_diagn_grp))
      ),
      
      # 3. switch proportion vs absolute
      checkboxInput(
        inputId = "prop_abs",
        label   = "",
        choices = list("proportion" = 1, "absolute" = 2),
        selected = 1
      )
    ),
    
    mainPanel(
      h3("Quality of FATE results"),
      plotOutput("plot_quality", height = "450px"),
      br(),
      h4("Summary table"),
      tableOutput("table_quality")
    )
  )
)

# SERVER --------
server <- function(input, output, session) {
  
  # filtered data, depending Examiner & Pathology
  filtered_data <- reactive({
    dat <- linelist
    
    if (!is.null(input$examiner) && length(input$examiner) > 0) {
      dat <- dat %>% filter(examiner %in% input$examiner)
    }
    
    if (!is.null(input$diagn_grp) && length(input$diagn_grp) > 0) {
      dat <- dat %>% filter(clinical_diagn_grp %in% input$diagn_grp)
    }
    
    dat
  })
  
  # summary: per Examiner x Pathologie x korrekt/inkorrekt
  summary_data <- reactive({
    filtered_data() %>%
      group_by(examiner, clinical_diagn_grp, correct_label) %>%
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
  
  # Plot -------
  output$plot_quality <- renderPlot({
    dat <- summary_data()
    req(nrow(dat) > 0)   # plot only, if data
    
    if (input$show_prop) {
      ggplot(dat, aes(x = clinical_diagn_grp, y = prop, fill = correct_label)) +
        geom_col(position = "stack") +
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
    } else {
      ggplot(dat, aes(x = clinical_diagn_grp, y = n, fill = correct_label)) +
        geom_col(position = "stack") +
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
  
  # Tabelle -------
  output$table_quality <- renderTable({
    summary_data() %>%
      arrange(examiner, clinical_diagn_grp, desc(correct_label)) %>%
      mutate(prop = scales::percent(prop, accuracy = 0.1))
  })
}

shinyApp(ui, server)