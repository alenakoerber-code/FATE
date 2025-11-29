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
      h3("Quality of FATE results"),
      plotOutput("plot_quality", height = "450px"),
      br(),
      ## h4("Summary table"), tableOutput("table_quality")
      h3("FATE heatmap"),
      plotOutput("heatmap_plot", height = "500px")
    
    )
  )
)

# SERVER --------
server <- function(input, output, session) {
  
  # filtered data, depending Examiner & Pathology
  filtered_data <- reactive({
    dat <- linelist_long
    
    if (!is.null(input$examiner) && length(input$examiner) > 0) {
      dat <- dat %>% filter(examiner %in% input$examiner)
    }
    
    if (!is.null(input$diagn_grp) && length(input$pathology_fate) > 0) {
      dat <- dat %>% filter(pathology_fate %in% input$pathology_fate)
    }
    
    dat
  })
  
  # summary: per Examiner x Pathology x (in)correct
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
  
  # Plot -------
  output$plot_quality <- renderPlot({
    dat <- summary_data()
    req(nrow(dat) > 0)   # plot only, if data
    
  ## radiobutton proportion vs abs count
    ### proportion
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
    
    ### count
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
  
  # Table -------
  ## output$table_quality <- renderTable({  summary_data() %>% arrange(examiner, pathology_fate, desc(correct_label)) %>% mutate(prop = scales::percent(prop, accuracy = 0.1))})
}

shinyApp(ui, server)
 