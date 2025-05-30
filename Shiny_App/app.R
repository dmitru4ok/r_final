ui <- page_sidebar(
  sidebar = sidebar(
    helpText("The treemap shows the distribution of license categories."),
    selectInput("category_select",
                label = "Select Category for Gender Distribution:",
                choices = NULL)
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(plotOutput("treemap_plot_output", width = "900px")),
    card(plotOutput("gender_plot_output"))
  )
)


server = function(input, output, session) {
  
  # Reactive expression for preparing categories_counted data for the treemap
  categories_data_for_treemap = reactive({
    all_cats_list = str_extract_all(refined$categories, "\\w+")
    all_cats_unlist = unlist(Filter(length, all_cats_list))
    
    if (length(all_cats_unlist) == 0) {
      return(data.frame(category = character(0), count = integer(0), label = character(0), stringsAsFactors = FALSE))
    }
    
    categories_table <- table(all_cats_unlist)
    df <- data.frame(categories_table, stringsAsFactors = FALSE)
    colnames(df) <- c("category", "count")
    
    df <- df[df$category %in% dr_categories, ]
    
    if (nrow(df) == 0) {
      return(data.frame(category = character(0), count = integer(0), label = character(0), stringsAsFactors = FALSE))
    }
    
    df$label <- paste0(df$category, "\n", round(df$count / 1000), "K")
    df <- df[df$count > 0, ]
    return(df)
  })
  
  # Dynamically update choices for the selectInput widget
  observe({
    cat_data <- categories_data_for_treemap()
    if (!is.null(cat_data) && nrow(cat_data) > 0) {
      # Use the 'category' column (raw category names) for choices
      available_categories <- sort(unique(cat_data$category))
      updateSelectInput(session, "category_select", choices = available_categories, selected = available_categories[1])
    } else {
      updateSelectInput(session, "category_select", choices = character(0))
    }
  })
  
  # left plot
  output$treemap_plot_output <- renderPlot({
    tm_input_df <- categories_data_for_treemap()
    
    if (is.null(tm_input_df) || nrow(tm_input_df) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No data available for treemap.", cex = 1.2)
      return()
    }
    
    treemap(tm_input_df,
            index = "label", # This uses the "label" column like "AM\n123K"
            vSize = "count",
            type = "index",
            title = "License Category Distribution",
            fontsize.title = 20,
            
            border.lwds = 1,
            palette = "Set1"
    )
  })
  
  # right plot
  output$gender_plot_output <- renderPlot({
    actual_category <- input$category_select
    
    if (is.null(actual_category) || actual_category == "") {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "Select a category from the dropdown\nto see gender distribution.", cex = 1.2)
      return()
    }
    
    pattern <- paste0("\\b", actual_category, "\\b")
    subset_data <- refined[str_detect(refined$categories, pattern) & !is.na(refined$categories), ]
    
    if (nrow(subset_data) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, paste("No license data found for category:", actual_category), cex = 1.2)
      return()
    }
    
    gender_counts <- subset_data %>%
      filter(!is.na(gender)) %>%
      group_by(gender) %>%
      summarise(count = n(), .groups = 'drop')
    
    if (nrow(gender_counts) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, paste("No gender data available for category:", actual_category), cex = 1.2)
      return()
    }
    
    # right plot
    ggplot(gender_counts, aes(x = gender, y = count, fill = gender)) +
      geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
      labs(title = paste("Gender Distribution for Category", actual_category),
           x = "Gender",
           y = "Number of Licenses"
           ) +
      theme_minimal(base_size = 16) +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = count), vjust = -1, size = 6) +
      scale_fill_manual(values = c("Moteris" = "#FF66B1", "Vyras" = "#17AEFF")) + 
      theme(
        plot.title = element_text(size = 19, hjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14)
        
      )
  })
}


# app bootstrap
shinyApp(ui = ui, server = server)

