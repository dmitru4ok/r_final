ui <- page_sidebar(
  sidebar = sidebar(
    helpText("The treemap shows the distribution of license categories."),
    selectInput("category_select",
                label = "Select Category for Gender Distribution:",
                choices = df$category)
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(plotOutput("treemap_plot_output", width = "1000px")),
    card(plotOutput("gender_plot_output"))
  )
)


server = function(input, output, session) {
  # tree map plot (static)
  output$treemap_plot_output <- renderPlot({
    treemap(df,
            index = "label",
            vSize = "count",
            type = "index",
            title = "License Category Distribution",
            fontsize.title = 20,
            border.lwds = 1,
            palette = "Set1"
    )
  })
  
  # gender plot (interactive)
  output$gender_plot_output = renderPlot({
    category_selected = input$category_select # selected from drop-down
    pattern = paste0("\\b", category_selected, "\\b")
    
    gender_counts <- refined %>%
      filter(str_detect(refined$categories, pattern)) %>%
      group_by(gender) %>%
      summarise(count = n())
    
    
    ggplot(gender_counts, aes(x = gender, y = count, fill = gender)) +
      geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
      labs(title = paste("Gender Distribution for Category", category_selected),
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



shinyApp(ui = ui, server = server)

