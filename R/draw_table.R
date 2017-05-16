# utility function for drawing a table with ggplot

draw_table <- function(table_data, title = "") {
  table_header <- data_frame(
    x = seq_along(table_data),
    y = nrow(table_data) +1,
    text = names(table_data),
    color_level = "AAA_first_color_level"
  )

  table_body <- table_data %>%
    arrange(desc(row_number())) %>%
    mutate(y = row_number()) %>%
    gather(x, text, -y) %>%
    mutate(color_level = ifelse(is.na(text), "ZZZ_last_color_level", "BBB_somewhere_in_between"),
           text = ifelse(is.na(text), "NA", text),
           x = as.numeric(factor(x, levels = unique(names(table_data)))))

  ggplot(bind_rows(table_header, table_body), aes(x, y, fill = color_level)) +
    ggtitle(title) +
    geom_tile(color = "black") +
    scale_fill_manual(values = rev(brewer.pal(4, "RdYlBu"))) +
    geom_text(aes(label = text)) +
    theme(legend.position='none',
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(lineheight=.8, face="bold"))
}
