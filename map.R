theme_map <- function(...) {
    theme_minimal() +
        theme(
            text = element_text(color = "#666666"),
            # remove all axes
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            # add a subtle grid
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # background colors
            plot.background = element_rect(fill = "#F1EAEA",
                                           color = NA),
            panel.background = element_rect(fill = "#F1EAEA",
                                            color = NA),
            legend.background = element_rect(fill = "#F1EAEA",
                                             color = NA),
         
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 9, hjust = 0,
                                       color = "#666666"),
            plot.title = element_text(size = 15, hjust = 0.5,
                                      color = "#666666"),
            plot.subtitle = element_text(size = 10, hjust = 0.5,
                                         color = "#666666",
                                         margin = margin(b = -0.1,
                                                         t = -0.1,
                                                         l = 2,
                                                         unit = "cm"),
                                         debug = F),
            # captions
            plot.caption = element_text(size = 7,
                                        hjust = .5,
                                        margin = margin(t = 0.2,
                                                        b = 0,
                                                        unit = "cm"),
                                        color = "#939184"),
            ...
        )
}
