manhattan_plot <- function(data, pcol, title ="", x.title = "Chromosome", y.title = "-log10(P)", font.size = 10, axis.size = 0.5) {
  y.max <- floor(max(data[,pcol])) + 2
  
  data <- data[,c("CHR", "POS", pcol), drop = F]
  colnames(data)[3] <- "PV"
  
  ggplot(data, aes(x = POS, y = PV, colour = as.factor(CHR))) +
    geom_point() +
    facet_grid(.~CHR, scale = "free_x", switch = "x") +
    scale_y_continuous(expand = c(0, 0), limit = c(0, y.max), breaks = seq(from = 0, to = y.max, by = 2)) +
    scale_x_continuous() +
    theme(legend.position = "none",
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing.x=unit(0.1, "lines"),
          axis.line.y = element_line(size = axis.size, color = "black"),
          axis.ticks.y = element_line(size = axis.size, color = "black"),
          axis.ticks.length = unit(axis.size * 10, "points"),
          plot.title = element_text(hjust = (0.5), size = 18),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text = element_text(size = 10),
          strip.text.x = element_text(size = 10)) +
    labs(x = x.title, y = y.title, title = title)
}