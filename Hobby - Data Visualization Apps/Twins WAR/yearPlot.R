yearPlot <- function(df, calYear) {
  simple <- data.frame(Player = df$Player,
                       year = df$year, Total.WAR = df$Total.WAR)
  simple <- simple[simple$year == calYear,]
  
  simple$Player <- factor(simple$Player, levels = simple$Player)
  
  q <- ggplot(data = simple, aes(x = Player, y = Total.WAR, fill = Total.WAR)) +
    scale_fill_gradient(low = "red", high = "green", guide = "none") +
    geom_col()  +
    ylab("Total WAR") +
    coord_flip() +
    ylim(-3, 8) +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_x_discrete(limits = rev(levels(simple$Player)))
  q
}

yearPlot(full_data, 1991)

save(yearPlot, file = "yearPlot.Rdata")
