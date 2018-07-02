# creates the plot from Heublein_2010

library(ggplot2)

motives <- read.csv(
  file = ".../03_orga/Heublein_drop-out motives.csv", 
  sep = ";"
)

my_plot <- ggplot(
  data = motives,
  aes(x = reorder(Motive, Percent),
      y = Percent
  )
) + 
geom_bar(
  stat = "identity",
  fill = "steelblue",
  width = .5
) +
coord_flip() +
theme_bw() + 
labs(
  #title = "Most crucial motives of study drop-out in 2008",
  x = "Motive"
)

plot(my_plot)

spath <- ".../02_output"

ggsave(
  plot = my_plot,
  filename = "barplot_drop-out motives_heublein.png",
  device = "png",
  path = spath,
  width = 6,
  height = 4
)
