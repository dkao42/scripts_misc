
#### bluewhite

theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightblue"),
      panel.border = element_rect(color = "lightblue", fill = NA),
      axis.line = element_line(color = "lightblue"),
      axis.ticks = element_line(color = "lightblue"),
      axis.text = element_text(color = "steelblue")
    )
}

# Using our new theme
ggplot(ToothGrowth, aes(dose, len)) +
  geom_boxplot() +
  theme_bluewhite()



### Alice grey

theme_alicegrey <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "grey90"),
      panel.background = element_rect(fill = "aliceblue"),
      panel.border = element_rect(color = "aliceblue", fill = NA),
      axis.line = element_line(color = "aliceblue"),
      axis.ticks = element_line(color = "aliceblue"),
      axis.text = element_text(color = "skyblue4")
    )
}


### Oldlace grey

theme_oldlacegrey <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "grey90"),
      panel.background = element_rect(fill = "floralwhite"),
      panel.border = element_rect(color = "floralwhite", fill = NA),
      axis.line = element_line(color = "floralwhite"),
      axis.ticks = element_line(color = "floralwhite"),
      axis.text = element_text(color = "burlywood4")
    )
}




### Blue yellow grey

theme_blueyellow <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "yellow"),
      panel.background = element_rect(fill = "blue4"),
      panel.border = element_rect(color = "blue4", fill = NA),
      axis.line = element_line(color = "blue4"),
      axis.ticks = element_line(color = "blue4"),
      axis.text = element_text(color = "blue4")
    )
}
