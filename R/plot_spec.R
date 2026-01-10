# loading color palette
redGreen0102 <- wesanderson::wes_palette("Darjeeling1")

# MDRC Color Palette
mdrc_colors <- data.frame(a = c("#cae3eb", "#F9DABC"    , "#D9C89E"    , "#DAE6D6"    , "#e6e7e7", NA_character_),
                          b = c("#63b6cf", "#EFA967"    , "#A89968"    , "#A1BD7A"    , "#b1b3b5", NA_character_),
                          c = c("#00558C", "#D57800"    , "#816040"    , "#538549"    , "#808284", "#B7312C"    ),
                          d = c("#002B49", NA_character_, NA_character_, NA_character_, "#000000", NA_character_),
                          row.names = c("blue", "orange", "brown", "green", "grey", "red"))

# MDRC Theme
theme_mdrc <- function(base_size = 9, base_family= "ArialMT") {

  # All text should appear in Arial 9-point font, except axis labels and titles.
  theme_bw(base_size = base_size, base_family = base_family) %+replace%

    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),

          # No legend - use labels directly in graph
          # legend.position = "none",

          # Show only x and y borders
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),

          # All labels should be horizontal
          axis.title.y = element_text(angle = 360),

          # Making axis values darker
          axis.text = element_text(colour = "black", size = 9),

          # Center and bold titles should (Arial Bold 11-point)
          plot.title = element_text(face = "bold",
                                    hjust = 0.5,
                                    size = 11,
                                    margin= ggplot2::margin(0,0,1,0, unit = "line")),

          # Left-align source/notes/footnotes
          plot.caption = element_text(hjust= 0,
                                      margin= ggplot2::margin(1,0,0,0, unit = "line")),
          # Additional formatting added
          # Remove tickmarks
          axis.ticks.y =element_blank(),
          axis.ticks.x =element_blank(),

          # Remove background color for panel labels created by facet_*
          strip.background = element_blank(),

          # Make panel label horizontal
          strip.text = element_text(size = 9),
          strip.text.y.right = element_text(angle = 0),
          strip.text.y.left = element_text(angle = 0),

          # Make panel label outside of the axis line (outside/inside)
          strip.placement = "outside"

    )
}
