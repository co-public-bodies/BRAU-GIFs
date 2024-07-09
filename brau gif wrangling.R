## BRAU Gif

library(tidyverse)
library(tidylog)
library(gganimate)

# load dummy data
brau_gif_data <- readxl::read_xlsx("~/Martin Ingram R Work/Test Area/BRAU gif/BRAU Gif Data.xlsx") |> 
    group_by(Group) |> 
    mutate(avg_line = mean(value))

# dimension for height of gifs
height_dim <- 400

# dimension for width of gifs
width_dim <- 1000

# No Text Version -----------------------------------------------------------

# create basic plot with animation
# no name, white background
no_text_brau_gif_output <- brau_gif_data |> 
    ggplot(aes(x = location, y = value,
               fill = Group, group = location)) +
    geom_col(color = "black",
             alpha = 0.8) +
    geom_line(aes(y = avg_line,
                  group = Group),
              color = 'black',
              linewidth = 1.5)+
    scale_fill_brewer(palette = "RdBu",
                      direction = -1)+
    theme_classic()+
    theme(axis.line = element_line(color = NA),
          axis.ticks = element_line(color = NA))+
    theme(legend.position = "none")+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(labels = NULL)+
    scale_y_continuous(labels = NULL, 
                       limits = c(0, 105))+
    labs(title = NULL)+
    transition_reveal(time)

# render to gif 
no_text_brau_gif_output <- animate(no_text_brau_gif_output,
        height = height_dim,
        width = width_dim, 
        renderer = gifski_renderer(loop = TRUE))

# save gif
anim_save(animation = no_text_brau_gif_output, 
          filename = "BRAU_no_text.gif")

# Full Name Version ----------------------------------------------------------

# create basic plot with animation
# version with name on the gif
named_brau_gif_output <- brau_gif_data |> 
    ggplot(aes(x = location, y = value,
               fill = Group, group = location)) +
    geom_col(color = "black",
             alpha = 0.8) +
    geom_line(aes(y = avg_line,
                  group = Group),
              color = 'black',
              linewidth = 1.5)+
    scale_fill_brewer(palette = "RdBu",
                      direction = -1)+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.line = element_line(color = NA),
          axis.ticks = element_line(color = NA))+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(labels = NULL)+
    scale_y_continuous(labels = NULL,
                       limits = c(0, 100))+
    labs(title = NULL)+
    ggtext::geom_richtext(x = 8.7, y = 99,
                  label = "**Benchmarking and Reform Analysis Unit**",
                  label.color = NA,
                  fill = NA, 
                  size = 14)+
    transition_reveal(time)

# render to gif 
named_brau_gif_output <- animate(named_brau_gif_output,
                           height = height_dim,
                           width = width_dim, 
                        renderer = gifski_renderer(loop = TRUE))

# save gif
anim_save(animation = named_brau_gif_output, 
          filename = "Benchmarking.gif")

# DPL Edition -------------------------------------------------------------

# create basic plot with animation
# Dark version with blue background
dpl_brau_gif_output <- brau_gif_data |> 
    ggplot(aes(x = location, y = value,
               fill = Group, group = location)) +
    geom_col(color = "white",
             alpha = 0.8) +
    geom_line(aes(y = avg_line,
                  group = Group),
              color = 'white',
              linewidth = 1.5)+
    scale_fill_brewer(palette = "Greys",
                      direction = 1)+
    theme_classic()+
    theme(legend.position = "none")+
    theme(axis.line = element_line(color = NA),
          axis.ticks = element_line(color = NA))+
    theme(plot.background = element_rect(fill = "#005abb"))+
    theme(panel.background = element_rect(fill = "#005abb"))+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(labels = NULL)+
    scale_y_continuous(labels = NULL,
                       limits = c(0, 100))+
    labs(title = NULL)+
    ggtext::geom_richtext(x = 8.7, y = 99,
                          label = "**Benchmarking and Reform Analysis Unit**",
                          label.color = NA,
                          color = 'white',
                          fill = NA,
                          size = 14)+
    transition_reveal(time)

# render to gif 
dpl_brau_gif_output <- animate(dpl_brau_gif_output,
                           height = height_dim,
                           width = width_dim, 
                           renderer = gifski_renderer(loop = TRUE))

# save gif
anim_save(animation = dpl_brau_gif_output, 
          filename = "~/Martin Ingram R Work/Test Area/BRAU gif/Benchmarking_DPL.gif")