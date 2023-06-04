library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(viridis)

# The data
hdc <- data.frame(read.delim("re4b_commits.csv", sep=";", header = TRUE))

# Nous filtrons ici les auteurs avec au moins 48 commits, et cherchons
# We keep only authors with 48+ commits, and search how they split
# during the day.
# c hold the maximum for each user, to have a separate scale.
#
hdc2 <-hdc %>% group_by(Author) %>%
  mutate(nb=n()) %>%
  filter(nb>=48) %>%
  group_by(Author,Hour) %>%
  mutate(c=n()) %>%
  group_by(Author) %>%
  mutate(p=max(c))

update_geom_defaults("text", list(colour = "black", alpha=0.3, family = theme_get()$text$family))

our_labs_title_fr <- "Répartitions des commits dans la journée par contributeur, pour ceux en ayant au moins 48, entre 2013 et 2020."
our_labs_title_en <- "Daily commits distribution by contributor, for those having at least 48, between 2013 and 2020."
our_palette <- "Spectral"
our_labs_y_fr <- "Nombre de commits."
our_labs_y_en <- "Number of commits"

our_labs_y <- our_labs_y_en
our_labs_title <- our_labs_title_en

# Display each user, with own scale based on his maximum,
# to have 100% for each user. The absolute number is based
# on the color.
#
g11 <- hdc2 %>%
  ungroup() %>%
  group_by(Author) %>% ggplot( aes(x=Hour ) ) +
  geom_rect( aes( fill=c, xmin=Hour, xmax=Hour+1, ymin=0, ymax=100*c/p ) ) +
  scale_x_continuous( breaks=0:23 ) +
  labs( title=our_labs_title, x=NULL, y=NULL ) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        panel.border = element_blank()
        ) +
  scale_fill_distiller( palette = our_palette, name = our_labs_y ) +
  coord_polar(start=0) +
  facet_wrap(. ~ Author, ncol=4) +
    geom_text( aes(label=c(nb), x=0, y=-50 ))

# Save into a PNG file
#
img_file_name <- "img_ch1.png"
img_width <- 840
img_height <- 630
png( filename=img_file_name, width=img_width, height=img_height)
g11
dev.off()

