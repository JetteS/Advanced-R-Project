library(tidyverse)
library(gganimate)

theme_set(theme_bw())

sysfonts::font_add("AUPassata_Rg", regular = "U:/WCPG2022_Oral_Presentation/fonte/AUPassata_Rg.ttf")
sysfonts::font_add("AUPassata_Light", regular = "U:/WCPG2022_Oral_Presentation/fonte/AUPassata_Light.ttf")
sysfonts::font_add("AUPassata_Bold", regular = "U:/WCPG2022_Oral_Presentation/fonte/AUPassata_Bold.ttf")
sysfonts::font_add("AU_Peto", regular = "U:/WCPG2022_Oral_Presentation/fonte/AU_Peto.ttf")
sysfonts::font_add("AULogoBold", regular = "U:/WCPG2022_Oral_Presentation/fonte/AULogoBold.ttf")
sysfonts::font_add("AULogoReg", regular = "U:/WCPG2022_Oral_Presentation/fonte/AULogoReg.ttf")

showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames

# tuesdata <- tidytuesdayR::tt_load('2022-01-18')
# chocolate <- tuesdata$chocolate

rm(tuesdata)

colnames(babynames)
hist(babynames$year)
range(babynames$year)
table(babynames$sex)


babynames <- babynames %>%  
  mutate(., init_letter = substr(name,1,1))

babynames %>% group_by(init_letter) %>% 
  summarise(., n_total = sum(n)) %>% 
  ggplot(., aes(x = init_letter, y = n_total/1e6)) + 
  geom_bar(stat = "identity", width = .5) + 
  labs(title = "Initial letters", 
       caption = "source: tidytuesday - Baby names") + 
  theme(axis.line.x       = element_line(color = 'black'),
        axis.text = element_text(size = 18, family = "AUPassata_Rg"),
        plot.title = element_text(size = 25, hjust = 0.5, family = "AUPassata_Bold"),
        text = element_text(size = 20, family = "AUPassata_Rg")) +
  xlab("Initial letter (1880-2017)") +
  ylab("Total number (in millions)") +
  theme_minimal()

babynames %>% 
  group_by(., year, sex) %>% 
  arrange(., year, sex, desc(prop)) %>% 
  slice(., 1:5) %>% 
  ggplot(., aes(sex, fill = name)) + 
  geom_bar() +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

pop_babynames <- babynames %>% 
  group_by(., year, sex) %>% 
  arrange(., year, sex, desc(prop)) %>% 
  slice(., 1:3) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(., id = row_number())

F_names <- filter(pop_babynames, sex != "M") %>% 
  pull(., name) %>% 
  unique() %>% 
  as_tibble_col() %>% 
  mutate(., col = paletteer::paletteer_c("ggthemes::Classic Red-White-Black Light", n()))


M_names <- filter(pop_babynames, sex == "M") %>% 
  pull(., name) %>% 
  unique() %>% 
  as_tibble_col() %>% 
  mutate(., col = paletteer::paletteer_c("ggthemes::Blue", n()))
  
name_colour <- bind_rows(F_names, M_names) 

pop_babynames <- pop_babynames %>% 
  left_join(.,name_colour, by = c("name" = "value"))

# Get the name and the y position of each label
label_data <- pop_babynames
number_of_bars <- 3
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bars     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# prepare a data frame for base lines
base_data <- pop_babynames %>% 
  summarize(start = min(id), end = max(id), .groups = "keep") %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(pop_babynames, aes(x = as.factor(id), y = prop, fill = name)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x = as.factor(id), y = prop, fill = name), stat = "identity", alpha = 0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data = grid_data, aes(x = end, y = 0.08, xend = start, yend = 0.08), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 0.06, xend = start, yend = 0.06), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 0.04, xend = start, yend = 0.04), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 0.02, xend = start, yend = 0.02), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(pop_babynames$id),4), y = c(0.02, 0.04, 0.06, 0.08), label = c("0.02", "0.04", "0.06", "0.08") , color = "grey", size = 3 , angle = 0, fontface = "bold", hjust = 1) +
  geom_bar(aes(x = as.factor(id), y = prop, fill = name), stat = "identity", alpha = 0.5) +
  # ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  transition_states(year, state_length = 5) +
  labs(title = "Year: {year}") +
  scale_fill_manual(values = name_colour$col, name = "Names",
                    labels = name_colour$value,
                    guide = guide_legend(reverse = TRUE))


filter(babynam)