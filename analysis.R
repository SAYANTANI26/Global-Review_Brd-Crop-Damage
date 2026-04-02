rm(list = ls())

require(tidyverse)
require(readxl)

# Set path
setwd("~/Documents/RStudio/crop_damage_review/")

# Range of sampling
# temp2 <- read_excel("./Book1.xlsx", sheet = 3, col_names = TRUE)
temp2 <- read.delim2("./dataset.txt", sep= "\t", header = TRUE, row.names = 1)

temp2$Year <- as.numeric(temp2$Year)
temp2$Article.type <- as.factor(temp2$Article.type)
temp2$Birds <- as.factor(temp2$Birds)
temp2$Crop <- as.factor(temp2$Crop)
temp2$Scale <- as.factor(temp2$Scale)

p_bar <- temp2 %>% 
  mutate(Year = as.factor(as.character(Year))) %>% 
  group_by(Year, Scale) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(Scale = as.factor(str_replace_all(.$Scale, "^$", "Not specified"))) %>% 
  mutate(Scale = (str_replace_na(Scale, "Not specified"))) %>%
  mutate(Scale = factor(Scale, levels = c("Not specified", "Local", "Regional", "National"))) %>% 
  ggplot(aes(x = Year, y = n, fill = Scale)) +
  geom_bar(colour = "grey5", lwd = 0.15, width = 0.8, stat = "identity") +
  scale_fill_manual(values = c(`Local` = "grey70", `Regional` = "grey30", `National` = "grey10", `Not specified` = "grey99")) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  labs(x = "", y = "")

p_temporal <- temp2 %>% 
  select(key, Year, `Detecting.conflict..identification.`, `Resolving.conflict..deterrents.`, `Increasing.yeild`, `Farmer.s.perception`) %>% 
  gather(key = "Cat", value = "in_set", -key, -Year) %>% 
  filter(in_set == 1) %>% 
  mutate(Year = as.factor(as.character(Year)),
         Cat = as.factor(Cat)
         ) %>% 
  group_by(Year, Cat) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = Year, y = n, group = Cat, colour = Cat)) +
  geom_line(lwd = 0.5) +
  geom_point(shape = 3) +
  scale_colour_manual(values = c("tomato4", "cyan3", "green4", "darkmagenta")) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5), legend.position = "top") +
  labs(x = "", y = "")

p_temporal.iot <- temp2 %>% 
  select(key, Year, Article.type) %>% 
  # filter(IOT.used == 1) %>% 
  filter(Year %in% as.character(2015:2025)) %>% 
  mutate(Year = factor(as.character(Year), levels = c(as.character(2015:2025))),
         Article.type = as.factor(Article.type)
         ) %>% 
  group_by(Year, Article.type) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = Year, y = n, group = Article.type, fill = Article.type)) +
  geom_bar(colour = "grey5", lwd = 0.15, width = 0.8, stat = "identity", position = "stack") +
  scale_fill_manual(values = c(`Conference` = "grey70", `Research` = "grey10")) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5), legend.position = "top") +
  labs(x = "", y = "")

# Sankey plot
# temp2 %>%
#   filter(!is.na(Crop) & !is.na(Birds)) %>%
#   group_by(Country, Birds, Crop) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   data.frame(.) %>%
#   write.csv2(., "./sankey.connection_country.csv", quote = FALSE, row.names = FALSE)

# links <- read.csv2("./sankey.connection_corrected_manually.csv", header = TRUE)
links <- read.csv2("./sankey.connection_corrected_manually_v2.csv", header = TRUE, sep = "\t")
# links <- links[, !str_detect(colnames(links), "^X")] %>% 
#   mutate(Birds = str_replace_all(Birds, ",\\s+|\\s+and\\s+|and\\s+|\\s+and", ","),
#          Crop = str_replace_all(Crop, ",\\s+|\\s+and\\s+|and\\s+|\\s+and", ",")) %>% #.$Birds %>% unique()
#   separate(Birds, into = c(paste("x", 1:20, sep= "_")), fill = "right", sep = ",") %>% 
#   gather(key = "vals", value = "source", -Crop, -Country, -Continent, -n) %>% 
#   filter(!is.na(source) & source != "") %>%
#   select(-vals) %>% 
#   separate(Crop, into = c(paste("x", 1:20, sep= "_")), fill = "right", sep = ",") %>% 
#   gather(key = "vals", value = "target", -source, -Country, -Continent, -n) %>% 
#   filter(!is.na(target) & target != "") %>% 
#   select(-vals) %>% 
#   mutate(target = str_replace_all(tolower(target), "^$", "Not specified"),
#          source = str_replace_all(tolower(source), "^$", "Not specified")
#          ) %>% 
#   arrange(desc(n))

# Correcting the strings
bird_top_damage <- links %>% 
  mutate(source = ifelse(str_detect(source, "geese|goose|Goose"), "geese", source)) %>% 
  mutate(source = ifelse(str_detect(source, "crane"), "crane", source)) %>% 
  mutate(source = ifelse(str_detect(source, "crow|jackdaw|raven|Raven|corvid|rook"), "crow", source)) %>% 
  mutate(source = ifelse(str_detect(source, "sparrow"), "sparrow", source)) %>% 
  mutate(source = ifelse(str_detect(source, "blackbird"), "blackbird", source)) %>% 
  mutate(source = ifelse(str_detect(source, "pigeon|dove"), "pigeon", source)) %>% 
  mutate(source = ifelse(str_detect(source, "parakeet"), "parakeets", source)) %>% 
  mutate(source = ifelse(str_detect(source, "swan"), "swan", source)) %>% 
  mutate(source = ifelse(str_detect(source, "starling"), "startling", source)) %>% 
  mutate(source = ifelse(str_detect(source, "mallard|spot-billed ducks"), "mallard", source)) %>% 
  group_by(source) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% 
  data.frame(.)

crop_top_damage <- links %>% 
  mutate(target = tolower(target)) %>% 
  mutate(target = ifelse(str_detect(target, "grape"), "grape", target)) %>%
  mutate(target = ifelse(str_detect(target, "rapeseed"), "oilseed rape", target)) %>%
  mutate(target = ifelse(str_detect(target, "pea"), "pea", target)) %>%
  mutate(target = ifelse(str_detect(target, "cabbage"), "cabbage", target)) %>%
  mutate(target = ifelse(str_detect(target, "corn|maize"), "maize", target)) %>%
  # mutate(target = ifelse(str_detect(target, "pigeon|dove"), "pigeon", target)) %>% 
  # mutate(target = ifelse(str_detect(target, "parakeet"), "parakeets", target)) %>% 
  # mutate(target = ifelse(str_detect(target, "swan"), "swan", target)) %>% 
  # mutate(target = ifelse(str_detect(target, "starling"), "startling", target)) %>% 
  # mutate(target = ifelse(str_detect(target, "mallard|spot-billed ducks"), "mallard", target)) %>% 
  group_by(target) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% 
  data.frame(.)

p_bar_top_bird <- bird_top_damage %>% 
  mutate(source = factor(source, levels = rev(.$source))) %>% 
  filter(n>=10) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x = source, y = n)) +
  geom_bar(colour = "transparent", fill = "#7f9ba3", lwd = 0.15, width = 0.8, stat = "identity") +
  geom_text(aes(y = n+2, label = as.character(n)), size = 4, colour = "grey10", hjust = 0.5, vjust = 0.5) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(x = "", y = "") +
  coord_flip()

p_bar_all_bird <- bird_top_damage %>% 
  mutate(source = factor(source, levels = rev(.$source))) %>% 
  # filter(n>=10) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = source, y = n)) +
  geom_bar(colour = "transparent", fill = "#7f9ba3", lwd = 0.15, width = 0.8, stat = "identity") +
  geom_text(aes(y = n+2, label = as.character(n)), size = 4, colour = "grey10", hjust = 0.5, vjust = 0.5) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(x = "", y = "") +
  coord_flip()

p_bar_top_crop <- crop_top_damage %>% 
  mutate(target = factor(target, levels = rev(.$target))) %>% 
  filter(n>=10) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x = target, y = n)) +
  geom_bar(colour = "transparent", fill = "#6aa84f", lwd = 0.15, width = 0.8, stat = "identity") +
  geom_text(aes(y = n+2, label = as.character(n)), size = 4, colour = "grey10", hjust = 0.5, vjust = 0.5) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(x = "", y = "") +
  coord_flip()

p_bar_all_crop <- crop_top_damage %>% 
  mutate(target = factor(target, levels = rev(.$target))) %>% 
  # filter(n>=10) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = target, y = n)) +
  geom_bar(colour = "transparent", fill = "#6aa84f", lwd = 0.15, width = 0.8, stat = "identity") +
  geom_text(aes(y = n+2, label = as.character(n)), size = 4, colour = "grey10", hjust = 0.5, vjust = 0.5) +
  theme_classic() +
  # scale_x_continuous(aes(labels = as.character(Year), breaks = YEAR)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(x = "", y = "") +
  coord_flip()

write.csv2(bird_top_damage, "./bird_top_damage.csv", quote = FALSE, row.names = FALSE)
write.csv2(crop_top_damage, "./crop_top_damage.csv", quote = FALSE, row.names = FALSE)


links.1 <- links %>% 
  mutate(source = Continent, target = birds_category_short) %>% 
  group_by(source, target) %>% summarise(n = sum(n)) %>% 
  select(source, target, n)

links.2 <- links %>% 
  mutate(source = birds_category_short, target = crops_category) %>% 
  group_by(source, target) %>% summarise(n = sum(n)) %>% 
  select(source, target, n)
  

links.cont <- rbind.data.frame(links.1, links.2)

# write.csv2(links, "./sankey.connection_country_curated.csv", quote = FALSE, row.names = FALSE)
source.sort <- links.cont %>% group_by(source) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% .$source
target.sort <- links.2 %>% group_by(target) %>% summarise(n = sum(n)) %>% arrange(desc(n)) %>% .$target
  
df.an <- data.frame(id = c(source.sort, target.sort)) %>% 
  mutate(description = ifelse(id %in% unique(links$Continent), "A", ifelse(id %in% unique(links$birds_category_short), "B", "C"))
         ) %>% arrange(description)

# Making links as per SankeyNetwork functions
links.cont$IDsource <- match(links.cont$source, df.an$id)-1
links.cont$IDtarget <- match(links.cont$target, df.an$id)-1
links.cont$group <- df.an$description[match(links.cont$source, df.an$id)]
links.cont$group[which(links.cont$target %in% target.sort)] <- "C"
links.cont$group <- as.factor(links.cont$group)
df.an$group <- as.factor(df.an$description)
my_color <- 'd3.scaleOrdinal() .domain(["A", "B", "C"]) .range(["#cab5b0", "#7f9ba3", "#6aa84f"])'
links.cont %>% data.frame(.)
# Prepare network diagram
p5 <- networkD3::sankeyNetwork(Links = links.cont, 
                               Nodes = df.an,
                               Source = "IDsource",
                               Target = "IDtarget", 
                               Value = "n", 
                               NodeGroup = "group",
                               LinkGroup = "group",
                               sinksRight=TRUE,
                               NodeID = "id", 
                               fontSize = 8, 
                               nodeWidth = 10, 
                               iteration = 0, fontFamily = "Arial", 
                               colourScale = my_color, height = 1024, width = 1024)


sankey <- htmlwidgets::prependContent(p5, htmltools::tags$h1("Birds and crop damage"))

#range(["orangered","orangered","orangered","orangered","seagreen","seagreen","seagreen","seagreen","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","grey"])'
networkD3::saveNetwork(sankey, file = paste0("sankey_crop_damage.html"), selfcontained = FALSE)


require(patchwork)
require(ggiraph)
require(sf)

world_sf <- read_sf("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world.geojson")

country.df <- temp2 %>% 
  group_by(Country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  data.frame(.) %>% 
  mutate(Country = str_replace_all(Country, "^USA", "United States of America")) %>% 
  data.frame(.)

write.table(world_sf$name, "./world_name.txt", quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)
write.table(country.df, "./world_n_studies.txt", quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)

world_sf$n <- country.df$n[match(world_sf$name, country.df$Country)]
world_sf$n <- factor(str_replace_na(ifelse(world_sf$n >= 10, ">10", as.character(world_sf$n)), "0"), levels = c(as.character(0:9), ">10"))

col.pal <- c("#ffffff", "#f1eceb", "#e4dad7", "#d7c7c4", "#cab5b0", "#bda29d", "#af9089", "#a27d75", "#956a62", "#88584e", "#241511")
names(col.pal) <- c(as.character(0:9), ">10")


p_world <- ggplot() +
  geom_sf(data = world_sf, fill = "grey99", color = "grey30") +
  geom_sf_interactive(
    data = filter(world_sf, !is.na(n)),
    aes(fill = n, tooltip = name, data_id = name)
  ) +
  scale_fill_manual(values = col.pal) +
  # coord_sf(crs = st_crs(3857)) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top"
  )



ggsave(p_bar, file = "./barplot_temporal_study_all_study.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 7,
       height = 3)

ggsave(p_bar_top_bird, file = "./barplot_top_damage_bird.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 4,
       height = 7)

ggsave(p_bar_top_crop, file = "./barplot_top_damage_crop.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 4,
       height = 7)

ggsave(p_bar_all_bird, file = "./barplot_all_damage_bird.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 4,
       height = 9)

ggsave(p_bar_all_crop, file = "./barplot_all_damage_crop.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 4,
       height = 9)

ggsave(p_temporal, file = "./barplot_temporal_study.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 7,
       height = 3)

ggsave(p_temporal.iot, file = "./barplot_temporal_study_IOT.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 4,
       height = 3)

ggsave(p_world, file = "./world_heeatmap.pdf", 
       device = "pdf", 
       dpi = 300, 
       bg = "transparent", 
       units = "in",
       width = 6,
       height = 6)

# END
sessionInfo()

