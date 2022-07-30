
pacman::p_load(ggplot2, treemapify, showtext, waffle, arrow, dplyr, ggpomological, magick, showtext, forcats, stringr, plotly)

font_add_google("Comfortaa", "Comfortaa")
showtext_auto()
# count of each individual pronoun set

#############
#::
#:: Pronouns % of total
#::
#############

pronouns_cols <- read_parquet("data/pronoun_cols.parquet")

pron_df <- pronoun_cols %>%
  mutate(pronoun_check = ifelse(all == 0, "No pronouns identified", "Pronouns identified"),
         pronoun_check = factor(pronoun_check, levels = c("Pronouns identified", "No pronouns identified"))) %>%
  group_by(pronoun_check) %>%
  summarise(count = n())

write.csv(pron_df, "data/plot_data/pronoun_df.csv", row.names = FALSE)

pron_df %>%
  ggplot() +
  geom_waffle(aes(fill = pronoun_check, values = count), color = "#fffeea", size = .25, n_rows = 5, make_proportional = TRUE) +
  scale_fill_manual(values=c("No pronouns identified" = "#d4d4d4", "Pronouns identified" = "#4a3d1e")) +
  coord_equal() +
  labs(fill = "PRONOUNS\nIN PROFILES ") +
  theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 36),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
        panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
        legend.position = "left",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.justification = c("left", "top"),
        legend.title = element_text(face = "bold", lineheight = 0.5),
        legend.text = element_text(size = 18),
        strip.text = element_text(color = "#54544d", hjust = 0),
        panel.border = element_blank())
  

#############
#::
#:: Prevalance of masculine, feminine and gender non-conforming pronouns
#::
#############

gender_classification_2 <- read_parquet("data/gender_classification.parquet")

class_summary <- gender_classification_2 %>%
  group_by(class, gnc_class) %>%
  summarise(count = n() / 10)

## can i group the legend by GNC / GC??

pal <- c("Masculine pronouns only" = "#e8b139",
         "Feminine pronouns only" = "#c06ee6",
         "Traditional neutral pronouns only" = "#6cce96",
         "Masculine and feminine only" = "#c96363",
         "Feminine and traditional neutral combo" = "#5a9c40",
         "Masculine and traditional neutral combo" = "#745d94",
         "All traditional pronouns" = "#756764",
         "Neo and traditional combo" = "#038058",
         "Neopronouns only" = "#046e4c")

write.csv(class_summary, "data/plot_data/class_summary.csv", row.names = FALSE)

# Waffle chart
class_summary %>% 
  ggplot() +
  geom_waffle(aes(fill = gnc_class, values = count), color = "#fffeea", size = .25, n_rows = 10) +
  facet_wrap(~class, strip.position = "top", nrow = 3, labeller = labeller(groupwrap = label_wrap_gen(6))) +
  # annotate(geom = "segment", x = 1, xend = 2, y = 1, yend = 4) +
  scale_fill_manual(values=pal) +
  coord_equal() +
  theme_pomological() +
  labs(fill = "PRONOUN USE\nBY GENDER TYPE\n") +
  theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 24),
        axis.text = element_blank(),
        legend.position = "left",
        strip.background = element_blank(),
        plot.title = element_text(color = "#1d1d1d", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 12),
        strip.text = element_text(color = "#54544d", hjust = 0),
        panel.border = element_blank())

#############
#::
#:: Use of Neopronouns
#::
#############

neopronouns_df <- gender_classification_2 %>%
  select(author_id, class, gnc_class, contains("_neo_")) %>%
  pivot_longer(cols = contains("_neo_"), names_to = "neopronoun", values_to = "count") %>%
  group_by(neopronoun, gnc_class) %>%
  summarise(count = sum(count)) %>%
  filter(count > 0)

neopronoun_lkp <- c("gnc_neo_em" = "e/ey/em",
                    "gnc_neo_faer" = "fae/ae/faer",
                    "gnc_neo_ver" = "ve/ver",
                    "gnc_neo_xem" = "xe/xem",
                    "gnc_neo_ze" = "ze/shi/hir")

neo_plot_df <- neopronouns_df %>%
  mutate(neopronoun = recode(neopronoun, !!!neopronoun_lkp)) %>%
  mutate(neopronoun = fct_reorder(neopronoun, count, sum))

neo_pal <- pal[names(pal) %in% c("Neopronouns only", "Neo and traditional combo")]

write.csv(data.frame(neo_plot_df), "data/plot_data/neo_plot_df.csv", row.names = FALSE)

# Neopronouns waffle chart
data.frame(neo_plot_df) %>%
  mutate(neopronoun = fct_reorder(neopronoun, -count, sum)) %>%
  ggplot() +
  geom_waffle(aes(fill = gnc_class, values = count), color = "#fffeea", size = .25, n_rows = 2) +
  facet_wrap(~neopronoun, strip.position = "top", nrow = 7, labeller = labeller(groupwrap = label_wrap_gen(6))) +
  scale_fill_manual(values=neo_pal) +
  coord_equal() +
  theme_pomological() +
  labs(fill = "USE OF\nNEOPRONOUNS\n") +
  theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 24),
        axis.text = element_blank(),
        legend.position = "left",
        strip.background = element_blank(),
        plot.title = element_text(color = "#1d1d1d", hjust = 0.5),
        legend.justification = c("left", "top"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(color = "#54544d", hjust = 0),
        panel.border = element_blank())


#############
#::
#:: Individual GNC pronouns plotly
#::
#############

ind_pron_list = c()

for (i in 1:nrow(gender_classification_2)) {
  it <- ""
  
  for (j in 1:nrow(core_pronouns)) {
    ind <- str_extract(gender_classification_2$users.all[i], core_pronouns$regex_chk[j])
    if (is.na(ind) == FALSE) {
      if (nchar(it) == 0) {
        it <- paste(it, ind, sep = "")
      } else {
        it <- paste(it, ind, sep = "/")
      }
      
    } 
  }
  ind_pron_list = c(ind_pron_list, it)
}

ind_gnc_df <- gender_classification_2 %>%
  select(class, gnc_class) %>%
  mutate(gnc_class = as.factor(gnc_class))

ind_gnc_df$pronouns <- ind_pron_list

# format for constructed waffle plot
ind_gnc_df <- ind_gnc_df %>%
  filter(class == "Gender non-conforming pronouns") %>%
  arrange(gnc_class)

ind_gnc_df$col_id <- rep(seq(1:106), each = 29)
ind_gnc_df$count <- 1

# ind_plot <- ind_gnc_df %>%
#   ggplot(aes(x = col_id, y = count, text = paste(pronouns))) +
#   geom_col(aes(fill = gnc_class, group = pronouns), width = 1, color = "#ffffff", size = 0.1) +
#   scale_fill_manual(values = pal) +
#   coord_fixed() +
#   theme_minimal() +
#   theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 36),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
#         panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
#         legend.position = "none",
#         plot.caption = element_text(size = 10),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank(),
#         legend.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
#         strip.background = element_blank(),
#         plot.title = element_text(color = "#1d1d1d", hjust = 0.5),
#         legend.title = element_text(face = "bold", lineheight = 0.5),
#         legend.justification = c("left", "top"),
#         legend.text = element_text(size = 18),
#         strip.text = element_text(color = "#54544d", hjust = 0),
#         panel.border = element_blank())
# 
# ggplotly(ind_plot, tooltip = "text")


ind_gnc_df <- ind_gnc_df %>%
  mutate(gnc_copy = as.character(gnc_class),
         shortName = pronouns,
         label = gnc_copy,
         gnc_class = paste(class, gnc_class, sep="."),
         pronouns = paste(class, gnc_class, pronouns, sep="."))

label_lkp <- c("All traditional pronouns" = "ALL",
  "Feminine and traditional neutral combo" = "N+F",
  "Masculine and feminine only" = "F+M",
  "Masculine and traditional neutral combo" = "N+M",
  "Neopronouns only" = "NEO",
  "Traditional neutral pronouns only" = "N",
  "Neo and traditional combo" = "NEO+")

ind_gnc_df <- ind_gnc_df %>%
  mutate(label = recode(label, !!!label_lkp))

top_level <- ind_gnc_df %>%
  select(class, gnc_class) %>%
  rename(from = class, to = gnc_class) %>%
  unique()

bottom_level <- ind_gnc_df %>%
  select(gnc_class, pronouns) %>%
  rename(from = gnc_class, to = pronouns) %>%
  unique()

edges <- rbind(top_level, bottom_level)

vertices_count <- ind_gnc_df %>%
  group_by(pronouns) %>%
  summarise(count = sum(count),
            gnc_class = first(gnc_copy),
            shortName = first(shortName)) %>%
  rename(name = pronouns)

vertices_label <- ind_gnc_df %>%
  group_by(gnc_class) %>%
  summarise(label = first(label)) %>%
  rename(name = gnc_class)

vertices <- data.frame(name = c(as.character(edges$to), edges$from)) %>%
  unique() %>%
  left_join(vertices_count, by = "name") %>%
  left_join(vertices_label, by = "name") %>%
  mutate(count = ifelse(is.na(count), 0, count),
         gnc_class = ifelse(is.na(gnc_class), "main", gnc_class))

write.csv(vertices, "data/plot_data/vert.csv", row.names = FALSE)
write.csv(edges,"data/plot_data/edges.csv", row.names = FALSE)
