
pacman::p_load(ggplot2, treemapify, showtext, waffle, dplyr, plotly, ggpomological, magick, showtext, forcats, stringr)


## full core pronoun list

gc_trad_her = c("she", "her", "hers", "herself", "ela", "ella", "dela", "ihr")
gc_trad_him = c("he", "him", "his", "himself", "ele", "dele", "er", "ihn")
gnc_trad_them = c("they", "them", "their", "themself", "elu", "delu")
gnc_trad_non.them = c("it", "its", "itself", "thou", "thee", "thy")
gnc_neo_xem = c("xe", "xey", "xem", "xemself")
gnc_neo_ver = c("ve", "ver", "vem")
gnc_neo_faer = c("fae", "faer", "ae", "aer")
gnc_neo_em = c("ey", "em", "eir", "e", "emself")
gnc_neo_ze = c("ze", "hir", "zir", "zim", "zis")

gc_trad_her <- data.frame(ind_pronoun = c(gc_trad_her),
                          core_pronoun = "gc_trad_her") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gc_trad_him <- data.frame(ind_pronoun = c(gc_trad_him),
                          core_pronoun = "gc_trad_him") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_trad_them <- data.frame(ind_pronoun = c(gnc_trad_them),
                          core_pronoun = "gnc_trad_them") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_trad_non.them <- data.frame(ind_pronoun = c(gnc_trad_non.them),
                          core_pronoun = "gnc_trad_non.them") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_neo_xem <- data.frame(ind_pronoun = c(gnc_neo_xem),
                          core_pronoun = "gnc_neo_xem") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_neo_ver <- data.frame(ind_pronoun = c(gnc_neo_ver),
                          core_pronoun = "gnc_neo_ver") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_neo_faer <- data.frame(ind_pronoun = c(gnc_neo_faer),
                          core_pronoun = "gnc_neo_faer") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_neo_em <- data.frame(ind_pronoun = c(gnc_neo_em),
                          core_pronoun = "gnc_neo_em") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
gnc_neo_ze <- data.frame(ind_pronoun = c(gnc_neo_ze),
                          core_pronoun = "gnc_neo_ze") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))

core_pronouns <- rbind(gc_trad_her, gc_trad_him, 
                       gnc_trad_them, gnc_trad_non.them,
                       gnc_neo_em, gnc_neo_faer, gnc_neo_ver, gnc_neo_xem, gnc_neo_ze)

gender_profiles %>%
  mutate(users.all = paste(users.description, users.name),
         author_id = as.character(author_id)) %>%
  mutate(check = str_count(users.all, "(?<![a-z0-9])ae(?=\\/)|(?<=\\/)ae(?![a-z0-9])")) %>%
  filter(check > 0) %>%
  select(users.all)

pronoun_cols <- gender_profiles %>%
  mutate(users.all = paste(users.description, users.name),
         author_id = as.character(author_id)) %>%
  mutate(gc_trad_her = str_count(users.all, paste(c(gc_trad_her$regex_chk),collapse="|")),
         gc_trad_him = str_count(users.all, paste(c(gc_trad_him$regex_chk),collapse="|")),
         gnc_trad_them = str_count(users.all, paste(c(gnc_trad_them$regex_chk),collapse="|")),
         gnc_trad_non.them = str_count(users.all, paste(c(gnc_trad_non.them$regex_chk),collapse="|")),
         gnc_neo_em = str_count(users.all, paste(c(gnc_neo_em$regex_chk),collapse="|")),
         gnc_neo_xem = str_count(users.all, paste(c(gnc_neo_xem$regex_chk),collapse="|")),
         gnc_neo_ver = str_count(users.all, paste(c(gnc_neo_ver$regex_chk),collapse="|")),
         gnc_neo_faer = str_count(users.all, paste(c(gnc_neo_faer$regex_chk),collapse="|")),
         gnc_neo_ze = str_count(users.all, paste(c(gnc_neo_ze$regex_chk),collapse="|"))
           ) %>%
  mutate(gc_all = rowSums(select(., starts_with("gc_"))),
         gnc_all = rowSums(select(., starts_with("gnc_"))),
         trad_all = rowSums(select(., contains("_trad_"))),
         neo_all = rowSums(select(., contains("_neo_"))),
         all = rowSums(select(., where(is.numeric)))
  )

gender_breakdown_2 <- pronoun_cols %>%
  filter(all > 1)

gender_classification_2 <- gender_breakdown_2 %>%
  mutate(class = ifelse(gc_trad_her > 0 & gc_trad_her == all, "Feminine pronouns only",
                        ifelse(gc_trad_him > 0 & gc_trad_him == all, "Masculine pronouns only",
                               "Gender non-conforming pronouns")),
         gnc_class = ifelse(class == "Gender non-conforming pronouns",
                            ifelse(gnc_trad_them + gnc_trad_non.them == all, "Traditional neutral pronouns only",
                            ifelse(gnc_trad_them + gnc_trad_non.them + gc_trad_her == all, "Feminine and traditional neutral combo",
                                   ifelse(gnc_trad_them + gnc_trad_non.them + gc_trad_him == all, "Masculine and traditional neutral combo",
                                          ifelse(neo_all == all, "Neopronouns only",
                                                 ifelse(gc_trad_her + gc_trad_him == all, "Masculine and feminine only",
                                                        ifelse(trad_all == all, "All traditional pronouns",
                                                               "Neo and traditional combo")))))), class),
         count = 1)

class_summary <- gender_classification_2 %>%
  group_by(class, gnc_class) %>%
  summarise(count = n() / 10)

## can i group the legend by GNC / GC??

pal <- c("Masculine pronouns only" = "#e8b139",
         "Feminine pronouns only" = "#c06ee6",
         "Traditional neutral pronouns only" = "#6cce96",
         "Masculine and feminine only" = "#af4c33",
         "Feminine and traditional neutral combo" = "#628f22",
         "Masculine and traditional neutral combo" = "#515987",
         "All traditional pronouns" = "#4a3d1e",
         "Neo and traditional combo" = "#038058",
         "Neopronouns only" = "#024D35")

# Waffle chart
class_summary %>% 
  ggplot() +
  geom_waffle(aes(fill = gnc_class, values = count), color = "#fffeea", size = .25, n_rows = 10) +
  facet_wrap(~class, strip.position = "top", nrow = 3, labeller = labeller(groupwrap = label_wrap_gen(6))) +
  # annotate(geom = "segment", x = 1, xend = 2, y = 1, yend = 4) +
  scale_fill_manual(values=pal) +
  coord_equal() +
  theme_pomological() +
  labs(fill = "", title = "PRONOUN USE BY GENDER TYPE") +
  theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 24),
        axis.text = element_blank(),
        legend.position = "left",
        strip.background = element_blank(),
        plot.title = element_text(color = "#1d1d1d", hjust = 0.5),
        legend.text = element_text(size = 12),
        strip.text = element_text(color = "#54544d", hjust = 0),
        panel.border = element_blank())

neopronouns_df <- gender_classification_2 %>%
  select(author_id, class, gnc_class, contains("_neo_")) %>%
  pivot_longer(cols = contains("_neo_"), names_to = "neopronoun", values_to = "count") %>%
  group_by(neopronoun, gnc_class) %>%
  summarise(count = sum(count)) %>%
  filter(count > 0)

# Waffle chart
neo_plot <- neopronouns_df %>%
  ggplot() +
  geom_waffle(aes(fill = gnc_class, values = count), color = "#fffeea", size = .25, n_rows = 2) +
  facet_wrap(~neopronoun, strip.position = "top", nrow = 7, labeller = labeller(groupwrap = label_wrap_gen(6))) +
  #scale_fill_manual(values=neo_pal) +
  coord_equal() +
  theme_pomological() +
  labs(fill = "", title = "NEO-PRONOUNS") +
  theme(text = element_text(family = "Comfortaa", color = "#54544d", size = 24),
        axis.text = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        plot.title = element_text(color = "#1d1d1d", hjust = 0.5),
        strip.text = element_text(color = "#54544d", hjust = 0),
        panel.border = element_blank())

ggplotly(neo_plot)
