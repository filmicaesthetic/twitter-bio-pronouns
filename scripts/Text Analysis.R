
pacman::p_load(tidytext, textstem, stopwords, openNLP, NLP, arrow, caret)

gender_classification_2 <- read_parquet("data/gender_classification.parquet")

text_sample <- gender_classification_2 %>%
  mutate(class = as.factor(class)) %>%
  sample_n(3000)

bio_words_bigram <-
  text_sample %>% 
  mutate(users.description = gsub("\\<u.*\\>", "", gsub("^[a-zA-Z0-9]", "", users.description))) %>%
  select(class, gnc_class, users.description) %>%
  mutate(id = seq(1:n())) %>%
  unnest_ngrams(bigram, users.description, n=2) %>% # unnest groups of 3 words
  separate(bigram, into = c("word_1", "word_2"), sep = " ") %>%
  mutate(word_2 = lemmatize_strings(word_2))

negative_words <- c("no", "not", "none", "nobody", "nothing", "neither",
                    "nowhere", "never", "hardly", "scarcely", "barely",
                    "doesn't", "isn't", "wasn't", "shouldn't", "wouldn't",
                    "couldn't", "won't", "can't", "don't", "without")

custom_stopwords <- c(data_stopwords_nltk$en, "https", "t. co")

bio_words <-
  bio_words_bigram %>%
  mutate(is_preceding_negative = as.numeric((word_1 %in% negative_words))) %>%
  filter(!(word_2 %in% custom_stopwords)) %>%
  select(-word_1) %>%
  rename(word = word_2) %>%
  mutate(word = ifelse(is_preceding_negative == 1, paste("NEG_",word), word)) %>%
  filter(!is.na(word))

freq_dist <- bio_words %>%
  group_by(class, gnc_class, word) %>%
  summarise(n = n()) %>%
  arrange(-n)

freq_dist %>%
  filter(gnc_class == "Traditional neutral pronouns only") %>%
  mutate(count = 1,
         perc = n / 3000) %>%
  filter(word == "autistic")

no_pronoun_bigram <- gender_profiles[sample(nrow(gender_profiles), 20000), ] %>%
  filter(!(author_id %in% gender_classification_2$author_id)) %>%
  filter(grepl(" my | i'm | mine | i've ", users.description)) %>%
  mutate(users.description = gsub("\\<u.*\\>", "", gsub("^[a-zA-Z0-9]", "", users.description))) %>%
  select(users.description) %>%
  mutate(gnc_class = "No pronouns provided",
         class = "No pronouns provided") %>%
  mutate(id = seq(1:n())) %>%
  unnest_ngrams(bigram, users.description, n=2) %>% # unnest groups of 3 words
  separate(bigram, into = c("word_1", "word_2"), sep = " ") %>%
  mutate(word_2 = lemmatize_strings(word_2))

no_pronoun_words <-
  no_pronoun_bigram %>%
  mutate(is_preceding_negative = as.numeric((word_1 %in% negative_words))) %>%
  filter(!(word_2 %in% custom_stopwords)) %>%
  select(-word_1) %>%
  rename(word = word_2) %>%
  mutate(word = ifelse(is_preceding_negative == 1, paste("NEG_",word), word)) %>%
  filter(!is.na(word))

no_pronoun_freq_dist <- no_pronoun_words %>%
  group_by(class, gnc_class, word) %>%
  summarise(n = n()) %>%
  arrange(-n)

no_pron_autism_adhd <- no_pronoun_freq_dist %>%
  mutate(count = 1,
         perc = n / 3000) #%>%
  filter(word %in% c("autistic", "autism", "adhd", "neurodivergent", "dyslexic", "actuallyautistic", "adhder"))

no_pron_autism_adhd <- no_pron_autism_adhd[sample(nrow(no_pron_autism_adhd), 3000), ]

pron_autism_adhd <- freq_dist %>%
  #filter(gnc_class == "Traditional neutral pronouns only") %>%
  group_by(class) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(perc = n / 3000) %>%
  # filter(word %in% c("autistic", "autism", "adhd", "neurodivergent", "dyslexic", "actuallyautistic", "adhder")) %>%
  arrange(-perc)

word_search <- rbind(no_pron_autism_adhd, pron_autism_adhd)

write.csv(word_search, "data/word_search.csv", row.names = FALSE)

top_10_by_group <- word_search %>%
  group_by(class, word) %>%
  summarise(n = sum(n)) %>%
  arrange(-n) %>%
  group_by(class) %>%
  mutate(word_rank = 1:n()) %>%
  filter(word_rank <= 20)

write.csv(top_10_by_group, "data/top_10_words_by_group.csv", row.names = FALSE)

View(bio_words %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(-n))

search_pal <- c("Masculine pronouns only" = "#e8b139",
         "Feminine pronouns only" = "#c06ee6",
         "Gender non-conforming pronouns" = "#6cce96",
         "No pronouns provided" = "grey")

data.frame(word_search) %>%
  filter(word %in% c("thon")) %>%
  mutate(class = fct_reorder(as.factor(class), perc, sum)) %>%
  ggplot(aes(x = class, y = perc)) +
  geom_col(aes(fill = class)) +
  scale_fill_manual(values = search_pal) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none") +
  facet_wrap(~word)

bio_numbers <- word_search %>%
  mutate(word = round(as.numeric(word),0)) %>%
  group_by(class, word) %>%
  summarise(n = sum(perc)) %>%
  filter(!(is.na(word))) %>%
  filter(word < 80, word >= 18) %>%
  arrange(class, word)

write.csv(bio_numbers, "data/bio_numbers.csv", row.names = FALSE)

bio_numbers %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(aes(fill = class)) +
  geom_smooth(method = "lm", color = "#1d1d1d") +
  scale_fill_manual(values = search_pal) +
  facet_wrap(~class, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "none")

# LGBTQIA words

## gender
gender_words <- c("agender", "bigender", "demigender", "genderfluid", "genderqueer",
                  "neurogender", "neutrois", "nonbinary", "pangender", "queer", "xenogender")

sexuality_words <- c("abrosexual", "achillean", "allosexual", "asexual", "bisexual", "ceterosexual", "demisexual",
                     "gay", "greysexual", "heterosexual", "lesbian", "omnisexual", "pansexual", "polysexual", "queer",
                     "sapphic")

romantic_words <- c("abroromantic", "alloromantic", "aromantic", "biromantic", "demiromantic",
                    "greyromantic", "heteroromantic", "homoromantic", "panromantic", "polyromantic")  

abbrev_words <- c("pan", "poly", "allo", "aro", "abro", "demi", "xeno", "hetero", "ace")

word_search %>%
  filter(word %in% abbrev_words) %>%
  group_by(class, word) %>%
  summarise(n = sum(perc)) %>%
  ggplot(aes(x = class, y = n)) +
  geom_col(aes(fill = class)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = search_pal) +
  coord_polar() +
  facet_wrap(~word)


autism_class <- data.frame(word_search) %>%
  select(class, word) %>%
  mutate(class = as.factor(class),
         word = word %in% c("autistic", "autism", "asd", "asc"))

autism_class <- within(autism_class, class <- relevel(class, ref = 4))

autism_model <- lm(word ~ class, autism_class)
summary(autism_model)

saveRDS(autism_model, file = "data/autism_model.rda")



