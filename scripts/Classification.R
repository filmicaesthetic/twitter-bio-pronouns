#:: load packages
pacman::p_load(dplyr, stringr, tidyr, arrow)

#:: import dataset
tweet_list <- read_parquet("../Tweet Scraping/data/tweet_list_v2.parquet")

#:: select bio text and user id, only keep unique profiles
gender_profiles <- tweet_list %>%
  select(author_id, users.description, users.name) %>%
  mutate(users.description = tolower(users.description),
         users.name = tolower(users.name)) %>%
  mutate(users.description = gsub(" \\/ |\\/ | \\/", "", users.description)) %>% # remove spaces around forward slashes
  distinct()

## define pronoun groups
gc_trad_her = c("she", "her", "hers", "herself", "ela", "ella", "dela", "ihr")
gc_trad_him = c("he", "him", "his", "himself", "ele", "dele", "er", "ihn")
gnc_trad_them = c("they", "them", "their", "themself", "elu", "delu", "iel")
gnc_trad_non.them = c("it", "its", "itself", "thou", "thee", "thy")
gnc_neo_xem = c("xe", "xey", "xem", "xemself")
gnc_neo_ver = c("ve", "ver", "vem")
gnc_neo_faer = c("fae", "faer", "ae", "aer")
gnc_neo_em = c("ey", "em", "eir", "e", "emself")
gnc_neo_ze = c("ze", "hir", "zir", "zim", "zis", "shi", "hirs")

# create data frames for each pronoun group
# she/her...
gc_trad_her <- data.frame(ind_pronoun = c(gc_trad_her),
                          core_pronoun = "gc_trad_her") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# he/him...
gc_trad_him <- data.frame(ind_pronoun = c(gc_trad_him),
                          core_pronoun = "gc_trad_him") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# they/them...
gnc_trad_them <- data.frame(ind_pronoun = c(gnc_trad_them),
                            core_pronoun = "gnc_trad_them") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# it/its...
gnc_trad_non.them <- data.frame(ind_pronoun = c(gnc_trad_non.them),
                                core_pronoun = "gnc_trad_non.them") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# xe/xem...
gnc_neo_xem <- data.frame(ind_pronoun = c(gnc_neo_xem),
                          core_pronoun = "gnc_neo_xem") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# ve/ver...
gnc_neo_ver <- data.frame(ind_pronoun = c(gnc_neo_ver),
                          core_pronoun = "gnc_neo_ver") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# fae/faer...
gnc_neo_faer <- data.frame(ind_pronoun = c(gnc_neo_faer),
                           core_pronoun = "gnc_neo_faer") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# e/ey/em...
gnc_neo_em <- data.frame(ind_pronoun = c(gnc_neo_em),
                         core_pronoun = "gnc_neo_em") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))
# ze/zir/hir...
gnc_neo_ze <- data.frame(ind_pronoun = c(gnc_neo_ze),
                         core_pronoun = "gnc_neo_ze") %>%
  mutate(regex_chk = paste0("(?<![a-z0-9])",as.character(ind_pronoun),"(?=\\/)|(?<=\\/)",as.character(ind_pronoun),"(?![a-z0-9])"))


# combine all dataframes into one group
core_pronouns <- rbind(gc_trad_her, gc_trad_him, 
                       gnc_trad_them, gnc_trad_non.them,
                       gnc_neo_em, gnc_neo_faer, gnc_neo_ver, gnc_neo_xem, gnc_neo_ze)

# count occurrences of pronouns in bio and username by pronoun group
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

# remove any with just 1 pronoun detected, as not likely to be used in the context of defining pronouns
gender_breakdown_2 <- pronoun_cols %>%
  filter(all > 1)

# classify into core groups by values in each group
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

write_parquet(gender_classification_2, "data/gender_classification.parquet")
write_parquet(pronoun_cols, "data/pronoun_cols.parquet")
