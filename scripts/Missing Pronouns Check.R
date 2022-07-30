

#:: define filter patterns for each pronoun category
hehim_filter <- c("(?<!s)he\\/him", "(?<!s)he\\/his", "ele\\/dele", "him\\/he(?!r)", "er\\/ihn")
sheher_filter <- c("she\\/her", "sie\\/she", "her\\/hers", "ela\\/dela", "her\\/she", "she\\/ella", "ela\\/she")
theythem_filter <- c("they\\/them", "them\\/they", "they\\/it", "it\\/they", "xe\\/they")
she_they_filter <- c("she\\/they", "they\\/she", "she\\/it", "she\\/them", "them\\/she", "she\\/fae", "ela\\/elu")
he_they_filter <- c("(?<!s)he\\/they", "(?<!s)he\\/ze", "they\\/he(?!r)", "(?<!s)he\\/it", "it\\/he", "they\\/him", "him\\/them", "(?<!s)he\\/them")
she_he_filter <- c("he\\/she", "she\\/him", "him\\/her", "her\\/they", "she\\/he(?!r)", "(?<!s)he\\/her")
any_all_filter <- c("(?<!s)he\\/she\\/they", "any\\/all", "they\\/any", "any\\/any")
neo_filter <- c("e\\/em", "hie\\/hym", "ne\\/nem", "tey\\/tem",
                "xe\\/xem", "xe\\/xey", "xey\\/they", "ze\\/hir", "ve\\/ver", "zie\\/zim", "ey\\/em", "she\\/it",
                "fae\\/faer", "thou\\/thee", "thee\\/thou", "ey\\/they", "(?<!s)he\\/it", 
                "it\\/its", "it\\/that", "it\\/they", "thy\\/thou", "she\\/fae", "it\\/he", "they\\/it")

# combine all the separate filters into one
pronoun_filter <- c(hehim_filter, sheher_filter, 
                    theythem_filter, he_they_filter,
                    she_they_filter, she_he_filter,
                    any_all_filter, neo_filter)

## Checking for unidentified pronouns by looking for xx(xx)/xx(xx) patterns
chk <- gender_profiles %>%
  filter(grepl(" [a-z]{2,4}\\/[a-z]{2,4}", users.description)) %>%
  mutate(extract = str_extract(users.description, " [a-z]{2,4}\\/[a-z]{2,4}")) %>%
  select(users.description, extract)

chk_list <- chk %>%
  group_by(extract) %>%
  summarise(n = n()) %>%
  arrange(-n)

chk_x <- chk %>%
  filter(!(grepl(paste(pronoun_filter,collapse="|"), users.description, perl = T)))

chk_xx <- chk_x %>%
  group_by(extract) %>%
  summarise(n = n()) %>%
  arrange(-n)
