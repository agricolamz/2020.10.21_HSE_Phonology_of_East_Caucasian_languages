setwd("/home/agricolamz/work/materials/2020.10.21_HSE_Phonology_of_East_Caucasian_languages/code")
library(tidyverse)
library(arules)
df <- read_tsv("../data/KiKo.csv")

df %>% 
  mutate(vowels = ifelse(features %in% c("front", "center", "back"), "vowel", "consonant")) %>% 
  distinct(language, segments_IPA, vowels) %>% 
  pivot_wider(names_from = vowels, values_from = vowels) %>% 
  mutate(vowels = ifelse(is.na(vowel), FALSE, TRUE)) %>% 
  select(segments_IPA, vowels) ->
  vowels

df %>% 
  distinct(language, segments_IPA) %>% 
  count(segments_IPA, sort = TRUE) %>% 
  filter(n == 23) ->
  present_in_all

df %>% 
#  anti_join(present_in_all) %>% 
  left_join(vowels) %>% 
  filter(vowels == TRUE) %>% 
  distinct(language, segments_IPA) %>% 
  as.data.frame() ->
  df_split
  
split(df_split[,"segments_IPA"], df_split[, "language"]) %>% 
  as("transactions") ->
  as_trans_vowels

inspect(as_trans_vowels[2])

vowel_rules <- apriori(as_trans_vowels, 
                 parameter = list(supp = 0.5,
                                  conf = 0.9, 
                                  minlen = 3,
                                  target = "frequent itemsets"))
inspect(vowel_rules) %>% 
  View()

df %>% 
  anti_join(present_in_all) %>% 
  left_join(vowels) %>% 
  filter(vowels == FALSE) %>% 
  distinct(language, segments_IPA) %>% 
  as.data.frame() ->
  df_split

split(df_split[,"segments_IPA"], df_split[, "language"]) %>% 
  as("transactions") ->
  as_trans_consonants

consonant_rules <- apriori(as_trans_consonants, 
                        parameter = list(supp = 0.5,
                                         conf = 0.9, 
                                         minlen = 3,
                                         target = "frequent itemsets"))
inspect(consonant_rules) %>% 
  View()
summary(consonant_rules)
arules::union()