setwd("/home/agricolamz/work/materials/2020.10.21_HSE_Phonology_of_East_Caucasian_languages/")
library(tidyverse)
library(arules)
df <- read_csv("data/data.csv")

df %>% 
  distinct(language, segments_IPA) %>% 
  count(segments_IPA, sort = TRUE) %>% 
  filter(n == 26) ->
  present_in_all

df %>% 
  filter(sound_type == "vowel") %>% 
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
  filter(sound_type == "consonant") %>% 
  anti_join(present_in_all) %>% 
  distinct(language, segments_IPA) %>% 
  as.data.frame() ->
  df_split

split(df_split[,"segments_IPA"], df_split[, "language"]) %>% 
  as("transactions") ->
  as_trans_consonants

consonant_rules <- apriori(as_trans_consonants, 
                        parameter = list(supp = 0.2,
                                         conf = 0.4, 
                                         minlen = 14,
                                         maxlen = 50,
                                         target = "frequent itemsets",
                                         maxtime = 0))

inspect(consonant_rules[1:153]) %>% 
  View()



summary(consonant_rules)
