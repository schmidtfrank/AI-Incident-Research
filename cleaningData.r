library(tidyverse)
library(tidytext)
library(SnowballC)

initialDownload <- read.csv("initialDownload.csv")
ourStopWords <- data.frame(word = c('ai', 'allegedly', 'reportedly'))

main <- initialDownload %>%
  select(incident_id, date, Alleged.deployer.of.AI.system, Alleged.developer.of.AI.system, description, title)

tokenizedDesc <- main %>%
  select(incident_id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  anti_join(ourStopWords) %>%
  mutate(word = wordStem(word))

tokenizedTitle <- main %>%
  select(incident_id, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  anti_join(ourStopWords) %>%
  mutate(word = wordStem(word))

cleanedDescriptions <- tokenizedDesc %>%
  group_by(incident_id) %>%
  summarise(cleaned_description = paste(word, collapse = " ")) %>%
  ungroup()

cleanedTitles <- tokenizedTitle %>%
  group_by(incident_id) %>%
  summarise(cleaned_title = paste(word, collapse = " ")) %>%
  ungroup()

exportIncidents <- main %>%
  left_join(cleanedTitles, by = "incident_id") %>%
  left_join(cleanedDescriptions, by = "incident_id") %>%
  select(-description, -title, -incident_id)


exportIncidents <- exportIncidents %>%
  rename(Deployer = Alleged.deployer.of.AI.system, Developer = Alleged.developer.of.AI.system) %>%
  arrange(date)

write.csv(exportIncidents, "cleanIncidents.csv")
