# Describe the stimuli with lexical measures obtained from the 
# [English Lexicon Project, ELP](http://elexicon.wustl.edu/default.asp)

library("dplyr")

# Load my English target stimuli for memory task
targets <- read.csv("stimuli/english_target_verbs.csv")
head(targets)

# Load characteristics of target verbs retrieved from ELP:
d <- read.csv("stimuli/english_target_verbs_ELP.csv")
head(d)
str(d)

d <- dplyr::left_join(d, targets %>% rename(Word = verb))
head(d)


# Summaries of all the variables for leg vs arm verbs
by(d, d$type, summary)

lapply(d, summary)
sort(d$Ortho_N)

