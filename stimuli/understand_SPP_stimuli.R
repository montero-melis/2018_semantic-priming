## Understand the design of stimuli used in SPP (Hutchison et al., 2013)

library(dplyr)
library(magrittr)
library(purrr)


# Load word list
ite <- read.csv("stimuli/development/spp_targets-primes-unrel.csv",
                stringsAsFactors = FALSE)
head(ite)
ite$Prime <- tolower(ite$Prime)
ite$Unrel <- tolower(ite$Unrel)
head(ite)


## Words should appear only once in each of the columns Target, Prime, Unrel. 
## Do they?
ite %>% select(Target, Prime, Unrel) %>%
  map(~ table(.)[table(.) != 1])
# Not really, for some reason 3 words were repeated in each column...


## Are the same words used as targets, primes and unrelated?

# Convenience function
overlap <- function(df, col1, col2) {
  print(paste("Number of", col1, "that are also", col2, ":"))
  print(table(df[[col1]] %in% df[[col2]]))
  cat("\n\n")
  print(paste("Number of", col2, "that are also", col1, ":"))
  print(table(df[[col2]] %in% df[[col1]]))
}

# Due to counterbalancing, we should expect all Primes to appear as Unrel as well:
overlap(ite, "Prime", "Unrel")  # well, almost
# Which are the exceptions?
ite[with(ite, ! Prime %in% Unrel), 1:4] # Primes don't appear as Unrel
ite[with(ite, ! Unrel %in% Prime), 1:4] # Unrel don't appear as Primes


# Overlap between Targets and Primes
overlap(ite, "Target", "Prime")
overlap(ite, "Target", "Unrel")

ite[with(ite, Target %in% Prime), 1:4] %>% head(10)
ite[with(ite, Target %in% Unrel), 1:4] %>% head(10)

ite[with(ite, ! Target %in% Prime), 1:4] %>% head(15)
ite[with(ite, ! Target %in% Unrel), 1:4] %>% head(10)

ite[with(ite, ! Target %in% Prime), 1:4] %>% tail(10)
ite[with(ite, ! Target %in% Unrel), 1:4] %>% tail(10)


ite[with(ite, ! Target %in% Prime), 1:4] %>% head(20)
ite[with(ite, Target %in% Unrel), 1:4] %>% head(20)



# Which ones?
ite[with(ite, Target %in% Prime), 1:4] %>% head(20)  # Targets appear also as primes
ite[with(ite, ! Target %in% Prime), 1:4] %>% head(20)  # Targets do not appear as primes

# Targets that are also primes
targ_prime <- ite[with(ite, Target %in% Prime & Prime %in% Target), ]
# How many of them are also unrelated?
overlap(targ_prime, "Target", "Unrel")
overlap(targ_prime, "Prime", "Unrel")
overlap(targ_prime, "Target", "Prime")



# Eprime task -------------------------------------------------------------

# Stimuli downloaded from Eprime script I got from Hutchison (List 1)


