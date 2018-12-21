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

ep <- read.csv("stimuli/development/stimuli_eprime-list1_complete.csv",
               stringsAsFactors = FALSE)
ep$prime <- tolower(ep$prime)
names(ep)[names(ep) == "prime"] <- "Prime"
names(ep)[names(ep) == "target"] <- "Target"
head(ep)


# Each block ("List") had either isi=50 or 1050, and contained the same amount
# of words and nonwords
with(ep, table(lexicality, isi, List, EprimeScript))


# Any primes that were also targets or the other way round?
overlap(ep, "Prime", "Target")  # Oh, there is one!
ep[which(ep$Prime %in% ep$Target), ]  # seems to be some kind of error
ep[which(ep$Target %in% ep$Prime), ]


# Compare targets from eprime list with the full list downloaded from spp:
table(ep$Target %in% ite$Target)  # 50% of course, the others are nonwords
# e.g.
ep[! ep$Target %in% ite$Target, ] %>% head
# Are the targets in ep not contained in ite always the nonwords?
table(ep[! ep$Target %in% ite$Target, "Target"] %in% ep[ep$lexicality == 2, "Target"])
# The exception is probably just a typo...


# How many of all the targets are in ep?
table(ite$Target %in% ep$Target)  # Roughly 50%


# So were nonwords always generated from actual targets?
mycompare <- function (df1, df2, n = 20) {
  # nonwords in ep
  nonw_df1 <- df1[df1$lexicality == 2, "Target"]
  df2_only <- df2[! df2$Target %in% df1$Target, "Target"]
  tibble(
    nonwords  = sort(nonw_df1) %>% tail(n),
    othertarg = sort(df2_only) %>% tail(n)
    )
}
mycompare(ep, ite)




# Create quick and dirty eprime lists -------------------------------------

head(ep)

# Is the procedure always the same?
table(ep$Procedure)
table(ep$primecond)

# Create some random lists of given length
quick_rand_lists <- function (df, n) {
  # take only from isi = 50
  df %<>% dplyr::filter(isi == 50) %>% select(- c(EprimeScript, List))
  df[base::sample(seq_len(nrow(df)), n), ]
}
quick_rand_lists(ep, 10)

save_eprime_list <- function (nbLists) {
  for(i in seq_len(nbLists)) {
    quick_rand_lists(ep, 10) %>% 
      write.table(paste("stimuli/development/my_eprime_list_", i, ".txt", sep = ""),
                  row.names = FALSE, sep = "\t", quote = FALSE, na = "")
  }
}
set.seed(10)
save_eprime_list(4)




# Are items constructed in pairs? -----------------------------------------

# Q: In SPP, are items constructed in pairs, such that if P1-T1 is related
# prime-target pair and P2-T1 is the corresponding unrelated pair for T1,
# then P1-T2 is the unrelated pair for T2?
# Let's check that...

# simplified dataframe of items
ites <- ite %>% select(Target, Prime, Unrel, Relation)
head(ites)

ites[with(ites, Prime == "disown"), ]
ites[with(ites, Prime == "disown" | Unrel == "disown"), ]

find_pairs <- function (targ, df) {
  tg <- df %>% filter(Target == targ)
  tg_prime <- tg[, "Prime"]
  tg_unrel <- tg[, "Unrel"]
  pr <- df %>% filter(Unrel == tg_prime)
  unrel_tg <- df %>% filter(Prime == tg_unrel)
  list(target = tg, prime_as_unrelated = pr, target_of_unrel = unrel_tg)
}
find_pairs("abandon", ites)
find_pairs("ability", ites)
find_pairs("new", ites)
find_pairs("after", ites)


# Take a look at a few random targets:
lapply(sample(ites$Target, 5), find_pairs, df = ites)  
# Conclusion: It's definitely NOT the case that items are constructed in pairs

# recursively track targets: 
# 1) choose a target T 
# 2) What's the unrelated prime for T; call it U
# 3) What's the target T' for which U is the actual (related) prime
# 4) Let T' be your new target and start from 1) again (repeat n times)
track_target <- function (targ, df, n) {
  while (n > 0) {
    cat("\n\n")
    print(paste(n, "left!"))
    tg_df <- df %>% filter(Target == targ)  # item with targ as target
    unrel <- tg_df[, "Unrel"]               # unrelated prime for targ
    unrel_tg_df <- df %>% filter(Prime == unrel)  # item where unrel is the related prime
    unrel_tg <- unrel_tg_df[, "Target"]  # target for which unrel is a prime
    print(tg_df)
    write.table(tg_df, file = "stimuli/development/recursive-track.csv", sep = "\t",
              col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
    return(track_target(unrel_tg, df, n - 1))  # recursiveness
  }
  return("Done!")
}
track_target("ability", ites, 16)     # loop closes after 15 items
track_target("annually", ites, 207)   # loop closes after 207 items
track_target("arm", ites, 26)   # loop closes after 25 items
track_target("you", ites, 72)   # loop closes after 71 items
track_target("her", ites, 111)  # loop closes after 110 items
track_target("film", ites, 68)  # loop closes after 67 items
track_target("shine", ites, 97)  # loop closes after 97 items
track_target("republican", ites, 97)  # loop closes after 97 items


track_target("lemon", ites, 95)  # But some loops are broken!!!
# This one breaks after "after" (conveniently!)

# Create an analogous function that BACTRACKS the targets
backtrack_target <- function (targ, df, n) {
  while (n > 0) {
    cat("\n\n")
    print(paste(n, "left!"))
    tg_df <- df %>% filter(Target == targ)  # item with targ as target
    prime <- tg_df[, "Prime"]               # related prime for targ
    prime_unrel_df <- df %>% filter(Unrel == prime)  # item where prime is the unrelated prime
    prime_unrel_tg <- prime_unrel_df[, "Target"]  # target for which prime is unrel is a prime
    print(tg_df)
    return(backtrack_target(prime_unrel_tg, df, n - 1))  # recursiveness
  }
  return("Done!")
}
# notice that it works in the same way
backtrack_target("ability", ites, 16)     # loop closes after 15 items

# Now we can backtrack from lemon, etc.
backtrack_target("lemon", ites, 171)  # Breaks at "often"
backtrack_target("pass", ites, 26)  # Breaks at "temperature"

# So this is a very long but unclosed loop!
track_target("often", ites, 263)  # It breaks if taken one step further
# And this is a short unclosed loop!
track_target("temperature", ites, 31)  # It breaks if taken one step further

