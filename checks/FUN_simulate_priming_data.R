## Functions to simulate priming data

library(MASS)
library(dplyr)


# Modular functions -------------------------------------------------------

# Create first modular functions that are then combined together
# into a wrap-up function that actually generates the data:

## Simulate random effects (r.e.) for a grouping variable (typically subjects or items)
sim_RE <- function(
  groupingVar,  # string specifying grouping factor for r.e. (e.g. Subj or Item)
  N,            # number of observations per grouping variable (=number of Subj/Item)
  my_mean = 0,  # mean by-subject/item adjustment is 0 for all effects by assumption
  my_sd,        # vector of SDs for each random effect; not used if my_sigma provided
  my_sigma,     # covariance matrix of random effects; overrules my_sd
  effNames      # optional character vector of effect names
  ) {
  
  # by-grouping variable covariance matrix (if provided)
  groupCov <- if (! missing(my_sigma)) {
    my_sigma  # use it if explicitly provided
    } else {  # otherwise use SDs with no correlations between random effects
      diag(nrow = length(my_sd)) * my_sd ^ 2  # convoluted but otherwise won't work with just 1 r.e.
    }
  
  # variable to access the number of r.e. in code below
  nbEffects <- ncol(groupCov)
  
  # by-unit adjustments
  groupAdj <- MASS::mvrnorm(n = N, mu = rep(my_mean, nbEffects), Sigma = groupCov)
  
  # Names of the effects
  effectNames <- if(! missing(effNames)) { effNames } else { seq_len(nbEffects) }
  colnames(groupAdj) <- paste(groupingVar, "RE", effectNames, sep = "_")
  
  # As data frame
  subjAdj_df <- dplyr::tibble(unit = paste(groupingVar, seq_len(N), sep = "")) %>%
    bind_cols(as.data.frame(groupAdj))
  names(subjAdj_df)[1] <- groupingVar  # e.g. "Subjects" rather than "unit"
  subjAdj_df
}

# # E.g.,
# sim_RE(groupingVar = "Subj", N = 5, my_sd = c(30, 10), effNames = c("Int", "Prim"))
# sim_RE(groupingVar = "Item", N = 5, my_sd = 15)


## Function to massage the by-subject random effects so they can be joined with
## design matrix later.
# What it does is to transform the output of sim_RE for subjects into a kind
# of long format, so there is one row for each of the two levels of relatedness
# in the priming task (related vs unrelated). The priming effect is the 
# difference between the two: the priming effect is always 0 for the unrelated
# pair and the actual priming effect (adjustment) for the related pair.
# The by-subject random adjustment for the intercept of course stays constant
# across both conditions. (Look at the output!)
massage_subj_df <- function(
  RE_df,  # dataframe with random effects per unit (output from sim_RE() )
  fixefs  # list of fixed effects that vary per random effect grouping
  ) {
  nbLevels <- length(unique(fixefs))
  RE_df <- RE_df[rep(seq_len(nrow(RE_df)), each = nbLevels), ]
  RE_df$Related <- fixefs
  # Priming effect relates only to related condition
  RE_df[RE_df$Related == "unrel", "Subj_RE_Prim"] <- 0
  RE_df
}

# # E.g.
# set.seed(89)
# sim_RE(groupingVar = "Subj", N = 5, my_sd = c(30, 10), effNames = c("Int", "Prim")) %>%
#   massage_subj_df(fixefs = c("rel", "unrel"))
# # Compare that with 
# set.seed(89)
# sim_RE(groupingVar = "Subj", N = 5, my_sd = c(30, 10), effNames = c("Int", "Prim"))


## Create the basic data matrix based on number of subjects and items
data_matrix <- function(
  nbSubj,   # number of subjects
  nbItems,  # number of items
  fixefs = c("rel", "unrel")   # fixed effects
  ) {
  
  if (nbSubj %% 2 != 0 | nbItems %% 2 != 0) {
    stop ("Number of subjects and items must both be multiples of 2!") 
    }
  
  Subj <- paste("Subj", seq_len(nbSubj), sep = "")
  Item <- paste("Item", seq_len(nbItems), sep = "")
  
  d_m <- data.frame(
    Subj = rep(Subj, each = length(Item)),
    Item = Item,
    # Each item appears as related/unrelated, counterbalanced across lists:
    Related = c(rep(fixefs,      length.out = length(Item)),
                rep(fixefs[2:1], length.out = length(Item))),
    List = rep(1:2, each = length(Item)),
    stringsAsFactors = FALSE
  )
  d_m
}

# # E.g.
# data_matrix(nbSubj = 2, nbItems = 4)  # fine
# data_matrix(nbSubj = 2, nbItems = 5)  # error

# # Some sanity checks
# s <- data_matrix(nbSubj = 10, nbItems = 12)
# # All subjects see the same number of items in each condition
# with(s, addmargins(table(Related, Subj)))
# # All items are seen by the same number of subjects in each condition
# with(s, addmargins(table(Related, Item)))
# # All items appear in one condition per List
# with(s, addmargins(table(Related, Item, List)))



# Wrap-up function --------------------------------------------------------

sim_priming <- function(
  nbSubj,       # number of subjects; has to be multiple of 2 bc there are 2 lists
  nbTargets,    # number of targets appearing in both related/unrelated condition (between lists)
  intercept,    # RT intercept
  priming,      # prime effect: ms faster in related vs unrelated
  levelsRel = c("rel", "unrel"),  # levels of Related variable
  subjInt_sd,   # random by-subject intercepts (SD)  
  subjPrim_sd,  # random by-subject slope for priming effect (SD)  
  targInt_sd,   # random by-item intercepts (SD); NB: items are defined by targets
  resid         # residual variance (SD)
) {
  
  # Data matrix
  d <- data_matrix(nbSubj = nbSubj, nbItems = nbTargets)
  
  # Add RT intercept
  d$Int <- intercept
  # fixed effects for priming:
  fixef <- tibble(
    Related = levelsRel,
    Priming = c(-priming, 0)
    )
  # Add priming effect to data matrix (join with fixed effects for priming)
  d <- left_join(d, fixef)
  
  # By-subject random adjustments:
  sbjRE <- sim_RE(
    groupingVar = "Subj", N = nbSubj, my_sd = c(subjInt_sd, subjPrim_sd),
    effNames = c("Int", "Prim")
    ) %>%
    massage_subj_df(fixefs = levelsRel)
  # Add by-subject random adjustments to data matrix
  d <- left_join(d, sbjRE)
  
  # By-item random adjustments:
  itemRE <- sim_RE(
    groupingVar = "Item", N = nbTargets, my_sd = targInt_sd, effNames = "Int"
    )
  # Add to d
  d <- left_join(d, itemRE)
  
  # Add residual noise
  d$Resid <- rnorm(nrow(d), mean = 0, sd = resid)
  
  d
}

# sim_priming(nbSubj = 4, nbTargets = 4, intercept = 600, priming = 50,
#             subjInt_sd = 50, subjPrim_sd = 10, targInt_sd = 20, resid = 30)
# 
# sim_priming(nbSubj = 40, nbTargets = 24, intercept = 600, priming = 50,
#             subjInt_sd = 50, subjPrim_sd = 10, targInt_sd = 20, resid = 30)
