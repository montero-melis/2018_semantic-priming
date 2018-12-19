## Functions to simulate priming data

library(MASS)
library(dplyr)


# Modular functions -------------------------------------------------------

# Create first modular functions that are then combined together
# into a wrap-up function that actually generates the data:

## Simulate random effects for a grouping variable (typically subjects or items)
sim_RE <- function(
  groupingVar,  # grouping factor for random effects (e.g. Subj or Item)
  N,            # number of observations per grouping variable (Subj/Item)
  my_mean = 0,  # mean by-subject/item adjustment is 0 for all effects by assumption
  my_sd,        # vector of SDs for each random effect; not used if my_sigma provided
  my_sigma,     # covariance matrix of random effects; overrules my_sd
  effNames      # optional character vector of effect names
  ) {
  
  # by-grouping variable covariance matrix
  groupCov <- if (! missing(my_sigma)) {
    my_sigma  # use it if explicitly provided
    } else {  # otherwise no correlations between random effects
      diag(nrow = length(my_sd)) * my_sd ^ 2  # convoluted but otherwise won't work with just 1 r. int.
    }
  
  # variable to access the number of r. effects
  nbEffects <- ncol(groupCov)
  
  # by-unit adjustments
  groupAdj <- MASS::mvrnorm(n = N, mu = rep(my_mean, nbEffects), Sigma = groupCov)
  
  # Names of the effects
  effectNames <- if(! missing(effNames)) { effNames } else { seq_len(nbEffects) }
  colnames(groupAdj) <- paste(groupingVar, "RE", effectNames, sep = "_")
  
  # As data frame
  subjAdj_df <- data.frame(
    unit = paste(groupingVar, seq_len(N), sep = ""),
    groupAdj,
    stringsAsFactors = FALSE
  )
  names(subjAdj_df)[1] <- groupingVar
  subjAdj_df
}

# # E.g.,
# sim_RE(groupingVar = "Subj", N = 5, my_sd = c(30, 10), effNames = c("Int", "Prim"))
# sim_RE(groupingVar = "Item", N = 5, my_sd = 15)


## Function to massage the by-subject random effects so they can be joined with
## design matrix:
massage_subj_df <- function(
  RE_df,  # dataframe with random effects per unit, output from sim_RE()
  fixefs  # list of fixed effects that vary per random effect grouping
  ) {
  nbLevels <- length(unique(fixefs))
  RE_df <- RE_df[rep(seq_len(nrow(RE_df)), each = nbLevels), ]
  RE_df$Related <- fixefs
  # Priming effect relates only to related condition
  RE_df[RE_df$Related == "unrel", "Subj_RE_Prim"] <- 0
  RE_df
}

# E.g.
sim_RE(groupingVar = "Subj", N = 5, my_sd = c(30, 10), effNames = c("Int", "Prim")) %>%
  massage_subj_df(fixefs = c("rel", "unrel"))


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



# Wrap-up function --------------------------------------------------------

sim_priming <- function(
  nbSubj,       # number of participants; ideally multiple of 2 because there are 2 lists
  nbTargets,    # number of targets appearing in both related/unrelated condition (between lists)
  intercept,    # RT intercept
  priming,      # prime effect: ms faster in related vs unrelated
  levelsRel = c("rel", "unrel"),  # levels of Related variable
  subjInt_sd,   # random by-subject intercepts (SD)  
  subjPrim_sd,  # random by-subject slope for priming effect (SD)  
  targInt_sd,   # random by-item intercepts (SD), where items defined by targets
  resid   # residual variance (SD)
) {
  
  # Data matrix
  d <- data_matrix(nbSubj = nbSubj, nbItems = nbTargets)
  
  # Add RT intercept
  d$Int <- intercept  
  # Add priming effect (join with fixed effects for priming)
  d <- left_join(d, data.frame(Related = levelsRel, Priming = c(-priming, 0),
                               stringsAsFactors = FALSE))
  
  # By-subject random adjustments:
  sbjRE <- sim_RE(
    groupingVar = "Subj", N = nbSubj, my_sd = c(subjInt_sd, subjPrim_sd),
    effNames = c("Int", "Prim")) %>%
    massage_subj_df(fixefs = levelsRel)
  # Add to d
  d <- left_join(d, sbjRE)
  
  # By-item random adjustments:
  itemRE <- sim_RE(groupingVar = "Item", N = nbTargets, my_sd = targInt_sd,
                   effNames = "Int")
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
