
#############################################################################
## utility functions implementing the SJPPDS approaches
#############################################################################

## discretize a single numerical variable
CategorizeVariable <- function(x, n_levels) {
  ## Inputs:
  ## x: vector of numeric values
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ##
  ## Output:
  ## out: vector of categorical variables
  
  var_levels <- seq(n_levels)
  out <- cut(x, breaks = n_levels, labels = var_levels)
  out <- as.character(as.numeric(out))
  
  return(out)
}


## discretize the entire numeric dataset
CategorizeData <- function(dat, n_levels) {
  ## Inputs:
  ## dat: numerical dataset
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ##
  ## Output:
  ## out: dataset of categorical variables
  
  n_vars <- ncol(dat)
  dat_C <- dat
  for (i in seq(n_vars)) {
    dat_C[, i] <- CategorizeVariable(dat[, i], n_levels)
  } 
  
  return(dat_C)
}


## implement the full JPPDS approach (i.e., Algorithm 1 in the paper)
JointProbabilityPreservingDataShufflingF <- function(dat, dat_C) {
  ## Inputs:
  ## dat: numerical dataset
  ## dat_C: categorical dataset
  ##
  ## Output:
  ## dat_S: masked dataset
  
  p <- ncol(dat)
  dat_S <- dat
  for (i in seq(p - 1)) {
    col_idx <- seq(i + 1, p)
    lcombs <- apply(dat_C[, col_idx, drop = FALSE], 1, function (x) paste(x, collapse = "_"))
    ucombs <- unique(lcombs)
    for (j in seq(length(ucombs))) {
      idx <- which(lcombs == ucombs[j])
      shuffled_idx <- idx[sample(length(idx))]
      dat_S[idx, seq(1, i)] <- dat[shuffled_idx, seq(1, i)]
    }
  }
  ## because the last columns of dat and dat_S are
  ## identical, we perform a final random shuffling 
  ## of the entire data set
  dat_S <- dat_S[sample(nrow(dat_S)),]
  
  return(dat_S)
}


## implement the simplified JPPDS approach (i.e., Algorithm 2 in the paper)
JointProbabilityPreservingDataShufflingS <- function(dat, dat_C) {
  ## Inputs:
  ## dat: numerical dataset
  ## dat_C: categorical dataset
  ##
  ## Output:
  ## dat_S: masked dataset
  
  p <- ncol(dat)
  dat_S <- dat
  last_col_classes <- unique(dat_C[, p])
  col_idx <- seq(1, p - 1)
  for (j in seq(length(last_col_classes))) {
    idx <- which(dat_C[, p] == last_col_classes[j])
    shuffled_idx <- idx[sample(length(idx))]
    dat_S[idx, col_idx] <- dat[shuffled_idx, col_idx]
  }
  
  ## because the last columns of dat and dat_S are
  ## identical, we perform a final random suffling 
  ## of the entire data set
  dat_S <- dat_S[sample(nrow(dat_S)),]
  
  return(dat_S)
}


## implement the SJPPDS approach (i.e., Algorithm 3 in the paper)
SequentialJPPDS <- function(dat, 
                            n_levels,
                            shuffle_type = "simple",
                            verbose = TRUE) {
  ## Inputs:
  ## dat: numerical dataset
  ## n_levels: number of categories/levels (i.e., n_c in the paper's notation)
  ## shuffle_type: "simple" implements the simplified SJPPDS, while "full"
  ##               implements the full SJPPDS approach
  ## verbose: whether to print the computation progress
  ##
  ## Output:
  ## dat_S: masked dataset
  
  ## switch between the simplified and full approaches
  GenerateShuffledDataset <- function(dat, n_levels, shuffle_type = "simple") {
    dat <- data.frame(dat)
    dat_C <- CategorizeData(dat, n_levels)
    if (shuffle_type == "simple") {
      dat_S <- JointProbabilityPreservingDataShufflingS(dat, dat_C)
    }
    else if (shuffle_type == "full") {
      dat_S <- JointProbabilityPreservingDataShufflingF(dat, dat_C)
    }
    
    return(dat_S)
  }
  
  p <- ncol(dat) 
  if (verbose) {
    cat("shuffle", 1, "\n") 
  }
  dat_S <- GenerateShuffledDataset(dat, n_levels, shuffle_type)
  for (i in seq(1, p - 1)) {
    if (verbose) {
      cat("shuffle", i + 1, "\n") 
    }
    dat_S <- dat_S[, c(2:p, 1)]
    dat_S <- GenerateShuffledDataset(dat_S, n_levels, shuffle_type)
  }
  dat_S <- dat_S[, c(2:p, 1)] ## revert the data to original variable order
  
  return(dat_S)
}


###############################################
## Illustration of the method's application
###############################################

## load sdcMicro package and assess the Tarragona dataset
library(sdcMicro)
dat <- Tarragona

## apply the full version of the SJPPDS approach
sdat_f <- SequentialJPPDS(dat = dat, 
                          n_levels = 30,
                          shuffle_type = "full",
                          verbose = TRUE)

## apply the simplified version of the SJPPDS approach
sdat_s <- SequentialJPPDS(dat = dat, 
                          n_levels = 30,
                          shuffle_type = "simple",
                          verbose = TRUE)

## load the corrplot package for visualization
## of correlation matrices
library(corrplot)

## compute Pearson correlations
cor_o <- cor(dat)
cor_f <- cor(sdat_f)
cor_s <- cor(sdat_s)

## correlation matrix of the original data
corrplot(cor_o)

## correlation matrix of the full SJPPDS method
corrplot(cor_f)

## correlation matrix of the simplified SJPPDS method
corrplot(cor_s)




