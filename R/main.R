# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Generates a simulation of voting according to a beta law, returns voters preferences
#' @export
#' @param n_voters integer, represents the number of voters
#' @param n_candidats integer
#' @param beta_a double
#' @param beta_b double
#' @param lambda double
#' @param min int
#' @param max int
#' @importFrom stats rbeta
#' @returns scores
generate_beta <- function(n_voters,n_candidats,beta_a = 0.5,beta_b = 0.5, lambda = 0,min = 0,max = 1) {
  #set.seed(2023)
  scores<-matrix(rbeta(n_candidats*n_voters, shape1 = beta_a, shape2 = beta_b, ncp=lambda),c(n_candidats,n_voters))
  scores<-scores*(max-min)+min
  scores <- rename_rows(scores)
  print(scores)
  return(scores)
}

#' Generates a simulation of voting according to a uniform law, returns voters preferences
#' @export
#' @param n_voters integer
#' @param n_candidats integer
#' @param min integer
#' @param max integer
#' @importFrom stats runif
#' @returns scores
generate_unif_continu <-function(n_voters, n_candidats, min=0, max=1){
  #set.seed(2023)
  scores <- matrix(runif(n_candidats*n_voters, min=min, max=max),c(n_candidats,n_voters))
  scores <- rename_rows(scores)
  print(scores)
  return(scores)
}

#' Generate spatial simulation
#'
#' This function generates spatial data consisting of \code{n_voters} voters and \code{n_candidats} candidates. The spatial model is created by placing the candidates on a 2-dimensional plane according to the \code{placement} parameter, and then computing a distance matrix between voters and candidates. The distances are then transformed into scores using the \code{score_method} parameter. Finally, a plot of the candidates and voters is produced.
#'
#' @param n_voters The number of voters.
#' @param n_candidats The number of candidates.
#' @param placement The method used to place the candidates on the 2-dimensional plane. Must be either "uniform" or "beta". Default is "uniform".
#' @param score_method The method used to transform distances into scores. Must be either "linear" or "sigmoide". Default is "linear".
#' @return A matrix of scores.
#' @export
#' @importFrom graphics text
#' @importFrom graphics points
#' @importFrom stats rbeta
#' @importFrom stats runif
#' @examples
#' generate_spatial(n_voters = 100, n_candidats = 5, placement = "uniform", score_method = "linear")
generate_spatial <- function(n_voters,n_candidats,placement = "uniform",score_method = "linear"){
  #set.seed(2023)
  n_dim <- 2 # constante
  # === placement === #
  if (placement == "uniform"){
    candidats<-matrix(runif(n_candidats*n_dim), nrow = n_candidats, ncol=n_dim)
    voters<-matrix(runif(n_voters*n_dim), nrow = n_voters, ncol=n_dim)
  }else if(placement == "beta"){
    beta_a= 1.2 # 2 = points centrés
    beta_b= 1.2
    candidats <- matrix(rbeta(n_candidats*n_dim, shape1 = beta_a, shape2 = beta_b),nrow = n_candidats,ncol = n_dim)
    voters <- matrix(rbeta(n_voters*n_dim, shape1 = beta_a, shape2 = beta_b), nrow = n_voters, ncol = n_dim)
  }else{
    # ...
  }
  # === distance between voters / candidats === #
  matrix_distances<-apply(voters,1, function(x) distance(x,candidats))

  # === distance to score === # (linear / sigmoide)
  matrix_scores<-DistToScores(matrix_distances,method = score_method)

  #View(matrix_distances) # test
  #View(matrix_scores) # test

  # === plots === #
  plot(candidats, xlab="dim. 1", ylab="dim. 2", xlim=c(0,1), ylim=c(0,1), col="red", pch=c(17), cex=1.5, main="Spatial model")
  text(candidats[,1]+0.001, candidats[,2]+0.001, labels=1:n_candidats, pos=4, col="red")
  if(n_voters <= 200){
    points(voters)
  }else{
    points(voters[sample(n_voters,200),])
  }

  #pref_rank <- preferences_to_ranks(matrix_scores)
  #View(pref_rank)


  #View(distance_to_pref(pref_rank))
  return(rename_rows(matrix_scores))
}


library(truncnorm)
#' Generate truncated normal scores
#'
#' This function generates truncated normal scores using the 'rtruncnorm' function from the 'truncnorm' package.
#'
#' @param n_candidats The number of candidates to generate scores for.
#' @param n_voters The number of voters to generate scores for.
#' @param min The minimum value of the truncated normal distribution.
#' @param max The maximum value of the truncated normal distribution.
#' @param mean The mean of the truncated normal distribution.
#' @param sd The standard deviation of the truncated normal distribution.
#'
#' @return A matrix of scores with 'n_candidats' rows and 'n_voters' columns.
#' @import truncnorm
#'
#' @export
generate_norm<-function(n_candidats, n_voters, min=0, max=1, mean=0.5, sd=0.25){
  #set.seed(2023)
  scores<-matrix(rtruncnorm(n_candidats*n_voters, a=min, b=max, mean = mean, sd = sd),c(n_candidats,n_voters))
  scores <- rename_rows(t(scores))
  print(scores)
  return(scores)
}


#' generate one beta and two unif candidate, returns voters preferences
#' @export
#' @param n_voters integer
#' @param n_candidats integer
#' @param beta_a double
#' @param beta_b double
#' @param lambda double
#' @param min integer
#' @param max integer
#' @importFrom stats runif
#' @importFrom stats rbeta
#' @returns scores
generate_one_beta_two_unif_candidate <-function(n_voters, n_candidats,beta_a = 0.5,beta_b = 0.5, lambda = 0, min=0, max=1){
  n_candidates_unif <- n_candidats - 1
  scores1 <- matrix(runif(n_candidates_unif*n_voters, min=min, max=max),c(n_candidates_unif,n_voters))
  scores2 <- matrix(rbeta(1*n_voters, shape1 = beta_a, shape2 = beta_b, ncp=lambda),c(1,n_voters))
  print(scores1)
  print(scores2)
  scores <- rbind(scores1,scores2)
  scores<-scores*(max-min)+min
  scores <- rename_rows_beta_candidate(scores)
  print(scores)
  return(scores)
}
