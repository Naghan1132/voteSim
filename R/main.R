# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# faire qql chose de la manière (pour le package méthode de vote :
# scrutin1 <- function(nVoter,nCandidat,situation) {
#
#     le paramètre "situation" est le résultat d'une fonction du package "voteSim"
#     peut-être pas besoin de nVoter et nCandidat, juste à récupérer le nombre de ligne/col
#     dans le tableau situation (si elle retourne bien un tableau)
#
# }


generate_unif_continu <-function(nVoters, nCandidats, min=0, max=1){
  pref<-array(runif(nCandidats*nVoters, min=min, max=max),c(nCandidats,nVoters))
  return(pref)
}

generate_alea <- function(nVoters,nCandidats,para) {


}

generate_beta <- function(nVoters,nCandidats,para) {


}

generate_spatial <- function(n_voters,n_candidats,n_dim = 2,placement = "uniform",score_method = "linear"){
  # mettre un paramètre pour choisir la manière de placer les candidats/votants (uniform, beta, ...), aussi pour la fonction score
  ### placement ###
  if (placement == "linear"){
    candidats<-data.frame(matrix(runif(n_candidats*n_dim), nrow = n_candidats, ncol=n_dim))
    voters<-data.frame(matrix(runif(n_voters*n_dim), nrow = n_voters, ncol=n_dim))
  }

  ### transformation distance to score

  mat_scores<-DistToScores(mat_distances,method = score_method)
  mat_distances<-t(apply(voters,1, function(x) distance(x,candidats)))

}




