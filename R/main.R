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


generate_unif_continu <-function(n_voters, n_candidats, min=0, max=1){
  scores <- matrix(runif(n_candidats*n_voters, min=min, max=max),c(n_candidats,n_voters))
  return(scores)
}

generate_beta <- function(n_voters,n_candidats,beta_a = 0.5,beta_b = 0.5, lambda = 0,min = 0,max = 1) {
  scores<-matrix(rbeta(n_candidats*n_voters, shape1 = beta_a, shape2 = beta_b, ncp=lambda),c(n_candidats,n_voters))
  scores<-scores*(max-min)+min # on borne les prefs entre 0 et 1
  return(scores)
}

generate_spatial <- function(n_voters,n_candidats,placement = "uniform",score_method = "linear"){
  # mettre un paramètre pour choisir la manière de placer les candidats/votants (uniform, beta, ...), aussi pour la fonction score
  n_dim <- 2 # constante

  # === placement === #
  if (placement == "uniform"){
    candidats<-data.frame(matrix(runif(n_candidats*n_dim), nrow = n_candidats, ncol=n_dim))
    voters<-data.frame(matrix(runif(n_voters*n_dim), nrow = n_voters, ncol=n_dim))
  }else if(placement == "beta"){
    beta_a= 1.2 # 2 = points centrés
    beta_b= 1.2
    candidats <- data.frame(matrix(rbeta(n_candidats*n_dim, shape1 = beta_a, shape2 = beta_b),nrow = n_candidats,ncol = n_dim))
    voters <- data.frame(matrix(rbeta(n_voters*n_dim, shape1 = beta_a, shape2 = beta_b), nrow = n_voters, ncol = n_dim))
  }else{
  }
  # === distance between voters / candidats === #
  mat_distances<-t(apply(voters,1, function(x) distance(x,candidats)))

  # === distance to score === # (linear / sigmoide)
  mat_scores<-DistToScores(mat_distances,method = score_method)

  View(mat_distances) # test
  View(mat_scores) # test

  # === plots === #
  plot(candidats, xlab="dim. 1", ylab="dim. 2", xlim=c(0,1), ylim=c(0,1), col="red", pch=c(17), cex=1.5, main="Spatial model")
  text(candidats[,1]+0.001, candidats[,2]+0.001, labels=1:n_candidats, pos=4, col="red")
  if(n_voters <= 200){
    points(voters)
  }else{
    points(voters[sample(n_voters,200),])
  }

  return(mat_scores)
}

generate_alea <- function(n_voters,n_candidats,para) {

}



