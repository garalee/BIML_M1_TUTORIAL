## HMM Tutorial

## Generate Like Unlike example
transitionmatrix = matrix(c(0.8,0.2,0.2,0.8),byrow=TRUE,nrow=2)

actions <- c("A", "R", "P", "S") 
likestateprobs <- c(0.3, 0.2, 0.2, 0.3) 
unlikestateprobs <- c(0.4, 0.39, 0.01, 0.2) 
emissionmatrix = matrix(c(likestateprobs,unlikestateprobs),2,4,byrow=TRUE)

library(HMM)
hmm=initHMM(c('Like','Unlike'),c('A','R','P','S'),transProbs=transitionmatrix,emissionProbs=emissionmatrix)
actionseq = c("S","R","R","R","R","S","A","R","A","R","R","A","A","A","A","A","A","S","A","A","R","S","A","A","R","A","S","A","R","S")

viterbi(hmm,actionseq)