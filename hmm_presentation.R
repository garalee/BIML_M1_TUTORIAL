#A Markov model of DNA sequence evolution

# A Hidden Markov Model of DNA sequence evolution

states <- c("AT-rich", "GC-rich") 
ATrichprobs <- c(0.7, 0.3) 
GCrichprobs <- c(0.1, 0.9) 
transitionmatrix <- matrix(c(ATrichprobs, GCrichprobs), 2, 2, byrow = TRUE) 
rownames(transitionmatrix) <- states
colnames(transitionmatrix) <- states
transitionmatrix 

nucleotides <- c("A", "C", "G", "T") 
ATrichstateprobs <- c(0.39, 0.1, 0.1, 0.41) 
GCrichstateprobs <- c(0.1, 0.41, 0.39, 0.1) 
emissionmatrix <- matrix(c(ATrichstateprobs, GCrichstateprobs), 2, 4, byrow = TRUE)
rownames(emissionmatrix) <- states
colnames(emissionmatrix) <- nucleotides
emissionmatrix 


# Function to generate a DNA sequence, given a HMM and the length of the sequence to be generated.

nucleotides = c("A", "C", "G", "T")   
states = c("AT-rich", "GC-rich") 
mysequence = character()             
mystates = character()             
initialprobs <- c(0.5, 0.5)
firststate = 'AT-rich'
  
probabilities = emissionmatrix[firststate,]
firstnucleotide = sample(nucleotides, 1, prob=probabilities)
mysequence[1] = firstnucleotide         
mystates[1] = firststate              

seqlength = 1000
for (i in 2:seqlength)
{
    prevstate    <- mystates[i-1]          
    stateprobs   <- transitionmatrix[prevstate,]
    state        <- sample(states, 1, prob=stateprobs)
    probabilities <- emissionmatrix[state,]
    nucleotide   <- sample(nucleotides, 1, prob=probabilities)
    mysequence[i] <- nucleotide             
    mystates[i]  <- state     
}

library(HMM)
hmm=initHMM(states,nucleotides,transProbs=transitionmatrix,emissionProbs=emissionmatrix)
DNAseq = simHMM(hmm,1000)

## viterbi algorithm
hmm = initHMM(states,nucleotides,transProbs=transitionmatrix,emissionProbs=emissionmatrix)
viterbi(hmm,DNAseq$observation)


## Baum-Welch algorithm
hmm = initHMM(states,nucleotides,emissionProbs=emissionmatrix)
result = baumWelch(hmm,DNAseq$observation)
