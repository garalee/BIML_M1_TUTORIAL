## HMM Tutorial


## Generate simulation Data
transition.matrix = matrix(c(0.2,0.8,0.7,0.3),byrow=TRUE,nrow=2)

observed.data = c()
hidden.states = c(1)

states = c(10,30)
current.state = 1
for(i in seq(1,1000)){
  data = rnorm(1,mean=states[current.state],sd=1) ## emission
  current.state = sample(c(1,2),1,prob=transition.matrix[current.state,]) ## transition
  
  hidden.states=c(hidden.states,current.state)
  observed.data = c(observed.data,data)
}

emission.vector = dnorm(observed.data[hidden.states==1],mean=10,sd=1)
emission.vector2 = dnorm(observed.data[hidden.states=2],mean=30,sd=1)