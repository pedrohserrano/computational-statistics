#grafica redes
library(ape)
stockspan <- mst(1 - cor(spall.ret[, 2:11]))
plot(stockspan, graph='nsca')


#####-------------viterbi algorithm
library(HMM)
# Initialise HMM
hmm = initHMM(c("A","B"), #estados de la cadena (dos estados escondidos)
              c("L","R"), #simbolos (letras por ejemplo)
              transProbs=matrix(c(.6,.4,.4,.6),2), #matriz de prob de transicion
              emissionProbs=matrix(c(.6,.4,.4,.6),2)) #probs de las observaciones
print(hmm)
# Sequence of observations
observations = c("L","L","R","R") #una secuancia que queremos estudiar

#probabilidades posteriores
posterior = posterior(hmm,observations)
print(posterior)
#la matriz de los estados vs la secuancia de observaciones

# Calculate Viterbi path
viterbi = viterbi(hmm,observations) 
print(viterbi)
#es el camino mas probable 
#"A" "A" "B" "B"


####---------------casino example

symbol.sequence<-c(6,6,4,1,5,3,2,1,6,1,6,2,1,1,5,2,3,4,6,5,3,2,1,4,3,5,6,6,3,4,2,6,1,6,5,5,2,3,4,2,3,2,3,1,5,1,4,2,4,
                   6,4,1,5,6,6,6,3,2,4,6,2,6,6,5,6,5,6,3,6,5,6,4,6,6,3,1,5,6,3,4,6,3,6,4,5,6,5,1,4,3,1,3,2,6,1,6,3,3,
                   1,2,3,6,1,6,6,5,4,3,6,1,1,5,1,3,3,3,2,1,2,6,6,6,5,2,6,5,6,1,5,2,2,4,2,4,4,3,4,2,5,3,3,5,6,1,2,3,5,
                   6,6,6,3,6,3,2,6,3,6,1,6,6,2,3,3,5,3,2,3,2,6,2,4,6,4,6,6,5,5,6,6,3,1,2,1,3,1,4,3,6,6,4,2,4,6,3,4,2,
                   3,3,2,4,2,3,4,5,3,6,6,1,2,3,2,4,6,6,4,5,2,3,5,6,4,2,1,5,1,4,6,6,1,5,1,4,6,2,2,1,4,5,5,6,1,4,4,4,6,
                   1,5,5,4,3,5,1,3,4,5,1,2,3,6,6,6,2,6,6,4,6,4,6,5,6,6,1,2,4,6,4,2,6,5,6,2,6,5,1,5,2,1,1,1,6,6,6,6,6,
                   4,6,3,3,4,6)


real.path<-c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","F","F","F","F","F","F","L","L","L","L","L",
             "L","L","L","L","L","L","L","L","L","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
             "F","F","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","F","F","F","L","L","L","L","L",
             "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
             "F","F","F","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L",
             "L","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","L",
             "L","L","L","L","L","L","L","L","L","F","F","F","F","F","L","L","F","F","F","F","F","F","F","F","F",
             "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
             "F","F","F","F","F","F","F","F","F","F","L","L","L","L","F","F","F","F","F","F","F","F","F","F","F",
             "L","L","L","L","L","L","L","L","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
             "F","F","F","F","F","F","F","F","L","L","L","L","F","F","F","F","F","F","F","F","L","L","L","L","L",
             "L","L","L","L","L","L","L","L","F","F","F","L","L","L","L","L","L","L","L","L","L","F","F","L","L")


real.viterbi<-c("F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","L","L","L",
                "L","L","L","L","L","L","L","L","L","L","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F",
                "F","F","F","F","F","F","F","F","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L",
                "L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L")


# we'll represent loaded die as "L", and the fair one as "F"
states <- c("F", "L")

# following matrix defines the probability of switching the die
transition.matrix <- t(matrix(data = log2(c(0.95, 0.05, 0.1, 0.9)), nrow = 2, ncol = 2, dimnames = list(c("F", "L"), c("F", "L"))))

# emission probabilities tell you what is the change of landing on each side given that the particular die is selected
emission.matrix <- matrix(data = log2(c(rep(1/6, 6), c(rep(1/10, 5), 1/2))), nrow = 6, ncol = 2, dimnames = list(seq(1,6), c("F", "L")))

# initial probabilities define the chance of starting outcome (in our case we are equally likely to start with either states)
initial.matrix <- matrix(data = log2(c(0.5, 0.5)), nrow = 2, ncol = 1, dimnames = list(c("F", "L"), ""))

#After we defined the model, we need to initialize two object that will keep the track of probability history and state path (Pi) during the recursion process.

prob.history  <- data.frame()
state.history <- data.frame()

# we start by calculating the probability of being in particular state given the first symbol and initial matrix
# notice a change in log space - every multiplication is converted to summation
prob.history  <- rbind(prob.history, unlist(lapply(states, function(k) {
  initial.matrix[k, 1] + emission.matrix[symbol.sequence[1], k]
})))

state.history <- rbind(state.history, states)

colnames(prob.history)  <- c("F", "L")
colnames(state.history) <- c("F", "L")


####hacer el viterbi en el caso del casino

hmm = initHMM(states, #estados de la cadena (dos estados escondidos)
              symbol.sequence, #simbolos (letras por ejemplo)
              transProbs=transition.matrix, #matriz de prob de transicion
              emissionProbs=initial.matrix) #probs de las observaciones
print(hmm)
# Sequence of observations
real.viterbi #una secuancia que queremos estudiar

#probabilidades posteriores
posterior = posterior(hmm, real.viterbi)
print(posterior)
#la matriz de los estados vs la secuancia de observaciones

# Calculate Viterbi path
viterbi = viterbi(hmm,real.viterbi) 
print(viterbi)
#es el camino mas probable 
#"A" "A" "B" "B"


