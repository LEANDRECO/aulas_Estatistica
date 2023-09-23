
Dataset <- 
  readXL("/home/2022.1.08.011/Documents/Estatistica_Basica/aula2/Dados_Aula_Pratica_Amostragem.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Plan1", stringsAsFactors=TRUE)
#Tamanho da população
dados<-Dataset$Idade

#Tamanho da população
N<-length(dados)
N
#Tamanho da amostra
n<-80
n
Obscs<-sample(N,n)
Obscs

amostracs<-c()

for(i in 1:n)
amostracs[i]<-dados[Obscs[i]]

amostracs

mediacs<-mean(amostracs)

mediacs

#Amostragem Sistemática

#Tamanho do pulo

pulo<-round(N/n,0)

pulo

obsist<-c()

obsist[1]<-sample(pulo,1)

obsist[1]

for(i in 2:n)
obsist[i]<-obsist[i-1]+pulo

obsist

amostrasist<-c()

for(i in 1:n)
amostrasist[i]<-dados[obsist[i]]

amostrasist

mediasist<-mean(amostrasist)

mediasist

mediacs

#Amostragem estratificada

atuacao<-Dataset$Atuacao
atuacao

summary(atuacao)

#População de aprendizes (A)

Nap<-491

#População de profissionais (P)

Np<-6710

#População das outras atuações (O)

No<-511


#Definir o tamanho da amostra em cada extrato
nap<-round((Nap/N)*n,0)

np<-round((Np/N)*n,0)

no<-round((No/N)*n,0)

idadeA<-c()

idadeB<-c()

idadeC<-c()

a<-1
b<-1
c<-1

for(i in i:N){
	if(atuacao[i]=="A"){
		idadeA[a]<-dados[i]
		a<-a+1
	}
	else{
		if(atuacao[i]=="P"){
			idadeB[b]<-dados[i]
			b<-b+1
		}
		else{
			idadeC[c]<-dados[i]
			c<-c+1
		}
	}
}
#Media A
mediacsA<-mean(idadeA)
mediacsA

#Media P
mediacsB<-mean(idadeB)
mediacsB

#Media O
mediacsC<-mean(idadeC)
mediacsC

amostraest<-c(idadeA,idadeB,idadeC)
amostraest

mediaest<-mean(amostraest)
mediaest

