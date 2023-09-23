#Lendo os dados
D
Dataset <- 
  readXL("/home/2022.1.08.011/Documents/Estatistica_Basica/aula2/Dados_Aula_Pratica_Amostragem.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Plan1", stringsAsFactors=TRUE)

#Determinando que a Idade � a vari�vel resposta
dados<-Dataset$Idade

#Tamanho da popula��o
N<-length(dados)

N

#Tamanho da amostra
n<-80

#Amostragem casual simples

#Fazendo a amostra direta da vari�vel idade


amostracs<-sample(dados,n)
amostracs


mediacs<-mean(amostracs)
mediacs

#Amostragem sistemática

#Tamanho do pulo

pulo<-round(N/n,0)
pulo

#Criando um vetor para guardar as observa��es sorteadas
obsist<-c()

#Sorteando o primeiro elemento amostral
obsist[1]<-sample(pulo,1)
obsist[1]

#Determinado os demais elementos amostrais
for(i in 2:n)
  obsist[i]<-obsist[i-1]+pulo

obsist

#Tomando a idade dos elementos amostrados

amostrasist<-c()
for(i in 1:n)
  amostrasist[i]<-dados[obsist[i]]

amostrasist
mediasist<-mean(amostrasist)
mediasist


#Amostragem estratificada

#Estratificando pela atua��o
atuacao<-Dataset$Atuacao
atuacao

#Verificando quantos elementos tem na popula��o de cada atua��o
summary(atuacao)

#Tamanho da popula��o de aprendizes (A)
Nap<-491

#Tamanho da popula��o de profissionais (P)
Np<-6710

#Tamanho da popula��o das outras atua��es (O)
No<-511

#Definir o tamanho da amostra em cada estrato

#Aprendizes
nap<-round((Nap/N)*n,0)
nap

#Profissionais
np<-round((Np/N)*n,0)
np

#Outras atua��es
no<-round((No/N)*n,0)
no

#Conferindo se a soma dos tamanhos amostrais de cada estrato � igual
#ao tamanho amostral.
nap+np+no


#Separando as idades da popula��o de cada estrato, ou seja, de cada atua��o

#Aprendizes
idadeA<-c()

#Profissionais
idadeP<-c()

#Outras atua��es
idadeO<-c()

a<-1
p<-1
o<-1
for(i in 1:N)
{if(atuacao[i]=="A")
{idadeA[a]<-dados[i]
a<-a+1}else
{if(atuacao[i]=="P")
{idadeP[p]<-dados[i]
p<-p+1}else
{idadeO[o]<-dados[i]
o<-o+1
}      
}
}

idadeA
idadeP
idadeO

#Amostrando o estrato dos aprendizes

amostraap<-sample(idadeA,nap)


#Amostrando o estrato dos profissionais

amostrap<-sample(idadeP,np)


#Amostrando o estrato das outras atua��es

amostrao<-sample(idadeO,no)

#Juntando os elementos amostrados em um �nico conjunto de dados

amostraest<-c(amostraap,amostrap,amostrao)
amostraest

#M�dia da idade a amostra obtida pela t�cnica de amostragem estratificada.

mediaest<-mean(amostraest)
mediaest

#Calculando os erros de estima��o;

#M�dia da popula��o

mediapop<-mean(dados)

#Erro de estima��o: casual simples

errocs<-abs(mediacs-mediapop)
errocs


#Erro de estima��o: sistem�tica

errosist<-abs(mediasist-mediapop)
errosist


#Erro de estima��o: estratificada

erroest<-abs(mediaest-mediapop)
erroest

#Qual m�todo de estima��o apresentou menor erro de estima��o?



