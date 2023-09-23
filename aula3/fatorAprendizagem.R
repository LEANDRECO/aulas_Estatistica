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
aprendeu<-factor(Dataset$Aprendeu)
aprendeu

#Verificando quantos elementos tem na popula��o de cada atua��o
summary(aprendeu)

#Tamanho da popula��o de nivel 1
N1<-1867

#Tamanho da popula��o de nivel 2
N2<-1148

#Tamanho da popula��o de nivel 3
N3<-1176

#Tamanho da popula��o de nivel 4
N4<-1207

#Tamanho da popula��o de nivel 5
N5<-229

#Tamanho da popula��o de nivel 6
N6<-264

#Tamanho da popula��o de nivel 7
N7<-1082

#Tamanho da popula��o de nivel 8
N8<-551

#Tamanho da popula��o de nivel 9
N9<-188
N1
N2
N3
N4
N5
N6
N7
N8
N9
TamanhoPop<-c(N1,N2,N3,N4,N5,N6,N7,N8,N9)
TamanhoPop

Tamanhoamos<-round((TamanhoPop/N)*n,0)
Tamanhoamos

sum(Tamanhoamos)

#Conferindo se a soma dos tamanhos amostrais de cada estrato � igual
#ao tamanho amostral.
Np1+Np2+Np3+Np4+Np5+Np6+Np7+Np8+Np9


#Separando as idades da popula��o de cada estrato, ou seja, de cada atua��o

aprendeu1<-c()
aprendeu2<-c()
aprendeu3<-c()
aprendeu4<-c()
aprendeu5<-c()
aprendeu6<-c()
aprendeu7<-c()
aprendeu8<-c()
aprendeu9<-c()

c1<-1
c2<-1
c3<-1
c3<-1
c4<-1
c5<-1
c6<-1
c7<-1
c8<-1
c9<-1

for(i in 1:N)
{if(aprendeu[i]=="1")
{aprendeu1[c1]<-dados[i]
c1<-c1+1}else
{if(aprendeu[i]=="2")
{aprendeu2[c2]<-dados[i]
c2<-c2+1}else
{if(aprendeu[i]=="3")
{aprendeu3[c3]<-dados[i]
c3<-c3+1}else
{if(aprendeu[i]=="4")
{aprendeu4[c4]<-dados[i]
c4<-c4+1}else
{if(aprendeu[i]=="5")
{aprendeu5[c5]<-dados[i]
c5<-c5+1}else
{if(aprendeu[i]=="6")
{aprendeu6[c6]<-dados[i]
c6<-c6+1}else
{if(aprendeu[i]=="7")
{aprendeu7[c7]<-dados[i]
c7<-c7+1}else
{if(aprendeu[i]=="8")
{aprendeu8[c8]<-dados[i]
c8<-c8+1}else
{if(aprendeu[i]=="9")
{aprendeu9[c9]<-dados[i]
c9<-c9+1}
}
}
}
}
}
}
}
}
}

aprendeu1
aprendeu2

idadep
idadeO

#Amostrando o estrato dos aprendizes

amostra1<-sample(aprendeu1,Tamanhoamos[1])
amostra2<-sample(aprendeu2,Tamanhoamos[2])
amostra3<-sample(aprendeu3,Tamanhoamos[3])
amostra4<-sample(aprendeu4,Tamanhoamos[4])
amostra5<-sample(aprendeu5,Tamanhoamos[5])
amostra6<-sample(aprendeu6,Tamanhoamos[6])
amostra7<-sample(aprendeu7,Tamanhoamos[7])
amostra8<-sample(aprendeu8,Tamanhoamos[8])
amostra9<-sample(aprendeu9,Tamanhoamos[9])

#Juntando os elementos amostrandos em um unico conjunto de dados

amostraest<-c(amostra1,amostra2,amostra3,amostra4,amostra5,amostra6,amostra7,amostra8,amostra9)
amostraest

#Media da idade a amostra obtida pela tecnica de amostragem estratificada

mediaest<-mean(amostraest)
mediaest

#Calculando os erros de estimacao

#Media da populacao

mediapop<-mean(dados)

#Erro de estimacao: casual simples

errocs<-abs(mediacs-mediapop)
errocs


#Erro de estimacao: sistematica

errosist<-abs(mediasist-mediapop)
errosist

#Erro de estimacao: estratificada

erroest<-abs(mediaest-mediapop)
erroest


