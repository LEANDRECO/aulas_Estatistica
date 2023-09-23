
Dataset <- 
  readXL("/home/2022.1.08.011/Documents/Estatistica_Basica/Aulas_Estatistica/aula4/Dados_Aula_Pratica_Amostragem.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Plan1", stringsAsFactors=TRUE)


#Tabela corrigida com o n�mero de casas decimais
summary(Dataset)

dados<-Dataset$Idade

tabela<-function(dados,cd)
{A<-max(dados)-min(dados)
n<-length(dados) 
if(n<=100)
{k<-ceiling(sqrt(n))}else
{k<-ceiling(5*log10(n))}
C<-round(A/(k-1),cd)
LI<-c(rep(0,(k+1)))
media<-c(NA)
LI[1]<-round(min(dados)-(C)/2,cd)
for(i in 2:(k+1))
{LI[i]<-round(LI[i-1]+C,cd)
media[i-1]<-mean(c(LI[i],LI[i-1]))
}
limites<-LI
TDF<-hist(dados,breaks=limites,plot=FALSE,right=FALSE)
tabela<-matrix(c(rep(6*k)),k,6)

for(i in 1:k)
{tabela[i,1]<-round(LI[i],cd)
tabela[i,2]<-round(LI[i+1],cd)
tabela[i,3]<-round(media[i],cd)
tabela[i,4]<-(TDF$counts[i])
tabela[i,5]<-round(((TDF$counts[i])/n),5)
tabela[i,6]<-round((100*TDF$counts[i])/n,3)
}

colnames(tabela)<-c("LI","LS","X","Fa","Fr","Fp")
return(tabela)

}

tabela(dados,2)


# 1 Qual porcentagem dos trabalhadores de TI possuem menos que 30 anos?

porcentagem<-tabela(dados,1)[,6]
porcentagem
sum(porcentagem[1:6])

# 2 Qual idade deixa, aproximadamente, 10%  dos trabalhadores  de TI acima dela?
sum(porcentagem[13:20])

#Histograma

quebras<-function(dados,cd)
{A<-max(dados)-min(dados)
n<-length(dados) 
if(n<=100)
{k<-ceiling(sqrt(n))}else
{k<-ceiling(5*log10(n))}
C<-round(A/(k-1),cd)

LI<-c(rep(0,(k+1)))
LI[1]<-round(min(dados)-(C)/2,cd)
for(i in 2:(k+1))
{LI[i]<-round(LI[i-1]+C,cd)
}
return(LI)
}



limites<-quebras(dados,3)
limites

#Construindo as informa��es Tabela de Distribui��o de Frequ�ncias

TDF<-hist(dados,breaks=limites,plot=FALSE,right=FALSE)
TDF

hist(dados,label=FALSE,main="",
xlab=expression(paste("Idade (Anos)")), 
ylab="Frequência absoluta", 
ylim=c(0,(max(TDF$counts)+1)),
breaks=limites,axes=FALSE,right=FALSE)
axis(1,at=limites,pos=c(0,0))
axis(2,at=c(seq(0:(max(TDF$counts)+1))-1))



