setwd('D:\Desktop\Faculdade\TCC\R')
getwd()

install.packages('Amelia')
install.packages('caret')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('reshape')
install.packages('randomForest')
install.packages('e1071')

library(Amelia)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
library(e1071)
library(caret)

dados_clientes <- read.csv('D:/Desktop/Faculdade/TCC/R/dados/dataset.csv')

View(dados_clientes)
dim(dados_clientes)
str(dados_clientes)
summary(dados_clientes)

#removendo a primeira coluna
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

#renomeando a coluna classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- 'inadimplente'
View(dados_clientes)

#verificar valores ausentes
sapply(dados_clientes,function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes,main='valores missing')
dados_clientes <- na.omit(dados_clientes)


#renomear 
colnames(dados_clientes)
colnames(dados_clientes)[2]<- 'genero'
colnames(dados_clientes)[3]<- 'escolaridade'
colnames(dados_clientes)[4]<- 'estado_civil'
colnames(dados_clientes)[5]<- 'idade'
colnames(dados_clientes)
View(dados_clientes)

#alterando genero
View(dados_clientes$genero)
str(dados_clientes$genero)
summary(dados_clientes$genero)
?cut
dados_clientes$genero <- cut(dados_clientes$genero, c(0,1,2),labels = c('masculino','feminino'))

View(dados_clientes$genero)
str(dados_clientes$genero)
summary(dados_clientes$genero)

#alterando escolaridade

View(dados_clientes$escolaridade)
str(dados_clientes$escolaridade)
summary(dados_clientes$escolaridade)
?cut
dados_clientes$escolaridade <- cut(dados_clientes$escolaridade, c(0,1,2,3,4),labels = c('Pos graduado','Graduado','Ensino medio','Outros'))

View(dados_clientes$escolaridade)
str(dados_clientes$escolaridade)
summary(dados_clientes$escolaridade)

#alterando estado civil

View(dados_clientes$estado_civil)
str(dados_clientes$estado_civil)
summary(dados_clientes$estado_civil)
?cut
dados_clientes$estado_civil <- cut(dados_clientes$estado_civil, c(-1,0,1,2,3),labels = c('Desconhecido','Casado','Solteiro','Outro'))

View(dados_clientes$estado_civil)
str(dados_clientes$estado_civil)
summary(dados_clientes$estado_civil)

#converter idades
str(dados_clientes$idade)
summary(dados_clientes$idade)
hist(dados_clientes$idade)
dados_clientes$idade<- cut(dados_clientes$idade,c(0,30,50,100),labels = c('jovem','adulto','idoso'))

View(dados_clientes$idade)
str(dados_clientes$idade)

#converter os pagamentos para tipo fator
dados_clientes$PAY_0 <-as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <-as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <-as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <-as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <-as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <-as.factor(dados_clientes$PAY_6)

str(dados_clientes)
sapply(dados_clientes,function(x) sum(is.na(x)))
missmap(dados_clientes,main = 'valores missing')
#removeu os registros
dados_clientes<- na.omit(dados_clientes)
missmap(dados_clientes,main = 'valores missing')
dim(dados_clientes)

View(dados_clientes)

#alterando a variavel dependente alvo 
colnames(dados_clientes)
dados_clientes$inadimplente<- as.factor(dados_clientes$inadimplente)
str(dados_clientes$inadimplente)
View(dados_clientes)
str(dados_clientes$inadimplente)

#checando classes

#relação de inadimplentes
table(dados_clientes$inadimplente)
#proporção entre as classes
prop.table(table(dados_clientes$inadimplente))
#plot da distribuição
qplot(inadimplente,data=dados_clientes,geom='bar')+theme(axis.text.x=element_text(angle=90,hjust=1))

#set seed
set.seed(12345)

#amostragem estratificada 
#seleciona as linhas de acordo com a variavel inadimplente como strata
?createDataPartition
indice<- createDataPartition(dados_clientes$inadimplente,p=0.75,list=FALSE)
dim(indice)

#definimos os dados de treinamento como subconjunto do conjunto original
dados_treino <- dados_clientes[indice,]
table(dados_treino$inadimplente)

#comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)),prop.table(table(dados_clientes$inadimplente)))
colnames(compara_dados)<- c('treinamento','original')
compara_dados

#assim as proporções ficam iguais
dados_teste <-dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)



#primeiro modelo ponto representa todas as variaveis
?randomForest
modelo_v1 <- randomForest(inadimplente~.,data = dados_treino)
modelo_v1
#agora temos o modelo
plot(modelo_v1)

#fazendo previsoes
previsoes_v1 <- predict(modelo_v1, dados_teste)
#confusionmatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$inadimplente, positive='1')
cm_v1
#calculando precision, recall e F1score 
y <- dados_teste$inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1,y)
precision

recall <- sensitivity(y_pred_v1,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1


#importancia das variaveis preditoras
varImpPlot(modelo_v1)
imp_var <- importance(modelo_v1)
varImportance <- data.frame(Variables = row.names(imp_var),Importance=round(imp_var[,"MeanDecreaseGini"],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
ggplot(rankImportance, 
       aes(x = reorder(Variables, Importance), 
           y = Importance, 
           fill = Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), 
            hjust = 0, 
            vjust = 0.55, 
            size = 4, 
            colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 


#construindo um novo modelo 
colnames(dados_treino)
modelo_v2 <- randomForest(inadimplente~PAY_0+PAY_2+PAY_3+PAY_5+BILL_AMT1,data = dados_treino)
modelo_v2


#agora temos o modelo
plot(modelo_v2)

#fazendo previsoes
previsoes_v2 <- predict(modelo_v2, dados_teste)
#confusionmatrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$inadimplente, positive='1')
cm_v2
#calculando precision, recall e F1score 
y <- dados_teste$inadimplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2,y)
precision

recall <- sensitivity(y_pred_v2,y)
recall

F2 <- (2*precision*recall)/(precision+recall)
F2


#salvando o modelo
saveRDS(modelo_v1,file ='D:/Desktop/Faculdade/TCC/R/modelos/modelo_v1.rds')


#fazendo previsões com o modelo 

#dados
PAY_0 <- c(0, 0, 0) 
PAY_2 <- c(0, 0, 0) 
PAY_3 <- c(1, 0, 0) 
PAY_AMT1 <- c(1100, 1000, 1200) 
PAY_AMT2 <- c(1500, 1300, 1150) 
PAY_5 <- c(0, 0, 0) 
BILL_AMT1 <- c(350, 420, 280) 

#concatenar os novos clientes

novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)



#convertendo
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino$PAY_5))
str(novos_clientes)

# Previsões
previsoes_novos_clientes <- predict(modelo_v2, novos_clientes)
str(previsoes_novos_clientes)





