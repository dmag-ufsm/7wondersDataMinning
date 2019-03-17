#Lê dados contidos em um CSV e transforma para um formato de Lista de Presença
#Utilizar com algoritmo apriori 

options(max.print = 10000000)

#Caminho da pasta de trabalho 
setwd('C:/Users/gabri/OneDrive/Documentos/Projeto - DM/')

#Carrega os dados 
data <- read.csv('Dados coletados/7wonders.csv', header = TRUE)

#Nome de todas as colunas 
colmap <- colnames(data)

#Nome de todas as linhas 
rowmap <- data$Game.result

#Remove o espaço no nome das colunas
rowmap <- data$Game.result 
rowmap <- gsub(" ", "_", rowmap)

#Filtros para as colunas(por ranking)

#Filtro para os ganhadores 
filter_1st <- grepl("^X1st.*$", colmap)
  
  #filter_2nd <- grepl("^X2nd.*$", x)
  #filter_3rd <- grepl("^X3rd.*$", colmap)
  #filter_4th <- grepl("^X4th.*$", colmap)
  #filter_5th <- grepl("^X5th.*$", colmap)
  #filter_6th <- grepl("^X6th.*$", colmap)
  #filter_7th <- grepl("^X7th.*$", colmap)

#Filtro para os não ganhadores
column_params <- c("^X2nd.*$", "^X3rd.*$", "^X4th.*$", "^X5th.*$", "^X6th.*$", "^X7th.*$")
filter_nowinners <- grepl(paste(column_params, collapse = "|"), colmap)

#Aplicação do filtro nos dados para retornar valores de interesse 
data_1st <- data[filter_1st]
rownames(data_1st) <- data$Game.result

data_nowinners <- data[filter_nowinners]
rownames(data_nowinners) <- data$Game.result

#Matriz transposta e transformação em data.frame
data_1st_t <- as.data.frame(t(data_1st))
colnames(data_1st_t) <- rowmap

data_nowinners_t <- as.data.frame(t(data_nowinners))
colnames(data_nowinners_t) <- rowmap

#Preparação dos dados para associação

#colunas de interesse
head <- c("civil", "cientifica", "guilda", "militar",  "comercial", "materia_prima", "manufatura", "wonder_sideA", "resultado")

#matriz de presença
data_1st_list <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
colnames(data_1st_list) <- head

data_nowinners_list <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
colnames(data_nowinners_list) <- head

#discretização e carga dos dados para a matriz de presença

#param: com isso podemos associar a quantidade de cartas jogadas com algo 
#exemplo: se param = 2, todos os jogadores que baixaram 2 cartas de determinada classe recebem 1 na coluna correspondente
param <- 2 

#aqui como só possuímos os dados dos ganhadores então todos recebem 1 nessa coluna (resultado == ganhador)
#os demais receberiam 0 
data_1st_list[, "resultado"] <- 1
data_1st_list$civil <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= param)
data_1st_list$cientifica <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= param)
data_1st_list$guilda <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= param)
data_1st_list$militar <- as.integer(as.integer(as.character(data_1st_t$Military_Structures)) >= param)
data_1st_list$comercial <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= param)
data_1st_list$materia_prima <- as.integer(as.integer(as.character(data_1st_t$Raw_Materials)) >= param)
data_1st_list$manufatura <- as.integer(as.integer(as.character(data_1st_t$Manufactured_Goods)) >= param)
data_1st_list$wonder_sideA <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list[, "resultado"] <- 0
data_nowinners_list$civil <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= param)
data_nowinners_list$cientifica <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= param)
data_nowinners_list$guilda <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= param)
data_nowinners_list$militar <- as.integer(as.integer(as.character(data_nowinners_t$Military_Structures)) >= param)
data_nowinners_list$comercial <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= param)
data_nowinners_list$materia_prima <- as.integer(as.integer(as.character(data_nowinners_t$Raw_Materials)) >= param)
data_nowinners_list$manufatura <- as.integer(as.integer(as.character(data_nowinners_t$Manufactured_Goods)) >= param)
data_nowinners_list$wonder_sideA <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist <- rbind(data_1st_list, data_nowinners_list)

write.csv(full_datalist, "7wonders_assoc.csv")

library(arules)

rules <- apriori(full_datalist, parameter = list(target = "rules", conf = 0.5, supp = 0.5, minlen = 2))
inspect(rules)

#regras com saída resultado
sub <- subset(rules, (rhs %in% resultado))









