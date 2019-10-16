#Lê dados contidos em um CSV e transforma para um formato de Lista de Presença
#Utilizar com algoritmo apriori 

options(max.print = 10000000)

#Caminho da pasta de trabalho 
setwd('C:/Users/LanaR/Documents/faculdade/projeto')

#Carrega os dados 
data <- read.csv('7wonders7players.csv', header = TRUE, sep = ';')

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

head <- c("n", "n", "n", "n", "n","n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "Wonder_side_A", "n", "Civilian_Structures", "Scientific_Structures", "Guilds", "Military_Structures", "Commercial_Structures", "Raw_Materials", "Manufactured_Goods")

#Matriz transposta e transformação em data.frame
data_1st_t <- as.data.frame(t(data_1st))
colnames(data_1st_t) <- head

data_nowinners_t <- as.data.frame(t(data_nowinners))
colnames(data_nowinners_t) <- head

#Preparação dos dados para associação

#colunas de interesse
head <- c("civil", "cientifica", "guilda", "militar",  "comercial", "materia_prima", "manufatura", "wonder_sideA", "resultado")

#matriz de presença
data_1st_list_m <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
data_1st_list_1 <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
data_1st_list_3 <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
colnames(data_1st_list_m) <- head
colnames(data_1st_list_1) <- head
colnames(data_1st_list_3) <- head

data_nowinners_list_m <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
data_nowinners_list_1 <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
data_nowinners_list_3 <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
colnames(data_nowinners_list_m) <- head
colnames(data_nowinners_list_1) <- head
colnames(data_nowinners_list_3) <- head

#medias das colunas
mediaCivil = mean(as.integer(as.character(data_1st_t$Civilian_Structures)))
mediaCientifica = mean(as.integer(as.character(data_1st_t$Scientific_Structures)))
mediaGuilda = mean(as.integer(as.character(data_1st_t$Guilds)))
mediaMilitar = mean(as.integer(as.character(data_1st_t$Military_Structures)))
mediaComercial = mean(as.integer(as.character(data_1st_t$Commercial_Structures)))
mediaMateriaPrima = mean(as.integer(as.character(data_1st_t$Raw_Materials)))
mediaManufatura = mean(as.integer(as.character(data_1st_t$Manufactured_Goods)))

#1 quartil das colunas
quartil1Civil = summary(as.integer(as.character(data_1st_t$Civilian_Structures)))[[2]]
quartil1Cientifica = summary(as.integer(as.character(data_1st_t$Scientific_Structures)))[[2]]
quartil1Guilda = summary(as.integer(as.character(data_1st_t$Guilds)))[[2]]
quartil1Militar = summary(as.integer(as.character(data_1st_t$Military_Structures)))[[2]]
quartil1Comercial = summary(as.integer(as.character(data_1st_t$Commercial_Structures)))[[2]]
quartil1MateriaPrima = summary(as.integer(as.character(data_1st_t$Raw_Materials)))[[2]]
quartil1Manufatura = summary(as.integer(as.character(data_1st_t$Manufactured_Goods)))[[2]]

#3 quartil das colunas
quartil3Civil = summary(as.integer(as.character(data_1st_t$Civilian_Structures)))[[5]]
quartil3Cientifica = summary(as.integer(as.character(data_1st_t$Scientific_Structures)))[[5]]
quartil3Guilda = summary(as.integer(as.character(data_1st_t$Guilds)))[[5]]
quartil3Militar = summary(as.integer(as.character(data_1st_t$Military_Structures)))[[5]]
quartil3Comercial = summary(as.integer(as.character(data_1st_t$Commercial_Structures)))[[5]]
quartil3MateriaPrima = summary(as.integer(as.character(data_1st_t$Raw_Materials)))[[5]]
quartil3Manufatura = summary(as.integer(as.character(data_1st_t$Manufactured_Goods)))[[5]]

#discretização e carga dos dados para a matriz de presença

#param: com isso podemos associar a quantidade de cartas jogadas com algo 
#exemplo: se param = 2, todos os jogadores que baixaram 2 cartas de determinada classe recebem 1 na coluna correspondente
param <- 1

#aqui como só possuímos os dados dos ganhadores então todos recebem 1 nessa coluna (resultado == ganhador)
#os demais receberiam 0 

#media
data_1st_list_m[, "resultado"] <- 1
data_1st_list_m$civil <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= mediaCivil)
data_1st_list_m$cientifica <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= mediaCientifica)
data_1st_list_m$guilda <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= mediaGuilda)
data_1st_list_m$militar <- as.integer(as.integer(as.character(data_1st_t$Military_Structures)) >= mediaMilitar)
data_1st_list_m$comercial <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= mediaComercial)
data_1st_list_m$materia_prima <- as.integer(as.integer(as.character(data_1st_t$Raw_Materials)) >= mediaMateriaPrima)
data_1st_list_m$manufatura <- as.integer(as.integer(as.character(data_1st_t$Manufactured_Goods)) >= mediaManufatura)
data_1st_list_m$wonder_sideA <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_m[, "resultado"] <- 0
data_nowinners_list_m$civil <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= mediaCivil)
data_nowinners_list_m$cientifica <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= mediaCientifica)
data_nowinners_list_m$guilda <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= mediaGuilda)
data_nowinners_list_m$militar <- as.integer(as.integer(as.character(data_nowinners_t$Military_Structures)) >= mediaMilitar)
data_nowinners_list_m$comercial <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= mediaComercial)
data_nowinners_list_m$materia_prima <- as.integer(as.integer(as.character(data_nowinners_t$Raw_Materials)) >= mediaMateriaPrima)
data_nowinners_list_m$manufatura <- as.integer(as.integer(as.character(data_nowinners_t$Manufactured_Goods)) >= mediaManufatura)
data_nowinners_list_m$wonder_sideA <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_media <- rbind(data_1st_list_m, data_nowinners_list_m)

#1 quartil
data_1st_list_1[, "resultado"] <- 1
data_1st_list_1$civil <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= quartil1Civil)
data_1st_list_1$cientifica <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= quartil1Cientifica)
data_1st_list_1$guilda <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= quartil1Guilda)
data_1st_list_1$militar <- as.integer(as.integer(as.character(data_1st_t$Military_Structures)) >= quartil1Militar)
data_1st_list_1$comercial <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= quartil1Comercial)
data_1st_list_1$materia_prima <- as.integer(as.integer(as.character(data_1st_t$Raw_Materials)) >= quartil1MateriaPrima)
data_1st_list_1$manufatura <- as.integer(as.integer(as.character(data_1st_t$Manufactured_Goods)) >= quartil1Manufatura)
data_1st_list_1$wonder_sideA <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_1[, "resultado"] <- 0
data_nowinners_list_1$civil <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= quartil1Civil)
data_nowinners_list_1$cientifica <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= quartil1Cientifica)
data_nowinners_list_1$guilda <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= quartil1Guilda)
data_nowinners_list_1$militar <- as.integer(as.integer(as.character(data_nowinners_t$Military_Structures)) >= quartil1Militar)
data_nowinners_list_1$comercial <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= quartil1Comercial)
data_nowinners_list_1$materia_prima <- as.integer(as.integer(as.character(data_nowinners_t$Raw_Materials)) >= quartil1MateriaPrima)
data_nowinners_list_1$manufatura <- as.integer(as.integer(as.character(data_nowinners_t$Manufactured_Goods)) >= quartil1Manufatura)
data_nowinners_list_1$wonder_sideA <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_1quartil <- rbind(data_1st_list_1, data_nowinners_list_1)

#3 quartil
data_1st_list_3[, "resultado"] <- 1
data_1st_list_3$civil <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= quartil3Civil)
data_1st_list_3$cientifica <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= quartil3Cientifica)
data_1st_list_3$guilda <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= quartil3Guilda)
data_1st_list_3$militar <- as.integer(as.integer(as.character(data_1st_t$Military_Structures)) >= quartil3Militar)
data_1st_list_3$comercial <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= quartil3Comercial)
data_1st_list_3$materia_prima <- as.integer(as.integer(as.character(data_1st_t$Raw_Materials)) >= quartil3MateriaPrima)
data_1st_list_3$manufatura <- as.integer(as.integer(as.character(data_1st_t$Manufactured_Goods)) >= quartil3Manufatura)
data_1st_list_3$wonder_sideA <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_3[, "resultado"] <- 0
data_nowinners_list_3$civil <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= quartil3Civil)
data_nowinners_list_3$cientifica <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= quartil3Cientifica)
data_nowinners_list_3$guilda <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= quartil3Guilda)
data_nowinners_list_3$militar <- as.integer(as.integer(as.character(data_nowinners_t$Military_Structures)) >= quartil3Militar)
data_nowinners_list_3$comercial <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= quartil3Comercial)
data_nowinners_list_3$materia_prima <- as.integer(as.integer(as.character(data_nowinners_t$Raw_Materials)) >= quartil3MateriaPrima)
data_nowinners_list_3$manufatura <- as.integer(as.integer(as.character(data_nowinners_t$Manufactured_Goods)) >= quartil3Manufatura)
data_nowinners_list_3$wonder_sideA <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_3quartil <- rbind(data_1st_list_3, data_nowinners_list_3)

write.csv(full_datalist_media, "7wonders_assoc_media.csv")
write.csv(full_datalist_1quartil, "7wonders_assoc_1quartil.csv")
write.csv(full_datalist_3quartil, "7wonders_assoc_3quartil.csv")

library(arules)

for(i in 1:9){
  full_datalist_media[,i] <- factor(full_datalist_media[,i])
  full_datalist_1quartil[,i] <- factor(full_datalist_1quartil[,i])
  full_datalist_3quartil[,i] <- factor(full_datalist_3quartil[,i])
}

rules_media <- apriori(full_datalist_media, parameter = list(target = "rules", conf = 0.5, supp = 0.5, minlen = 2))
rules_1quartil <- apriori(full_datalist_1quartil, parameter = list(target = "rules", conf = 0.5, supp = 0.5, minlen = 2))
rules_3quartil <- apriori(full_datalist_3quartil, parameter = list(target = "rules", conf = 0.5, supp = 0.5, minlen = 2))

rules_sub_media <- subset(rules_media, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")
rules_sub_1quartil <- subset(rules_1quartil, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")
rules_sub_3quartil <- subset(rules_3quartil, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")

write(rules_media, file = "rules_media_7layers.csv", sep = ';', quote = FALSE, row.names = FALSE)
write(rules_1quartil, file = "rules_1quartil_7players.csv", sep = ';', quote = FALSE, row.names = FALSE)
write(rules_3quartil, file = "rules_3quartil_7players.csv", sep = ';', quote = FALSE, row.names = FALSE)

#write(rules_sub_media, file = "rules_sub_media_7players.csv", sep = ';', quote = FALSE, row.names = FALSE)
#write(rules_sub_1quartil, file = "rules_sub_1quartil_7players.csv", sep = ';', quote = FALSE, row.names = FALSE)
#write(rules_sub_3quartil, file = "rules_sub_3quartil_7players.csv", sep = ';', quote = FALSE, row.names = FALSE)
