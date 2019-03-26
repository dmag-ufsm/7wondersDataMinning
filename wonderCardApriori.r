options(max.print = 10000000)

setwd('C:/Users/gabri/OneDrive/Área de Trabalho/7wondersDataMinning/')


data <- read.csv('7wonders6players.csv', header = TRUE, sep = ';')

colmap <- colnames(data)

rowmap <- data$Game.result

rowmap <- gsub(" ", "_", rowmap)

filter_1st <- grepl("^X1st.*$", colmap)


data_1st <- data[filter_1st]
rownames(data_1st) <- data$Game.result





data_1st_t <- as.data.frame(t(data_1st))
colnames(data_1st_t) <- rowmap


head <- c("science", "military", "guilds", "civilian", "Commercial", "RawMaterials", "ManufacturedGoods", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")

data_1st_list <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
colnames(data_1st_list) <- head

data_1st_list[,] <- 0 

data_1st_list$science <- toInteger(data_1st_t$Scientific_Structures)
data_1st_list$military <- toInteger(data_1st_t$Military_Structures)
data_1st_list$guilds <- toInteger(data_1st_t$Guilds)
data_1st_list$civilian <- toInteger(data_1st_t$Civilian_Structures)
data_1st_list$Commercial <- toInteger(data_1st_t$Commercial_Structures)
data_1st_list$RawMaterials <- toInteger(data_1st_t$Raw_Materials)
data_1st_list$ManufacturedGoods <- toInteger(data_1st_t$Manufactured_Goods)

toInteger<- function(column) {
  return (as.integer(as.character(column)))
}


for(i in 1: nrow(data_1st_t)) {
  data_1st_list[i, as.character(toInteger(data_1st_t[i, "Wonder_ID"]))] <- 1
}


maxIndex <- function(array) {
  temp <- -1
  index <- 0
  matches <- 0
  for(i in 1: length(array)) {
    if(array[i] > temp) {
      temp <- array[i]
      index <- i
    } 
  }
  
  for(i in 1: length(array)) {
    if(i != index && array[i] == temp) {
      index <- -1
      break
    } 
  }
  
  return(index)
}

for(i in 1: nrow(data_1st_list)) {
  index <- maxIndex(data_1st_list[i, c(1, 2, 3, 4, 5, 6, 7)])
  
  if(index == -1) {
    data_1st_list[i, c(1, 2, 3, 4, 5, 6, 7)] <- 0 
  } else {
    data_1st_list[i, index] <- 1
    for(j in 1: 7) {
      if(j != index) {
        data_1st_list[i, j] <- 0
      }
    }
  }
}


library(arules)

#full_datalist <- as.matrix(full_datalist)

targetData <- as.matrix(data_1st_list)


rules <- apriori(targetData, parameter = list(target = "rules", conf = 0.1, supp = 0.1, minlen = 2))
inspect(rules)

rules_df <- data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)),
  rules@quality
)

library(gridExtra)

png(filename = "m_rules_01.png", width = 100 * ncol(rules_df) , height = 50 * nrow(rules_df), bg = "white")
grid.table(rules_df)
dev.off()
