# importa a base de dados
library(readxl)
consumo <- read_excel("./Consumo de carne.xlsx")
attach(consumo)

# grafico de dispersao preco X consumo de carne bovina
plot(
    preco_bovino_varejo, consumo_bovino, type='p', 
    main = "Preço e consumo de carne bovina no Reino Unido\n1975 - 2017",
    col = "red", pch = 16, cex = 0.8, ylim = c(0, 100), 
    xlab = "Preço médio no varejo (0,01￡/lb)",
    ylab = "Consumo anual por habitante (lb)"
)
dev.copy(png, "Graficos/grafico1.png")
dev.off() 

# calcula a correlacao de postos de Spearman entre preco e consumo
# de carne bovina
cor(preco_bovino_varejo, consumo_bovino, method="spearman")

# grafico de dispersao preco X consumo de carne de frango
plot(
    preco_frango_varejo, consumo_frango, type='p', 
    main = "Preço e consumo de carne de frango no Reino Unido\n1975 - 2017",
    col = "red", pch = 16, cex = 0.8, ylim = c(0, 100), 
    xlab = "Preço médio no varejo (0,01￡/lb)",
    ylab = "Consumo anual por habitante (lb)"
)
dev.copy(png, "Graficos/grafico2.png")
dev.off() 

# calcula a correlacao de postos de Spearman entre preco e consumo
# de carne de frango
cor(preco_frango_varejo, consumo_frango, method="spearman")

#
consumo$preco_bovinoXfrango <- preco_bovino_varejo / preco_frango_varejo
consumo$consumo_bovinoXfrango <- consumo_bovino / consumo_frango
plot(
    consumo$preco_bovinoXfrango, consumo$consumo_bovinoXfrango, type='p', 
    main = "Razões entre o preço e o consumo de carne\nbovina e de frango no Reino Unido\n1975 - 2017",
    col = "red", pch = 16, cex = 0.8, #ylim = c(0, 100), 
    xlab = "Razão entre os preços da carnes bovina e de frango",
    ylab = "Razão entre o consumo de carnes bovina e de frango"
)
dev.copy(png, "Graficos/grafico3.png")
dev.off() 
cor(consumo$consumo_bovinoXfrango, consumo$preco_bovinoXfrango, 
    method = "spearman")

library(dplyr)
consumo_1993_ <- filter(consumo, ano>1992)
plot(
    consumo_1993_$preco_bovinoXfrango, consumo_1993_$consumo_bovinoXfrango, type='p', 
    main = "Razões entre o preço e o consumo de carne\nbovina e de frango no Reino Unido\n1993 - 2017",
    col = "red", pch = 16, cex = 0.8, #ylim = c(0, 100), 
    xlab = "Razão entre os preços da carnes bovina e de frango",
    ylab = "Razão entre o consumo de carnes bovina e de frango"
)
dev.copy(png, "Graficos/grafico4.png")
dev.off() 
cor(consumo_1993_$consumo_bovinoXfrango, consumo_1993_$preco_bovinoXfrango, 
    method = "spearman")
cor(consumo_1993_$consumo_bovinoXfrango, consumo_1993_$preco_bovinoXfrango, 
    method = "pearson")
