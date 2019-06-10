# importa a base de dados
library(readxl)
consumo <- read_excel("./Consumo de carne.xlsx")
attach(consumo)

# formata a coluna 'ano' como data
ano <- as.Date.character(ano, '%Y')
ano <- format(ano, '%Y')

# divide a janela de graficos horizontalmente
par(mfrow=c(1,2)) # comentar quando for plotar o grafico 1, descomentar para grafico 2

# plota a serie historica de consumo em um grafico de linhas
plot(
    ano, consumo_bovino, type='l', 
    main = "Consumo de carnes no Reino Unido\n1975 - 2017",
    col = "red", lwd = 2, ylim = c(0, 100), 
    ylab = "Consumo anual por habitante (lb)", 
    xlab = "Ano"
)
lines(ano, consumo_frango, col = "blue", lwd = 2)
legend(
    1980, 30, legend=c("Carne bovina", "Carne de frango"), 
    col=c("red", "blue"), lwd=2, cex = 0.9
)
#dev.copy(png, "Graficos/grafico1.png") # comentar para plotar grafico 2
#dev.off() # comentar para plotar grafico 2

# plota a serie historica de precos no varejo em um grafico de linhas
plot(
    ano, preco_bovino_varejo, type='l', 
    main = "Preço das carnes no Reino Unido\n1975 - 2017",
    col = "red", lwd = 2, ylim = c(0, 650), 
    ylab = "Preço médio anual (0,01￡/lb)", 
    xlab = "Ano"
)
lines(ano, preco_frango_varejo, col = "blue", lwd = 2)
legend(
    1975, 650, legend=c("Carne bovina", "Carne de frango"), 
    col=c("red", "blue"), lwd=2, cex = 0.9
)
dev.copy(png, "Graficos/grafico2.png", width = 800)
dev.off() 

# volta a mostrar apenas um grafico na tela
par(mfrow=c(1,1))

###############################################################################

# segunda parte: mesmos gráficos, mas  em euros e quilogramas

# altera as unidades de medida
consumo_bovino_kg <- consumo_bovino * 0.453592
consumo_frango_kg <- consumo_frango * 0.453592
preco_bovino_varejo_eur <- preco_bovino_varejo * 0.0114 / 0.453592 # 1 GBP = 1,14EUR
preco_frango_varejo_eur <- preco_frango_varejo * 0.0114 / 0.453592 # em 14/10/2018

# divide a janela de graficos horizontalmente
par(mfrow=c(1,2)) # comentar quando for plotar o grafico 3, descomentar para grafico 4

# plota a serie historica de consumo em um grafico de linhas
plot(
    ano, consumo_bovino_kg, type='l', 
    main = "Consumo de carnes no Reino Unido\n1975 - 2017",
    col = "red", lwd = 2, ylim = c(0, 50), 
    ylab = "Consumo anual por habitante (kg)", 
    xlab = "Ano"
)
lines(ano, consumo_frango_kg, col = "blue", lwd = 2)
legend(
    1992, 10, legend=c("Carne bovina", "Carne de frango"), 
    col=c("red", "blue"), lwd=2, cex = 0.9, bty = 'n'
)
#dev.copy(png, "Graficos/grafico3.png") # comentar para plotar grafico 4
#dev.off() # comentar para plotar grafico 4

# plota a serie historica de precos no varejo em um grafico de linhas
plot(
    ano, preco_bovino_varejo_eur, type='l', 
    main = "Preço das carnes no Reino Unido\n1975 - 2017",
    col = "red", lwd = 2, ylim = c(0, 20), 
    ylab = "Preço médio anual (EUR/kg)", 
    xlab = "Ano"
)
lines(ano, preco_frango_varejo_eur, col = "blue", lwd = 2)
legend(
    1975, 20, legend=c("Carne bovina", "Carne de frango"), 
    col=c("red", "blue"), lwd=2, cex = 0.9,bty = 'n'
)
dev.copy(png, "Graficos/grafico4.png", width = 800)
dev.off()

# volta a mostrar apenas um grafico na tela
par(mfrow=c(1,1))

###############################################################################

# algumas analises exploratorias dos dados
rel_precos <- as.data.frame(preco_bovino_varejo / preco_frango_varejo, row.names = ano)
rel_precos <- sapply(rel_precos, as.numeric)
rel_precos$ano <- ano

mean(
    subset(
        rel_precos, 
        ano >= 1975 & ano <= 2006, 
        select= 'preco_bovino_varejo/preco_frango_varejo'
    )
)
mean(
    subset(
        rel_precos, 
        ano >= 1975 & ano <= 2009, 
        select= 'preco_bovino_varejo/preco_frango_varejo'
    )
)

###############################################################################
