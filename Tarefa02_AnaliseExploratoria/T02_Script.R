# carrega o banco de dados
library(readr)
poluicao <- read_csv("poluicao.csv", 
                    col_types = cols(DATA = col_date(format = "%d/%m/%Y")))

# define as quebras de intervalos de classe
quebras <- seq(0, 400, by=20)

# histograma para PM10
hist(poluicao$PM10/1000, main="Medições de partículas sólidas (1997)",
     xlab="Concentração de partículas sólidas (‰)", 
     ylab = "Frequência de medições", 
     breaks = quebras, col = "red", 
     xlim = c(0, 400), ylim=c(0,160))
dev.copy(png, "Graficos/hist1.png")
dev.off() 

# histograma para CO
hist(poluicao$co/1000, main="Medições de monóxido de carbono (1997)",
     xlab="Concentração de monóxido de carbono (‰)", 
     ylab = "Frequência de medições", 
     breaks = quebras, col = "red", 
     xlim=c(0,400), ylim=c(0,160))
dev.copy(png, "Graficos/hist2.png")
dev.off() 

# histograma para NO2
hist(poluicao$no2/1000, main="Medições de dióxido de nitrogênio (1997)",
     xlab="Concentração de dióxido de nitrogênio (‰)", 
     ylab = "Frequência de medições", 
     breaks = quebras, col = "red", 
     xlim=c(0,400), ylim=c(0,160))
dev.copy(png, "Graficos/hist3.png")
dev.off() 

# gera uma tabela de frequencia para PM10, CO e NO2
# ( adaptado de https://stackoverflow.com/questions/27839432 )
intervalos <- paste(head(quebras,-1), quebras[-1], sep=" - ")
freq_PM10 <- hist(poluicao$PM10/1000, breaks=quebras, include.lowest=TRUE, plot=FALSE)
freq_CO <- hist(poluicao$co/1000, breaks=quebras, include.lowest=TRUE, plot=FALSE)
freq_NO2 <- hist(poluicao$no2/1000, breaks=quebras, include.lowest=TRUE, plot=FALSE)
tabela1 <- data.frame(Observacoes = intervalos, 'PM10x10^(-3)' = freq_PM10$counts, 
                'COx10^(-3)' = freq_CO$counts, 'NO2x10^(-3)' = freq_NO2$counts)
write.csv(tabela1, "tabela1.csv")

# gera serie temporal das observacoes de PM10
plot(poluicao$DATA, poluicao$PM10/1000, type = "l",
     main="Medições de partículas sólidas (1997)",
     xlab = "Data", ylab = "Concentração (‰)", 
     ylim=c(0,400), col = "red", lwd = 2)
dev.copy(png, "Graficos/PM10.png")
dev.off() 

# gera serie temporal das observacoes de co
plot(poluicao$DATA, poluicao$co/1000, type = "l",
     main="Medições de monóxido de carbono (1997)",
     xlab = "Data", ylab = "Concentração (‰)", 
     ylim=c(0,400), col = "red", lwd = 2)
dev.copy(png, "Graficos/CO.png")
dev.off() 

# gera serie temporal das observacoes de no2
plot(poluicao$DATA, poluicao$no2/1000, type = "l",
     main="Medições de dióxido de nitrogênio (1997)",
     xlab = "Data", ylab = "Concentração (‰)", 
     ylim=c(0,400), col = "red", lwd = 2)
dev.copy(png, "Graficos/NO2.png")
dev.off() 

# cria colunas de semana e mes
library(lubridate)
poluicao$semana <- week(poluicao$DATA)
poluicao$mes <- month(poluicao$DATA)

# agrupa as observacoes por medias semanais
library(dplyr)
poluicao_semana <- group_by(poluicao, semana)
poluicao_semana <- summarize(poluicao_semana, PM10 = mean(PM10),
                        co = mean(co), no2 = mean(no2))

# agrupa as observacoes por medias mensais
poluicao_mes <- group_by(poluicao, mes)
poluicao_mes <- summarize(poluicao_mes, PM10 = mean(PM10),
                             co = mean(co), no2 = mean(no2))

# gera serie temporal das medias semanais de PM10
plot(poluicao_semana$semana, poluicao_semana$PM10/1000, type = "l",
     main="Medições de partículas sólidas (1997)\nMédias semanais",
     xlab = "Semana", ylab = "Concentração (‰)", 
     ylim=c(0,200), col = "red", lwd = 2)
dev.copy(png, "Graficos/PM10_semana.png")
dev.off() 

# gera serie temporal das medias semanais de co
plot(poluicao_semana$semana, poluicao_semana$co/1000, type = "l",
     main="Medições de monóxido de carbono (1997)\nMédias semanais",
     xlab = "Semana", ylab = "Concentração (‰)", 
     ylim=c(0,200), col = "red", lwd = 2)
dev.copy(png, "Graficos/CO_semana.png")
dev.off() 

# gera serie temporal das medias semanais de co
plot(poluicao_semana$semana, poluicao_semana$no2/1000, type = "l",
     main="Medições de dióxido de nitrogênio (1997)\nMédias semanais",
     xlab = "Semana", ylab = "Concentração (‰)", 
     ylim=c(0,200), col = "red", lwd = 2)
dev.copy(png, "Graficos/NO2_semana.png")
dev.off() 

# gera serie temporal das medias mensais de PM10
plot(poluicao_mes$mes, poluicao_mes$PM10/1000, type = "l",
     main="Medições de partículas sólidas (1997)\nMédias mensais",
     xlab = "Mês", ylab = "Concentração (‰)", 
     ylim=c(0,140), col = "red", lwd = 2)
dev.copy(png, "Graficos/PM10_mes.png")
dev.off() 

# gera serie temporal das medias mensais de co
plot(poluicao_mes$mes, poluicao_mes$co/1000, type = "l",
     main="Medições de monóxido de carbono (1997)\nMédias mensais",
     xlab = "Mês", ylab = "Concentração (‰)", 
     ylim=c(0,140), col = "red", lwd = 2)
dev.copy(png, "Graficos/CO_mes.png")
dev.off() 

# gera serie temporal das medias mensais de co
plot(poluicao_mes$mes, poluicao_mes$no2/1000, type = "l",
     main="Medições de dióxido de nitrogênio (1997)\nMédias mensais",
     xlab = "Mês", ylab = "Concentração (‰)", 
     ylim=c(0,140), col = "red", lwd = 2)
dev.copy(png, "Graficos/NO2_mes.png")
dev.off() 

# gera serie temporal das medias semanais de PM10, co e no2 no mesmo grafico
plot(poluicao_semana$semana, poluicao_semana$no2/1000, type = "n",
     main="Medições de poluentes (1997)\nMédias semanais",
     xlab = "Semana", ylab = "Concentração (‰)", 
     ylim=c(0,200))
lines(poluicao_semana$semana, poluicao_semana$PM10/1000, lwd=2, col = "red")
lines(poluicao_semana$semana, poluicao_semana$co/1000, lwd=2, col = "brown")
lines(poluicao_semana$semana, poluicao_semana$no2/1000, lwd=2, col = "purple")
legend(
    -0.5, 204, legend=c("Partículas sólidas", "Monóxido de carbono",
                       "Dióxido de nitrogênio"), 
    col=c("red", "brown", "purple"), lwd=2, cex = 0.9
)
dev.copy(png, "Graficos/poluentes_semana.png")
dev.off() 

# gera serie temporal das medias mensais de PM10, co e no2 no mesmo grafico
plot(poluicao_mes$mes, poluicao_mes$no2/1000, type = "n",
     main="Medições de poluentes (1997)\nMédias mensais",
     xlab = "Mês", ylab = "Concentração (‰)", 
     ylim=c(0,160))
lines(poluicao_mes$mes, poluicao_mes$PM10/1000, lwd=2, col = "red")
lines(poluicao_mes$mes, poluicao_mes$co/1000, lwd=2, col = "brown")
lines(poluicao_mes$mes, poluicao_mes$no2/1000, lwd=2, col = "purple")
legend(
    0.65, 164, legend=c("Partículas sólidas", "Monóxido de carbono",
                        "Dióxido de nitrogênio"), 
    col=c("red", "brown", "purple"), lwd=2, cex = 0.9
)
dev.copy(png, "Graficos/poluentes_mes.png")
dev.off() 
