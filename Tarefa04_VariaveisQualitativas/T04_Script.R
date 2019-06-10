packages <- c("readxl", "xlsx", "epiDisplay", "DescTools")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg)
    }
}

# Carregar dataset
funcionarios <- read_excel("funcionarios.xlsx", 
                           sheet = "Sheet1", 
                           col_types = c("numeric", "text", "text", "numeric", 
                                         "numeric", "numeric", "text"), 
                           na = "-", skip = 1)
funcionarios[which(funcionarios$mês==0),] <- NA

### Análise Bivariada de dados qualitativos 
# Importe os dados do arquivo funcionarios.xlsx
# Tabela de contingência 
tabela <- table(funcionarios$`Estado civil`,funcionarios$`Grau de instrução`)

# Tabela para exportação 
# função as.data.frame é não útil neste caso 
tabela1 <- as.data.frame(
    table(funcionarios$`Estado civil`,funcionarios$`Grau de instrução`))
# possível solução 
tabela2 <- matrix(tabela[1:6], nrow = 2, ncol = 3)
colnames(tabela2) <- c("fundamental", "médio", "superior")
row.names(tabela2) <- c("casado", "solteiro")
# Exportar dados em formato xlsx
write.xlsx2(
    tabela2, file = "T04_TabelasContingencia.xlsx", 
    sheetName = "EstadoCivil_X_Instrucao")

# Utilizando a função tabpct
tabela3 <- tabpct(
    funcionarios$`Estado civil`, funcionarios$`Grau de instrução`, 
    ylab = "Grau de instrução", xlab = "Estado civil",
    main = "Tabela de contingência")

# apenas a tabela com o percentual por linha: percent = "row" ou "col" ou "both" 
# argumento: graph = T (com gráfico) ou F (sem gráfico)
tabpct(
    funcionarios$`Estado civil`,funcionarios$`Grau de instrução`, 
    percent = "row", graph = F)
tabpct(
    funcionarios$`Estado civil`,funcionarios$`Grau de instrução`, 
    percent = "col", graph = F)

### Exportar as tabelas para o arquivo T04_TabelasContingencia.xlsx
perc_linhas <- matrix(tabela3$table.row.percent, nrow = 2, ncol = 3)
colnames(perc_linhas) <- c("fundamental", "médio", "superior")
row.names(perc_linhas) <- c("casado", "solteiro")
write.xlsx2(
    perc_linhas, file = "T04_TabelasContingencia.xlsx", 
    sheetName = "Percentual_Linha", append = T)

perc_colunas <- matrix(tabela3$table.column.percent, nrow = 2, ncol = 3)
colnames(perc_colunas) <- c("fundamental", "médio", "superior")
row.names(perc_colunas) <- c("casado", "solteiro")
write.xlsx2(
    perc_colunas, file = "T04_TabelasContingencia.xlsx", 
    sheetName = "Percentual_Coluna", append = T)

### Gráfico de mosaico
mosaicplot(table(funcionarios$`Estado civil`, funcionarios$`Grau de instrução`), 
           col = c("green", "blue", "red"), main = "Gráfico de Mosaico")

# Qui-quadrado
q <- chisq.test(funcionarios$`Estado civil`,funcionarios$`Grau de instrução`)$statistic
q

# Coeficiente de contingência 
C <- ContCoef(funcionarios$`Estado civil`, funcionarios$`Grau de instrução`)
t <- 2 # t = min{linha, conlunas} = min{2,3} = 2
C_modificado <- C*sqrt(t/(t - 1)) 
C
C_modificado

