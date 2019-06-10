packages <- c(
    "readxl", "data.table", "mvShapiroTest", "Hotelling", "heplots", "MASS")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg)
    }
}

# importar bases
fsaude <- read_excel("FSAUDE.xls")
msaude <- read_excel("MSAUDE.xls")

# converter em data.tables
setDT(fsaude)
setDT(msaude)

# a) criar coluna "Sexo"
fsaude[, Sexo := "F"]
msaude[, Sexo := "M"]

# b) unir tabelas
fsaude[, `:=`(ID = Mulheres, Mulheres = NULL)]
msaude[, `:=`(ID = Homens, Homens = NULL)]
saude <- merge(fsaude, msaude, all = T)

# c) verificar normalidade multivariada
m1 <- as.matrix(saude[, -(Sexo:ID)])
mvShapiro.Test(m1) # p-Valor: 4.9e-14 (nao e normal multivariada)

# d) selecionar variaveis normais com qqnorm
par(mfrow = c(3,5))
for (variable in colnames(m1)) {
    values = m1[, variable]
    qqnorm(values, main = variable)
    qqline(values, col = "red")
    pV <- shapiro.test(values)$p.value
    text(x = -1.7, y = max(values), labels = round(pV, 3), cex = 1.3)
}
    # nao sao normais: Idade, CINT, TXPUL, SIST, COL, IMC e Perna

# apenas variaveis normais
m2 <- saude[, -c("Idade", "CINT", "TXPUL", "SIST", "COL", "IMC", "Perna", "ID")]
m2f <- as.matrix(m2[Sexo == "F", !c("Sexo")])
m2m <- as.matrix(m2[Sexo == "M", !c("Sexo")])

# e) Teste de Hotelling para igualdade dos vetores das medias
print(hotelling.test(x = m2f, y = m2m)) #p-Valor: 1,1e-16 (vetores das  medias sao diferentes)

# f) Teste BoxM para igualdade dos vetores de variancia e covariancia
boxM(m2[, !c("Sexo")], group = m2[,Sexo]) # p-Valor: 0,054 (nao eh possivel afirmar que as variancias
                                          # e covariancias diferem, com significancia de 0,05)

# g) analise discriminante
analysis <- lda(m2[, !c("Sexo")], group = m2[,Sexo], CV = T)
classif_table = xtabs(~ analysis$class + Sexo, data = m2)
diag(classif_table)/colSums(classif_table)
