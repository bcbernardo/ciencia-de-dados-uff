packages <- c("readxl", "sjstats")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(
            pkg, dependencies = TRUE, INSTALL_opts = c('--no-lock'))
        library(pkg)
    }
}

# Carregar dataset
funcionarios <- read_excel("funcionarios.xlsx", 
                           sheet = "Plan1", 
                           col_types = c("numeric", "text", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "text", "text"), 
                           na = "-", skip = 1)
funcionarios[which(funcionarios$mês == 0),] <- NA


# Correlacao entre variaveis qualitativas ordenadas ---------------------------

x <- factor(funcionarios$'Grau de Instrução')
levels(x) # fornece as categorias da variável 'Grau de Instrução' e sua ordenação

y <- factor(funcionarios$'Classe Social', levels = c("C", "B", "A"))
levels(y) # fornece as categorias da variável 'Classe Social' e sua ordenação.

x <- as.numeric(x) # transforma x (grau de instrucao) em valor numérico
y <- as.numeric(y) # transforma y (classe social) em valor numérico

cor(x,y,method = "spearman") # correlacao entre classe social e grau de instrucao, met. Spearman
cor(x,y,method = "kendall") # correlacao entre classe social e grau de instrucao, met. Kendall


# Correlacao entre variaveis qualitativas e variaveis quantitativas -----------
data(efc)
str(efc)

idosos <- efc[c("c12hour", "e16sex", "e42dep")] #filtra colunas
colnames(idosos) <- c("tempo", "sexo", "dependencia") #renomeia colunas

# tempo X sexo
ajuste1 <- aov(tempo ~ sexo, data = idosos)
eta_sq(ajuste1)

# tempo X grau de dependencia
ajuste2 <- aov(tempo ~ dependencia, data = idosos)
eta_sq(ajuste2)
