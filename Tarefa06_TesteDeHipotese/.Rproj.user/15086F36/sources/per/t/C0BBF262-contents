packages <- c("UsingR", "epiDisplay", "DescTools", "MASS")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg)
    }
}


#################################### EXERCICIO 1 ##############################

## 1
# a
data("babies")
# b/c
babies <- subset(babies, gestation != 999 & wt != 999 & age != 99 & smoke != 9,
                 select = c("gestation", "wt", "age", "smoke"))

## 2
# a
t.test(babies$gestation, mu = 280, alternative = "two.sided") # p-Valor = 0.137
# b
gest_fumantes <- subset(babies, smoke == 1)[["gestation"]]
# c
t.test(gest_fumantes, mu = 280, alternative = "less") # p-Valor = 0.00187
# d
gest_Nfumantes <- subset(babies, smoke != 1)[["gestation"]]
shapiro.qqnorm(gest_fumantes) # P<.001: não normal
shapiro.qqnorm(gest_Nfumantes) # P<.001: não normal
plot(density(gest_Nfumantes))
lines(density(gest_fumantes), col = "red") # densidades com formato semelhante
var.test(gest_fumantes, gest_Nfumantes, alternative = "t") # p-valor = 0,02: variancia diferente
wilcox.test(gest_fumantes, gest_Nfumantes, alternative = "l") # p-Valor=0.00048

## 3
wt_fumantes <- subset(babies, smoke == 1)[["wt"]]
wt_Nfumantes <- subset(babies, smoke != 1)[["wt"]]
shapiro.qqnorm(wt_fumantes) # p-Valor = 0.56: normal
shapiro.qqnorm(wt_Nfumantes) # p-Valor < .001: não normal
plot(density(wt_Nfumantes))
lines(density(wt_fumantes), col = "red") # densidades com formato semelhante
var.test(wt_fumantes, wt_Nfumantes, alternative = "t") # p-Valor = 0,26: variancia igual
wilcox.test(wt_fumantes, wt_Nfumantes, alternative = "l") # p-Valor = 2.2e-16


#################################### EXERCICIO 2 ##############################

data("exec.pay")

## 1
SignTest(exec.pay, mu = 22, alternative = "g")

## 2
plot(density(exec.pay))
log.exec <- log(exec.pay)
hist(log.exec)
wilcox.test(log.exec, mu = log(22), alternative = "g")


#################################### EXERCICIO 3 ##############################

data("shoes")
plot(density(shoes$A))
lines(density(shoes$B), col = "green")
shapiro.qqnorm(shoes$A - shoes$B) # p-Valor = 0,80: normal

t.test(shoes$A, shoes$B, alternative = "t", paired = T) # p-Valor = 0,0085
t.test(shoes$A, shoes$B, alternative = "l", paired = T) # p-Valor = 0,0042
t.test(shoes$A, shoes$B, alternative = "g", paired = T) # p-Valor = 0,9957
