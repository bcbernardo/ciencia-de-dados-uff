packages <- c("readxl", "lmtest")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg)
    }
}

################################## QUESTAO 1 ##################################


bb <- read_excel("Beisebol.xlsx")
attach(bb)


################################## QUESTAO 2 ##################################
### VITORIAS X RUNS FEITOS 

# grafico de dispersao
plot(x = Runs_Feitos, xlab = "Runs feitos",
     y = Proporcao_de_vitorias, ylab = "Proporção de vitórias",
     pch = 21, col = "blue", bg = "blue")

# coeficiente de correlacao linear e teste de correlacao
cor.test(Runs_Feitos, Proporcao_de_vitorias, method = "pearson") 
    # p-valor: 0,000384
    # coef. de correlacao de Pearson: 0,60616

# modelo linear
ajuste_runs_feitos <- lm(Proporcao_de_vitorias ~ Runs_Feitos)
summary(ajuste_runs_feitos)
    # intercepto: -0,0202 (p-Valor: 0,877) # IGNORAR!!!
ajuste_runs_feitos <- lm(Proporcao_de_vitorias ~ Runs_Feitos - 1)
summary(ajuste_runs_feitos)
abline(ajuste_runs_feitos, col = "red")
    # coeficiente de regressao: 0,0006353 (p-valor < 2e-16)
    # coeficiente de determinacao ajustado: 0,9903

### analise de residuos
# visao geral dos residuos
par(mfrow = c(2,2))
plot(ajuste_runs_feitos)
par(mfrow = c(1,1))
# diagnostico de normalidade
ks.test(rstandard(ajuste_runs_feitos), pnorm, mean = 0, sd = 1)
    # p-valor: 0,88 (nao rejeita H0, i.e., a hipótese de normalidade)
# diagnostico de homocedasticidade
plot(fitted(ajuste_runs_feitos), residuals(ajuste_runs_feitos))
abline(h = 0, lty = 3, col = "red")
gqtest(ajuste_runs_feitos)
    #  p-valor: 0,966 (não rejeita H0, i.e., a hipotese de homocedasticidade)
# diagnostico de independencia
res2 <- residuals(ajuste_runs_feitos)
plot(res2[-length(res2)], res2[-1], main = "Lag Plot")
dwtest(ajuste_runs_feitos)
    # p-valor: 0,2767 (não rejeita H0, i.e., a hipótese de independencia)


################################## QUESTAO 3 ##################################
### VITORIAS X RUNS SOFRIDOS

# grafico de dispersao
plot(x = Runs_Permitidos, xlab = "Runs permitidos",
     y = Proporcao_de_vitorias, ylab = "Proporção de vitórias",
     pch = 21, col = "blue", bg = "blue")

# coeficiente de correlacao linear e teste de correlacao
cor.test(Runs_Permitidos, Proporcao_de_vitorias, method = "p") 
# p-valor: 2,8e-07
# coef. de correlacao de Pearson: -0,785

# modelo linear
ajuste_runs_permitidos <- lm(Proporcao_de_vitorias ~ Runs_Permitidos)
summary(ajuste_runs_permitidos)
    # intercepto: 1,099 (p-Valor: 8,95e-13)
    # coeficiente de regressao: -0,000762 (p-valor < 2,8e-07)
    # coeficiente de determinacao ajustado: 0,6026

### analise de residuos
# visao geral dos residuos
par(mfrow = c(2,2))
plot(ajuste_runs_permitidos)
par(mfrow = c(1,1))
# diagnostico de normalidade
ks.test(rstandard(ajuste_runs_permitidos), pnorm, mean = 0, sd = 1)
    # p-valor: 0,98 (nao rejeita H0, i.e., a hipótese de normalidade)
# diagnostico de homocedasticidade
plot(fitted(ajuste_runs_permitidos), residuals(ajuste_runs_permitidos))
abline(h = 0, lty = 3, col = "red")
gqtest(ajuste_runs_permitidos)
    # p-valor: 0,81 (não rejeita H0, i.e., a hipotese de homocedasticidade)
# diagnostico de independencia
res3 <- residuals(ajuste_runs_permitidos)
plot(res3[-length(res3)], res3[-1], main = "Lag Plot")
dwtest(ajuste_runs_permitidos)
    # p-valor: 0,06442 (não rejeita H0, i.e., a hipótese de independencia)


################################## QUESTAO 4 ##################################
### VITORIAS X SALDO

# grafico de dispersao
plot(x = Saldo, xlab = "Saldo",
     y = Proporcao_de_vitorias, ylab = "Proporção de vitórias",
     pch = 21, col = "blue", bg = "blue")

# coeficiente de correlacao linear e teste de correlacao
cor.test(Saldo, Proporcao_de_vitorias, method = "p") 
    # p-valor: 2,7e-12
    # coef. de correlacao de Pearson: 0,911

# modelo linear
ajuste_saldo <- lm(Proporcao_de_vitorias ~ Saldo)
summary(ajuste_saldo)
    # intercepto: 4,99 (p-Valor: 2e-16)
    # coeficiente de regressao: -0,000608 (p-valor < 2,7e-12)
    # coeficiente de determinacao ajustado: 0,8241

### analise de residuos
# visao geral dos residuos
par(mfrow = c(2,2))
plot(ajuste_saldo)
par(mfrow = c(1,1))
# diagnostico de normalidade
ks.test(rstandard(ajuste_saldo), pnorm, mean = 0, sd = 1)
    # p-valor: 0,762 (nao rejeita H0, i.e., a hipótese de normalidade)
# diagnostico de homocedasticidade
plot(fitted(ajuste_saldo), residuals(ajuste_saldo))
abline(h = 0, lty = 3, col = "red")
gqtest(ajuste_saldo)
    #  p-valor: 0,946 (não rejeita H0, i.e., a hipotes e de homocedasticidade)
# diagnostico de independencia
res4 <- residuals(ajuste_saldo)
plot(res4[-length(res4)], res4[-1], main = "Lag Plot")
dwtest(ajuste_saldo)
    # p-valor: 0,421 (não rejeita H0, i.e., a hipótese de independencia)


################################## QUESTAO 6 ##################################
bb$razao <- Runs_Feitos / Runs_Permitidos
attach(bb)
plot(x = razao, xlab = "Razão de runs feitos por runs sofridos",
     y = Proporcao_de_vitorias, ylab = "Proporção de vitórias",
     pch = 21, col = "blue", bg = "blue")

# correlacao e regressao linear da taxa de runs feitos / runs sofridos
cor.test(Proporcao_de_vitorias, razao) # pearson: 0,91 (p-Valor: 1,3e-12)
ajuste_customizado <- lm(Proporcao_de_vitorias ~ razao)
summary(ajuste_customizado)
    # coeficiente de regressao: 0,49375 (p-valor < 1,33e-12)
    # coeficiente de determinacao ajustado: 0,8328

# analise de residuos
ks.test(rstandard(ajuste_customizado), pnorm, mean = 0, sd = 1) # p-Valor = 0,81 -> normalidade OK!
gqtest(ajuste_customizado) # p-Valor = 0,95 -> homocedasticidade OK!
dwtest(ajuste_customizado) # p-Valor = 0,41 -> independencia OK!

# predicao para 844 runs feitos contra 722 sofridos, com o ajuste pela taxa de feitos X sofridos
predict(
    ajuste_customizado, newdata = data.frame(razao = c(844/722)), 
    interval = "prediction"
)   
    # valor estimado: 57,95% de vitorias
    # intervalo de predicao (95%): entre 52,5% e 63,4% de vitorias

# predicao para 844 runs feitos contra 722 sofridos, com outros ajustes (para comparacao)
predict(
    ajuste_runs_feitos, newdata = data.frame(Runs_Feitos = c(844)), 
    interval = "prediction"
)   # 53,6% (95% de chance de estar entre 43,3 e 63,9%)
predict(
    ajuste_runs_permitidos, newdata = data.frame(Runs_Permitidos = c(722)), 
    interval = "prediction"
)   # 54,9% (95% de chance de estar entre 46,6 e 63,2%)
predict(
    ajuste_saldo, newdata = data.frame(Saldo = c(844 - 722)), 
    interval = "prediction"
)   # 57,4% (95% de chance de estar entre 51,8 e 63%)
