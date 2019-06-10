packages <- c("data.table", "car", "tidytext", "qdap")
for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg)
    }
}

# Carregar dataset
wine <- fread("winemag.csv")


###################### DELIMITACAO DOS ANOS DAS SAFRAS ########################

wine[, year := 
         as.numeric(sub("(?:^.* )(\\d{4})(?: .*$)", "\\1", title, perl = T))]
wine[year > 2017 | year < 1890, year := NA]

################################ ESTUDO DESCRITIVO ############################

# numero de rotulos individuais
unique_titles <- wine[,.N, by = title]
nrow(unique_titles) # 118840
hist(unique_titles$N)
nrow(unique_titles[N == 1,])
nrow(unique_titles[N > 2,])

# estatisticas descritivas para a pontuacao
hist(wine$points, probability = T, ylim = c(0, 0.15), col = "cyan",
     main = "Frequência relativa das pontuações de vinhos",
     xlab = "Pontuação", ylab = "Proporção do total")
abline(v = mean(wine$points), col = "red")
abline(v = mean(wine$points) + sd(wine$points), col = "red", lty = 2)
abline(v = mean(wine$points) - sd(wine$points), col = "red", lty = 2)
summary(wine$points)
mu <- mean(wine$points)
var(wine$points)
StDev <- sd(wine$points)

# estatisticas descritivas para o preço
nrow(wine[is.na(price) == F & price < 160,])/nrow(wine[is.na(price) == F]) #proporcao abaixo de UDS160
nrow(wine[is.na(price) == F & price > 500,])/nrow(wine[is.na(price) == F]) #proporcao acima de USD500
hist(wine$price, probability = T, ylim = c(0, 0.04), xlim = c(0, 160), col = "cyan",
     main = "Frequência relativa dos preços de vinhos",
     xlab = "Preço", ylab = "Proporção do total", breaks = "scott")
abline(v = mean(wine$price, na.rm = T), col = "red")
abline(v = mean(wine$price, na.rm = T) + sd(wine$price, na.rm = T), 
       col = "red", lty = 2)
abline(v = mean(wine$price, na.rm = T) - sd(wine$price, na.rm = T), 
       col = "red", lty = 2)
summary(wine$price)
price_mu <- mean(wine$price)
var(wine$price, na.rm = T)
price_StDev <- sd(wine$price, na.rm = T)

# estatisticas descritivas para os provadores
tasters_freq <- wine[taster_name != "", .N, by = taster_name][order(-N)]
nrow(tasters_freq)
summary(tasters_freq$N)
sd(tasters_freq$N)
plot(density(tasters_freq$N), xlim = c(0, 30000))

# estatistica descritiva para as variedades de uva
varieties_freq <- wine[variety != "", .N, by = variety][order(-N)]
nrow(varieties_freq)
summary(varieties_freq$N)
sd(varieties_freq$N)
plot(density(varieties_freq$N))
nrow(varieties_freq[N >= 100])/nrow(varieties_freq)

# estatistica descritiva dos anos das safras
summary(wine$year)
nrow(wine[is.na(year) == F & year > 1997])/nrow(wine[is.na(year) == F])
hist(wine$year, xlim = c(1990, 2020), breaks = "scott", col = "cyan",
     main = "Frequência das safras de vinhos",
     xlab = "Ano da safra", ylab = "Quantidade de críticas")
abline(v = mean(wine$year, na.rm = T), col = "red")
abline(v = mean(wine$year, na.rm = T) + sd(wine$year, na.rm = T), 
       col = "red", lty = 2)
abline(v = mean(wine$year, na.rm = T) - sd(wine$year, na.rm = T), 
       col = "red", lty = 2)
var(wine$year, na.rm = T)
sd(wine$year, na.rm = T)

######################## CONTROLAR VIESES DOS CRITICOS ########################

# comparar distribuicao das notas entre os principais criticos
table(wine$taster_name)
plot(density(wine[taster_name == "Kerin O’Keefe", points], adjust = 1.5), 
     col = 1, main = "Distribuição das pontuações segundo diferentes provadores",
     xlab = "Pontuação", ylab = "Proporção do total")
lines(density(wine[taster_name == "Michael Schachner", points], 
              adjust = 1.5), col = 2)
lines(density(wine[taster_name == "Roger Voss", points], 
              adjust = 1.5), col = 3)
lines(density(wine[taster_name == "Paul Gregutt", points], 
              adjust = 1.5), col = 4)
lines(density(wine[taster_name == "Virginie Boone", points], 
              adjust = 1.5), col = 5)

pointsXtaster <- wine[taster_name != "", 
                      .(points, taster = as.factor(taster_name))]
kruskal.test(points ~ taster, pointsXtaster)
leveneTest(points ~ taster, pointsXtaster)

# comparar a distribuicao das notas entre as principais variedades
table(wine$variety)
plot(density(wine[variety == "Bordeaux-style Red Blend", points], 
             adjust = 1.5), 
     col = 1, ylim = c(0, 0.16), 
     main = "Distribuição das pontuações para diferentes variedades",
     xlab = "Pontuação", ylab = "Proporção do total")
lines(density(wine[variety == "Red Blend", points], adjust = 1.5), col = 2)
lines(density(wine[variety == "Cabernet Sauvignon", points], 
              adjust = 1.5), col = 3)
lines(density(wine[variety == "Chardonnay", points], 
              adjust = 1.5), col = 4)
lines(density(wine[variety == "Pinot Noir", points], 
              adjust = 1.5), col = 5)
pointsXvariety <- wine[variety != "", 
                      .(points, variety = as.factor(variety))]
kruskal.test(points ~ variety, pointsXvariety)
car::leveneTest(points ~ variety, pointsXvariety)

# calcular media e desvio padrao por par provador-variedade
wine <- wine[
    wine[, .(points_Mean = mean(points), points_sd = sd(points)), 
         by = .(taster_name, variety)], 
    on = .(taster_name, variety)]

# normalizar pontuacao para cada par provador-variedade
wine[, z_points := (points - points_Mean)/points_sd]

# restituir pontuacao a uma escala comum, 
# utilizando os parametros da distribuicao da amostra completa
wine[, points.norm := (z_points * StDev) + mu]

# limpar campos intermediarios
wine <- wine[, !(points_Mean:points_sd)]

################### PREPARACAO PARA ANALISE DA DESCRICAO ######################

# visao geral das palavras mais comuns
data("stop_words")
description <- unnest_tokens(wine, words, description)
description <- setDT(description)[!stop_words$word, on = "words"]
head(description[, .N, by = words][order(-N)], 15)

##### Etiquetamento de partes do discurso
# ATENCAO!! PROCESSAMENTO LENTO!
wine[, tagged := pos(description, parallel = T)$POStagged$POStagged]

########################### EXTRACAO DE PALAVRAS RELEVANTES ###################

# funcao para gerar regex das classes morfologicas
regPOS <- function(POS = char, exact = FALSE, capture = FALSE, allbut = FALSE,
                   consume_tag = TRUE, wrap = TRUE) {
    word <- ifelse(capture == T, "\\b([a-z]+)", # capturar palavras dessas classes
                   "\\b[a-z]+") # encontrar sem capturar
    
    complete <- ifelse(exact == T, "", # procurar tags tal qual escritas vetor POS
                       "[A-Z\\$]{0,3}") # permitir completar tags
    POS <- as.character(POS) # forcar a ser vetor de caracteres (caso haja so um, por exemplo)
    POS <- paste0("/", POS, complete, "\\b") # adicionar barra e, opcionalmente, completar tags
    POS_joined <- paste0(POS, collapse = "|") # concatenar opcoes validas de TAGs
    
    anyTag <- "/[A-Z\\$]{2,4}\\b *" # regex generico para qualquer tag
    
    negation <- ifelse(allbut == T, "!", # transformar a busca em EXCETO essas classes
                       "=") # manter como busca dessas classes
    
    all <- paste0(word, # palavra propriamente dita
                  "(?", negation, POS_joined, ")") # checar a classe morfologica
    if (consume_tag == T) {all <- paste0(all, anyTag) } # consumir caracteres da tag
    if (wrap == TRUE) {all <- paste0("(?:", all, ")")}
    return(all)
}

# adaptacao da funcao regmatches para localizar primeiro um trecho de interesse
# e depois retirar um tipo de palavra do classe morfologica dali
POSmatch <- function(char_vector, pattern, POS, debug = FALSE, ...) {
    bigger_match <- regmatches(char_vector, 
                               regexec(pattern, char_vector, perl = T))
    if (debug == T) {return(bigger_match)}
    little_matches <- regmatches(bigger_match, 
                                 gregexpr(regPOS(POS, consume_tag = F, 
                                                 capture = F, wrap = F, ...),
                                          bigger_match, perl = T))
    return(unlist(little_matches))
}

# conjuntos de tags mais comuns
anyTag <- "/[A-Z\\$]{2,4}\\b"
adjectiviish <- c("JJ", "VBD", "VBG", "VBN")
varieties_words <- unique(unnest_tokens(wine, var_name, variety)$var_name)
instrumental_words <- c("flavor", "flavors", "finish", "notes", "note", 
                        "hint", "hints", "drink", "feel", "touch", "bit", 
                        "texture", "wine", "body", "structure", "aftertaste",
                        "aroma", "aromas", "scent", "scents", "fragrance",
                        "fragrances", "mouth", "nose", "blend")
    
# identificar adjetivos usados para caracterizacao geral do vinho
general_sect_pattern <- paste0(
    "(?:this/DT |here/RB |it/PRP )", # referencia ao vinho
    regPOS(c("VBZ"), exact = T), "?\\K", # verbo opcional 
    regPOS(c("RB", adjectiviish, "DT", "IN", "CC"), exact = T), # trecho com adjetivos
    "+?(?:wine|blend|chardonnay|white|pinot)" # tambem referencia ao vinho
)

general <- wine[, .(general = POSmatch(tagged, general_sect_pattern, c("JJ"))), V1]
length(unique(general$V1))/length(unique(wine[is.na(description) == F, V1])) # 17,6% de sucesso


# identificar substantivos relativos a aromas
aroma_sect_pattern1 <- paste0(
    "(?:aroma[s]?|scent[s]?|fragrance[s]?|nose)", anyTag, # palavra relacionada a aroma
    " (?:(?:of/IN )|(?: evoking/JJ )|", regPOS("V"), ")\\K",
    "(?:(?:palate|flavor|mouth|body|acidity|texture)(*SKIP)(*FAIL)|.)*?",
    "and/CC [a-z]+", anyTag,  #qualquer palavra depois de "...e..."
    " (?:(?:palate|flavor|mouth|body|acidity|texture)(*SKIP)(*FAIL)|",
    regPOS(c("NN", "RB", adjectiviish)), ")*"
    #".*?", regPOS(c("NN", "RB", adjectiviish), allbut = T) #parar na primeira palavra que nao for substantivo ou adjetivo
)

aroma_sect_pattern2 <- paste0(
    "(?:(?:palate|flavor|mouth|body|acidity|texture)(*SKIP)(*FAIL)|", #evitar falsos positivos
    regPOS(c("NN", "NNZ", adjectiviish, "RB", "IN", "CC")), ")+", # trecho de interesse
    "(?:aroma|scent|fragrance|the/DT nose)"
)

aroma1 <- wine[, .(aromas = POSmatch(tagged, aroma_sect_pattern1, c("NN"))), V1]
aroma2 <- wine[, .(aromas = POSmatch(tagged, aroma_sect_pattern2, c("NN"))), V1]
aroma <- merge(aroma1, aroma2, all = T)
aroma <- aroma[!c(varieties_words, instrumental_words), on = "aromas"]
length(unique(aroma$V1))/length(unique(wine[is.na(description) == F, V1])) #33% de sucesso

# identificar substantivos relativos ao paladar
palate_sect_pattern1 <- paste0(
    "(?:aroma|scent|fragrance|nose|body|acidity|texture)(*SKIP)(*FAIL)|", #evitar falsos positivos
    "(?:", regPOS(c("NN", "NNS", "IN", "CC", "DT", "RB", adjectiviish)), ")+",
    "(?=the/DT palate/NN|flavor[s]?/NN|the/DT mouth/NN)" 
 )
# identificar substantivos relativos ao paladar
palate_sect_pattern2 <- paste0(
    "(?:palate/NN |flavor[s]?/NN.? )(?:doles/NNS out/IN |of/IN |", 
    regPOS(c("VBZ"), exact = T), ")\\K",
    "(?:(?:aroma|scent|fragrance|nose|body|acidity|texture)(*SKIP)(*FAIL)|.)*?", #evitar falsos positivos
    "and/CC [a-z]+", anyTag,  #qualquer palavra depois de "...e..."
    " (?:(?:aroma|scent|fragrance|nose|body|acidity|texture)(*SKIP)(*FAIL)|",
    regPOS(c("NN", "RB", adjectiviish)), ")*"
    #"\\K.*and/CC [a-z]+", anyTag, ".*?", #qualquer palavra antes, ou uma depois de "...e..."
    #"(?:", regPOS(c("NN", "IN", "RB", adjectiviish), allbut = T), 
    #"|alongside|$)"
)

palate1 <- wine[, 
                .(flavors = POSmatch(tagged, palate_sect_pattern1, c("NN"))), 
                V1]

palate2 <- wine[, 
               .(flavors = POSmatch(tagged, palate_sect_pattern2, c("NN"))), V1]
palate <- merge(palate1, palate2, all = TRUE)
palate <- setkey(palate, V1, flavors)[!setkey(aroma, V1, aromas)]
palate <- palate[!c(varieties_words, instrumental_words), on = "flavors"]
length(unique(palate$V1))/length(unique(wine[is.na(description) == F, V1])) # 56% de sucesso

############################ NUVENS DE PALAVRAS ###############################
top_adjectives <- head(general[, .(.N), by = general][order(-N)], 100)
top_aromas <- head(aroma[, .(.N), by = aromas][order(-N)], 100)
top_flavors <- head(palate[, .(.N), by = flavors][order(-N)],100)

library(wordcloud)
wordcloud(top_adjectives$general, freq = top_adjectives$N)
wordcloud(top_aromas$aromas, freq = top_aromas$N)
wordcloud(top_flavors$flavors, freq = top_flavors$N)

########################### PERCENTUAIS DE SUCESSO POR PROVADOR ###############
successes_by_tasters <- wine[taster_name != "", 
                             .(total = .N), 
                             by = taster_name]
successes_general <- wine[V1 %in% general$V1][taster_name != "", 
                                              .(general = .N), 
                                              by = taster_name]
successes_aroma <- wine[V1 %in% aroma$V1][taster_name != "", 
                                          .(aroma = .N), 
                                          by = taster_name]
successes_palate <- wine[V1 %in% palate$V1][taster_name != "", 
                                            .(palate = .N), 
                                            by = taster_name]
successes_by_tasters <- merge(successes_by_tasters, successes_general, all = T)
successes_by_tasters <- merge(successes_by_tasters, successes_aroma, all = T)
successes_by_tasters <- merge(successes_by_tasters, successes_palate, all = T)

successes_by_tasters_pc <- successes_by_tasters[, .SD/total, 
                                                by = taster_name,
                                                .SDcols = c("general", 
                                                            "aroma", "palate")]
