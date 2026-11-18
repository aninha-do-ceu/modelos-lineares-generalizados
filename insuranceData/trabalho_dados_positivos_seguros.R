# avaliacao 1 - Regressão 2

# Ana Rosa

library(insuranceData)
library(ggplot2)
library(dplyr)

# Seção A
# pegando dados AutoClaims do pacote insuranceData

data("AutoClaims")
df_autoclaims <-AutoClaims
head(df_autoclaims)

summary(df_autoclaims)

# valores nulos, ausentes
sum(is.na(df_autoclaims))

# valores não plausiveis
df_autoclaims[df_autoclaims$PAID < 0,]
df_autoclaims[df_autoclaims$AGE < 0,]

table(df_autoclaims$AGE)
table(df_autoclaims$GENDER)
table(df_autoclaims$CLASS)
table(df_autoclaims$STATE)

freq <- sort(prop.table(table(df_autoclaims$CLASS)))
acum <- cumsum(freq)
categorias_outros <- names(freq)[acum <= 0.05]
df_autoclaims$CLASS_agrupado <- ifelse(
  df_autoclaims$CLASS %in% categorias_outros,
  "Outros",
  as.character(df_autoclaims$CLASS)
)

freq <- sort(prop.table(table(df_autoclaims$STATE)))
acum <- cumsum(freq)
categorias_outros <- names(freq)[acum <= 0.05]
df_autoclaims$STATE_agrupado <- ifelse(
  df_autoclaims$STATE %in% categorias_outros,
  "Outros",
  as.character(df_autoclaims$STATE)
)


# categorias de referencia

df_autoclaims$GENDER <- relevel(factor(df_autoclaims$GENDER), ref = "M")
df_autoclaims$CLASS <- relevel(factor(df_autoclaims$CLASS), ref = "C11")
df_autoclaims$STATE <- relevel(factor(df_autoclaims$STATE), ref = "STATE 15")

# Seção B

# tabela PAID
summary(df_autoclaims$PAID)
var(df_autoclaims$PAID)
sqrt(var(df_autoclaims$PAID))

# frequencia de Days
table(df_autoclaims$Days)

# distribuicao de Days

par(mfrow = c(1, 2), 
    oma = c(0, 0, 3, 0))  # espaço para o título geral


hist(
  df_autoclaims$PAID,
  main = "Escala Normal",
  xlab = "Valor Pago",
  ylab = "Frequência",
  col = "steelblue",
  border = "black"
)

hist(
  log(df_autoclaims$PAID),
  main = "Escala Log",
  xlab = "Logaritmo do Valor Pago",
  ylab = "Frequência",
  col = "steelblue",
  border = "black"
)

mtext(
  "Distribuição dos Valores Pagos PAID",
  outer = TRUE,
  cex = 1.4,
  font = 2
)

par(mfrow = c(1, 1))

# grafico de dispersão idade x valor

plot(
  df_autoclaims$AGE, df_autoclaims$PAID,
  main = "Valores Pagos por Idade",
  xlab = "Idade do Condutor",
  ylab = "Valor Pago",
  col = "steelblue",
  border = "black"
)


# funcao para retornar histograma por níveis de variáveis e estatísticas

analise_nivel <- function(coluna, nome_coluna){
  var <- rlang::sym(coluna)
  plot <- ggplot(df_autoclaims, aes(x = PAID)) +
    geom_histogram(
      bins = 20,
      fill = "steelblue",
      color = "black"
    ) +
    facet_wrap(facets = rlang::as_name(var)
    )+
    labs(
      title = paste0("Valores Pagos por ",nome_coluna),
      x = "Valores Pagos",
      y = "Frequência"
    ) +
    theme_minimal()
  
  estats <- df_autoclaims %>%
    group_by(!!var) %>%
    summarise(
      Mediana = median(PAID),
      Media = mean(PAID),
      Variancia = var(PAID),
      Tamanho = n()
    )
  print(estats)
  return(plot)
}
# distribuicao de Days por Níveis de GENDER

analise_nivel("GENDER",nome_coluna = "Gênero")

# distribuicao de Days por Níveis de CLASS

analise_nivel("CLASS_agrupado",nome_coluna = "Classe Tarifária")

# distribuicao de Days por Níveis de STATE

analise_nivel("STATE_agrupado",nome_coluna = "Estado")

# distribuicao de Days por Níveis de CLASS

analise_nivel("CLASS",nome_coluna = "Classe Tarifária")

# distribuicao de Days por Níveis de STATE

analise_nivel("STATE",nome_coluna = "Estado")


# seção D

# Ajuste dos modelos

# variavel idade 
df_autoclaims$AGE_ajustado <- df_autoclaims$AGE - mean(df_autoclaims$AGE)

#vars_modelo <- c("AGE","GENDER","STATE")
vars_modelo <- c("AGE_ajustado","GENDER","STATE_agrupado")
formula_m4 <- as.formula(
  paste(
    "PAID ~",
    paste(vars_modelo, collapse = " + ")
  )
)

modelo_normal<- glm(formula_m4, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama <- glm(formula_m4, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv <- glm(formula_m4, family = inverse.gaussian(link = "log"), data=df_autoclaims)

estatisticas_coeficientes <- function(modelo){
  coeficientes <- summary(modelo)$coefficients
  ic_coeficientes <- confint(modelo)
  
  tabela <- cbind(
    'Estimativa' = coeficientes[,1],
    'Exp(Estimativa)' = exp(coeficientes[,1]),
    'Variação Percentual' = 100*( exp(coeficientes[,1]) -1),
    'Erro-Padrão' = coeficientes[,2],
    'IC 2,5%'= ic_coeficientes[,1],
    'IC 97,5%'= ic_coeficientes[,2],
    'p-valor' = coeficientes[,4]
  )
  tabela <- round(tabela,3)
  print(tabela)
  
  cat("\nTabela em formato LaTeX:\n\n")
  
  for (i in 1:nrow(tabela)) {
    cat(
      rownames(tabela)[i], " & ",
      paste(tabela[i, ], collapse = " & "),
      " \\\\\n",
      sep = ""
    )
  }
}

# estatísticas dos coeficientes

estatisticas_coeficientes(modelo_normal)
estatisticas_coeficientes(modelo_gama)
estatisticas_coeficientes(modelo_gauss_inv)

# AIC e BIC

tabela_selecao <- data.frame(
  Modelo = c("Normal","Gama","Gaussiana-Inversa"),
  AIC = c(AIC(modelo_normal),AIC(modelo_gama),AIC(modelo_gauss_inv)),
  BIC = c(BIC(modelo_normal),BIC(modelo_gama),BIC(modelo_gauss_inv))
)
aic_min = min(tabela_selecao$AIC)
tabela_selecao$AIC_j = tabela_selecao$AIC - aic_min
tabela_selecao$pesos_akaike = exp(-tabela_selecao$AIC_j/2)/(sum(exp(-tabela_selecao$AIC_j/2)))
tabela_selecao

# demais modelos

formula_m0 <- as.formula(
  paste(
    "PAID ~ 1"
  )
)
vars_modelo_1 <- c("AGE_ajustado","GENDER")
formula_m1 <- as.formula(
  paste(
    "PAID ~",
    paste(vars_modelo_1, collapse = " + ")
  )
)
vars_modelo_2 <- c("AGE_ajustado","GENDER","STATE_agrupado")
formula_m2 <- as.formula(
  paste(
    "PAID ~",
    paste(vars_modelo_2, collapse = " + ")
  )
)
vars_modelo_3 <- c("CLASS_agrupado","STATE_agrupado")
formula_m3 <- as.formula(
  paste(
    "PAID ~",
    paste(vars_modelo_3, collapse = " + ")
  )
)
vars_modelo_4 <- c("AGE_ajustado","GENDER","CLASS_agrupado","STATE_agrupado")
formula_m4 <- as.formula(
  paste(
    "PAID ~",
    paste(vars_modelo_4, collapse = " + ")
  )
)
# modelo m0
modelo_normal_0<- glm(formula_m0, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama_0 <- glm(formula_m0, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv_0 <- glm(formula_m0, family = inverse.gaussian(link = "log"), data=df_autoclaims)

# modelo m1
modelo_normal_1<- glm(formula_m1, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama_1 <- glm(formula_m1, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv_1 <- glm(formula_m1, family = inverse.gaussian(link = "log"), data=df_autoclaims)

# modelo m2
modelo_normal_2<- glm(formula_m2, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama_2 <- glm(formula_m2, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv_2 <- glm(formula_m2, family = inverse.gaussian(link = "log"), data=df_autoclaims)

# modelo m3
modelo_normal_3<- glm(formula_m3, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama_3 <- glm(formula_m3, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv_3 <- glm(formula_m3, family = inverse.gaussian(link = "log"), data=df_autoclaims)

# modelo m4
modelo_normal_4<- glm(formula_m4, family = gaussian(link = "log"), data=df_autoclaims)
modelo_gama_4 <- glm(formula_m4, family = Gamma(link = "log"), data=df_autoclaims)
modelo_gauss_inv_4 <- glm(formula_m4, family = inverse.gaussian(link = "log"), data=df_autoclaims)

# tabela selecao geral

tabela_selecao_todos <- data.frame(
  Numero = c(rep(0:4, each = 3)),
  Modelo = c("Normal","Gama","Gaussiana-Inversa","Normal","Gama","Gaussiana-Inversa",
             "Normal","Gama","Gaussiana-Inversa","Normal","Gama","Gaussiana-Inversa",
             "Normal","Gama","Gaussiana-Inversa"),
  AIC = c(AIC(modelo_normal_0),AIC(modelo_gama_0),AIC(modelo_gauss_inv_0),
          AIC(modelo_normal_1),AIC(modelo_gama_1),AIC(modelo_gauss_inv_1),
          AIC(modelo_normal_2),AIC(modelo_gama_2),AIC(modelo_gauss_inv_2),
          AIC(modelo_normal_3),AIC(modelo_gama_3),AIC(modelo_gauss_inv_3),
          AIC(modelo_normal_4),AIC(modelo_gama_4),AIC(modelo_gauss_inv_4)),
  BIC = c(BIC(modelo_normal_0),BIC(modelo_gama_0),BIC(modelo_gauss_inv_0),
          BIC(modelo_normal_1),BIC(modelo_gama_1),BIC(modelo_gauss_inv_1),
          BIC(modelo_normal_2),BIC(modelo_gama_2),BIC(modelo_gauss_inv_2),
          BIC(modelo_normal_3),BIC(modelo_gama_3),BIC(modelo_gauss_inv_3),
          BIC(modelo_normal_4),BIC(modelo_gama_4),BIC(modelo_gauss_inv_4))
)
aic_min = min(tabela_selecao_todos$AIC)
tabela_selecao_todos$AIC_j = tabela_selecao_todos$AIC - aic_min
tabela_selecao_todos$pesos_akaike = exp(-tabela_selecao_todos$AIC_j/2)/(sum(exp(-tabela_selecao_todos$AIC_j/2)))
tabela_selecao_todos

# tabeal ordenada

tabela_selecao_todos_ord <- tabela_selecao_todos |> sort_by(~list(AIC))
tabela_selecao_todos_ord[] <- lapply(
  tabela_selecao_todos_ord,
  function(x) if (is.numeric(x)) round(x, 3) else x
)

for (i in 1:nrow(tabela_selecao_todos_ord)) {
  cat(
    rownames(tabela_selecao_todos)[i], " & ",
    paste(tabela_selecao_todos_ord[i, ], collapse = " & "),
    " \\\\\n",
    sep = ""
  )
}

# seleção com stepwise

modelos_stepwise_normal <- stepAIC(modelo_normal_0, 
                            scope = list(upper = formula_m4, lower = ~1),
                            direction="both")

modelos_stepwise_gama <- stepAIC(modelo_gama_0, 
                                   scope = list(upper = formula_m4, lower = ~1),
                                   direction="both")

modelos_stepwise_gaus_inv <- stepAIC(modelo_gauss_inv, 
                                   scope = list(upper = formula_m4, lower = ~1),
                                   direction="both")

AIC(modelos_stepwise_normal)
AIC(modelos_stepwise_gama)
AIC(modelos_stepwise_gaus_inv)

# Seção E - Analise residual

par(mfrow = c(1, 3), 
    oma = c(0, 0, 3, 0))

plot(x=modelo_normal_2$fitted.values, 
     y=residuals(modelo_normal_2, type = "pearson"),
     main="Normal",
     xlab="Valores Ajustados",
     ylab="Resíduos",
     col="steelblue")
plot(modelo_gama_2$fitted.values, 
     residuals(modelo_gama_2, type = "pearson"),
     xlab="Valores Ajustados",
     ylab="Resíduos",
     col="steelblue",
     main="Gama")
plot(modelo_gauss_inv_2$fitted.values, 
     residuals(modelo_gauss_inv_2, type = "pearson"),
     xlab="Valores Ajustados",
     ylab="Resíduos",
     col="steelblue",
     main="Gaussiana-Inversa")
mtext(
  "Gráfico de Dispersão Resíduos vs Valores Ajustados",
  outer = TRUE,
  cex = 1.4,
  font = 2
)


plot(x=df_autoclaims$AGE, 
     y=residuals(modelo_normal_2, type = "pearson"),
     main="Normal",
     xlab="Idade do Condutor",
     ylab="Resíduos",
     col="steelblue")
plot(df_autoclaims$AGE, 
     residuals(modelo_gama_2, type = "pearson"),
     xlab="Idade do Condutor",
     ylab="Resíduos",
     col="steelblue",
     main="Gama")
plot(df_autoclaims$AGE, 
     residuals(modelo_gauss_inv_2, type = "pearson"),
     xlab="Idade do Condutor",
     ylab="Resíduos",
     col="steelblue",
     main="Gaussiana-Inversa")
mtext(
  "Gráfico de Dispersão Resíduos vs Idade",
  outer = TRUE,
  cex = 1.4,
  font = 2
)

# residuos padronizados
res_padr_norm <- rstandard(modelo_normal_2, type="deviance")
res_padr_gama <- rstandard(modelo_gama_2, type="deviance")
res_padr_gaus_inv <- rstandard(modelo_gauss_inv_2, type="deviance")


qqnorm(res_padr_norm,
       main = "Normal",
      xlab = "Quantis teóricos",
      ylab = "Quantis dos resíduos",
      col = "steelblue",
      lwd = 2
)
qqline(
  res_padr,
  col = "red",
  lwd = 2
)

qqnorm(res_padr_gama,
       main = "Gama",
       xlab = "Quantis teóricos",
       ylab = "Quantis dos resíduos",
       col = "steelblue",
       lwd = 2
)
qqline(
  res_padr,
  col = "red",
  lwd = 2
)

qqnorm(res_padr_gaus_inv,
       main = "Gaussiana-Inversa",
       xlab = "Quantis teóricos",
       ylab = "Quantis dos resíduos",
       col = "steelblue",
       lwd = 2
)
qqline(
  res_padr,
  col = "red",
  lwd = 2
)
mtext(
  "Q-Q Plot dos Resíduos de Desvio Padronizados",
  outer = TRUE,
  cex = 1.4,
  font = 2
)

# residuos de alavancagem

df_residuos <- data.frame(PAID =df_autoclaims$PAID,Normal = res_padr_norm,
           Gama  = res_padr_gama, Gausinv = res_padr_gaus_inv)
df_residuos |> sort_by(~list(-Normal)) |> head(10)
df_residuos |> sort_by(~list(-Gama)) |> head(10)
df_residuos |> sort_by(~list(-Gausinv)) |> head(10)

