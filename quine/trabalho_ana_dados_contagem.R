# avaliacao 1 - Regressão 2

# Ana Rosa

rm(list=ls())
library(MASS)
library(ggplot2)
library(dplyr)

# Seção A
# pegando dados quine do pacote MASS

data("quine")
df_quine <- quine
head(df_quine)

# valores nulos, ausentes

sum(is.na(df_quine))

table(df_quine$Eth)
table(df_quine$Sex)
table(df_quine$Age)
table(df_quine$Lrn)

# aplicando encoding na variavel ordinal Age
df_quine$Age <- relevel(factor(df_quine$Age,
                               levels = c("F0","F1","F2","F3")), 
                        ref = "F0")

# categorias de referencia

df_quine$Eth <- relevel(factor(df_quine$Eth), ref = "N")
df_quine$Sex <- relevel(factor(df_quine$Sex), ref = "M")
df_quine$Lrn <- relevel(factor(df_quine$Lrn), ref = "AL")

# Seção B

# tabela days
summary(df_quine$Days)

# frequencia de Days
table(df_quine$Days)

# distribuicao de Days

hist(
  df_quine$Days,
  main = "Histograma da Variável Days",
  xlab = "Número de Dias de Ausência",
  ylab = "Frequência",
  col = "steelblue",
  border = "black"
)

# proporcao de zeros de Days
prop_zeros <- count(df_quine[df_quine$Days == 0,])*100/length(df_quine$Days)
paste0("A proporção de zeros da tabela é ", round(prop_zeros,3), "%")

# funcao para retornar histograma por níveis de variáveis e estatísticas

analise_nivel <- function(coluna, niveis, nome_coluna){
  var <- rlang::sym(coluna)
  plot <- ggplot(df_quine, aes(x = Days)) +
    geom_histogram(
      bins = 20,
      fill = "steelblue",
      color = "black"
    ) +
    facet_wrap(facets = rlang::as_name(var), 
               labeller = as_labeller(niveis)
    )+
    labs(
      title = paste0("Dias de Ausência por ",nome_coluna),
      x = "Número de Dias de Ausência",
      y = "Frequência"
    ) +
    theme_minimal()
  
  estats <- df_quine %>%
    group_by(!!var) %>%
    summarise(
      Mediana = median(Days),
      Media = mean(Days),
      Variancia = var(Days),
      Tamanho = n()
    )
  print(estats)
  return(plot)
}

# distribuicao de Days por Níveis de Eth

analise_nivel("Eth",niveis=c(A = "Aborígene",N = "Não aborígene"), nome_coluna = "Origem Étnica")

# distribuicao de Days por Níveis de Sex

analise_nivel("Sex",niveis=c(F = "Feminino",M = "Masculino"), nome_coluna = "Gênero")

# distribuicao de Days por Níveis de Lrn

analise_nivel("Lrn",niveis=c(AL = "Média",SL = "Lenta"), nome_coluna = "Situação de Aprendizagem")

# distribuicao de Days por Níveis de Age

analise_nivel("Age",niveis=c(F0 = "F0",F1 = "F1",F2 = "F2",F3 = "F3"), nome_coluna = "Grupo Escolar")

# seção D

formula_m0 <- as.formula(
  "Days ~ 1"
)

formula_m1 <- as.formula(
  "Days ~ Eth + Sex + Age + Lrn"
)
formula_m2 <- as.formula(
  "Days ~ Eth + Sex + Age + Lrn + Eth*Sex"
)
formula_m3 <- as.formula(
  "Days ~ Eth + Sex + Age + Lrn + Age*Lrn"
)
formula_m4 <- as.formula(
  "Days ~ Eth + Sex + Age + Lrn + Eth*Age + Age*Lrn"
)

ajuste_m0 <- glm(
  formula = formula_m0,
  family=poisson(link="log"),
  data = df_quine
)

ajuste_m1 <- glm(
  formula = formula_m1,
  family=poisson(link="log"),
  data = df_quine
)
summary(ajuste_m1)

ajuste_m2 <- glm(
  formula = formula_m2,
  family=poisson(link="log"),
  data = df_quine
)
summary(ajuste_m2)

ajuste_m3 <- glm(
  formula = formula_m3,
  family=poisson(link="log"),
  data = df_quine
)
summary(ajuste_m3)

ajuste_m4 <- glm(
  formula = formula_m4,
  family=poisson(link="log"),
  data = df_quine
)
summary(ajuste_m4)

# estimativas do modelo m1
coeficientes <- summary(ajuste_m1)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]


tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(ajuste_m1),
      exp(cbind(estimativa = coef(ajuste_m1), confint(ajuste_m1))),
      pvalor = coeficientes[,4])

tabela_estim

for (i in 1:nrow(tabela_estim)) {
  cat(rownames(tabela_estim)[i], " & ",
      paste(round(tabela_estim[i, ], 4), collapse = " & "),
      " \\\\\n", sep = "")
}

anova(ajuste_m1, test = "Chisq")
anova(ajuste_m1, test = "LRT")

variaveis <- colnames(df_quine)[seq(1:4)]
for (variavel in variaveis){
  modelo_reduzido <- update(
    ajuste_m1,
    as.formula(paste(". ~ . -", variavel))
  )
  print(paste("Testando significancia global da variavel: ",variavel))
  print(anova(modelo_reduzido, ajuste_m1, test = "LRT"))
}

anova(modelo_reduzido, ajuste_m1, test = "LRT")

# verificando significancia da interacao origem etnica x genero

anova(ajuste_m1, ajuste_m2, test="LRT")

# verificando significancia da interacao grupo escolar x situacao de aprendizagem

anova(ajuste_m1, ajuste_m3, test="LRT")

# verificando significancia das interacoes origem etnica x grupo escolar e grupo escolar x aprendizagem

anova(ajuste_m1, ajuste_m4, test="LRT")

# vendo significancia de cada interacao
# sem a interação etnia x grupo escolar
m4_sem_interacao1 <- update(
  ajuste_m4,
  . ~ . - Eth*Age
)

# sem a interação grupo escolar x aprendizagem
m4_sem_interacao2 <- update(
  ajuste_m4,
  . ~ . - Age * Lrn
)

anova(m4_sem_interacao1, ajuste_m4, test = "LRT")

anova(m4_sem_interacao2, ajuste_m4, test = "LRT")


# AIC 

tabela_selecao <- data.frame(
  Modelo = c("M0","M1","M2","M3","M4"),
  AIC = c(AIC(ajuste_m0),AIC(ajuste_m1),AIC(ajuste_m2),AIC(ajuste_m3),AIC(ajuste_m4))
)
aic_min = min(tabela_selecao$AIC)
tabela_selecao$AIC_j = tabela_selecao$AIC - aic_min
tabela_selecao$pesos_akaike = exp(-tabela_selecao$AIC_j/2)/(sum(exp(-tabela_selecao$AIC_j/2)))
tabela_selecao

# plots dos ajustes

par(mfrow = c(2,2))
plot(ajuste_m1)
plot(ajuste_m2)
plot(ajuste_m3)
plot(ajuste_m4)



r_dev <-residuals(ajuste_m4, type = "deviance")
r_pear <-residuals(ajuste_m4, type = "pearson")
mu_hat <-fitted(ajuste_m4)
phi_P <-sum(r_pear^2) / df.residual(ajuste_m4)
phi_P
plot(ajuste_m4)

par(mfrow=c(1,1))
plot(mu_hat, r_dev, xlab = "Valores ajustados",
     ylab = "Residuos de desvio")
abline(h = 0, lty = 2)


set.seed(1234)
F_inf <-ppois(df_quine$Days - 1, lambda = mu_hat)
F_sup <-ppois(df_quine$Days, lambda = mu_hat)
u <-runif(nrow(df_quine), F_inf, F_sup)
r_q <-qnorm(u)
par(mfrow=c(1,3))
qqnorm(r_q)
qqline(r_q, col = 2, lwd = 2)
plot(mu_hat, r_q, xlab = "Valores ajustados",
     ylab = "Residuos quantilicos")
abline(h = 0, lty = 2)

fit.model = ajuste_m4
X <-model.matrix(fit.model)
n <-nrow(X)
p <-ncol(X)
w <-fit.model$weights
W <-diag(w)
# erro aqui no H, falta de uma combinacao entre age e lrn
H <-solve(t(X)%*%W%*%X)
H <-sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <-diag(H)
ts <-resid(fit.model,type="pearson")*sqrt(1/(1-h))
td <-resid(fit.model,type="deviance")*sqrt(1/(1-h))
di <-(h/(1-h))*(ts^2)

plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16, ylim=c(0,3), type="h")
cut = mean(di) + 4*sd(di)
abline(cut,0,lty=2,col="red")


# erro com o modelo m4
table(df_quine$Eth, df_quine$Age)
table(df_quine$Lrn, df_quine$Age)

# era uma interação entre age e lrn que esta com frequencia zero

alias(ajuste_m4)

# vendo se removendo a interação age e lrn melhora

formula_m4_corrigida <- Days ~ Eth + Sex + Age + Lrn + Eth*Age

ajuste_m4_corrigido <- glm(
  formula_m4_corrigida,
  family = poisson(link = "log"),
  data = df_quine
)
AIC(
  ajuste_m1,
  ajuste_m2,
  ajuste_m3,
  ajuste_m4_corrigido
)

tabela_selecao_corrig <- data.frame(
  Modelo = c("M0","M1","M2","M3","M4 Corrigido"),
  AIC = c(AIC(ajuste_m0),AIC(ajuste_m1),AIC(ajuste_m2),AIC(ajuste_m3),AIC(ajuste_m4_corrigido))
)
aic_min = min(tabela_selecao_corrig$AIC)
tabela_selecao_corrig$AIC_j = tabela_selecao_corrig$AIC - aic_min
tabela_selecao_corrig$pesos_akaike = exp(-tabela_selecao_corrig$AIC_j/2)/(sum(exp(-tabela_selecao_corrig$AIC_j/2)))
tabela_selecao_corrig

par(mfrow = c(2,2))
plot(ajuste_m4_corrigido)


r_dev <-residuals(ajuste_m4_corrigido, type = "deviance")
r_pear <-residuals(ajuste_m4_corrigido, type = "pearson")
mu_hat <-fitted(ajuste_m4_corrigido)
phi_P <-sum(r_pear^2) / df.residual(ajuste_m4_corrigido)
phi_P

par(mfrow=c(1,1))
plot(mu_hat, r_dev, xlab = "Valores ajustados",
     ylab = "Residuos de desvio")
abline(h = 0, lty = 2)


set.seed(1234)
F_inf <-ppois(df_quine$Days - 1, lambda = mu_hat)
F_sup <-ppois(df_quine$Days, lambda = mu_hat)
u <-runif(nrow(df_quine), F_inf, F_sup)
r_q <-qnorm(u)
par(mfrow=c(1,2))
qqnorm(r_q)
qqline(r_q, col = 2, lwd = 2)
plot(mu_hat, r_q, xlab = "Valores ajustados",
     ylab = "Residuos quantilicos")
abline(h = 0, lty = 2)


fit.model = ajuste_m4_corrigido
X <-model.matrix(fit.model)
n <-nrow(X)
p <-ncol(X)
w <-fit.model$weights
W <-diag(w)
H <-solve(t(X)%*%W%*%X)
H <-sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <-diag(H)
ts <-resid(fit.model,type="pearson")*sqrt(1/(1-h))
td <-resid(fit.model,type="deviance")*sqrt(1/(1-h))
di <-(h/(1-h))*(ts^2)
par(mfrow=c(1,1))
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16, ylim=c(0,10), type="h")
cut = mean(di) + 4*sd(di)
abline(cut,0,lty=2,col="red")

# numero medio de faltas por perfis de estudantes

modelo_selecionado = ajuste_m4_corrigido

# estimativas do modelo selecionado
coeficientes <- summary(modelo_selecionado)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]


tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(modelo_selecionado),
                      exp(cbind(estimativa = coef(modelo_selecionado), confint(modelo_selecionado))),
                      pvalor = coeficientes[,4])

tabela_estim

for (i in 1:nrow(tabela_estim)) {
  cat(rownames(tabela_estim)[i], " & ",
      paste(round(tabela_estim[i, ], 4), collapse = " & "),
      " \\\\\n", sep = "")
}


# perfil 1: estudante aborigene mulher no grupo escolar F3

novo_dado <- data.frame(
  Eth = "A",
  Sex = "F",
  Age = "F3",
  Lrn = "AL"
)

pred <- predict.glm(modelo_selecionado, newdata = novo_dado, se.fit=TRUE)

data.frame(
  media = exp(pred$fit),
  limite_inferior = exp(pred$fit - 1.96 * pred$se.fit),
  limite_superior = exp(pred$fit + 1.96 * pred$se.fit)
)

# perfil 2: estudante nao-aborigene mulher no grupo escolar F3

novo_dado <- data.frame(
  Eth = "N",
  Sex = "F",
  Age = "F3",
  Lrn = "AL"
)

pred <- predict.glm(modelo_selecionado, newdata = novo_dado, se.fit=TRUE)

data.frame(
  media = exp(pred$fit),
  limite_inferior = exp(pred$fit - 1.96 * pred$se.fit),
  limite_superior = exp(pred$fit + 1.96 * pred$se.fit)
)

# perfil 3: estudante aborigene homem com aprendizagem lenta

novo_dado <- data.frame(
  Eth = "A",
  Sex = "M",
  Age = "F3",
  Lrn = "SL"
)

pred <- predict.glm(modelo_selecionado, newdata = novo_dado, se.fit=TRUE)

data.frame(
  media = exp(pred$fit),
  limite_inferior = exp(pred$fit - 1.96 * pred$se.fit),
  limite_superior = exp(pred$fit + 1.96 * pred$se.fit)
)


# perfil 4: estudante homem no grupo escolar F3 com aprendizagem lenta
novo_dado <- data.frame(
  Eth = "N",
  Sex = "M",
  Age = "F3",
  Lrn = "SL"
)

pred <- predict.glm(modelo_selecionado, newdata = novo_dado, se.fit=TRUE)

data.frame(
  preds = pred,
  media = exp(pred$fit),
  limite_inferior = exp(pred$fit - 1.96 * pred$se.fit),
  limite_superior = exp(pred$fit + 1.96 * pred$se.fit)
)

library(emmeans)
emm <- emmeans(modelo_selecionado, ~Eth | Age + Lrn + Sex)
summary(emm, type = "response")

pairs(emm, adjust = "tukey", type = "response")

emm <- emmeans(modelo_selecionado, ~Eth)
summary(emm, type = "response")

pairs(emm, adjust = "tukey", type = "response")

# seção E

# dispersao de pearson

phi_P <-sum(residuals(modelo_selecionado, type = "pearson")^2) / df.residual(modelo_selecionado)
phi_P

# dispersao baseada no desvio

phi_D <- deviance(modelo_selecionado) / df.residual(modelo_selecionado)
phi_D

# proporção de zeros do modelo

prop_zeros
faltas <- 0
mu_hat <- fitted(modelo_selecionado)
prop_zeros_modelo <- sum(exp(-mu_hat))*100/length(df_quine$Days)

paste0("A proporção de zeros da tabela é ", round(prop_zeros,3), "%")
paste0("A proporção de zeros do modelo é ", round(prop_zeros_modelo,3), "%")

# ajustando quase-poisson e binomial negativa
# Erros-padrao ajustados, mesma funcao media
fit_QP <-glm(formula_m4_corrigida,
             family = quasipoisson(link = "log"),
             data = df_quine)
# Variancia quadratica na media
fit_NB <-MASS::glm.nb(formula_m4_corrigida,
                      data = df_quine)

summary(fit_QP)
summary(fit_NB)
summary(modelo_selecionado)

coeficientes <- summary(fit_QP)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]
# razoes de medias
cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(fit_QP),
      exp(cbind(estimativa = coef(fit_QP), confint(fit_QP))),
      pvalor = coeficientes[,4])

coeficientes <- summary(fit_NB)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]
# razoes de medias
cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(fit_NB),
      exp(cbind(estimativa = coef(fit_NB), confint(fit_NB))),
      pvalor = coeficientes[,4])

anova(fit_QP, test = "LRT")
anova(fit_NB, test = "LRT")

c(
  dispersao_Pearson = phi_P,
  dispersao_deviance = phi_D,
  theta_NB = fit_NB$theta
)

novos <-expand.grid(
  Eth = levels(df_quine$Eth),
  Sex = levels(df_quine$Sex),
  Age = levels(df_quine$Age),
  Lrn = levels(df_quine$Lrn)
)
novos$Poisson <-predict(
  modelo_selecionado, newdata = novos, type = "response"
)
novos$Quase_Poisson <-predict(
  fit_QP, newdata = novos, type = "response"
)
novos$Binomial_negativa <-predict(
  fit_NB, newdata = novos, type = "response"
)
novos


par(mfrow = c(2, 2))
plot(
  fitted(modelo_selecionado),
  residuals(modelo_selecionado, type = "deviance"),
  main = "Poisson",
  xlab = "Valores ajustados",
  ylab = "Residuos de desvio"
)
abline(h = 0, lty = 2)
plot(
  fitted(fit_NB),
  residuals(fit_NB, type = "deviance"),
  main = "Binomial negativa",
  xlab = "Valores ajustados",
  ylab = "Residuos de desvio"
)
abline(h = 0, lty = 2)

n <- length(df_quine$Lrn)
plot(cooks.distance(modelo_selecionado), type = "h")
abline(
  h = 4/n,
  col = "red",
  lty = 2
)
plot(cooks.distance(fit_NB), type = "h")
abline(
  h = 4/n,
  col = "red",
  lty = 2
)

# residuos
par(mfrow = c(2,2))
plot(modelo_selecionado)
plot(fit_QP)
plot(fit_NB)

# Resíduos
r_dev <- residuals(fit_NB, type = "deviance")
r_pear <- residuals(fit_NB, type = "pearson")

# Valores ajustados
mu_hat <- fitted(fit_NB)

# Dispersão de Pearson
phi_P <- sum(r_pear^2) / df.residual(fit_NB)
phi_P

# Resíduos quantílicos aleatorizados
set.seed(1234)
theta <- fit_NB$theta

F_inf <- pnbinom(df_quine$Days - 1,mu = mu_hat,size = theta)
F_sup <- pnbinom(df_quine$Days,mu = mu_hat,size = theta)
u <- runif(nrow(df_quine), F_inf, F_sup)
r_q <- qnorm(u)
# Gráficos dos resíduos quantílicos
par(mfrow = c(1, 2))
qqnorm(
  r_q,
  main = "QQ-plot dos resíduos quantílicos"
)
qqline(r_q, col = 2, lwd = 2)
plot(
  mu_hat, r_q,
  xlab = "Valores ajustados",
  ylab = "Resíduos quantílicos"
)

abline(h = 0, lty = 2)


B <- 10000
n <- length(df_quine$Eth)
# Simulações sob o modelo ajustado
res_sim <- matrix(NA, nrow = n, ncol = B)

for (b in 1:B) {
  
  y_sim <- rnbinom(
    n,
    mu = mu_hat,
    size = theta
  )
  
  F_inf_sim <- pnbinom(
    y_sim - 1,
    mu = mu_hat,
    size = theta
  )
  
  F_sup_sim <- pnbinom(
    y_sim,
    mu = mu_hat,
    size = theta
  )
  
  u_sim <- runif(n, F_inf_sim, F_sup_sim)
  
  res_sim[, b] <- sort(qnorm(u_sim))
}


# Envelope
lim_inf <- apply(res_sim, 1, quantile, probs = 0.025)
lim_sup <- apply(res_sim, 1, quantile, probs = 0.975)

par(mfrow = c(1, 1))
# QQ-plot
qq_teorico <- qnorm(ppoints(n))

plot(
  qq_teorico,
  sort(r_q),
  xlab = "Quantis teóricos normais",
  ylab = "Resíduos quantílicos",
  main = "QQ-plot com envelope de 95% - Modelo Binomial Negativa",
  pch = 19
)

polygon(
  c(qq_teorico, rev(qq_teorico)),
  c(lim_inf, rev(lim_sup)),
  col = adjustcolor("gray", alpha.f = 0.4),
  border = NA
)

points(qq_teorico, sort(r_q), pch = 19)
abline(0, 1, col = "red", lwd = 2)

# fazendo analise de sensibilidade
observacoes <- c(36,72,104)
resultados_sens <- data.frame()
fits_analise <- c()
result_selecao <- data.frame()
for (obs in observacoes) {

  # Remover a observação
  df_temp <- df_quine[-obs, ]

  fits_analise[[obs]] <- glm(
    formula_m4_corrigida,
    family = poisson(link = "log"),
    data = df_temp
  )
  # pegando estimativas
  coeficientes <- summary(fits_analise[[obs]])$coefficients
  coeficientes
  estimativas <- coeficientes[,1]
  erro_padrao <- coeficientes[,2]


  tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(fits_analise[[obs]]),
                        exp(cbind(estimativa = coef(fits_analise[[obs]]), confint(fits_analise[[obs]]))),
                        pvalor = coeficientes[,4])


  df_analise_sens <- as.data.frame(tabela_estim)
  df_analise_sens$observacao_removida <- obs
  resultados_sens <- rbind(resultados_sens, df_analise_sens)

  selecao <- data.frame(
    observacao_removida = obs,
    AIC = AIC(fits_analise[[obs]]))
  result_selecao <- rbind(result_selecao, selecao)
}
View(resultados_sens)

result_selecao
aic_min = min(result_selecao$AIC)
result_selecao$AIC_j = result_selecao$AIC - aic_min
result_selecao$pesos_akaike = exp(-result_selecao$AIC_j/2)/(sum(exp(-result_selecao$AIC_j/2)))
result_selecao

fit.model = fits_analise[[72]]
X <-model.matrix(fit.model)
n <-nrow(X)
p <-ncol(X)
w <-fit.model$weights
W <-diag(w)
H <-solve(t(X)%*%W%*%X)
H <-sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <-diag(H)
ts <-resid(fit.model,type="pearson")*sqrt(1/(1-h))
td <-resid(fit.model,type="deviance")*sqrt(1/(1-h))
di <-(h/(1-h))*(ts^2)
par(mfrow=c(1,1))
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16, ylim=c(0,15), type="h")
cut = mean(di) + 4*sd(di)
abline(cut,0,lty=2,col="red")

# removendo 72 e 104 em conjunto

df_temp <- df_quine[-c(72,104,36), ]

fits_analise[["conjunto_72_104_36"]] <- glm(
  formula_m4_corrigida,
  family = poisson(link = "log"),
  data = df_temp
)
# pegando estimativas
coeficientes <- summary(fits_analise[["conjunto_72_104_36"]])$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]


tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(fits_analise[["conjunto_72_104_36"]]),
                      exp(cbind(estimativa = coef(fits_analise[["conjunto_72_104_36"]]), confint(fits_analise[["conjunto_72_104_36"]]))),
                      pvalor = coeficientes[,4])


AIC(fits_analise[["conjunto_72_104_36"]])

# vendo distancia de cook

fit.model = fits_analise[["conjunto_72_104_36"]]
X <-model.matrix(fit.model)
n <-nrow(X)
p <-ncol(X)
w <-fit.model$weights
W <-diag(w)
H <-solve(t(X)%*%W%*%X)
H <-sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <-diag(H)
ts <-resid(fit.model,type="pearson")*sqrt(1/(1-h))
td <-resid(fit.model,type="deviance")*sqrt(1/(1-h))
di <-(h/(1-h))*(ts^2)
par(mfrow=c(1,1))
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16, ylim=c(0,8), type="h")
cut = mean(di) + 4*sd(di)
abline(cut,0,lty=2,col="red")


# estimativas do modelo m4
coeficientes <- summary(ajuste_m4_corrigido)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]


tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(ajuste_m4_corrigido),
                      exp(cbind(estimativa = coef(ajuste_m4_corrigido), confint(ajuste_m4_corrigido))),
                      pvalor = coeficientes[,4])

tabela_estim


# validação cruzada


set.seed(1234)
V <- 10
fold <- sample(rep(1:V, length.out = nrow(df_quine)))
pred_cv <- rep(NA_real_, nrow(df_quine))
df_result_fold <- data.frame()
for (v in 1:V) {
  treino <- df_quine[fold != v, ]
  validacao <- df_quine[fold == v, ]
  ajuste_poisson <- glm(formula_m4_corrigida,
                        family = poisson(link = "log"),
                        data = treino)
  ajuste_qp <- glm(formula_m4_corrigida,
                   family = quasipoisson(link = "log"),
                   data = treino)
  ajuste_nb <- MASS::glm.nb(formula_m4_corrigida,
                           data = treino)
  
  
  df_fold <- data.frame(
    fold = v,
    pred_poi = predict(ajuste_poisson,
                       newdata = validacao,type = "response"),
    pred_qp = predict(ajuste_qp,
                       newdata = validacao,type = "response"),
    pred_nb = predict(ajuste_nb,
                       newdata = validacao,type = "response")
  )
  df_result_fold <- rbind(df_result_fold, df_fold)
  
}

df_result_fold

y <- df_quine$Days
mu_poi <- df_result_fold$pred_poi
mu_qp <- df_result_fold$pred_qp
mu_nb <- df_result_fold$pred_nb

erro_poi <- y - mu_poi
erro_qp <- y - mu_qp
erro_nb <- y - mu_nb

termo_poi <- ifelse(y == 0,0,y*log(y/mu_poi))
termo_qp <- ifelse(y == 0,0,y*log(y/mu_qp))
termo_nb <- ifelse(y == 0,0,y*log(y/mu_nb))

desvio_cv_poi <- 2*sum(termo_poi-(y - mu_poi))
desvio_medio_poi <- desvio_cv_poi/length(y)

desvio_cv_qp <- 2*sum(termo_qp-(y - mu_qp))
desvio_medio_qp <- desvio_cv_qp/length(y)

desvio_cv_nb <- 2*sum(termo_nb-(y - mu_nb))
desvio_medio_nb <- desvio_cv_nb/length(y)

desvio_medio_poi
desvio_medio_qp
desvio_medio_nb
  
MAE <- mean(abs(erro_poi))
RMSE <- sqrt(mean(erro_poi^2))
c(MAE = MAE, RMSE = RMSE)

MAE <- mean(abs(erro_qp))
RMSE <- sqrt(mean(erro_qp^2))
c(MAE = MAE, RMSE = RMSE)


MAE <- mean(abs(erro_nb))
RMSE <- sqrt(mean(erro_nb^2))
c(MAE = MAE, RMSE = RMSE)

# residuos dos modelos

# dispersao de pearson

phi_P <-sum(residuals(modelo_selecionado, type = "pearson")^2) / df.residual(modelo_selecionado)
phi_P

# dispersao baseada no desvio

phi_D <- deviance(modelo_selecionado) / df.residual(modelo_selecionado)
phi_D

# quase-poisson
# dispersao de pearson

phi_P <-sum(residuals(ajuste_qp, type = "pearson")^2) / df.residual(ajuste_qp)
phi_P

# dispersao baseada no desvio

phi_D <- deviance(ajuste_qp) / df.residual(ajuste_qp)
phi_D

# binomial negativa
# dispersao de pearson

phi_P <-sum(residuals(ajuste_nb, type = "pearson")^2) / df.residual(ajuste_nb)
phi_P

# dispersao baseada no desvio

phi_D <- deviance(ajuste_nb) / df.residual(ajuste_nb)
phi_D

# comparação com a binomial negativa

emm <- emmeans(ajuste_nb, ~Eth)
summary(emm, type = "response")

pairs(emm, adjust = "tukey", type = "response")

emm <- emmeans(ajuste_nb, ~Sex)
summary(emm, type = "response")

pairs(emm, adjust = "tukey", type = "response")
