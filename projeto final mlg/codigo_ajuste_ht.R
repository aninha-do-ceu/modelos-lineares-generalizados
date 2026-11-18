# trabalho final - MLG Reg 2 - Analise Municipal de Denuncias de Traf Humano

setwd("c:/users/rosan/projeto/Regressão 2 - Modelos Lineares Generalizados/projeto final mlg")

library(MASS)
library(ggplot2)
library(dplyr)
library(corrplot)

df_ht <- read.csv('df_denuncias_municipio.csv',sep=';')
head(df_ht)

# valores nulos, ausentes

sum(is.na(df_ht))

table(df_ht$contato_com_br)

# cirando a taxa de denuncias por 100 mil habitantes
df_ht <- df_ht %>%
  mutate(
    taxa_denuncias = (total_denuncias/ total_populacao)
  )

# categorias de referencia de contato com br (referencia sao municipios sem contato)

df_ht$contato_com_br <- relevel(factor(df_ht$contato_com_br), ref = "0")

# medidas descritivas das variaveis numericas
summary(df_ht)

# distribuicao de denuncias
par(mfrow=c(1,1))
hist(
  df_ht$total_denuncias,
  main = "Histograma de Denúncias",
  xlab = "Número de Denúncias",
  ylab = "Frequência",
  col = "steelblue",
  border = "black"
)

# proporcao de municipios sem denuncias
prop_zeros <- count(df_ht[df_ht$total_denuncias == 0,])*100/length(df_ht$total_denuncias)
paste0("A proporção de zeros da tabela é ", round(prop_zeros,3), "%")

# 47% de zeros

# por ter contato com BR

plot <- ggplot(df_ht, aes(x = total_denuncias)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  scale_x_continuous(
    trans = "log1p",
    breaks = c(0, 1, 5, 10, 20, 50, 100, 500, 1500)
  )+
  facet_wrap(facets = vars(contato_com_br), 
             labeller =as_labeller(c("0" = "Sem Contato","1" = "Com Contato"))
  )+
  labs(
    title = "Número de Denúncias por Municípios com e sem BR",
    x = "Número de Denúncias",
    y = "Frequência"
  ) +
  theme_minimal()
plot

estats <- df_ht %>%
  group_by(contato_com_br) %>%
  summarise(
    Mediana = median(total_denuncias),
    Media = mean(total_denuncias),
    Variancia = var(total_denuncias),
    Tamanho = n()
  )
estats

df_ht_plot <- df_ht %>%
  filter(total_denuncias > 0)

plot <- ggplot(df_ht_plot, aes(x = total_denuncias)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "black"
  ) +
  scale_x_continuous(
    trans = "log1p",
    breaks = c(0, 1, 5, 10, 20, 50, 100, 500, 1500)
  )+
  facet_wrap(
    vars(contato_com_br),
    labeller = as_labeller(c(
      `0` = "Sem Contato",
      `1` = "Com Contato"
    ))
  ) +
  labs(
    title = paste0("Número de Denúncias por Municípios com e sem BR "),
    subtitle = "Considerando apenas municípios com pelo menos uma denúncia",
    x = "Número de Denúncias",
    y = "Frequência"
  ) +
  theme_minimal()

plot

colnames(df_ht)
cols_num <- c('total_denuncias','media_anos_estudo_mulheres',
              'media_anos_estudo_homens', 'total_populacao', 'qtde_homens',
              'qtde_mulheres', 'proporcao_rural', 'prop_mulheres_trabalho',
              'forca_trabalho_mulheres_ocup', 'forca_trabalho_homens_ocup',
              'num_criancas_casadas_masc', 'num_criancas_casadas_femin',
              'num_criancas_casadas')
mat_cor <- cor(
  df_ht[, cols_num],
  method = "spearman",
  use = "complete.obs"
)
corrplot(mat_cor)

# normalizar entre 0 e 1

df_ht <-df_ht %>%
  mutate(
    media_anos_estudo_mulheres_norm = (media_anos_estudo_mulheres - min(media_anos_estudo_mulheres))/(max(media_anos_estudo_mulheres ) - min(media_anos_estudo_mulheres )),
    prop_mulheres_trabalho_norm = (prop_mulheres_trabalho - min(prop_mulheres_trabalho))/(max(prop_mulheres_trabalho ) - min(prop_mulheres_trabalho )),
    forca_trabalho_mulheres_ocup_norm = (forca_trabalho_mulheres_ocup - min(forca_trabalho_mulheres_ocup))/(max(forca_trabalho_mulheres_ocup ) - min(forca_trabalho_mulheres_ocup )),
    num_criancas_casadas_norm = (num_criancas_casadas - min(num_criancas_casadas))/(max(num_criancas_casadas ) - min(num_criancas_casadas ))
    )

variaveis_modelo <-c('total_denuncias','media_anos_estudo_mulheres', 'total_populacao', 
                     'proporcao_rural', 'prop_mulheres_trabalho',
                     'forca_trabalho_mulheres_ocup',
                     'num_criancas_casadas')
mat_cor <- cor(
  df_ht[, variaveis_modelo],
  method = "spearman",
  use = "complete.obs"
)
corrplot(mat_cor)

# correlacao com contato_com_br

df_ht$contato_com_br_num <- ifelse(
  df_ht$contato_com_br == "1", 1, 0
)
cor_br <- sapply(variaveis_modelo, function(x) {
  cor(
    df_ht[[x]],
    df_ht$contato_com_br_num,
    method = "pearson",
    use = "complete.obs"
  )
})

sort(cor_br, decreasing = TRUE)

# sem alta correlação com as demais variaveis

formula_m0 <- as.formula(
  "total_denuncias ~ offset(log(total_populacao)) + contato_com_br + proporcao_rural")


formula_m1 <- as.formula(
  "total_denuncias ~  offset(log(total_populacao)) + contato_com_br + 
     proporcao_rural + prop_mulheres_trabalho + num_criancas_casadas")



formula_m2 <- as.formula(
  "total_denuncias ~  offset(log(total_populacao)) + contato_com_br + 
     proporcao_rural + num_criancas_casadas")


formula_m3 <- as.formula(
  paste("total_denuncias ~ offset(log(total_populacao)) + contato_com_br +",paste(variaveis_modelo[c(-1,-3)], collapse = " + "))
)

# formula 3 corrigida

formula_m3_corrig <- as.formula(
  paste("total_denuncias ~ offset(log(total_populacao)) + contato_com_br +",paste(variaveis_modelo[c(-1,-3,-6)], collapse = " + "))
)

ajuste0 <-glm(
  formula_m0,
  family = poisson(link = "log"),
  data = df_ht
)
summary(ajuste0)


ajuste1 <-glm(
  formula_m1,
  family = poisson(link = "log"),
  data = df_ht
)
summary(ajuste1)


ajuste2 <-glm(
  formula_m2,
  family = poisson(link = "log"),
  data = df_ht
)
summary(ajuste2)


ajuste3 <-glm(
  formula_m3,
  family = poisson(link = "log"),
  data = df_ht
)
summary(ajuste3)

ajuste3_corrig <-glm(
  formula_m3_corrig,
  family = poisson(link = "log"),
  data = df_ht
)
summary(ajuste3_corrig)

# comparando os ajustes

anova(ajuste0,ajuste1, test = "LRT")
anova(ajuste1,ajuste2, test = "LRT")
anova(ajuste2,ajuste3, test = "LRT")
anova(ajuste3,ajuste3_corrig, test="LRT")


# AIC 

tabela_selecao <- data.frame(
  Modelo = c("M0","M1","M2","M3","M3 corrigido"),
  AIC = c(AIC(ajuste0),AIC(ajuste1),AIC(ajuste2),AIC(ajuste3),AIC(ajuste3_corrig))
)
aic_min = min(tabela_selecao$AIC)
tabela_selecao$AIC_j = tabela_selecao$AIC - aic_min
tabela_selecao$pesos_akaike = exp(-tabela_selecao$AIC_j/2)/(sum(exp(-tabela_selecao$AIC_j/2)))
tabela_selecao

# plots dos ajustes

par(mfrow = c(2,2))
plot(ajuste0)
plot(ajuste1)
plot(ajuste2)
plot(ajuste3)
plot(ajuste3_corrig)

# os residuos nao seguem uma normal, parecem seguir exponencial,
# uma binomial negativa pode fazer mais sentido pela relacao media-variancia

mean(df_ht$total_denuncias)
var(df_ht$total_denuncias)

# a variancia é muito maior que a media: 2794.142 x 7
# vendo por estado

df_ht_estado <- merge(
  aggregate(total_denuncias ~ estado_x, data = df_ht, FUN = mean),
  aggregate(total_denuncias ~ estado_x, data = df_ht, FUN = var),
  by = "estado_x",
  suffixes = c("_media", "_variancia")
)

# modelo selecionado para verificar os coeficientes

# estimativas do modelo selecionado
modelo_selecionado <- ajuste3_corrig
coeficientes <- summary(modelo_selecionado)$coefficients
coeficientes
estimativas <- coeficientes[,1]
erro_padrao <- coeficientes[,2]


tabela_estim <- cbind(coeficiente = estimativas, erro_padrao = erro_padrao, confint(modelo_selecionado),
                      exp(cbind(estimativa = coef(modelo_selecionado), confint(modelo_selecionado))),
                      pvalor = coeficientes[,4])

tabela_estim


# municipios com contato com rodovias tem uma razão de média levemente menor que os demais
# conforme a media de anos de estudo de mulheres cresce, a razão de medias de denuncias
# cai também, é um decrescimo de 42,8%
# quanto mais rural, há acréscimo de 38,9% no numero medio de denuncias
# quanto maior a proporcao de mulheres no trabalho, há acréscimo de 88,9% no numero medio de denuncias

# proporcao de zeros
prop_zeros
mu_hat <- fitted(modelo_selecionado)
prop_zeros_modelo <- sum(exp(-mu_hat))*100/length(df_ht$Cód.)

paste0("A proporção de zeros da tabela é ", round(prop_zeros,3), "%")
paste0("A proporção de zeros do modelo é ", round(prop_zeros_modelo,3), "%")

# para o modelo é 29% enquanto a proporcao de zeros é 47% na base

# testando com binomial negativa

fit_QP <-glm(formula_m3_corrig,
             family = quasipoisson(link = "log"),
             data = df_ht)
# Variancia quadratica na media
fit_NB <-MASS::glm.nb(formula_m3_corrig,
                      data = df_ht)

summary(fit_QP)
summary(fit_NB)
# somente media de anos de estudos e proporcao de mulheres trab sao significantes...
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

plot(fit_QP)
plot(fit_NB)

AIC(modelo_selecionado)
AIC(fit_NB)

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


n <- length(df_ht$Município)
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

F_inf <- pnbinom(df_ht$total_denuncias - 1,mu = mu_hat,size = theta)
F_sup <- pnbinom(df_ht$total_denuncias,mu = mu_hat,size = theta)
u <- runif(nrow(df_ht), F_inf, F_sup)
eps <- 1e-10
u <- pmin(pmax(u, eps), 1 - eps)
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

# testando os dois modelos QP e NB

formula_qp_nb <- as.formula(
  paste("total_denuncias ~ offset(log(total_populacao)) +",paste(variaveis_modelo[c(2,5)], collapse = " + "))
)

fit_QP <-glm(formula_qp_nb,
             family = quasipoisson(link = "log"),
             data = df_ht)
# Variancia quadratica na media
fit_NB <-MASS::glm.nb(formula_qp_nb,
                      data = df_ht)

summary(fit_QP)
summary(fit_NB)

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

# quase poisson indica decrescimo com as duas variaveis
# binomial negativa indica acrescimo com a var prop_mulheres_trabalho

anova(fit_QP, test = "LRT")
anova(fit_NB, test = "LRT")

par(mfrow = c(2, 2))
plot(fit_QP)
plot(fit_NB)

# testando com distribuição Bell

library(LambertW)
require(LambertW)
bell.reg <- function(formula, data){
  cl <- match.call()
  if(missing(data))
    data <- environment(formula)

  mf <- match.call(expand.dots =FALSE)
  m <- match(c("formula","data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  oformula <- as.formula(formula)
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- terms(formula, data=data)
  Y <- model.response(mf, "numeric")
  X <- model.matrix(mf)
  if (length(Y) < 1)
    stop("empty model")
  if (!(min(Y) >= 0))
    stop("invalid dependent variable")

  floglikBell <- function(vPar){
    veta <- X%*%vPar
    vmu <- exp(veta)
    loglik <- sum(-exp(LambertW::W(vmu)) + Y*log(LambertW::W(vmu)))
    loglik
  }
  
  fscoreBell <- function(vPar){
    veta <- X%*%vPar
    vmu <- exp(veta)
    vt <- 1/(1 + LambertW::W(vmu))
    mT <- diag(as.vector(vt))
    score <- t(X) %*% mT %*% (Y-vmu)
    score
  }
  
  fFisherBell <- function(vPar){
    veta <- X%*%vPar
    vmu <- exp(veta)
    vw <- vmu/(1+LambertW::W(vmu))
    mW <- diag(as.vector(vw))
    mF <- t(X)%*%mW %*% X
    mF
    
  }

  fit0 <- glm(formula, family=poisson(link=log), data=data)
  start <- fit0$coef
  opt <- optim(start, fn=floglikBell, gr = fscoreBell,
               method="BFGS", control=list(fnscale=-1),hessian=FALSE)
  if (opt$conv != 0)
    stop("nao convergiu")

  beta <- opt$par
  se <- sqrt(diag(solve(fFisherBell(beta))))
  z.value <- beta/se
  p.value <- 1 - pnorm(abs(z.value))
  names(beta) <- colnames(X)
  rval <- cbind(round(beta, 6), round(se, 6),
                round(z.value, 6), round(p.value, 6))
  colnames(rval) <- c("Estimate","Std. Error","z value","Pr(>|z|)")
  return(list(
    resumo = rval,
    coefficients = beta,
    vcov = solve(fFisherBell(beta)),
    formula = formula,
    terms = terms(formula),
    xlevels = lapply(mf, function(x) {
      if (is.factor(x)) levels(x) else NULL
    })
  ))
}

bell.fit <- bell.reg(df_ht$total_denuncias ~ offset(log(df_ht$total_populacao)) + 
                       df_ht$media_anos_estudo_mulheres + 
                       df_ht$prop_mulheres_trabalho
                     )
bell.fit$resumo

# fazendo predict
predict.bell <- function(object, newdata, type = "response") {
  
  X <- model.matrix(
    delete.response(object$terms),
    data = newdata
  )
  
  beta <- object$coefficients
  
  eta <- as.vector(X %*% beta)
  
  if (type == "link") {
    return(eta)
  }
  
  mu <- exp(eta)
  
  if (type == "response") {
    return(mu)
  }
  
  if (type == "variance") {
    return(mu * (1 + mu))
  }
  
  stop("Tipo de previsão inválido")
}

# Valores observados
y <- df_ht$total_denuncias

# Média estimada
pred_media <- predict.bell(
  bell.fit,
  newdata = df_ht,
  type = "response"
)
res_bell <- y - pred_media
par(mfrow=c(1,1))
# residuos vs valores ajustados
plot(res_bell, pred_media)


# Matriz do modelo
X <- model.matrix(formula_qp_nb, data = df_ht)

# Coeficientes estimados
beta <- bell.fit$resumo[,"Estimate"]

colnames(X) <- ifelse(
  colnames(X) == "(Intercept)",
  "(Intercept)",
  paste0("df_ht$", colnames(X))
)
# Garantir a ordem correta dos coeficientes
beta <- beta[colnames(X)]

# Valores ajustados (média)
mu_hat <- as.vector(exp(X %*% beta))

# Função de Lambert W
W_mu <- LambertW::W(mu_hat)

# Inicializar componentes do desvio
d2 <- numeric(length(y))

# desvio de bell
d2 <- ifelse(y == 0,
             2*exp(1 - LambertW::W(mu_hat)),
             2*(exp(LambertW::W(mu_hat)) - exp(LambertW::W(y)) + y*(log(LambertW::W(y)/LambertW::W(mu_hat)))))


# Resíduos componentes do desvio
dev_res <- sign(y - mu_hat) * sqrt(d2)

# Adicionar à base
df_ht$residuos_desvio_bell <- dev_res

# Desvio total
desvio <- sum(d2)

desvio

# Pesos
w <- mu_hat / (1 + LambertW::W(mu_hat))

# Matriz de informação
M <- crossprod(X, w * X)

# Matriz chapéu
H <- diag(sqrt(w)) %*%
  X %*%
  solve(M) %*%
  t(X) %*%
  diag(sqrt(w))

# Alavancagem
h <- diag(H)

# Resíduos modificados
df_ht$residuos_desvio_mod_bell <- dev_res / sqrt(1 - h)

par(mfrow = c(1, 2))

plot(
  mu_hat,
  dev_res,
  xlab = "Valores ajustados",
  ylab = "Resíduos do desvio",
  main = "Resíduos componentes do desvio"
)
abline(h = 0, col = "red", lty = 2)

# Distância de Cook
cook <- (dev_res^2 / length(beta)) * h / (1 - h)

# Gráfico
plot(
  cook,
  type = "h",
  ylab = "Distância de Cook",
  xlab = "Observações",
  main = "Distância de Cook - Modelo Bell"
)

# Linha de referência
abline(h = 4 / nrow(df_ht), col = "red", lty = 2)

# residuos quantilicos

# acumulada da bell ZM
pBe <- function(y,theta){
  y = as.numeric(y)
  theta = as.numeric(theta)
  cum = 0
  f = exp(exp(theta) - 1)
  g = theta
  for(i in 1:length(y)){
    a = numbers::bell(y)/factorial(y)
    cum = cum + a*g/f
    
  }
  return(cum)
}

pBe <- function(y, theta) {
  
  y <- as.numeric(y)
  theta <- as.numeric(theta)
  
  log_p <- log(numbers::bell(y)) -
    lgamma(y + 1) +
    y * log(theta) -
    exp(theta) + 1
  
  exp(log_p)
}

set.seed(1234)

B_inf <- c()
B_sup <- c()
for (i in 1:nrow(df_ht)){
  if (y[i] == 0){
    b_sup <- pBe(y = y[i], theta=mu_hat[i])
    B_inf[i] = 0
    B_sup[i] =b_sup
  }
  else{
    b_inf <- pBe(y = y[i]-1, theta=mu_hat[i])
    b_sup <- pBe(y = y[i], theta=mu_hat[i])
    B_inf[i] = b_inf
    B_sup[i] = b_sup
  }
}

u <- runif(nrow(df_ht), B_inf, B_sup)
eps <- 1e-10
u <- pmin(pmax(u, eps), 1 - eps)
r_q <- qnorm(u)
# Gráficos dos resíduos quantílicos
par(mfrow = c(1, 2))
qqnorm(
  r_q,
  main = "QQ-plot dos resíduos quantílicos"
)
qqline(r_q, col = 2, lwd = 2)


plot(y, mu_hat)

# vendo os indices
# Índices dos valores NA/NaN
which(is.nan(B_inf))

# Índices dos valores Inf e -Inf
which(is.infinite(B_inf))

# Índices dos valores NA/NaN
which(is.nan(B_sup))

# Índices dos valores Inf e -Inf
which(is.infinite(B_sup))

[1]  112  951 1165 1339 1598 1698 1918 2164 2311 2312 3264 3265 3266 3267 3509 3510 3860
[18] 3861 4982 4983 5174 5602 5635

df_ht[112,]

plot(df_ht$total_populacao, df_ht$taxa_denuncias)


# modelo zero-inflacionado poisson

# modelo gamlss

library(gamlss)
help(gamlss)

# familias ZIBNB e ZIP

mo_ZIP <- gamlss(df_ht$total_denuncias ~ offset(log(df_ht$total_populacao)) + 
                   df_ht$media_anos_estudo_mulheres_norm + 
                   df_ht$prop_mulheres_trabalho,
                 data=df_ht,
                 family=ZIP
)
summary(mo_ZIP)
wp(mo_ZIP)
plot(mo_ZIP)

mo_ZIBNB <- gamlss(df_ht$total_denuncias ~ offset(log(df_ht$total_populacao)) + 
                     df_ht$media_anos_estudo_mulheres_norm + 
                     df_ht$prop_mulheres_trabalho,
                 data=df_ht,
                 family=ZIBNB,
                 n.cyc=200
)
summary(mo_ZIBNB)
wp(mo_ZIBNB)
plot(mo_ZIBNB)

mo_ZIPIG <- gamlss(df_ht$total_denuncias ~ offset(log(df_ht$total_populacao)) + 
                     df_ht$media_anos_estudo_mulheres_norm + 
                     df_ht$prop_mulheres_trabalho,
                   data=df_ht,
                   family=ZIPIG,
                   n.cyc=200
)
summary(mo_ZIPIG)
wp(mo_ZIPIG)
plot(mo_ZIPIG)


mo_ZIBNB_mu <- gamlss(total_denuncias ~offset(log(total_populacao)) ,
                   data=df_ht,
                   family=ZIBNB,
                   n.cyc = 200
)

# escolhendo modelo para mu
m_ZIBNB_mu <- stepGAIC(mo_ZIBNB_mu,
                     scope=list(lower = ~1,
                                upper= ~ offset(log(total_populacao)) + contato_com_br + 
                                  media_anos_estudo_mulheres_norm + proporcao_rural + prop_mulheres_trabalho_norm + 
                                  forca_trabalho_mulheres_ocup_norm + num_criancas_casadas_norm),
                     what="mu"
)
summary(m_ZIBNB_mu)

