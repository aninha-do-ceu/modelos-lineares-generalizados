rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Seção A
# pegando dados budworm
setwd("C:/Users/rosan/Projeto/Regressão 2 - Modelos Lineares Generalizados/modelo dose resposta")
load("budworm.rda")
ls()

df_bud <-budworm
head(df_bud)

summary(df_bud)

# renomeando para as variaveis da atividade
nomes <- c("x", "mortos", "sexo")
names(df_bud) <- nomes

# incluindo as variaveis dose e expostos
df_bud$expostos <- 20
df_bud$dose <- 2^df_bud$x
df_bud

# o sucesso é morte, assim pi_j = P(Y=1|d_j,s_j)e. O modelo é binomial com parametros 20 e 
#pi_j 

df_bud$proporcao_morte <- df_bud$mortos/df_bud$expostos
df_bud

# grafico

plt2<-ggplot(df_bud, aes(x = x, y = proporcao_morte, color = sexo,
                   group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Logaritmo da dose",
    y = "Proporção de mortalidade",
    color = "Sexo",
    title = "Proporção de Mortalidade por Sexo"
  ) +
  theme_minimal()


plt1<-ggplot(df_bud, aes(x=x, y = proporcao_morte, color = sexo,
                   group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Dose (microgramas)",
    y = "Proporção de mortalidade",
    color = "Sexo",
    title = "Proporção de Mortalidade por Sexo"
  ) +
  theme_minimal()

(plt1|plt2)

df_bud$sexo_bin <- ifelse(df_bud$sexo == "F", 0, 1)

# modelos M0,M1 e M2
modelos <- list(
  m0 = cbind(mortos, expostos - mortos) ~ x,
  m1 = cbind(mortos, expostos - mortos) ~ x+sexo_bin,
  m2 = cbind(mortos, expostos - mortos) ~ x*sexo_bin
)

#Ligacoes
links <- c("logit","probit","cloglog")

fits <- list()
for (m in names(modelos)) {
  for (lk in links) {
    nome <- paste(m, lk, sep = "_")
    
    fits[[nome]] <- glm(
      formula = modelos[[m]],
      family = binomial(link = lk),
      data = df_bud
    )
    df_bud[[nome]] <- fits[[nome]]$fitted.values
  }
}

summary(fits$m0_logit)
summary(fits$m1_logit)
summary(fits$m2_logit)
summary(fits$m0_probit)
summary(fits$m1_probit)
summary(fits$m2_probit)
summary(fits$m0_cloglog)
summary(fits$m1_cloglog)
summary(fits$m2_cloglog)

sapply(fits, AIC)

# todos os ajustes

cores_sexo <- c(
  "F" = "red",
  "M" = "blue"
)

# Logit
p_logit <- ggplot(df_bud, aes(x=x, y = proporcao_morte, color = sexo,
                   group = sexo)) +
  geom_point(size = 2) +
  geom_line(aes(y = m0_logit, color = "m0"), linewidth = 1) +
  geom_line(aes(y = m1_logit, color = "m1"), linewidth = 1) +
  geom_line(aes(y = m2_logit, color = "m2"), linewidth = 1) +
  scale_color_manual(values = c(
    "F" = "red",
    "M" = "blue",
    "m0" = "darkgreen",
    "m1" = "purple",
    "m2" = "orange"
  )) +
  labs(
    x = "Log(Dose) (log microgramas)",
    y = "Proporção de mortalidade",
    color = "Sexo / Modelo",
    title = "Modelos com Ligação Logit"
  ) +
  theme_minimal()

p_probit <- ggplot(df_bud, aes(x=x, y = proporcao_morte, color = sexo,
                   group = sexo)) +
  geom_point(size = 2) +
  geom_line(aes(y = m0_probit, color = "m0"), linewidth = 1) +
  geom_line(aes(y = m1_probit, color = "m1"), linewidth = 1) +
  geom_line(aes(y = m2_probit, color = "m2"), linewidth = 1) +
  scale_color_manual(values = c(
    "F" = "red",
    "M" = "blue",
    "m0" = "darkgreen",
    "m1" = "purple",
    "m2" = "orange"
  )) +
  labs(
    x = "Log(Dose) (log microgramas)",
    y = "Proporção de mortalidade",
    color = "Sexo / Modelo",
    title = "Modelos com Ligação Probit"
  ) +
  theme_minimal()

p_cloglog <- ggplot(df_bud, aes(x=x, y = proporcao_morte, color = sexo,
                   group = sexo)) +
  geom_point(size = 2) +
  geom_line(aes(y = m0_cloglog, color = "m0"), linewidth = 1) +
  geom_line(aes(y = m1_cloglog, color = "m1"), linewidth = 1) +
  geom_line(aes(y = m2_cloglog, color = "m2"), linewidth = 1) +
  scale_color_manual(values = c(
    "F" = "red",
    "M" = "blue",
    "m0" = "darkgreen",
    "m1" = "purple",
    "m2" = "orange"
  )) +
  labs(
    x = "Log(Dose) (log microgramas)",
    y = "Proporção de mortalidade",
    color = "Sexo / Modelo",
    title = "Modelos com Ligação Cloglog"
  ) +
  theme_minimal()

# Mostrar os gráficos
p_logit

p_probit

p_cloglog

# comparação entre os modelos
comparacao <- do.call(rbind, lapply(names(fits), function(nome) {
  fit <- fits[[nome]]
  data.frame(Modelo = nome, Desvio = deviance(fit), GL = df.residual(fit),
             p_desvio = pchisq(deviance(fit), df.residual(fit), lower.tail = FALSE),
             Pearson_GL = sum(residuals(fit, type = "pearson")^2) / df.residual(fit),
             AIC = AIC(fit))
}))
comparacao$Delta_AIC <- comparacao$AIC - min(comparacao$AIC)
comparacao$Peso_AIC <- with(comparacao,
                            exp(-Delta_AIC / 2) / sum(exp(-Delta_AIC / 2)))
knitr::kable(comparacao, digits = 4, row.names = FALSE)

# tabela com medidas
medidas <- function(nome) {
  fit <- fits[[nome]]
  
  b <- coef(fit)
  se <- sqrt(diag(vcov(fit)))
  
  data.frame(
    Modelo = nome,
    b0 = unname(b[1]),
    ep_b0 = unname(se[1]),
    # parametro x
    b1 = if (length(b) >= 2) unname(b[2]) else NA_real_,
    ep_b1 = if (length(se) >= 2) unname(se[2]) else NA_real_,
    # parametro sexo
    b2 = if (length(b) >= 3) unname(b[3]) else NA_real_,
    ep_b2 = if (length(se) >= 3) unname(se[3]) else NA_real_,
    # parametro dose x * sexo
    b3 = if (length(b) >= 4) unname(b[3]) else NA_real_,
    ep_b3 = if (length(se) >= 4) unname(se[3]) else NA_real_,
    logverossimilhanca = as.numeric(logLik(fit)),
    desvio_residual = deviance(fit),
    graus_liberdade = df.residual(fit),
    AIC = AIC(fit)
  )
}

# Construir o dataframe
tabela_modelos <- do.call(
  rbind,
  lapply(names(fits), medidas)
)

# Arredondar medidas numéricas
cols <- c(
  "b0", "ep_b0", "b1", "ep_b1","b2", "ep_b2","b3", "ep_b3",
  "logverossimilhanca", "desvio_residual", "AIC"
)

tabela_modelos[cols] <- lapply(
  tabela_modelos[cols],
  round,
  digits = 3
)

tabela_modelos
# vendo convergencia por modelo

modelos <- names(fits)

for(modelo in modelos){
  if(fits[[modelo]]$converged == TRUE){
    print(paste("Modelo ",modelo, " convergiu!"))
  }
  else{
    print(paste("Modelo ",modelo, " não convergiu..."))
  }
}

# pesos de akaike

tabela_selecao <- tabela_modelos[c("Modelo","AIC")]
aic_min = min(tabela_selecao$AIC)
tabela_selecao$AIC_j = tabela_selecao$AIC - aic_min
tabela_selecao$pesos_akaike = exp(-tabela_selecao$AIC_j/2)/(sum(exp(-tabela_selecao$AIC_j/2)))
tabela_selecao

# teste razao de verossimilhanças

tabela_modelos$ligacao <- rep(c("logit","probit","cloglog"),3)
tabela_modelos$modelo_num <- rep(c("M0","M1","M2"),each=3)
df_testes <- data.frame(
  ligacao = character(),
  gl_m1 = numeric(),
  gl_m2 = numeric(),
  p_valor = numeric()
)
for(ligacao in links){
  m1 <- fits[[paste0("m1_",ligacao)]]
  m2 <- fits[[paste0("m2_", ligacao)]]
  
  teste <- anova(m1,m2,test="LRT")
  
  #tab <- tabela_modelos[tabela_modelos$ligacao == ligacao,]
  
  # graus de liberdade m1
  df_m1 <- teste$`Resid. Df`[1]
  # graus de liberdade m2
  df_m2 <- teste$`Resid. Df`[2]
  
  pvalor <- teste$`Pr(>Chi)`[2]
  
  df_testes <- rbind(
    df_testes,
    data.frame(
      ligacao = ligacao,
      gl_m1 = df_m1,
      gl_m2 = df_m2,
      p_valor = pvalor
    )
  )
}
df_testes

dict_sexo <- list("F"="Fêmeas","M"="Machos")
sexos <- unique(df_bud$sexo)
modelos <- c("m0", "m1", "m2")
links <- c("logit", "probit", "cloglog")

plots_sexo <- list()
for (s in sexos) {
  df_sexo <- df_bud[df_bud$sexo == s, ]
  
  for (modelo in modelos){
    nomes_modelos <- paste0(modelo,"_",links)
    p <- ggplot(df_sexo, aes(x=x, y = proporcao_morte)) +
      geom_point(size = 2) +
      geom_line(aes(y = .data[[nomes_modelos[1]]], color = links[1]), linewidth = 1) +
      geom_line(aes(y = .data[[nomes_modelos[2]]], color = links[2]), linewidth = 1) +
      geom_line(aes(y = .data[[nomes_modelos[3]]], color = links[3]), linewidth = 1) +
      scale_color_manual(values = c(
        "logit" = "darkgreen",
        "probit" = "purple",
        "cloglog" = "orange"
      )) +
      labs(
        x = "Log(Dose) (log microgramas)",
        y = "Proporção de mortalidade",
        color = "Função de Ligação",
        title = paste("Modelo",modelo)
      ) +
      theme_minimal()
      plots_sexo[[paste0(as.character(s),"_",modelo)]] <- p
  }
  
}
  
(plots_sexo$M_m0 | plots_sexo$M_m1 | plots_sexo$M_m2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Modelos Ajustados para Mortalidade — Larvas Macho"
  ) &
  theme(legend.position = "bottom")


(plots_sexo$F_m0 | plots_sexo$F_m1 | plots_sexo$F_m2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Modelos Ajustados para Mortalidade — Larvas Fêmeas"
  ) &
  theme(legend.position = "bottom")



# Seção 3

# estimativas das doses letais (dl_10, dl_50 e dl_90) nos modelos M2

# funcao para calcular dose letal

dose_letal <- function(fit, p = c(.25, .50, .75), base = 10, nivel = .95, s) {
  stopifnot(all(p > 0 & p < 1), base > 1, nivel > 0, nivel < 1)
  b <- coef(fit)
  gp <- fit$family$linkfun(p)
  if (length(b) == 2){
    xp <- unname((gp - b[1]) / b[2])
    G <- cbind(-1 / b[2], (b[1] - gp) / b[2]^2)
  }
  else{
    if( length(b) == 3){
      xp <- unname((gp- b[1] - b[2]*s) / b[3])
      G <- cbind(-1 / b[2],-xp/b[2], -s/b[2])
    }
    else{
      xp <- unname((gp- b[1] - b[3]*s) / (b[2]+b[4]*s))
      G <- cbind(-1 / (b[2]+b[4]*s),-xp/(b[2]+b[4]*s), -s/(b[2]+b[4]*s), -xp/(b[2]+b[4]*s))
    }
  }
  
  ep <- sqrt(rowSums((G %*% vcov(fit)) * G))
  z <- qnorm((1 + nivel) / 2)
  li <- xp - z * ep
  ls <- xp + z * ep
  
  data.frame(s=s, p = p, Logdose = xp, EP_logdose = ep,
             LI_logdose = li, LS_logdose = ls,
             Dose = base^xp, LI_dose = base^li, LS_dose = base^ls)
}

# calculando doses por sexo
df_doses_letais <- data.frame()
doses <- c(0.1,0.5,0.9)
for(s in c(0,1)){
  for(ligacao in names(fits)){
    res <- dose_letal(fits[[ligacao]], p=doses, base=2, nivel=0.95,s=s)
    res$modelo <- ligacao
    df_doses_letais <- rbind(df_doses_letais, res)
  }
}
df_doses_letais

prob_alvo <- c(0.10, 0.50, 0.90)

inv_links <- list(
  logit   = plogis,
  probit  = pnorm,
  cloglog = function(eta) 1 - exp(-exp(eta))
)

resultados_validacao <- list()

for (ligacao in links) {
  
  fit <- fits[[paste0("m2_", ligacao)]]
  coefs <- fit$coefficients
  
  b0 <- coefs[1]
  b1 <- coefs[2]
  b2 <- coefs[3]
  b3 <- coefs[4]

  inv_g <- inv_links[[ligacao]]
  
  for (s in c(0, 1)) {
    
    # Nomes das colunas de doses letais
    cols <- paste0("DL", c(10, 50, 90), "_s", s)
    
    # Doses estimadas para essa ligação e grupo
    doses <- as.numeric(
      df_doses_letais[ligacao, cols]
    )
    
    # Preditor linear nas doses estimadas
    x <- log2(doses)
    eta <- b0 + b1*x + b2*s + b3*x*s
    
    # Probabilidades preditas pela curva ajustada
    p_predita <- inv_g(eta)
    
    resultados_validacao[[paste(ligacao, s, sep = "_")]] <-
      data.frame(
        ligacao = ligacao,
        s = s,
        mortalidade_alvo = prob_alvo,
        dose_letal = doses,
        probabilidade_predita = p_predita,
        diferenca = p_predita - prob_alvo
      )
  }
}
resultados_validacao

# seção 4 - estimacao intervalar

# metodo delta para ic de 95% de 2^{x_r}

par(mfrow = c(1,2))

grade <- data.frame(x = rep(seq(min(df_bud$x), max(df_bud$x), length.out = 250),2),
                    sexo_bin = rep(c(0,1),each=250))
s = 0
df_grade <- grade[grade$sexo_bin==s,]
pr <- predict(fits$m1_probit, newdata = df_grade, type = "link", se.fit = TRUE)
inversa <- fits$m1_probit$family$linkinv
crit_pontual <- qnorm(.975)
crit_simultaneo <- sqrt(qchisq(.95, df = length(coef(fits$m1_probit))))
limites_curva <- function(critico) {
  cbind(LI = inversa(pr$fit - critico * pr$se.fit),
        LS = inversa(pr$fit + critico * pr$se.fit))
}
pontuais <- limites_curva(crit_pontual)
simultaneos <- limites_curva(crit_simultaneo)
plot(df_grade$x, inversa(pr$fit), type = "n", ylim = c(0, 1),
     xlab = "Log-dose", ylab = "Probabilidade de morte",main="Bandas Simultâneas - Fêmeas")
polygon(c(df_grade$x, rev(df_grade$x)),
        c(simultaneos[, 1], rev(simultaneos[, 2])),
        col = "gray90", border = NA)
lines(df_grade$x, inversa(pr$fit), lwd = 2)
matlines(df_grade$x, pontuais, col = "steelblue", lty = 2)
points(df_bud[df_bud$sexo_bin==s,]$x, df_bud[df_bud$sexo_bin==s,]$proporcao_morte, pch = 19)
legend("topleft", legend = c("Curva ajustada", "IC pontual de 95%", "Banda simultânea assintótica"),
       col = c("black", "steelblue", "gray70"), lty = c(1, 2, 1),
       lwd = c(2, 1, 6), bty = "n", cex = .8)

s = 1
df_grade <- grade[grade$sexo_bin==s,]
pr <- predict(fits$m1_probit, newdata = df_grade, type = "link", se.fit = TRUE)
inversa <- fits$m1_probit$family$linkinv
crit_pontual <- qnorm(.975)
crit_simultaneo <- sqrt(qchisq(.95, df = length(coef(fits$m1_probit))))
limites_curva <- function(critico) {
  cbind(LI = inversa(pr$fit - critico * pr$se.fit),
        LS = inversa(pr$fit + critico * pr$se.fit))
}
pontuais <- limites_curva(crit_pontual)
simultaneos <- limites_curva(crit_simultaneo)
plot(df_grade$x, inversa(pr$fit), type = "n", ylim = c(0, 1),
     xlab = "Log-dose", ylab = "Probabilidade de morte",main="Bandas Simultâneas - Machos")
polygon(c(df_grade$x, rev(df_grade$x)),
        c(simultaneos[, 1], rev(simultaneos[, 2])),
        col = "gray90", border = NA)
lines(df_grade$x, inversa(pr$fit), lwd = 2)
matlines(df_grade$x, pontuais, col = "steelblue", lty = 2)
points(df_bud[df_bud$sexo_bin==s,]$x, df_bud[df_bud$sexo_bin==s,]$proporcao_morte, pch = 19)
legend("topleft", legend = c("Curva ajustada", "IC pontual de 95%", "Banda simultânea assintótica"),
       col = c("black", "steelblue", "gray70"), lty = c(1, 2, 1),
       lwd = c(2, 1, 6), bty = "n", cex = .8)



# residuos
residuos <- do.call(rbind, lapply(names(fits), function(nome) {
  mod <- fits[[nome]]
  data.frame(
    observacao = seq_len(nobs(mod)),
    modelo = nome,
    ajustado = fitted(mod),
    pearson = rstandard(mod, type = "pearson"),
    desvio = rstandard(mod, type = "deviance"),
    leverage = hatvalues(mod),
    cook = cooks.distance(mod)
  )
}))

head(residuos)
df_m1_probit <- residuos[residuos$modelo=="m1_probit",]

par(mfrow = c(1,2))
# distancia de cook
plot(df_m1_probit$observacao, df_m1_probit$cook,
     pch = 19, col = "#315d77",
     xlab = "Observação", ylab = "Distância de Cook",
     main = paste(nome, ": Distância de Cook"),
     ylim = c(0,0.6))
n<-length(df_bud$x)
estat_cook <- 4/n
abline(h = estat_cook, lty = 2, col = "#d9891b")
abline(h = 0.5, lty = 2, col = "red")

#alavancagem
plot(df_m1_probit$observacao, df_m1_probit$leverage,
     pch = 19, col = "#315d77",
     xlab = "Observação", ylab = "Alavancagem",
     main = paste(nome, ": Alavancagem"),
     ylim = c(0,0.6))
n<-length(df_bud$x)
p<- length(coef(fits$m1_probit))
estat_leverage1 <- 2*p/n
estat_leverage2 <- 3*p/n
abline(h = estat_leverage1, lty = 2, col = "#d9891b")
abline(h = estat_leverage2, lty = 2, col = "red")

op <- par(mfrow = c(3,3), mar = c(4, 4, 2.5, 1))
for (nome in names(fits)) {
  mod <- fits[[nome]]
  plot(fitted(mod), rstandard(mod, type = "deviance"),
       pch = 19, col = "#315d77",
       xlab = "Valor ajustado", ylab = "Resíduo de desvio",
       main = paste(nome, ": resíduos × ajustados"))
  abline(h = 0, lty = 2, col = "#d9891b")
}

for (nome in names(fits)) {
  mod <- fits[[nome]]
  qqnorm(rstandard(mod, type = "deviance"), pch = 19,
         col = "#315d77", main = paste(nome, ": gráfico Q-Q"))
  qqline(rstandard(mod, type = "deviance"), col = "#d9891b", lwd = 2)
}

# modelo selecionado: M1 probit
mod <- fits$m1_probit
nome <- "m1_probit"
par(mfrow = c(1,2), mar = c(4, 4, 2.5, 1))
plot(fitted(mod), rstandard(mod, type = "deviance"),
     pch = 19, col = "#315d77",
     xlab = "Valor ajustado", ylab = "Resíduo de desvio",
     main = paste(nome, ": resíduos × ajustados"))
abline(h = 0, lty = 2, col = "#d9891b")
qqnorm(rstandard(mod, type = "deviance"), pch = 19,
       col = "#315d77", main = paste(nome, ": gráfico Q-Q"))
qqline(rstandard(mod, type = "deviance"), col = "#d9891b", lwd = 2)

# Resíduos
res_pearson <- df_m1_probit$pearson
res_desvio <- df_m1_probit$desvio
  
# Número de grupos
m <- 12

# Número de parâmetros estidesvio# Número de parâmetros estimados
k <- length(coef(mod))

# Estatística qui-quadrado de Pearson
X2_P <- sum(res_pearson^2)

# Graus de liberdade residuais
gl <- m - k

# Dispersão estimada
phi <- X2_P/gl

# Estatística de desvio
X2_D <- sum(res_desvio^2)

X2_P
X2_D
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Resíduos de Pearson
plot(
  fitted(mod), res_pearson,
  pch = 19, col = "#315d77",
  xlab = "Valores ajustados",
  ylab = "Resíduos de Pearson",
  main = "Resíduos de Pearson"
)
abline(h = 0, lty = 2, col = "#d9891b", lwd = 2)

# Resíduos de desvio
plot(
  fitted(mod), res_desvio,
  pch = 19, col = "#315d77",
  xlab = "Valores ajustados",
  ylab = "Resíduos de desvio",
  main = "Resíduos de desvio"
)
abline(h = 0, lty = 2, col = "#d9891b", lwd = 2)

par(op)

# refazendo o modelo retirando cada grupo por vez

ligacao <- "probit"
resultados_DL50 <- data.frame()
fits_analise <- c()
for (i in 1:nrow(df_bud)) {
  
  # Remover uma observação de cada vez
  df_temp <- df_bud[-i, ]
  
  fits_analise[[i]] <- glm(
    formula = cbind(mortos, expostos - mortos) ~ x+sexo_bin,
    family = binomial(link = ligacao),
    data = df_temp
  )
  # pegando dose letal de 50%
  s = c(0,1)
  dose_50 <- dose_letal(fits_analise[[i]], p=0.5, base=2, nivel=0.95,s=s)
  dose_50 <- as.data.frame(dose_50)
  dose_50$observacao_removida <- i
  dose_50$proporcao_morte <- df_bud$proporcao_morte[i]
  resultados_DL50 <- rbind(resultados_DL50, dose_50)
}
resultados_DL50

resultados_DL50 <- resultados_DL50 %>%
  left_join(
    df_doses_letais[(df_doses_letais$modelo=="m1_probit") & 
                      (df_doses_letais$p == 0.5),] %>%
      select(s, Logdose, LI_logdose, LS_logdose ),
    by = c("s"),
    suffix = c("", "_referencia")
  ) %>%
  select(observacao_removida, proporcao_morte, s,p, Logdose, LI_logdose, LS_logdose,
         Logdose_referencia, LI_logdose_referencia, LS_logdose_referencia)

resultados_DL50$diferenca_dl_referencia <- (resultados_DL50$Logdose - resultados_DL50$Logdose_referencia)/resultados_DL50$Logdose_referencia
resultados_DL50 <- resultados_DL50[
  order(resultados_DL50$diferenca_dl_referencia),
]
resultados_DL50
