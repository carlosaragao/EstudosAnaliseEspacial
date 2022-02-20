# Moran Global
vetor_idh <- shp_sp@data$idh
vetor_idh

# Primeira parte
n <- length(vetor_idh) %>% - 1
n

s0 <- sum(matrizW_queen)
s0

primeira_parte <- n / s0
primeira_parte

# Segunda parte
vetor_Zidh <- scale(vetor_idh)

numerador <- t(vetor_Zidh) %*% matrizW_queen %*% vetor_Zidh
denominador <- t(vetor_Zidh) %*% vetor_Zidh

segunda_parte <- numerador / denominador
segunda_parte

# I de Moran
i_de_moran <- primeira_parte * segunda_parte

# Valor esperado

valor_esperado <- - (1 / (n - 1))


moran.test(x = shp_sp@data$idh, 
           listw = listw_queen, 
           zero.policy = TRUE)

###########################

# Diagrama de moran

widh <- lag.listw(x = listw_queen,
                  var = shp_sp@data$idh,
                  zero.policy = TRUE)
widh

df <- data.frame(cidade = shp_sp@data$NM_MUNICIP,
                 idh = vetor_idh,
                 widh)

df %>%
  ggplot(aes(label = cidade)) +
  geom_point((aes(x = idh, y = widh)))


df["widh2"] <- matrizW_queen %*% vetor_idh

df %>%
  ggplot(aes(label = cidade)) +
  geom_point((aes(x = idh, y = widh)), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = idh, y = widh), method = 'lm', se = F) +
  geom_hline(yintercept = mean(df$widh), lty = 2) +
  geom_vline(xintercept = mean(df$idh), lty = 2) +
  annotate("text", x = 0.66, y = 18.5, label = "Low-High") +
  annotate("text", x = 0.84, y = 18.5, label = "High-High") +
  annotate("text", x = 0.66, y = -1, label = "Low-Low") +
  annotate("text", x = 0.84, y = -1, label = "High-Low") +
  theme_bw()

plotly::ggplotly(
  df %>%
    ggplot(aes(label = cidade)) +
    geom_point((aes(x = idh, y = widh)), alpha = 0.5, size = 2) +
    geom_smooth(aes(x = idh, y = widh), method = 'lm', se = F) +
    geom_hline(yintercept = mean(df$widh), lty = 2) +
    geom_vline(xintercept = mean(df$idh), lty = 2) +
    annotate("text", x = 0.66, y = 18.5, label = "Low-High") +
    annotate("text", x = 0.84, y = 18.5, label = "High-High") +
    annotate("text", x = 0.66, y = -1, label = "Low-Low") +
    annotate("text", x = 0.84, y = -1, label = "High-Low") +
    theme_bw()
)


# Moran Local

rowSums(
  sweep(x = matrizW_queen_linha,
    MARGIN = 2,
    STATS = vetor_Zidh,
    FUN = "*")
) * vetor_Zidh
