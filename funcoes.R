
# Visualizar o summary de todos os competitivos
summary_modelos <- function(modelo, numero) {
  
  
  cat("\n       SUBMODELO", numero, "\n")
  
  
  cat("Fórmula:\n")
  print(formula(modelo))
  
  cat("\n--- Summary: ---\n")
  print(summary(modelo))
  
  cat("AICc:", round(AICc(modelo), 2), "\n")
}

#Converter seus estimates de 1 desvio padrão para 1 unidade numerica (ano; kg/m2)
# Função para converter coeficientes
sd_vars <- c(
  idade_inicio_sintomas = sd(metadados$idade_inicio_sintomas, na.rm = TRUE),
  imc = sd(metadados$imc, na.rm = TRUE)
)

coef_clm_clmm_real <- function(modelo, sd_vars = NULL) {
  
  # Identifica classe do modelo
  tipo_modelo <- class(modelo)[1]
  
  # Extrai coeficientes fixos
  tidy_mod <- broom::tidy(modelo)
  
  # Remove thresholds e efeitos aleatórios
  tidy_mod <- tidy_mod %>% filter(!grepl("\\|", term))
  
  # Converte coeficientes e erros para unidades reais
  tidy_mod <- tidy_mod %>%
    mutate(
      # Ajusta estimate
      beta_real = ifelse(term %in% names(sd_vars),
                         estimate / sd_vars[term],
                         estimate),
      
      # Ajusta erro padrão também!
      std.error_real = ifelse(term %in% names(sd_vars),
                              std.error / sd_vars[term],
                              std.error),
      
      # Intervalo de confiança
      ic_low = estimate - 1.96 * std.error,
      ic_high = estimate + 1.96 * std.error,
      
      ic_low_real = beta_real - 1.96 * std.error_real,
      ic_high_real = beta_real + 1.96 * std.error_real,
      
      # Odds Ratios
      OR = exp(estimate),
      OR_low = exp(ic_low),
      OR_high = exp(ic_high),
      
      OR_real = exp(beta_real),
      OR_real_low = exp(ic_low_real),
      OR_real_high = exp(ic_high_real),
      
      modelo_tipo = tipo_modelo
    ) %>%
    select(term, estimate, beta_real,
           std.error, std.error_real,
           ic_low, ic_high, ic_low_real, ic_high_real,
           OR, OR_low, OR_high,
           OR_real, OR_real_low, OR_real_high,
           p.value, modelo_tipo)
  
  return(tidy_mod)
}

printi_modelo<- function(modelo, nome) {
  cat("\n\n     SUBMODELO", nome, "\n")
  
  cat("Fórmula:\n")
  print(formula(modelo))
  
  cat("\n SUMÁRIO \n")
  print(summary(modelo))
  
  cat("\nMÉTRICAS \n")
  cat("AICc:", round(AICc(modelo), 2), "\n")
}
