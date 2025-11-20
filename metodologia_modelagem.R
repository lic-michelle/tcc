# Carregar bibliotecas necessárias
source("libraries.R")

#Carregar dados anonimizados

metadados <- read.csv("dados.csv")

#Seleção de variável desfecho
source("variavel_resposta.R")

#Seleçao das variáveis 
source("variaveis_independentes.R")

#Funções
source("funcoes.R")

# ==============================================================================
# Modelagem Estatística - Interpaciente - primeira consulta ou única consulta 
# ==============================================================================

# Variável dependente :estágio de Hurley 

# Variáveis independentes : obesidade (diagnóstico de obesidade), 
# familiar_hs (histórico familiar de hidradenite supurativa), 
# idade_inicio_sintomas (idade em que os sintomas começaram) e imc (índice de massa corporal)

# Critério:
# Uso de dados de pacientes com dados completos na primeira consulta. Os
# pacientes com uma única consulta são inclusos.

metadados_clmp <- metadados %>%
  select(user_id, estagio_hurley, data_consulta, all_of(variaveis_indep)) %>%
  group_by(user_id) %>%
  filter(data_consulta == min(data_consulta)) %>%
  ungroup() %>% 
  mutate(
    estagio_hurley = factor(estagio_hurley, 
                            levels = c(1 ,2, 3), 
                            ordered = TRUE), 
    obesidade = as.factor(obesidade),
    familiar_hs= as.factor(familiar_hs),
    idade_inicio_sintomas = as.numeric(scale(idade_inicio_sintomas)),
    imc = as.numeric(scale(imc))
  ) %>% 
  select(-user_id,-data_consulta) %>% 
  drop_na()

#Modelo global - todas as variáveis para realizar o dredge
# Fórmula do modelo global
formula_global_clmp <- as.formula(paste("estagio_hurley ~", paste(variaveis_indep, collapse = " + ")))
print(formula_global_clmp)

# Rodar o modelo global
modelo_global_clmp <- clm(formula_global_clmp, data = metadados_clmp)

# Criação de modelos com diferentes combinações de variáveis independentes
# Configurar opções para dredge
options(na.action = "na.fail")

# Executar dredge
selecao_clmp <- dredge(modelo_global_clmp, rank = "AICc", trace = 2)

# Restaurar opção padrão
options(na.action = "na.omit")

#Exibir os melhores
print(selecao_clmp[1:10,])

# Selecionar os modelos com menores delta -> modelos candidatos (aka competitivos aqui)
modelos_competitivos_clmp <- get.models(selecao_clmp, 
                                        subset = delta < 2 & df > 2)
# Extrair métricas
aicc_values <- sapply(modelos_competitivos_clmp, AICc)
delta_aicc <- aicc_values - min(aicc_values)
wi <- exp(-0.5 * delta_aicc) / sum(exp(-0.5 * delta_aicc))
loglik_values <- sapply(modelos_competitivos_clmp, logLik)
minus2loglik <- -2 * as.numeric(loglik_values)

# Aplicar a função para todos os modelos da tabela
tabela_modelos_clmmp <- tibble(
  Submodelo = paste0("M", seq_along(modelos_competitivos_clmp)),
  Formula = sapply(modelos_competitivos_clmp, function(x) paste(deparse(formula(x)), collapse = "")),
  N_parametros = sapply(modelos_competitivos_clmp, function(x) attr(logLik(x), "df")),
  AICc = round(aicc_values, 2),
  Delta_AICc = round(delta_aicc, 2)
)


# Visualizar todos os modelos candidatos

for(i in seq_along(modelos_competitivos_clmp)) {
  printi_modelo(modelos_competitivos_clmp[[i]], tabela_modelos_clmmp$Submodelo[i])
}


# Número de modelos competitivos
n_modelos <- length(modelos_competitivos_clmp)



# Extrair variáveis independentes, gradiente máximo e número de condição
variaveis <- sapply(modelos_competitivos_clmp, function(m) {
  paste(labels(terms(m)), collapse = " + ")
})

gradiente_max <- sapply(modelos_competitivos_clmp, function(m) {
  if(!is.null(m$gradient)) max(abs(m$gradient)) else NA
})

cond_hessiana <- sapply(modelos_competitivos_clmp, function(m) {
  if(!is.null(m$Hessian)) kappa(m$Hessian) else NA
})

# Criar dataframe com modelos candidatos
df_modelos <- data.frame(
  Modelo = paste0("Modelo ", seq_len(n_modelos)),
  Variaveis = variaveis,
  AICc = aicc_values,
  Delta_AICc = delta_aicc,
  Wi = wi,
  Minus2LogLik = minus2loglik,
  GradienteMax = gradiente_max,
  CondHessiana = cond_hessiana
)

# Ordenar pelo Delta_AICc
df_modelos <- df_modelos[order(df_modelos$Delta_AICc), ]

View(df_modelos)

# SOMENTE APÓS ESCOLHA DO MELHOR MODELO PROSSIGA #  

# Selecionar o modelo com menor AICc e sentido clinico/biológico
modelo_melhor_clmp <- modelos_competitivos_clmp[[1]] #Indique a posição

#  Converter estimates para valores despadronizados (de 1 desvio padrão para 1 unidade numérica (ano; kg/m2))
tabela_melhor_clmp <- coef_clm_clmm_real(modelo_melhor_clmp, sd_vars)
View(tabela_melhor_clmp)

