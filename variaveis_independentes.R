# 1. SELEÇÃO DE VARIÁVEIS CATEGÓRICAS (Fatores)

# Identificar variáveis categóricas (fatores)
variaveis_categoricas <- metadados %>%
  select(user_id, tabagismo:familiar_hs) %>%
  select(where(~ !is.factor(.x) ||
                 !(length(unique(.x)) == 1 && unique(.x) %in% c("sim", "nao")))) %>% 
select(-imc)

pacientes_com_sim <- variaveis_categoricas %>%
  select(-user_id) %>% 
  summarise(across(everything(), ~ sum(tapply(.x == "sim", variaveis_categoricas$user_id, any), na.rm = TRUE)))

colunas_pacientes_s <- names(pacientes_com_sim)[pacientes_com_sim[1, ] > 1]

#Análise da frequência das pre-selecionadas
source("frequencia_melhores.R") 
#Apenas obesidade e HS familiar possuem uma boa 
#proporção entre asno banco de dados


# Aprovadas
variaveis_categoricas_aprovadas <- variaveis_categoricas %>%
  select(all_of(colunas_pacientes_s)) %>% 
  select(obesidade,familiar_hs) %>% 
  names()


# 2. SELEÇÃO DE VARIÁVEIS NUMÉRICAS (Contínuas)
# Critério: Completude (mínimo de 50% de dados preenchidos)

# Identificar colunas numéricas que não são binárias (0/1) e não são IDs/Desfechos
colunas_numericas <- metadados %>%
  select(where(is.numeric)) %>% 
  select(-c(consulta,ihs4,estagio_hurley,user_id)) %>% 
  colnames()

# Rodar skim apenas nas variáveis numéricas contínuas
skim_completo <- metadados %>%
  select(all_of(colunas_numericas)) %>%
  skim()

# Variáveis numéricas aprovadas
melhores_numericas <- skim_completo %>%
  filter(complete_rate > 0.5) %>%
  pull(skim_variable)

# 3. LISTA FINAL DE PREDITORES 
preditores_ok <- union(
  variaveis_categoricas_aprovadas, 
  melhores_numericas
)

cat("\n--- LISTA FINAL DE PREDITORES ---\n")
print(preditores_ok)


# 3. Avaliação de  multicolineriadade

# Selecionar essas variáveis + variável resposta do dataframe original


subdados_vif <- metadados %>%
  select(user_id,estagio_hurley, data_consulta, all_of(preditores_ok)) %>%
  arrange(user_id, data_consulta) %>% 
  group_by(user_id) %>%
  summarise(
    estagio_hurley_max = max(estagio_hurley),
    idade_inicio_sintomas = idade_inicio_sintomas[1],
    across(where(is.character), ~ as.numeric(any(.x == "sim"))),   # 1 se teve ao menos uma vez
    across("imc", \(x) mean(x, na.rm = TRUE))# média só do imc
    ) %>%
  drop_na() %>% 
  ungroup()

vars_originais_vif <-subdados_vif %>% 
  select(-c(estagio_hurley_max,user_id,data_consulta)) %>% 
  names()

# Criar a fórmula para o modelo linear
formula_vif <- as.formula(paste("estagio_hurley_max ~", paste(vars_originais_vif, collapse = " + ")))

# Rodar o modelo linear
modelo_vif <- lm(formula_vif, data = subdados_vif)

# Calcular VIF

vif_valores <- vif(modelo_vif)
vif_df <- data.frame(
  variavel = names(vif_valores),
  VIF = as.numeric(vif_valores)
)

#Selecionando variaveis com VIF <5 
vif_baixas <- vif_df[vif_df$VIF < 5, ]
variaveis_indep<- vif_baixas$variavel
