
#### Importação ####

#Read and transform data
dataset = read.delim("todos_dados.csv", sep = ",")
dataset = dataset %>% 
  select(-X) %>% 
  relocate(estagio_hurley, .before = idade_inicio_sintomas) %>% 
  relocate(ihs4, .before = idade_inicio_sintomas) %>% 
  relocate(imc, .after = idade_inicio_sintomas) %>% 
  arrange(user_id, data_consulta) %>% 
  mutate(across(c("consulta", "user_id"), as.character)) %>% 
  mutate(data_consulta = ymd(data_consulta)) %>% 
  mutate(across(c("estagio_hurley", "ihs4"), as_factor)) %>% 
  mutate(estagio_hurley = fct_relevel(estagio_hurley, c("1", "2", "3"))) %>% 
  mutate(ihs4 = fct_relevel(ihs4, c(as.character(seq(1:12))))) %>% 
  mutate(across(tabagismo:familiar_hs, 
                ~ fct_relevel(.x, "nao", "sim")))

#Filtrar apenas a primeira consulta de cada paciente
dataset_first = dataset %>%
  group_by(user_id) %>%
  slice_min(data_consulta, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  arrange(user_id)


#### Summary das variáveis ####

# Variáveis quantitativas
skim_quantitative = skimr::skim(dataset_first) %>% 
  filter(skim_type == "numeric") %>% 
  select(skim_variable, contains("numeric")) 

#G Variáveis categóricas
skim_categorical = skimr::skim(dataset_first) %>% 
  filter(skim_type == "factor") %>% 
  select(skim_variable, contains("factor")) 

#### Testes univariados ####

#  Kruskal-wallis para variáveis quantitativas
univariate_quantitative = dataset_first %>% 
  select(estagio_hurley, idade_inicio_sintomas:imc) %>%
  pivot_longer(cols = -estagio_hurley, 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  kruskal_test(value ~ estagio_hurley)
  
# Teste para cada uma delas
#NOTA: primeiro  select() remove variáveis sem dados suficientes para a análise
univariate_categorical = dataset_first %>%
  select(-c(agenesia_unilateral_do_rim,
            carcinoma_espinocelular,
            depressao,
            diabetes_dois,
            disturbio_bipolar,
            doenca_celiaca,
            doenca_crohn,
            doenca_inflamatoria_intestinal,
            hipertensao_essencial_primaria,
            insuficiencia_da_valva_aortica,
            intolerancia_lactose,
            lupus_eritematoso_disseminado_sistemico_nao_especificado,
            osteoporose,
            psoriase_nao_especificada,
            sindrome_de_down,
            sindrome_metabolica,
            tuberculose_nao_especificada_das_vias_respiratorias_com_confirmacao_bacteriologica_e_histologica,
            urticaria_nao_especificada)) %>% 
  select(estagio_hurley, tabagismo:familiar_hs) %>%
  pivot_longer(cols = -estagio_hurley, 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    test = list(prop_trend_test(table(value, estagio_hurley))),
    .groups = "drop"
  ) %>%
  unnest(test)

