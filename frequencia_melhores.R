

#HIPOTIREOIDISMO
distribuicao_hipo <- metadados %>%
  dplyr::count(hipotireoidismo_nao_especificado, name = "contagem_total") #retirar

##TABAGISMO
distribuicao_tab<- metadados %>%
  dplyr::count(tabagismo, name = "contagem_total") #Retirar

##ALCOOLISMO
distribuicao_alc<- metadados %>%
  dplyr::count(alcoolismo, name = "contagem_total") #retirar

##DROGAS ILICITAS
distribuicao_drog <- metadados %>%
  dplyr::count(droga_ilicitas, name = "contagem_total")  #retirar

#ACNE
distribuicao_acne <- metadados %>%
  dplyr::count(acne_nao_especificada, name = "contagem_total") #retirar

#OBESIDADE
distribuicao_obesidade<- metadados %>%
  dplyr::count(obesidade, name = "contagem_total")

#SOP
distribuicao_sop<- metadados %>%
  dplyr::count(sop, name = "contagem_total") #retirar

#Ansiedade
distribuicao_ansiedade<- metadados %>%
  dplyr::count(ansiedade, name = "contagem_total") #retirar


#Artrite
distribuicao_artrie<- metadados %>%
  dplyr::count(artrite_qualquer, name = "contagem_total") #retirar


# HS familiar
distribuicao_hsfam<- metadados %>%
  dplyr::count(familiar_hs, name = "contagem_total")

