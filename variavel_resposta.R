
# COMPLETUDE DOS DADOS
completude <- metadados %>%
  summarise(
    `Total de Consultas` = n(),
    `Hurley - N Preenchido` = sum(!is.na(estagio_hurley)),
    `Hurley - %` = round(sum(!is.na(estagio_hurley)) / n() * 100, 1),
    `iHS4 - N Preenchido` = sum(!is.na(ihs4)),
    `iHS4 - %` = round(sum(!is.na(ihs4)) / n() * 100, 1)
  ) %>%
  pivot_longer(everything(), names_to = "Métrica", values_to = "Valor")
