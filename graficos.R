media_final <- media_final %>%
  pivot_longer(
    cols = c(Natureza, Humanas, Linguagem, Matematica, Redacao, Geral), 
    names_to = "Area_de_Conhecimento",
    values_to = "Media"   
  )

mediana_final = mediana_final %>%
  pivot_longer(
    cols = c(Natureza, Humanas, Linguagem, Matematica, Redacao, Geral), # Colunas para transformar
    names_to = "Area_de_Conhecimento", # Nova coluna para os NOMES (Natureza, etc.)
    values_to = "Mediana"          # Nova coluna para os VALORES (517.0, etc.)
  )

desvio_padrao_final = desvio_padrao_final %>%
  pivot_longer(
    cols = c(Natureza, Humanas, Linguagem, Matematica, Redacao, Geral), # Colunas para transformar
    names_to = "Area_de_Conhecimento",
    values_to = "Desvio_Padrao"
  )

#GRAFICO SOBRE A MÉDIA GERAL DA REGIÃO CENTRO OESTE
media_regional <- media_final %>%
  group_by(Area_de_Conhecimento) %>%
  summarise(Media_Regional = mean(Media, na.rm = TRUE))

# MAIOR PARA MENOR NOTA
media_regional$Area_de_Conhecimento <- factor(
  media_regional$Area_de_Conhecimento,
  levels = media_regional$Area_de_Conhecimento[order(media_regional$Media_Regional, decreasing = TRUE)]
)
