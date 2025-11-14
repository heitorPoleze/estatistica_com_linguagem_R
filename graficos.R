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
########################################################################
###GRAFICO REGIONAL#####
media_regional <- media_final %>%
  group_by(Area_de_Conhecimento) %>%
  summarise(Media_Regional = mean(Media, na.rm = TRUE))


cat("--- Gráfico 1: Média Regional por Área do Conhecimento --- \n")
grafico_media_regional <- ggplot(media_regional, aes(
  x = reorder(Area_de_Conhecimento, -Media), 
  y = Media_Regional, 
  fill = Area_de_Conhecimento
)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Media_Regional, 1)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Média de Notas por Área de Conhecimento",
    subtitle = "Média geral do Centro-Oeste",
    x = "Área de Conhecimento",
    y = "Nota Média Regional"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_media_regional)
################################################################

mediana_regional <- mediana_final %>%
  group_by(Area_de_Conhecimento) %>%
  summarise(Mediana = mean(Mediana, na.rm = TRUE))


cat("--- Gráfico 2: Mediana Regional por Área do Conhecimento --- \n")
grafico_mediana_regional <- ggplot(mediana_regional, aes(
  x = reorder(Area_de_Conhecimento, -Mediana), 
  y = Mediana, 
  fill = Area_de_Conhecimento
)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Mediana, 1)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Mediana de Notas por Área de Conhecimento",
    subtitle = "Mediana geral do Centro-Oeste",
    x = "Área de Conhecimento",
    y = "Nota Mediana Regional"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_mediana_regional)


################################################################

desviopadraoregional <- desvio_padrao_final %>%
  group_by(Area_de_Conhecimento) %>%
  summarise(Desvio_Padrao = mean(Desvio_Padrao, na.rm = TRUE))


cat("--- Gráfico 2: Mediana Regional por Área do Conhecimento --- \n")
grafico_dp_regional <- ggplot(desviopadraoregional, aes(
  x = reorder(Area_de_Conhecimento, -Desvio_Padrao), 
  y = Desvio_Padrao, 
  fill = Area_de_Conhecimento
)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Desvio_Padrao, 1)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Desvio Padrão por Área de Conhecimento",
    subtitle = "Desvio Padrão geral do Centro-Oeste",
    x = "Área de Conhecimento",
    y = "Desvio Padrão Regional"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_dp_regional)






#################################################################
############GRAFICO POR ESTADO##################################

grafico_medianas_por_area_estado <- ggplot(mediana_final, aes(
  x = UF,                  # Cada UF no eixo X de cada painel
  y = Mediana,             # O valor da mediana no eixo Y
  fill = Area_de_Conhecimento # A cor para cada área de conhecimento (opcional, mas bom para legendas)
)) +
  geom_bar(stat = "identity", color = "black") + # Barras baseadas nos valores 'Mediana'
  
  # Cria painéis (facetas) para cada Area_de_Conhecimento
  # O gráfico do Nordeste usa 'Area_de_Conhecimento ~ .'
  # O 'ncol = 3' organiza em 3 colunas de painéis.
  facet_wrap(~ Area_de_Conhecimento, ncol = 3, scales = "free_y") + 
  
  labs(
    title = "Medianas de Notas por Área de Conhecimento - Centro-Oeste",
    subtitle = "Comparativo por Unidade Federativa",
    x = "Estado",
    y = "Mediana da Nota",
    fill = "Área" # Legenda do preenchimento
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Gira rótulos do eixo X se necessário
    legend.position = "right", # Posição da legenda
    strip.text = element_text(face = "bold") # Negrito para os títulos dos painéis
  )

# Exibe o gráfico
print(grafico_medianas_por_area_estado)
###################################################################

grafico_medias_por_area_estado <- ggplot(media_final, aes(
  x = UF,
  y = Media,
  fill = Area_de_Conhecimento 
)) +
  geom_bar(stat = "identity", color = "black") + facet_wrap(~ Area_de_Conhecimento, ncol = 3, scales = "free_y") + 
  
  labs(
    title = "Média de Notas por Área de Conhecimento",
    subtitle = "Comparativo por Unidade Federativa",
    x = "Estado",
    y = "Média",
    fill = "Área"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "right", 
    strip.text = element_text(face = "bold") 
  )

print(grafico_medias_por_area_estado)

#######################################################
grafico_dp_por_area_estado <- ggplot(desvio_padrao_final, aes(
  x = UF,
  y = Desvio_Padrao,
  fill = Area_de_Conhecimento 
)) +
  geom_bar(stat = "identity", color = "black") + facet_wrap(~ Area_de_Conhecimento, ncol = 3, scales = "free_y") + 
  
  labs(
    title = "Desvio Padrão por Área de Conhecimento",
    subtitle = "Comparativo por Unidade Federativa",
    x = "Estado",
    y = "DP",
    fill = "Área"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "right", 
    strip.text = element_text(face = "bold") 
  )

print(grafico_dp_por_area_estado)


#################################################
####PROPORCAO FEMEAS E MACHOS##################

df_para_plotar_sexo <- porcentagem_com_total_geral %>%
  filter(UF != "Total Geral") %>% # Remove a linha de total
  select(UF, Homens, Mulheres) %>% # Seleciona apenas as colunas de contagem
  pivot_longer(
    cols = c(Homens, Mulheres), # As colunas que queremos "empilhar"
    names_to = "Sexo",          # Nova coluna para os nomes ("Homens", "Mulheres")
    values_to = "Contagem"      # Nova coluna para os valores (as contagens)
  )
g
# Agora, df_para_plotar_sexo se parece com:
# UF   Sexo      Contagem
# DF   Homens    29893
# DF   Mulheres  44627
# GO   Homens    60520
# GO   Mulheres  90638
# ... etc.

# --- PASSO 2: Criar o Gráfico ---
cat("--- Gerando Gráfico: Proporção de Homens e Mulheres por Estado (Centro-Oeste) --- \n")

grafico_proporcao <- ggplot(df_para_plotar_sexo, aes(x = UF, y = Contagem, fill = Sexo)) +
  
  # geom_bar com position="fill" cria o gráfico 100% empilhado
  # A "Contagem" (eixo Y) é automaticamente convertida em proporção
  geom_bar(stat = "identity", position = "fill", color = "black") +
  
  # Formata o eixo Y para mostrar "0%", "25%", "50%", etc.
  scale_y_continuous(labels = scales::percent_format()) +
  
  # Define cores personalizadas (opcional, mas fica parecido com o exemplo)
  # O gráfico de exemplo usava rosa (Mulheres) e azul (Homens)
  scale_fill_manual(
    values = c("Homens" = "#3b82f6", "Mulheres" = "#ec4899"),
    # Reordena a legenda para "Mulheres" vir primeiro, como no exemplo
    breaks = c("Mulheres", "Homens")
  ) +
  
  # Títulos e rótulos
  labs(
    title = "Proporção de Homens e Mulheres por Estado - Centro-Oeste",
    subtitle = "Distribuição proporcional dentro de cada estado",
    x = "Estado (UF)",
    y = "Proporção",
    fill = "Sexo"
  ) +
  
  # Tema limpo
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right" # Coloca a legenda à direita
  )

# Exibe o gráfico
print(grafico_proporcao)