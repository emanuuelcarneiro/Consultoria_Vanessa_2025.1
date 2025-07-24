#Tabelas dos cancer

df_cancer <-tipo_de_cancer |> 
  filter(munici_pio_da_residencia != "Total") |> 
  pivot_longer(
    cols = -munici_pio_da_residencia,
    names_to = "Tipo_De_Cancer",
    values_to = "Numero_de_casos"    
  ) 

df_para_top10 <-  df_cancer |> 
  filter(munici_pio_da_residencia != "Total",
         Tipo_De_Cancer != "total",
         munici_pio_da_residencia %in% c(" Paranavai", " Maringa")) |> 
  group_by(munici_pio_da_residencia, Tipo_De_Cancer) |> 
  summarise(Total_Casos = sum(Numero_de_casos, na.rm = TRUE),
            .groups = 'drop') |> 
  arrange(munici_pio_da_residencia, desc(Total_Casos))

# Top 10 
df_top10_por_municipio <- df_para_top10|> 
  group_by(munici_pio_da_residencia) |> 
  slice_head(n = 10) |> 
  ungroup() |> 
  mutate(Total_Casos = scales::comma(Total_Casos))

# Top 10 paranavai
df_paranavai_top10 <- df_top10_por_municipio |> 
  filter(munici_pio_da_residencia == " Paranavai") |> 
  select(-munici_pio_da_residencia)
df_paranavai_top10

# Top 10 maringa
df_maringa_top10 <- df_top10_por_municipio |> 
  filter(munici_pio_da_residencia == " Maringa") |> 
  select(-munici_pio_da_residencia)
df_maringa_top10



# Negocio do estadiamento


estadiamento <- tratam_estadiamento |>
  filter(munici_pio_da_residencia != "Total") |>
  select(-total) |> 
  pivot_longer(
    cols = c(x0, x1, x2, x3, x4), 
    names_to = "Estagio_cancer", 
    values_to = "Numero_de_casos" 
  ) |> 
  mutate(
    Estagio_cancer = case_when(
      Estagio_cancer == "x0" ~ "Estagio 0",
      Estagio_cancer == "x1" ~ "Estagio 1",
      Estagio_cancer == "x2" ~ "Estagio 2",
      Estagio_cancer == "x3" ~ "Estagio 3",
      Estagio_cancer == "x4" ~ "Estagio 4",
      TRUE ~ Estagio_cancer
    )
  )

# Uma mini tabela so pra visualizar o numero de casos de estadiamento por municipio

estadiamento |> group_by(munici_pio_da_residencia) |> 
  summarise(total = sum(Numero_de_casos)) |> 
  arrange(desc(total))

# Grafico do estadiamento Maringa vs Paranavai

ggplot(estadiamento_maringa_paranvai, 
       aes(x = Estagio_cancer, 
           y = Numero_de_casos, 
           fill = munici_pio_da_residencia)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  labs(
    title = "Número Absoluto de Casos de Câncer por Estágio",
    subtitle = "Comparativo entre Paranavaí e Maringá",
    x = "Estágio do Câncer",
    y = "Número de Casos",
    fill = "Município"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  geom_text(aes(label = Numero_de_casos), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3, color = "black")


estadiamento_maringa_paranvai <- estadiamento |>
  filter(munici_pio_da_residencia %in% c(" Paranavai", " Maringa")) |>
  group_by(munici_pio_da_residencia) |>
  mutate(Proporcao = Numero_de_casos / sum(Numero_de_casos, na.rm = TRUE)) |>
  ungroup() |>
  mutate(Estagio_cancer = factor(Estagio_cancer, 
                                 levels = c("Estagio 0", "Estagio 1", "Estagio 2", "Estagio 3", "Estagio 4")))

# Na tentativa de fazer algo com a proporção de estadiamento

ggplot(estadiamento_maringa_paranvai, 
       aes(x = munici_pio_da_residencia, 
           y = Proporcao, 
           fill = Estagio_cancer)) +
  geom_bar(stat = "identity", position = "fill", color = "white") + 
  labs(
    title = "Distribuição Proporcional dos Estágios de Câncer",
    subtitle = "Comparativo entre Paranavaí e Maringá",
    x = "Município",
    y = "Proporção de Casos",
    fill = "Estágio do Câncer"
  ) +
  scale_y_continuous(labels = percent) + 
  theme_minimal() 

