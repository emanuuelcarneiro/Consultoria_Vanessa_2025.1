library(tidyverse)
library(janitor)
library(geobr)
library(sf)

load("dados_limpo.RData")

# os tibbles municipio_residencia e tratam_munic_mesdiagnostico
# são iguais!!!

tratam_munic_mesdiagnostico <- tratam_munic_mesdiagnostico |> 
  mutate(munici_pio_da_residencia = str_trim(munici_pio_da_residencia))

tratam_munic_mesdiagnostico |> 
  mutate(porcentagem = total/total[1] * 100) |> 
  select(munici_pio_da_residencia, porcentagem, total) |> 
  arrange(desc(porcentagem)) |> 
  tail(-1)





rm(municipio_residencia)


parana <- read_municipality(code_muni = "PR", year = 2020)

faixa_etaria <- faixa_etaria |> 
  mutate(munici_pio_da_residencia = str_trim(munici_pio_da_residencia))

parana <- parana |>
  mutate(name_muni_clean = stringi::stri_trans_general(name_muni, "Latin-ASCII"))

parana <- parana |>
  mutate(destacado = name_muni_clean %in% unique(faixa_etaria$munici_pio_da_residencia))

# Mapa
ggplot(parana) +
  geom_sf(aes(fill = destacado), color = "white", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray90"),
                    labels = c("TRUE" = "Selecionados", "FALSE" = "Outros"),
                    name = "Legenda:") +
  theme_minimal()

# confirmando que deu 58 municipios
parana |> 
  count(destacado)


sexo |> 
  filter(munici_pio_da_residencia != "Total") |> 
  summarise(n = sum(masculino > feminino))


sexo <- sexo |> 
  pivot_longer(cols = c(masculino, feminino, total),
               names_to = "sexo",
               values_to = "valor") |> 
  pivot_wider(names_from = munici_pio_da_residencia,
              values_from = valor)

# gráfico sexo
sexo |>
  mutate(sexo = str_to_title(sexo)) |> 
  filter(sexo != "Total") |> 
  ggplot(aes(x = sexo, y = Total)) +
  geom_col(fill = "#22aaee", color = "black", width = 0.75) +
  geom_text(aes(label = Total), vjust = -0.5) +
  scale_y_continuous(breaks = seq(0, 125, 25), 
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Sexo", y = NULL, title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.title.x = element_text(face = "bold")
  )


faixa_etaria <- faixa_etaria |>
  pivot_longer(
    cols = -munici_pio_da_residencia,
    names_to = "faixa_etaria",
    values_to = "frequencia"
  ) |> 
  filter(faixa_etaria != "total")

faixa_etaria <- faixa_etaria |> 
  mutate(
    faixa_etaria = faixa_etaria |> 
      str_replace_all("^x", "") |>    
      str_replace_all("_a_", " a ") |>    
      str_replace_all("_anos", "") |> 
      str_replace_all("_", " "),            
      faixa_etaria = ifelse(
       str_detect(faixa_etaria, "^80"),
       "80 ou mais",
       faixa_etaria
      )
    )

# gráfico faixa etária  
faixa_etaria |>
  filter(munici_pio_da_residencia == "Total") |>
  ggplot(aes(x = faixa_etaria, y = frequencia)) +
  geom_col(fill = "#22aaee", color = "black", width = 1) +
  geom_text(aes(label = frequencia), vjust = -0.3, 
                size = 3.5) +
  labs(title = NULL, x = "Idade (em anos)", y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.title.x = element_text(face = "bold")
        )






# diagnostico e tratamento
dados_diag <- tratam_munic_mesdiagnostico |>
  pivot_longer(
    cols = -munici_pio_da_residencia, 
    names_to = "mes", 
    values_to = "frequencia"
  ) |>
  mutate(tipo = "Diagnóstico")

dados_trat <- tratam_munic_mestratam |>
  pivot_longer(
    cols = -munici_pio_da_residencia, 
    names_to = "mes", 
    values_to = "frequencia"
  ) |>
  mutate(tipo = "Início de Tratamento")

dados_juntos <- bind_rows(dados_diag, dados_trat)

dados_juntos <- dados_juntos |>
  filter(mes != "total") |> 
  mutate(
    mes = dmy(paste0("01_", mes))  # Ex.: "01_jan_2024" vira 2024-01-01
  )

Sys.setlocale("LC_ALL", "Portuguese")



dados_juntos |>
  filter(munici_pio_da_residencia == "Total") |>
  ggplot(aes(x = mes, y = frequencia, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Frequência",
       title = "total",
       color = "Tipo") +
  scale_color_manual(values = c("Diagnóstico" = "#22aaee", "Início de Tratamento" = "#e63946")) +
  scale_y_continuous(limits = c(0, 750)) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

dados_juntos |>
  filter(munici_pio_da_residencia == " Maringa") |>
  ggplot(aes(x = mes, y = frequencia, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Frequência",
       title = "maringa",
       color = "Tipo") +
  scale_color_manual(values = c("Diagnóstico" = "#22aaee", "Início de Tratamento" = "#e63946")) +
  scale_y_continuous(limits = c(0, 200)) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

dados_juntos |>
  filter(munici_pio_da_residencia == " Paranavai") |>
  ggplot(aes(x = mes, y = frequencia, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Frequência",
       title = "paranavai",
       color = "Tipo") +
  scale_color_manual(
    values = setNames(c("#22aaee", "#e63946"), unique(dados_juntos$tipo))
  ) + 
  scale_y_continuous(limits = c(0, 200)) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

dados_sem_maringa <- dados_juntos |>
  filter(!munici_pio_da_residencia %in% c("Maringá", "Total")) |>
  group_by(mes, tipo) |>
  summarise(frequencia = sum(frequencia, na.rm = TRUE), .groups = "drop")

ggplot(dados_sem_maringa, aes(x = mes, y = frequencia, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Frequência",
       title = "Evolução mensal (exceto Maringá)",
       color = "Tipo") +
  scale_color_manual(values = c("Diagnóstico" = "#22aaee", "Início de Tratamento" = "#e63946")) +
  scale_y_continuous(limits = c(0, 800)) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))




valor_aprovado <- valor_aprovado |> 
  rename(
    jan = janeiro,
    fev = fevereiro,
    mar = marco,
    abr = abril,
    mai = maio,
    jun = junho,
    jul = julho,
    ago = agosto,
    set = setembro,
    out = outubro,
    nov = novembro,
    dez = dezembro
  )

dados_valor_aprovado <- valor_aprovado |> 
  select(-total) |>   
  pivot_longer(
    cols = -municipio,
    names_to = "mes",
    values_to = "valor_aprovado"
  ) |> 
  mutate(
    mes = factor(mes, 
                 levels = c("jan", "fev", "mar", "abr", "mai", "jun",
                            "jul", "ago", "set", "out", "nov", "dez"))
  )

dados_valor_aprovado <- dados_valor_aprovado |> 
  mutate(municipio = str_trim(municipio))

dados_valor_aprovado <- dados_valor_aprovado |> 
  mutate(valor_aprovado = valor_aprovado/1000000)

dados_valor_aprovado |> 
  filter(municipio == "Total") |> 
  ggplot(aes(x = mes, y = valor_aprovado, group = municipio, 
             color = municipio)) +
    geom_line(linewidth = 1, colour = "#22aaee") +
    geom_point(size = 2, colour = "#22aaee") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    labs(
      title = "",
      x = "Mês",
      y = "Valor Aprovado (em milhões de reais)",
      color = "Município"
    ) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

dados_maringa <- dados_valor_aprovado |> 
  filter(municipio == "Maringa")

dados_outros <- dados_valor_aprovado |> 
  filter(!municipio %in% c("Maringa", "Total")) |>
  group_by(mes) |>
  summarise(valor_aprovado = sum(valor_aprovado, na.rm = TRUE), .groups = "drop") |>
  mutate(municipio = "Demais municípios")

dados_comparacao <- bind_rows(dados_maringa, dados_outros)

dados_comparacao |> 
  mutate(valor_aprovado)

dados_comparacao |> 
  mutate(municipio = ifelse(municipio == "Maringa", "Maringá", municipio)) |>
  ggplot(aes(x = mes, y = valor_aprovado, color = municipio, group = municipio)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Valor Aprovado", color = "Município") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))


# maringa x paranavai
dados_valor_aprovado |> 
  filter(municipio %in% c("Maringa", "Paranavai")) |> 
  ggplot(aes(x = mes, y = valor_aprovado, group = municipio, 
             color = municipio)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "",
    x = "Mês",
    y = "Valor Aprovado",
    color = "Município"
  ) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

dados_valor_aprovado |> 
  group_by(municipio) |> 
  summarise(media = mean(valor_aprovado)) |> 
  arrange(desc(media))


dados_valor_aprovado
dados_juntos

valor_aprovado <- valor_aprovado |> 
  mutate(municipio = str_trim(municipio))

casos_diagnosticados <- tratam_munic_mesdiagnostico |> 
  rename(municipio = munici_pio_da_residencia) |> 
  select(municipio, total)


valor_diagnostico <- left_join(valor_aprovado |> select(municipio, total), 
                               casos_diagnosticados, 
                               by = "municipio")

valor_diagnostico <- valor_diagnostico |> 
  mutate(total.x = total.x/1000000)

valor_diagnostico |> 
  filter(municipio != "Total") |> 
  ggplot(aes(x = total.y, y = total.x, label = municipio)) +
    geom_point(color = "blue", size = 1) +
    labs(
      x = "Número de Casos Diagnósticos (2024)",
      y = "Valor Aprovado (R$)",
      title = "Dispersão: Número de Casos vs. Valor Aprovado por Município"
    ) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# codigo para copiar
library(readxl)
tabela6579 <- read_excel("tabela6579.xlsx", 
                         range = "A3:B403", col_types = c("text", 
                                                          "numeric"))

valor_diagnostico

tabela6579 <- tabela6579[-1, ]
colnames(tabela6579) <- c("municipio", "populacao") 

library(stringi)
tabela6579 <- tabela6579 |> 
  mutate(
    municipio = str_remove(municipio, " \\(PR\\)"),              
    municipio = stri_trans_general(municipio, "Latin-ASCII"),
    municipio = str_to_title(municipio)
  )

dados_populacao <- valor_diagnostico |> 
  left_join(tabela6579 |> select(municipio, populacao), by = "municipio")


dados_populacao |>
  filter(municipio != "Total") |> 
  ggplot(aes(x = total.y, y = populacao)) +
  geom_point()

dados_populacao |> 
  mutate(taxa = populacao/(total.y*1000)) |> 
  arrange(desc(taxa))



valor_diagnostico |> 
  filter(!municipio %in% c("Total", "Maringa", "Paranavai", "Sarandi")) |> 
  ggplot(aes(x = total.y, y = total.x, label = municipio)) +
  geom_point(color = "blue", size = 1) +
  labs(
    x = "Número de Casos Diagnósticos (2024)",
    y = "Valor Aprovado (R$)",
    title = "Dispersão: Número de Casos vs. Valor Aprovado por Município"
  ) +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

cor(valor_diagnostico$total.x, valor_diagnostico$total.y)

valor_diagnostico |> 
  filter(municipio == "Nossa Senhora Das Gracas")
valor_diagnostico |> 
  arrange(total.y)


valor_diagnostico |> 
  mutate(media = total.x/total.y) |> 
  #summarise(ola = mean(media), variancia = sd(media))
  arrange(media)


tabela_media_caso <- valor_diagnostico |>
  filter(municipio != "Total") |>
  mutate(
    media_por_caso = (total.x * 1e6) / total.y # transforma milhões em unidades
  ) |>
  select(municipio, media_por_caso) |>
  arrange(desc(media_por_caso))

# Visualiza
print(tabela_media_caso)









# divi --------------------------------------------------------------------

tabela_media_caso <- valor_diagnostico |>
  filter(municipio != "Total") |>
  mutate(
    valor_total_reais = total.x * 1e6, # transforma milhões em reais
    media_por_caso = valor_total_reais / total.y
  )

# Filtra Itaguajé e Presidente Castelo Branco
selecionadas <- tabela_media_caso |>
  filter(municipio %in% c("Itaguaje", "Presidente Castelo Branco"))

# Calcula geral da região (exceto Total)
geral <- tabela_media_caso |>
  summarise(
    municipio = "Geral (região)",
    valor_total_reais = sum(valor_total_reais, na.rm = TRUE),
    total.y = sum(total.y, na.rm = TRUE),
    media_por_caso = valor_total_reais / total.y
  )

# Junta todas
tabela_final <- bind_rows(selecionadas, geral) |>
  select(municipio, total.y, valor_total_reais, media_por_caso) |>
  mutate(
    valor_total_reais = dollar(valor_total_reais, prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1),
    media_por_caso = dollar(media_por_caso, prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)
  ) |>
  rename(
    `Município` = municipio,
    `Número de casos` = total.y,
    `Valor total aprovado` = valor_total_reais,
    `Média por caso` = media_por_caso
  )


