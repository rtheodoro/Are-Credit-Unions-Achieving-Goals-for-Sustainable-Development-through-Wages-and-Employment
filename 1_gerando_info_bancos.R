################################################################################
#          Tratando dados dos trabalhadores em bancos privados da RAIS - SQL
#
#
#
#
#
################################################################################

# Dados baixados via: big_query_financialinstiturions.txt

# Importa base ----

bancpriv_20102022 <- arrow::read_parquet("data_raw/rais_bancpriv_cbo_2010a2022.parquet") 


# Tratando base -----------------------------------------------------------


# Criando regioes
uf_regiao <- data.frame(
  UF = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
         "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
         "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  Regiao = c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste", 
             "Centro-Oeste", "Sudeste", "Centro-Oeste", "Nordeste", "Centro-Oeste", 
             "Centro-Oeste", "Sudeste", "Norte", "Nordeste", "Sul", "Nordeste", 
             "Nordeste", "Sudeste", "Nordeste", "Sul", "Norte", "Norte", 
             "Sul", "Sudeste", "Nordeste", "Norte")
)


bancpriv_20102022 <- bancpriv_20102022 |> 
  dplyr::left_join(uf_regiao, by = c("sigla_uf" = "UF"))

rm(uf_regiao)

bancpriv_20102022 |> dplyr::count(cnae_2)

# Criando a coluna Grupo_cargo ----
#0) Apagar informação pois não tem relação operacional direta;
#1) Suporte – tarefas não relacionadas aos objetivos da empresa;
#2) Operacional – tarefas relacionadas aos objetivos da empresa, e;
#3) Estratégico – cargos executivos. Os trabalhadores também foram categorizados com base em suas características: gênero, raça e deficiência.

source("grupo_CBO.R")

bancpriv_20102022 <- bancpriv_20102022 |> 
  dplyr::mutate(grupo_cargo = dplyr::case_when(
    `cbo_2002` %in% estrategico ~ "Strategic",
    `cbo_2002` %in% operacional ~ "Operational",
    `cbo_2002` %in% suporte ~ "Support",
    TRUE ~ "Outros"
  )) 

# Calcular a taxa média de crescimento por ano da quantidade de trabalhadores
bancpriv_20102022 |> dplyr::count(ano) |> dplyr::arrange(ano) |> dplyr::mutate(taxa = ifelse(ano == lag(ano), NA, (n - lag(n))/lag(n)*100))


# importando cbo completa do concla
cbo_completa <- read.csv2("data_raw/CBO2002 - Ocupacao.csv")
cbo_bp <- bancpriv_20102022 |> 
  dplyr::count(`cbo_2002`, grupo_cargo) |>
  dplyr::mutate(cbo_2002 = as.numeric(`cbo_2002`)) |>
  unique() |> 
  dplyr::left_join(cbo_completa, by = c("cbo_2002" = "CODIGO")) |> 
  dplyr::filter(!is.na(TITULO))  
cbo_bp |> dplyr::filter(is.na(TITULO))  
write.csv(cbo_bp, "data/cbo_bp_2010a2019.csv", row.names = FALSE)
rm(cbo_completa, estrategico, operacional, suporte, cbo_coop)

# REMOVENDO Outros para facilitar a analise
banc_semOutros <- bancpriv_20102022 |> 
  dplyr::filter(grupo_cargo != "Outros")

# Agrupando por sexo, ano e grupo_cargo -----------------------------------

# Calculando a proporção de mulheres por ano e a taxa de crescimento

banc_semOutros |>
  dplyr::count(ano, sexo) |>
  dplyr::mutate(sexo = ifelse(sexo == 1, "Homem", "Mulher")) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(proporcao = n / sum(n) * 100) |>
  dplyr::ungroup() |>
  dplyr::select(ano, sexo, proporcao) |> 
  dplyr::filter(sexo == "Mulher") 


banc_semOutros |>
  dplyr::count(ano, sexo) |>
  dplyr::mutate(sexo = ifelse(sexo == 1, "Homem", "Mulher")) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(proporcao = n / sum(n) * 100) |>
  dplyr::ungroup() |>
  dplyr::select(ano, sexo, proporcao) |>
  dplyr::filter(sexo == "Mulher")

grupo_cargo_order <- c("Support", "Operational", "Strategic")


banc_semOutros |>
  dplyr::filter(grupo_cargo == "Strategic") |>
  dplyr::count(ano, sexo) |>
  dplyr::mutate(sexo = ifelse(sexo == 1, "Homem", "Mulher")) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(proporcao = n / sum(n) * 100) |>
  dplyr::ungroup() |>
  dplyr::select(ano, sexo, proporcao) |> 
  dplyr::filter(sexo == "Mulher") 




g_prop_women_group <- banc_semOutros |>
  dplyr::count(ano, grupo_cargo, sexo) |>
  tidyr::pivot_wider(names_from = sexo, values_from = n) |>
  dplyr::mutate(prop = (`2` / (`2` + `1`)) * 100) |> # 1 masculino e 2 feminino
  dplyr::select(-`2`, -`1`) |> 
  tidyr::pivot_longer(cols = c("prop"), names_to = "sexo", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = factor(ano), y = prop, color = sexo, group = sexo)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::facet_grid(grupo_cargo ~ ., scales = "free_y") + # Facetando na vertical
  ggplot2::labs(title = "Proportion of women by job group",
                x = "Year",
                y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    legend.position = "none")  # Remover a legenda de cores




g_prop_blackwomen_group <- banc_semOutros |>
  dplyr::mutate(mulher_negra = ifelse(sexo == 2 & raca_cor == 4 | raca_cor == 8 , 1, 0)) |> # 1 = mulher negra
  dplyr::count(ano, grupo_cargo, mulher_negra) |> 
  tidyr::pivot_wider(names_from = mulher_negra, values_from = n) |>
  dplyr::mutate(prop = (`1`/(`1` + `0`))*100) |> # 1 mulher negra
  dplyr::select(-`1`, -`0`) |>
  tidyr::pivot_longer(cols = c("prop"), names_to = "mulher_negra", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = factor(ano), y = prop, color = mulher_negra,  group = mulher_negra)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::facet_grid(grupo_cargo ~ ., scales = "free_y") + # Facetando na vertical
  ggplot2::labs(title = "Proportion of black women by job group",
                x = "Year",
                y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    legend.position = "none")  # Remover a legenda de cores



# Agrupando por raca, ano e grupo_cargo -----------------------------------

# INDIGENA	1
# BRANCA	2
# PRETA	4
# AMARELA	6
# PARDA	8
# NAO IDENT	9
# IGNORADO	-1

# Tabela de Proporção de pessoas negras por ano
banc_semOutros |> 
  dplyr::mutate(raca_cor = dplyr::case_when(
    raca_cor == 1 ~ "INDIGENA",
    raca_cor == 2 ~ "BRANCA",
    raca_cor == 4 ~ "NEGRA",
    raca_cor == 6 ~ "AMARELA",
    raca_cor == 8 ~ "NEGRA",
    TRUE ~ "OUTROS"
  )) |> 
  dplyr::count(ano, raca_cor) |> 
  dplyr::group_by(ano) |> 
  dplyr::mutate(proporcao = n / sum(n) * 100) |> 
  dplyr::filter(raca_cor == "NEGRA") |> 
  dplyr::select(ano, proporcao) |> 
  print(n = Inf)

g_prop_blackpeople_group <- banc_semOutros |> 
  dplyr::count(ano, grupo_cargo, `raca_cor`) |> 
  dplyr::mutate(raca_cor = dplyr::case_when(
    `raca_cor` == 1 ~ "INDIGENA",
    `raca_cor` == 2 ~ "BRANCA",
    `raca_cor` == 4 ~ "NEGRA",
    `raca_cor` == 6 ~ "AMARELA",
    `raca_cor` == 8 ~ "NEGRA",
    TRUE ~ "OUTROS"
  )) |>
  tidyr::pivot_wider(names_from = `raca_cor`, values_from = n, values_fn = sum) |> 
  dplyr::mutate(prop = (NEGRA/(BRANCA + NEGRA + AMARELA + INDIGENA + OUTROS))*100) |>
  dplyr::select(-NEGRA, -NEGRA, -AMARELA, -INDIGENA, -OUTROS) |>
  tidyr::pivot_longer(cols = c("prop"), names_to = "raca_cor", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  dplyr::mutate(raca_cor = factor(raca_cor, levels = c("prop"))) |>
  ggplot2::ggplot(ggplot2::aes(x = factor(ano), y = prop, color = raca_cor,  group = raca_cor)) +
  ggplot2::geom_line(size = 1.2) +
  ggplot2::facet_grid(grupo_cargo ~ ., scales = "free_y") + # Facetando na vertical
  ggplot2::labs(title = "Proportion of black individuals by job group",
                x = "Year",
                y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    legend.position = "none")  # Remover a legenda de cores


# Comparando salários de mulheres -----------------------------------------

# Ajustando salários pela inflação (deflacionando)
ajustar_salario <- function(salario, ano) {
  data_base <- as.Date(paste0(ano, "-12-01"))
  data_referencia <- "01/2024"
  inflacao <- deflateBR::deflate(salario, data_base, data_referencia, "ipca")
  return(inflacao)
}
# Aplicando a função para cada ano de 2010 a 2022
banc_semOutros <- banc_semOutros  |> 
  dplyr::mutate(
    salario_def = dplyr::case_when(
      ano == 2010 ~ ajustar_salario(valor_remuneracao_dezembro, 2010),
      ano == 2011 ~ ajustar_salario(valor_remuneracao_dezembro, 2011),
      ano == 2012 ~ ajustar_salario(valor_remuneracao_dezembro, 2012),
      ano == 2013 ~ ajustar_salario(valor_remuneracao_dezembro, 2013),
      ano == 2014 ~ ajustar_salario(valor_remuneracao_dezembro, 2014),
      ano == 2015 ~ ajustar_salario(valor_remuneracao_dezembro, 2015),
      ano == 2016 ~ ajustar_salario(valor_remuneracao_dezembro, 2016),
      ano == 2017 ~ ajustar_salario(valor_remuneracao_dezembro, 2017),
      ano == 2018 ~ ajustar_salario(valor_remuneracao_dezembro, 2018),
      ano == 2019 ~ ajustar_salario(valor_remuneracao_dezembro, 2019),
      ano == 2020 ~ ajustar_salario(valor_remuneracao_dezembro, 2020),
      ano == 2021 ~ ajustar_salario(valor_remuneracao_dezembro, 2021),
      ano == 2022 ~ ajustar_salario(valor_remuneracao_dezembro, 2022),
      TRUE ~ valor_remuneracao_dezembro
    )
  )

# Plotando gráfico comparando salários de mulheres e homens

# Calculando as médias dos salários por ano e sexo
salario_medio_mh <- banc_semOutros |>
  dplyr::mutate(sexo = ifelse(sexo == 1, "Men", "Women"),
                sex = sexo) |> 
  dplyr::group_by(ano, sex) |> 
  dplyr::summarise(salario = round(mean(salario_def)))

# Plotando o gráfico
# Identificando os primeiros e últimos valores de cada linha
primeiro_ultimo_valores_mh <- salario_medio_mh  |> 
  dplyr::group_by(sex)  |> 
  dplyr::slice(c(1, dplyr::n()))  # Seleciona o primeiro e último valor de cada grupo

# Plotando o gráfico com rótulos apenas para o primeiro e último valor
g_comp_salario_m_h <- ggplot2::ggplot(salario_medio_mh, ggplot2::aes(x = ano, y = salario, color = sex)) +
  ggplot2::geom_line() +
  ggplot2::geom_text(data = primeiro_ultimo_valores_mh, ggplot2::aes(label = salario), hjust = 0.5, vjust = -1) + 
  ggplot2::labs(title = "Comparison of salaries between men and women",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(breaks = salario_medio_mh$ano)


# Calculando os salários médios por ano, grupo de cargo e sexo
salario_medio_mh_g <- banc_semOutros |> 
  dplyr::mutate(sexo = ifelse(sexo == 1, "Men", "Women"),
                sex = sexo) |> 
  dplyr::group_by(ano, grupo_cargo, sex) |> 
  dplyr::summarise(salario = round(mean(salario_def)))

# Adicionando o primeiro e o último valor do salário médio para cada grupo
primeiro_ultimo_valores <- salario_medio_mh_g |> 
  dplyr::group_by(grupo_cargo, sex) |> 
  dplyr::slice(c(1, dplyr::n()))  # Seleciona o primeiro e último valor de cada grupo

# Plotando o gráfico com rótulos para os primeiros e últimos valores
g_comp_salario_m_h_group <- salario_medio_mh_g |> 
  ggplot2::ggplot(ggplot2::aes(x = factor(ano), y = salario, color = sex, group = sex)) +
  ggplot2::geom_line(size = 1.2) +
  ggplot2::geom_text(data = primeiro_ultimo_valores, ggplot2::aes(label = salario),
                     vjust = ifelse(primeiro_ultimo_valores$sex == "Men", -0.5, 0.05)) +
  ggplot2::facet_grid(grupo_cargo ~ ., scales = "free_y") + 
  ggplot2::labs(title = "Comparison of salaries between men and women by job group",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
  )


# Comparando salários de pessoas negras -----------------------------------------

# Calculando os salários médios por ano, grupo de cargo e raça/cor
salario_medio_rc_g <- banc_semOutros |> 
  dplyr::mutate(raca_cor = dplyr::case_when(
    raca_cor == 4 | raca_cor == 8 ~ "Black",
    TRUE ~ "Others"
  ),
  race_color = raca_cor) |> 
  dplyr::group_by(ano, grupo_cargo, race_color) |> 
  dplyr::summarise(salario = round(mean(salario_def)))

# Adicionando o primeiro e o último valor do salário médio para cada grupo
primeiro_ultimo_valores_rc <- salario_medio_rc_g |> 
  dplyr::arrange(grupo_cargo) |> 
  dplyr::group_by(grupo_cargo, race_color) |> 
  dplyr::slice(c(1, dplyr::n()))  # Seleciona o primeiro e último valor de cada grupo

# Plotando o gráfico com rótulos para os primeiros e últimos valores
g_salario_medio_rc_g <- salario_medio_rc_g |> 
  ggplot2::ggplot(ggplot2::aes(x = ano, y = salario, color = race_color)) +
  ggplot2::geom_line() +
  ggplot2::geom_text(data = primeiro_ultimo_valores_rc, ggplot2::aes(label = salario),
                     vjust = ifelse(primeiro_ultimo_valores_rc$race_color == "Black", -0.5, 1.5)) +
  ggplot2::facet_grid(grupo_cargo ~ ., scales = "free_y") + 
  ggplot2::labs(title = "Comparison of salaries between black individuals and others by group of job",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(breaks = unique(salario_medio_rc_g$ano), labels = scales::number_format(accuracy = 1))




