################################################################################
#          Baixando dados dos trabalhos em coop. cred. da RAIS - SQL
#
#
#
#
#
################################################################################


# A base foi baixada do BD OBSCOOP, usando a query
# SELECT sigla_uf, ano, cnae_2, cbo_2002,
# vinculo_ativo_3112, valor_remuneracao_dezembro, idade,
# raca_cor, grau_instrucao_apos_2005,sexo,
# tamanho_estabelecimento
# FROM `basedosdados.br_me_rais.microdados_vinculos`
# WHERE natureza_juridica LIKE "2143" AND vinculo_ativo_3112 = 1 AND cnae_2 LIKE '64%' AND ano >= 2010


# Importa base ----
coop1 <- read.csv("data_raw/rais_coopcred_CBO_2010a2016.csv")
coop2 <- read.csv("data_raw/rais_coopcred_CBO_2017a2022.csv")
coop <- rbind(coop1, coop2)
rm(coop1, coop2)

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


coop <- coop |> 
  dplyr::left_join(uf_regiao, by = c("sigla_uf" = "UF"))

rm(uf_regiao)

coop |> dplyr::count(cnae_2)

# Criando a coluna Grupo_cargo ----
#0) Apagar informação pois não tem relação operacional direta;
#1) Suporte – tarefas não relacionadas aos objetivos da empresa;
#2) Operacional – tarefas relacionadas aos objetivos da empresa, e;
#3) Estratégico – cargos executivos. Os trabalhadores também foram categorizados com base em suas características: gênero, raça e deficiência.

source("grupo_CBO.R")

coop <- coop |> 
  dplyr::mutate(grupo_cargo = dplyr::case_when(
    `cbo_2002` %in% estrategico ~ "Strategic",
    `cbo_2002` %in% operacional ~ "Operational",
    `cbo_2002` %in% suporte ~ "Support",
    TRUE ~ "Outros"
  )) 

# Calcular a taxa média de crescimento por ano da quantidade de trabalhadores
coop |> dplyr::count(ano) |> dplyr::arrange(ano) |> dplyr::mutate(taxa = ifelse(ano == lag(ano), NA, (n - lag(n))/lag(n)*100))


# importando cbo completa do concla
cbo_completa <- read.csv2("data_raw/CBO2002 - Ocupacao.csv")
cbo_coop <- coop |> 
  dplyr::count(`cbo_2002`, grupo_cargo) |>
  dplyr::mutate(cbo_2002 = as.numeric(`cbo_2002`)) |>
  unique() |> 
  dplyr::left_join(cbo_completa, by = c("cbo_2002" = "CODIGO")) |> 
  dplyr::filter(!is.na(TITULO))  
cbo_coop |> dplyr::filter(is.na(TITULO))  
write.csv(cbo_coop, "data/cbo_coop_2010a2019.csv", row.names = FALSE)
rm(cbo_completa, estrategico, operacional, suporte, con, cbo_coop)

# REMOVENDO Outros para facilitar a analise
coop_semOutros <- coop |> 
  dplyr::filter(grupo_cargo != "Outros")

# Agrupando por sexo, ano e grupo_cargo -----------------------------------

# Calculando a proporção de mulheres por ano e a taxa de crescimento

coop_semOutros |>
  dplyr::count(ano, sexo) |>
  dplyr::mutate(sexo = ifelse(sexo == 1, "Homem", "Mulher")) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(proporcao = n / sum(n) * 100) |>
  dplyr::ungroup() |>
  dplyr::select(ano, sexo, proporcao) |> 
  dplyr::filter(sexo == "Mulher") 

grupo_cargo_order <- c("Support", "Operational", "Strategic")

g_prop_women_group <- coop_semOutros |>
  dplyr::count(ano, grupo_cargo, sexo) |>
  tidyr::pivot_wider(names_from = sexo, values_from = n) |>
  dplyr::mutate(prop = (`2`/(`2` + `1`))*100) |> # 1 masculino e 2 feminino
  dplyr::select(-`2`, -`1`) |> 
  tidyr::pivot_longer(cols = c("prop"), names_to = "sexo", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = prop, color = sexo)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Proportion of women by job group",
       x = "Year",
       y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))


g_prop_blackwomen_group <- coop_semOutros |>
  dplyr::mutate(mulher_negra = ifelse(sexo == 2 & raca_cor == 4 | raca_cor == 8 , 1, 0)) |> # 1 = mulher negra
  dplyr::count(ano, grupo_cargo, mulher_negra) |> 
  tidyr::pivot_wider(names_from = mulher_negra, values_from = n) |>
  dplyr::mutate(prop = (`1`/(`1` + `0`))*100) |> # 1 femin
  dplyr::select(-`1`, -`0`) |>
  tidyr::pivot_longer(cols = c("prop"), names_to = "mulher_negra", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = prop, color = mulher_negra)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Proportion of women by job group",
                x = "Year",
                y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))




# Agrupando por raca, ano e grupo_cargo -----------------------------------

# INDIGENA	1
# BRANCA	2
# PRETA	4
# AMARELA	6
# PARDA	8
# NAO IDENT	9
# IGNORADO	-1

# Tabela de Proporção de pessoas negras por ano
coop_semOutros |> 
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

g_prop_blackpeople_group <- coop_semOutros |> 
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
  ggplot2::ggplot(ggplot2::aes(x = ano, y = prop, color = raca_cor)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Percentage of black people in the workforce of Credit Unions by Job Group",
                x = "Year",
                y = "Proportion (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))

#Calculando uma tabela com a proporcao de pessoas por raca e ano

coop_semOutros |> 
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
  dplyr::select(-BRANCA, -NEGRA, -AMARELA, -INDIGENA, -OUTROS) |>
  tidyr::pivot_longer(cols = c("prop"), names_to = "raca_cor", values_to = "prop") |>
  dplyr::mutate(raca_cor = factor(raca_cor, levels = c("prop"))) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = prop, color = raca_cor)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Proporção de Negros por grupo de cargo",
                x = "Ano",
                y = "Proporção de Negros") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))


# Comparando salários de mulheres -----------------------------------------

# Ajustando salários pela inflação (deflacionando)
ajustar_salario <- function(salario, ano) {
  data_base <- as.Date(paste0(ano, "-12-01"))
  data_referencia <- "01/2024"
  inflacao <- deflateBR::deflate(salario, data_base, data_referencia, "ipca")
  return(inflacao)
}
# Aplicando a função para cada ano de 2010 a 2022
coop_semOutros <- coop_semOutros  |> 
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

g_comp_salario_m_h <- coop_semOutros |> 
  dplyr::mutate(sexo = ifelse(sexo == 1, "Men", "Women")) |> 
  dplyr::group_by(ano, sexo) |> 
  dplyr::summarise(salario = mean(salario_def)) |> 
  ggplot2::ggplot(ggplot2::aes(x = ano, y = salario, color = sexo)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Comparison of salaries between men and women",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))

# Plotando gráfico comparando salários de mulheres e homens por grupo de cargo

g_comp_salario_m_h_group <- coop_semOutros |> 
  dplyr::mutate(sexo = ifelse(sexo == 1, "Men", "Women")) |> 
  dplyr::group_by(ano, grupo_cargo, sexo) |> 
  dplyr::summarise(salario = mean(salario_def)) |> 
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = salario, color = sexo)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Comparison of salaries between men and women by group job",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))



# Comparando salários de pessoas negras -----------------------------------------

# Plotando gráfico comparando salários de pessoas negras e brancas

coop_semOutros |> 
  dplyr::mutate(raca_cor = dplyr::case_when(
    raca_cor == 4 | raca_cor == 8 ~ "Black",
    TRUE ~ "Others"
  )) |> 
  dplyr::group_by(ano, grupo_cargo, raca_cor) |> 
  dplyr::summarise(salario = mean(salario_def)) |> 
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = salario, color = raca_cor)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~grupo_cargo) +
  ggplot2::labs(title = "Comparison of salaries between black and other people",
                x = "Year",
                y = "Salary (R$)") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1))










