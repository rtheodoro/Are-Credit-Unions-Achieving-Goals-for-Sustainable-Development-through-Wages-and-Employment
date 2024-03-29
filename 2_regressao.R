################################################################################
#          Gerando Regressão Para Avaliar variáveis que impactam os salários
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

source("grupo_CBO.R")
coop <- coop |> 
  dplyr::mutate(grupo_cargo = dplyr::case_when(
    `cbo_2002` %in% estrategico ~ "Strategic",
    `cbo_2002` %in% operacional ~ "Operational",
    `cbo_2002` %in% suporte ~ "Support",
    TRUE ~ "Outros"
  )) 

# REMOVENDO Outros para facilitar a analise
coop_semOutros <- coop |> 
  dplyr::filter(grupo_cargo != "Outros")

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


# Regressão ---------------------------------------------------------------
# Realizando a regressão de efeitos fixos

coop_semOutros <- coop_semOutros |> 
  dplyr::mutate(
    grupo_cargo = factor(grupo_cargo, levels = c("Strategic", "Operational", "Support")),
    raca_cor = factor(raca_cor, levels = c(1, 2, 3, 4, 5)),
    sexo = ifelse(sexo == 1, "Men", "Women"),
    raca_cor = dplyr::case_when(
      `raca_cor` == 1 ~ "BRANCA",
      `raca_cor` == 2 ~ "BRANCA",
      `raca_cor` == 4 ~ "NEGRA",
      `raca_cor` == 6 ~ "BRANCA",
      `raca_cor` == 8 ~ "NEGRA",
      TRUE ~ "BRANCA"
    ),
    raca_cor = factor(raca_cor, levels = c("NEGRA", "BRANCA")),
    tamanho_estabelecimento = factor(tamanho_estabelecimento, levels = c(1, 2, 3, 4, 5)),
    grau_instrucao_apos_2005 = factor(grau_instrucao_apos_2005, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
  )


reg_fe <- plm::plm(
  salario_def ~ ano + Regiao +
    idade + grau_instrucao_apos_2005 +
    raca_cor  + sexo + grupo_cargo +
    tamanho_estabelecimento,
  data = coop_semOutros,
  index = c("ano", "Regiao"),
  model = "within"
)


reg_fe |> summary()
# 