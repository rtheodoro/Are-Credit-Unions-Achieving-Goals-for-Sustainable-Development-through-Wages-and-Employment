
# A base da receita federal (RFB) foi baixada do servidor SQL do OBSCOOP, usando a query
con <-
  RPostgreSQL::dbConnect(
    RPostgreSQL::PostgreSQL(),
    user = "asfsadfasdf",
    dbname = "coop",
    password = "fasdfasdfasdf@4",
    host = "200.144.244.212",
    port = '5432'
  )                        

coop_cred_rfb <- RPostgreSQL::dbGetQuery(con,"
                   SELECT cnpj_basico, identificador_matriz_filial, situacao_cadastral, 
                          data_situacao_cadastral, data_inicio_atividade, cnae_fiscal_principal
                   FROM cnpj_dados_cadastrais_pj.dados_cnpj_2023 
                   WHERE (cnae_fiscal_principal BETWEEN 6400000 AND 6499999) AND data_inicio_atividade <= 20221231
                   ")
RPostgreSQL::dbDisconnect(con)
remove(con)
arrow::write_parquet(coop_cred_rfb, "data_raw/coop_cred_rfb.parquet")

