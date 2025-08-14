## man and women

# Função para analisar diferença salarial
analisar_diferenca_salarial <- function(dados) {
  
  # Criar lista para armazenar resultados
  resultados <- list()
  
  # Obter combinações únicas de ano e grupo_cargo
  combinacoes <- dados |>
    dplyr::select(ano, grupo_cargo) |>
    dplyr::distinct()
  
  # Iterar sobre cada combinação
  for(i in 1:nrow(combinacoes)) {
    ano_atual <- combinacoes$ano[i]
    cargo_atual <- combinacoes$grupo_cargo[i]
    
    # Filtrar dados para a combinação atual
    grupo <- dados |>
      dplyr::filter(ano == ano_atual, grupo_cargo == cargo_atual)
    
    # Separar salários por sexo
    salarios_homens <- grupo |>
      dplyr::filter(sexo == 1) |>
      dplyr::pull(valor_remuneracao_dezembro)
    
    salarios_mulheres <- grupo |>
      dplyr::filter(sexo == 2) |>
      dplyr::pull(valor_remuneracao_dezembro)
    
    # Verificar se há dados para ambos os sexos
    if(length(salarios_homens) > 0 & length(salarios_mulheres) > 0) {
      
      # Calcular estatísticas descritivas
      media_homens <- salarios_homens |> mean(na.rm = TRUE)
      media_mulheres <- salarios_mulheres |> mean(na.rm = TRUE)
      diferenca_percentual <- ((media_homens - media_mulheres) / media_mulheres) * 100
      
      # Realizar teste t se houver dados suficientes
      if(length(salarios_homens) > 1 & length(salarios_mulheres) > 1) {
        teste_t <- stats::t.test(salarios_homens, salarios_mulheres)
        t_stat <- teste_t$statistic
        p_value <- teste_t$p.value
        significativo <- p_value < 0.05
      } else {
        t_stat <- NA
        p_value <- NA
        significativo <- "Dados insuficientes"
      }
      
      # Armazenar resultado
      resultados[[length(resultados) + 1]] <- data.frame(
        ano = ano_atual,
        grupo_cargo = cargo_atual,
        n_homens = length(salarios_homens),
        n_mulheres = length(salarios_mulheres),
        media_homens = media_homens,
        media_mulheres = media_mulheres,
        diferenca_percentual = diferenca_percentual,
        t_statistic = as.numeric(t_stat),
        p_value = p_value,
        significativo = significativo,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combinar todos os resultados
  resultado_final <- resultados |> dplyr::bind_rows()
  
  return(resultado_final)
}

# Executar análise
resultado_teste <- coop_semOutros |> analisar_diferenca_salarial()

# Visualizar resultados
resultado_teste |> print()

# Criar tabela formatada dos resultados
resultado_formatado <- resultado_teste |>
  dplyr::mutate(
    diferenca_percentual = diferenca_percentual |> round(2),
    media_homens = media_homens |> round(2),
    media_mulheres = media_mulheres |> round(2),
    t_statistic = t_statistic |> round(4),
    p_value = p_value |> round(4)
  ) |>
  dplyr::select(ano, grupo_cargo, n_homens, n_mulheres, 
                media_homens, media_mulheres, diferenca_percentual,
                p_value, significativo)

# Visualizar tabela formatada
resultado_formatado |>
  knitr::kable(caption = "Análise de Diferença Salarial por Gênero")

# Criar visualização opcional
grafico_diferenca <- resultado_teste |>
  dplyr::filter(!is.na(p_value)) |>
  ggplot2::ggplot(ggplot2::aes(x = grupo_cargo, y = diferenca_percentual, 
                               fill = factor(ano))) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(title = "Diferença Percentual Salarial (Homens vs Mulheres)",
                x = "Grupo de Cargo",
                y = "Diferença Percentual (%)",
                fill = "Ano") +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red")

grafico_diferenca |> print()


## black and others

# Função para analisar diferença salarial
analisar_diferenca_salarial <- function(dados) {
  
  # Criar lista para armazenar resultados
  resultados <- list()
  
  # Obter combinações únicas de ano e grupo_cargo
  combinacoes <- dados |>
    dplyr::select(ano, grupo_cargo) |>
    dplyr::distinct()
  
  # Iterar sobre cada combinação
  for(i in 1:nrow(combinacoes)) {
    ano_atual <- combinacoes$ano[i]
    cargo_atual <- combinacoes$grupo_cargo[i]
    
    # Filtrar dados para a combinação atual
    grupo <- dados |>
      dplyr::filter(ano == ano_atual, grupo_cargo == cargo_atual) |> 
      dplyr::mutate(raca_cor = dplyr::case_when(
        raca_cor == 4 | raca_cor == 8 ~ "Black",
        TRUE ~ "Others"
      ),
      race_color = raca_cor)
    
    # Separar salários por sexo
    salarios_negros <- grupo |>
      dplyr::filter(raca_cor == "Black") |>
      dplyr::pull(valor_remuneracao_dezembro)
    
    salarios_outros <- grupo |>
      dplyr::filter(raca_cor == "Others") |>
      dplyr::pull(valor_remuneracao_dezembro)
    
    # Verificar se há dados para ambos os sexos
    if(length(salarios_negros) > 0 & length(salarios_outros) > 0) {
      
      # Calcular estatísticas descritivas
      media_negros <- salarios_negros |> mean(na.rm = TRUE)
      media_outros <- salarios_outros |> mean(na.rm = TRUE)
      diferenca_percentual <- ((media_negros - media_outros) / media_outros) * 100
      
      # Realizar teste t se houver dados suficientes
      if(length(salarios_negros) > 1 & length(salarios_outros) > 1) {
        teste_t <- stats::t.test(salarios_negros, salarios_outros)
        t_stat <- teste_t$statistic
        p_value <- teste_t$p.value
        significativo <- p_value < 0.05
      } else {
        t_stat <- NA
        p_value <- NA
        significativo <- "Dados insuficientes"
      }
      
      # Armazenar resultado
      resultados[[length(resultados) + 1]] <- data.frame(
        ano = ano_atual,
        grupo_cargo = cargo_atual,
        n_negros = length(salarios_negros),
        n_outros = length(salarios_outros),
        media_negros = media_negros,
        media_outros = media_outros,
        diferenca_percentual = diferenca_percentual,
        t_statistic = as.numeric(t_stat),
        p_value = p_value,
        significativo = significativo,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combinar todos os resultados
  resultado_final <- resultados |> dplyr::bind_rows()
  
  return(resultado_final)
}

# Executar análise
resultado_teste <- coop_semOutros |> analisar_diferenca_salarial()

# Visualizar resultados
resultado_teste |> print()

# Criar tabela formatada dos resultados
resultado_formatado <- resultado_teste |>
  dplyr::mutate(
    diferenca_percentual = diferenca_percentual |> round(2),
    media_homens = media_homens |> round(2),
    media_mulheres = media_mulheres |> round(2),
    t_statistic = t_statistic |> round(4),
    p_value = p_value |> round(4)
  ) |>
  dplyr::select(ano, grupo_cargo, n_homens, n_mulheres, 
                media_homens, media_mulheres, diferenca_percentual,
                p_value, significativo)

# Visualizar tabela formatada
resultado_formatado |>
  knitr::kable(caption = "Análise de Diferença Salarial por Gênero")

# Criar visualização opcional
grafico_diferenca <- resultado_teste |>
  dplyr::filter(!is.na(p_value)) |>
  ggplot2::ggplot(ggplot2::aes(x = grupo_cargo, y = diferenca_percentual, 
                               fill = factor(ano))) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(title = "Diferença Percentual Salarial (Homens vs Mulheres)",
                x = "Grupo de Cargo",
                y = "Diferença Percentual (%)",
                fill = "Ano") +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red")

grafico_diferenca |> print()
