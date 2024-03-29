
# Plotando gráfico com proporção de mulheres por grupo de cargo
coop_semOutros |>
  dplyr::count(ano, grupo_cargo, sexo) |>
  tidyr::pivot_wider(names_from = sexo, values_from = n) |>
  dplyr::mutate(prop = (`2` / (`2` + `1`)) * 100) |> # 1 masculino e 2 feminino
  dplyr::select(-`2`, -`1`) |> 
  tidyr::pivot_longer(cols = c("prop"), names_to = "sexo", values_to = "prop") |>
  dplyr::mutate(grupo_cargo = factor(grupo_cargo, levels = grupo_cargo_order)) |>
  ggplot(aes(x = factor(ano), y = prop, color = sexo, group = sexo)) +
  geom_line(size = 1.2) +
  facet_grid(grupo_cargo ~ ., scales = "free_y") + # Facetando na vertical
  labs(title = "Proportion of women by job group",
       x = "Year",
       y = "Proportion (%)") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none")  # Remover a legenda de cores


