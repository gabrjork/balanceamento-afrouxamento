##### ===== Rebalanceamento de Portfolio: a relevância de antecipar ciclos de cortes de juros ===== #####

library(GetBCBData)
library(dplyr)
library(ggplot2)
library(lubridate)
library(writexl)
library(tidyverse)
library(showtext)

# Configurando a fonte
font_add("plusjakarta", regular = "C:/Users/GabrielHenriqueMarti/AppData/Local/Microsoft/Windows/Fonts/PLUSJAKARTASANS-Regular.ttf")
showtext_auto()

# Puxando a Selic diária desde 2007 do SGS
#     2007 em função de limitação dos dados históricos usados mais tarde
historico <- gbcbd_get_series(
    id = c("selic" = 432), 
    first.date = "2007-10-01", 
    last.date = Sys.Date(),
    format.data = "wide"
)

colnames(historico) <- c("data", "selic")

head(historico)
tail(historico)
write_xlsx(historico, "historico_selic_diaria_2007_2024.xlsx")

## === Identificando cortes de juros na série diária === ##

# Identificando cortes de juros (redução da Selic)
cortes_juros <- historico %>%
    arrange(data) %>%
    mutate(
        selic_anterior = lag(selic),
        variacao = selic - selic_anterior,
        corte = variacao < 0
    ) %>%
    filter(corte == TRUE) %>%
    mutate(
        variacao_pp = round(variacao, 2)  # variação em pontos percentuais
    ) %>%
    select(data, selic_anterior, selic, variacao_pp)

# Visualizando os cortes de juros
print("=== DIAS COM CORTES DE JUROS ===")
print(cortes_juros)

head(cortes_juros)
tail(cortes_juros)

# --- PASSO 1: Criar um dataframe com TODAS as variações (altas e baixas) ---
# Precisamos deste dataframe para consultar qual foi a última variação antes de um corte.
todas_variacoes <- historico %>%
    arrange(data) %>%
    mutate(
        selic_anterior = lag(selic),
        variacao = selic - selic_anterior
    ) %>%
    # Filtra apenas os dias em que houve mudança
    filter(variacao != 0) %>%
    select(data, variacao)

# --- PASSO 2: Aplicar a nova lógica para identificar 'inicio_ciclos' ---
# Este bloco substitui o anterior e corrige o erro 'object .x not found'
inicio_ciclos <- cortes_juros %>%
    rowwise() %>%
    mutate(
        # Para cada corte, encontramos a última variação (alta ou baixa)
        # que ocorreu ESTRITAMENTE ANTES da data do corte atual (.env$data).
        ultima_variacao_info = list(
            todas_variacoes %>%
                # CORREÇÃO AQUI: Trocado '!!.x$data' por '.env$data'
                filter(data < .env$data) %>%
                arrange(desc(data)) %>%
                slice(1) # Pega apenas a mais recente
        ),
        
        # Extrai a data da última variação
        data_ultima_variacao = if (nrow(ultima_variacao_info) > 0) ultima_variacao_info$data else as.Date(NA),
        
        # Extrai o valor da última variação (positivo se foi alta, negativo se foi baixa)
        valor_ultima_variacao = if (nrow(ultima_variacao_info) > 0) ultima_variacao_info$variacao else NA_real_
    ) %>%
    ungroup() %>% # Sair do modo rowwise para os cálculos seguintes
    mutate(
        # --- Avaliar Critério 1: A última variação foi uma ALTA? ---
        criterio1_ultima_foi_alta = !is.na(valor_ultima_variacao) & valor_ultima_variacao > 0,
        
        # --- Avaliar Critério 2: Estabilidade por mais de 180 dias? ---
        # Calcular os dias desde a última variação (alta ou baixa)
        dias_de_estabilidade = as.numeric(data - data_ultima_variacao),
        criterio2_estabilidade_180d = !is.na(dias_de_estabilidade) & dias_de_estabilidade > 180,
        
        # --- Caso especial: É o primeiro corte da série histórica? ---
        # Se for, não há variação anterior (valor_ultima_variacao é NA)
        primeiro_corte_da_serie = is.na(valor_ultima_variacao)
    ) %>%
    # Um corte inicia um ciclo se:
    # 1. For o primeiro corte da série histórica (caso especial).
    # 2. A última variação antes dele foi uma ALTA (Critério 1).
    # 3. Houve estabilidade por mais de 180 dias desde a última variação (Critério 2).
    filter(primeiro_corte_da_serie | criterio1_ultima_foi_alta | criterio2_estabilidade_180d) %>%
    
    # Selecionar as colunas originais
    select(data, selic_anterior, selic, variacao_pp)

# Visualizando os novos inícios de ciclos
print("\n=== CORTES QUE INICIARAM CICLOS DE QUEDA (NOVA LÓGICA) ===")
print(inicio_ciclos, n = nrow(inicio_ciclos)) # Garante que todas as linhas sejam impressas

# Salva o resultado em um Excel (opcional)
write_xlsx(inicio_ciclos, "cortes_inicio_ciclos_selic_NOVA_LOGICA.xlsx")


#Plotando o gráfico da Selic ao longo do tempo
p_selic <- ggplot(historico, aes(x = data, y = selic)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(data = cortes_juros, aes(x = data, y = selic), 
               color = "red", size = 2, alpha = 0.7) +
    labs(title = "Taxa Selic ao Longo do Tempo",
         subtitle = "Pontos vermelhos indicam dias com cortes de juros",
         x = "Data",
         y = "Taxa Selic (%)") +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

print(p_selic)
showtext_opts(dpi = 300)
ggsave("grafico_selic_cortes.png", p_selic, width = 10, height = 9, dpi = 300)


### ======================================================================= ###
### ====== Puxando dados de retornos diários de um arquivo CSV ====== ###
retornos_cheio <- read.csv(
    "RetornosDiarios2007.csv", 
    sep = ";", 
    dec = ",", 
    na.strings = "nd"
)

retornos_cheio$Data <- as.Date(retornos_cheio$Data, format="%d/%m/%Y")

colnames(retornos_cheio) <- c(
    "Data", "CDI", "IBOV", "IRFM",
    "IMAB", "PTAXV"
)

cat("\n=== ESTRUTURA DOS DADOS APÓS LIMPEZA ===\n")
str(retornos_cheio)

cat("\n=== INÍCIO DOS DADOS APÓS LIMPEZA ===\n")
head(retornos_cheio)

retornos_cheio <- retornos_cheio %>% drop_na()
any(is.na(retornos_cheio))


retornos <- retornos_cheio %>%
    select(Data, CDI, IBOV, IRFM, IMAB, -PTAXV)

colnames(retornos)



### ======================================================================= ###
### FASE 1: ANÁLISE DE PERFORMANCE DOS ATIVOS AO REDOR DO INÍCIO DO CICLO   ###
### ======================================================================= ###

# 1. Parâmetros da Janela de Análise
dias_antes <- 126  # Aproximadamente 2 meses
dias_depois <- 252 # Aproximadamente 3 meses

# 2. Loop para extrair os dados de cada ciclo e calcular retornos acumulados
analise_ciclos <- purrr::map_dfr(inicio_ciclos$data, function(data_corte) {
    
    # Encontra o índice da data do corte no nosso dataframe de retornos
    indice_corte <- which(retornos$Data == data_corte)
    
    # Se não encontrar a data exata, pula para a próxima (segurança)
    if (length(indice_corte) == 0) return(NULL)
    
    # Define os índices de início e fim da janela
    indice_inicio <- indice_corte - dias_antes
    indice_fim <- indice_corte + dias_depois
    
    # Garante que a janela não saia dos limites do nosso histórico de dados
    if (indice_inicio < 1 | indice_fim > nrow(retornos)) return(NULL)
    
    # Recorta a janela de retornos
    janela_retornos <- retornos[indice_inicio:indice_fim, ]
    
    # 3. Calcula o retorno acumulado para cada ativo
    janela_acum <- janela_retornos %>%
        # Transforma os retornos diários em fatores (ex: 0.01 -> 1.01)
        mutate(across(-Data, ~ 1 + .)) %>%
        # Calcula o produto acumulado (retorno composto) e normaliza para base 100
        mutate(across(-Data, ~ cumprod(.) * 100)) %>%
        # Adiciona uma coluna para identificar o dia relativo ao corte (-63, -62, ..., 0, ..., 42)
        mutate(DiaRelativo = seq(-dias_antes, dias_depois, by = 1)) %>%
        # Adiciona a data do ciclo para referência
        mutate(Ciclo = data_corte)
        
    return(janela_acum)
})

# 4. Transforma os dados para o formato "longo", ideal para o ggplot
analise_long <- analise_ciclos %>%
    pivot_longer(
        cols = -c(Data, DiaRelativo, Ciclo),
        names_to = "Indice",
        values_to = "RetornoAcumulado"
    )

# 5. Calcula a trajetória média para cada índice
media_ciclos <- analise_long %>%
    group_by(Indice, DiaRelativo) %>%
    summarise(RetornoMedio = mean(RetornoAcumulado, na.rm = TRUE)) %>%
    ungroup()

####==================================================================####
#### ===== Puxando dados de retorno diário da carteira diversificada ===== ####
####==================================================================####

diversificada_cheio <- read.csv(
    "retornodiversificada.csv", 
    sep = ";", 
    dec = ",", 
    na.strings = "nd"
)

diversificada_cheio$Data <- as.Date(diversificada_cheio$Data, format="%d/%m/%Y")

colnames(diversificada_cheio) <- c(
    "Data", "diversificada"
)

cat("\n=== ESTRUTURA DOS DADOS APÓS LIMPEZA ===\n")
str(diversificada_cheio)

cat("\n=== INÍCIO DOS DADOS APÓS LIMPEZA ===\n")
head(diversificada_cheio)

diversificada <- diversificada_cheio %>% drop_na()
any(is.na(diversificada))




### ================================================================================= ###
### FASE 1 (REPETIÇÃO): ANÁLISE DE PERFORMANCE PARA A CARTEIRA diversificada                ###
### ================================================================================= ###

# 1. Loop para extrair os dados de cada ciclo e calcular retornos acumulados
analise_ciclos_diversificada <- purrr::map_dfr(inicio_ciclos$data, function(data_corte) {
    
    # Encontra o índice da data do corte no dataframe da carteira diversificada
    indice_corte <- which(diversificada_cheio$Data == data_corte)
    
    if (length(indice_corte) == 0) return(NULL)
    
    indice_inicio <- indice_corte - dias_antes
    indice_fim <- indice_corte + dias_depois
    
    if (indice_inicio < 1 | indice_fim > nrow(diversificada_cheio)) return(NULL)
    
    # Recorta a janela de retornos do dataframe da carteira diversificada
    janela_retornos <- diversificada_cheio[indice_inicio:indice_fim, ]
    
    # 2. Calcula o retorno acumulado
    janela_acum <- janela_retornos %>%
        mutate(across(-Data, ~ 1 + .)) %>%
        mutate(across(-Data, ~ cumprod(.) * 100)) %>%
        mutate(DiaRelativo = seq(-dias_antes, dias_depois, by = 1)) %>%
        mutate(Ciclo = data_corte)
        
    return(janela_acum)
})

# 3. Transforma os dados para o formato "longo"
analise_long_diversificada <- analise_ciclos_diversificada %>%
    pivot_longer(
        cols = -c(Data, DiaRelativo, Ciclo),
        names_to = "Indice",
        values_to = "RetornoAcumulado"
    )

# 4. Calcula a trajetória média
media_ciclos_diversificada <- analise_long_diversificada %>%
    group_by(Indice, DiaRelativo) %>%
    summarise(RetornoMedio = mean(RetornoAcumulado, na.rm = TRUE)) %>%
    ungroup()





###### ======================================= ######
###### REPETIÇÃO COMEÇANDO A PARTIR DO DIA 0   ######
###### ======================================= ######

### ================================================================================= ###
### ANÁLISE ADICIONAL (JANELA ESTENDIDA): PERFORMANCE DOS ATIVOS A PARTIR DO DIA 0    ###

# 1. Parâmetros da Janela de Análise (iniciando no Dia 0, janela estendida)
dias_antes_d0 <- 0
dias_depois_ext <- 378 # Novo parâmetro para a janela estendida (1.5 anos)

# 2. Loop para extrair os dados e calcular retornos acumulados (JANELA ESTENDIDA)
# Os resultados serão salvos em novos dataframes (terminados em _ext)
analise_ciclos_d0_ext <- purrr::map_dfr(inicio_ciclos$data, function(data_corte) {
    indice_corte <- which(retornos$Data == data_corte)
    if (length(indice_corte) == 0) return(NULL)
    
    indice_inicio <- indice_corte - dias_antes_d0
    # Modificado para usar a nova janela de 378 dias
    indice_fim <- indice_corte + dias_depois_ext 
    if (indice_inicio < 1 | indice_fim > nrow(retornos)) return(NULL)
    
    janela_retornos <- retornos[indice_inicio:indice_fim, ]
    
    janela_acum <- janela_retornos %>%
        mutate(across(-Data, ~ 1 + .)) %>%
        mutate(across(-Data, ~ cumprod(.) * 100)) %>%
        # Modificado para usar a nova janela de 378 dias
        mutate(DiaRelativo = seq(dias_antes_d0, dias_depois_ext, by = 1)) %>%
        mutate(Ciclo = data_corte)
        
    return(janela_acum)
})

# 3. Processamento e cálculo da média (lógica idêntica à anterior)
analise_long_d0_ext <- analise_ciclos_d0_ext %>%
    pivot_longer(cols = -c(Data, DiaRelativo, Ciclo), names_to = "Indice", values_to = "RetornoAcumulado")

media_ciclos_d0_ext <- analise_long_d0_ext %>%
    group_by(Indice, DiaRelativo) %>%
    summarise(RetornoMedio = mean(RetornoAcumulado, na.rm = TRUE)) %>%
    ungroup()




### ================================================================================= ###
### ANÁLISE ADICIONAL (JANELA ESTENDIDA): PERFORMANCE DA CARTEIRA diversificada A PARTIR DO DIA 0 ###
### ================================================================================= ###

# 1. Loop e cálculo de retorno acumulado para a carteira diversificada (JANELA ESTENDIDA)
analise_ciclos_diversificada_d0_ext <- purrr::map_dfr(inicio_ciclos$data, function(data_corte) {
    indice_corte <- which(diversificada_cheio$Data == data_corte)
    if (length(indice_corte) == 0) return(NULL)
    
    indice_inicio <- indice_corte - dias_antes_d0
    # Modificado para usar a nova janela de 378 dias
    indice_fim <- indice_corte + dias_depois_ext 
    if (indice_inicio < 1 | indice_fim > nrow(diversificada_cheio)) return(NULL)
    
    janela_retornos <- diversificada_cheio[indice_inicio:indice_fim, ]
    
    janela_acum <- janela_retornos %>%
        mutate(across(-Data, ~ 1 + .)) %>%
        mutate(across(-Data, ~ cumprod(.) * 100)) %>%
        # Modificado para usar a nova janela de 378 dias
        mutate(DiaRelativo = seq(dias_antes_d0, dias_depois_ext, by = 1)) %>%
        mutate(Ciclo = data_corte)
        
    return(janela_acum)
})

# 2. Processamento e cálculo da média
analise_long_diversificada_d0_ext <- analise_ciclos_diversificada_d0_ext %>%
    pivot_longer(cols = -c(Data, DiaRelativo, Ciclo), names_to = "Indice", values_to = "RetornoAcumulado")

media_ciclos_diversificada_d0_ext <- analise_long_diversificada_d0_ext %>%
    group_by(Indice, DiaRelativo) %>%
    summarise(RetornoMedio = mean(RetornoAcumulado, na.rm = TRUE)) %>%
    ungroup()



### ======================================================================= ###
### ANÁLISE cautelosa: CDI (ANTES DO DIA 0) + diversificada (A PARTIR DO DIA 0)    ###
### ======================================================================= ###

# 1. Preparação dos Dados: Combinar CDI e diversificada em um único dataframe
dados_combinados <- retornos %>%
    select(Data, CDI) %>%
    inner_join(diversificada_cheio, by = "Data")

# 2. Loop para extrair dados, criar a série cautelosa e calcular o retorno acumulado
analise_cautelosa <- purrr::map_dfr(inicio_ciclos$data, function(data_corte) {
    
    # Encontra o índice da data do corte no dataframe combinado
    indice_corte <- which(dados_combinados$Data == data_corte)
    if (length(indice_corte) == 0) return(NULL)
    
    # Continua usando as variáveis originais 'dias_antes' e 'dias_depois' (ex: 126 e 252)
    indice_inicio <- indice_corte - dias_antes
    indice_fim <- indice_corte + dias_depois
    if (indice_inicio < 1 | indice_fim > nrow(dados_combinados)) return(NULL)
    
    # Recorta a janela de retornos
    janela_retornos <- dados_combinados[indice_inicio:indice_fim, ]
    
    # 3. Cria a série de retornos cautelosa e calcula o patrimônio acumulado
    janela_acum_cautelosa <- janela_retornos %>%
        mutate(DiaRelativo = seq(-dias_antes, dias_depois, by = 1)) %>%
        # A lógica principal está aqui:
        mutate(RetornoHibrido = if_else(DiaRelativo < 0, CDI, diversificada)) %>%
        # Calcula o patrimônio acumulado para a nova série
        mutate(PatrimonioHibrido = cumprod(1 + RetornoHibrido) * 100) %>%
        mutate(Ciclo = data_corte) %>%
        select(DiaRelativo, Ciclo, PatrimonioHibrido)
        
    return(janela_acum_cautelosa)
})

# 4. Calcula a trajetória média para a estratégia cautelosa
media_cautelosa <- analise_cautelosa %>%
    group_by(DiaRelativo) %>%
    summarise(PatrimonioMedio = mean(PatrimonioHibrido, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Estrategia = "cautelosa (CDI->diversificada)")

# 5. Preparar dados de benchmark para o gráfico (CDI puro e diversificada pura)
media_benchmark_cdi <- media_ciclos %>% filter(Indice == "CDI") %>% select(DiaRelativo, PatrimonioMedio = RetornoMedio) %>% mutate(Estrategia = "CDI Puro")
media_benchmark_diversificada <- media_ciclos_diversificada %>% select(DiaRelativo, PatrimonioMedio = RetornoMedio) %>% mutate(Estrategia = "diversificada Pura")

# Combinar todos os dados para o plot
dados_plot_hibrido <- bind_rows(media_cautelosa, media_benchmark_cdi, media_benchmark_diversificada)





### ======================================================================= ###
### SEÇÃO DE VISUALIZAÇÃO: GERAÇÃO DE TODOS OS GRÁFICOS                     ###
### ======================================================================= ###

# GRÁFICO 1: Performance Média dos Ativos (Janela Completa)
plot_fase1 <- ggplot(media_ciclos, aes(x = DiaRelativo, y = RetornoMedio, color = Indice)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    annotate("text", x = 5, y = max(media_ciclos$RetornoMedio) * 0.95, 
            label = "", hjust = 0, color = "black") +
    labs(
        title = "Desempenho Médio dos Ativos em Ciclos de Corte da Selic",
        subtitle = "Janela de 6 meses antes (126 dias úteis) a 12 meses depois (+252 dias úteis)",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado Médio (Base 100)",
        color = "Índice"
    ) +
    scale_color_manual(
        values = c(
            "CDI" = "#252525ff",
            "IBOV" = "#189CD8",
            "IRFM" = "#929292ff",
            "IMAB" = "#4DD3C0"
        )
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_fase1)
showtext_opts(dpi = 300)
ggsave("analise_performance_ciclos.png", plot_fase1, width = 10, height = 9, dpi = 300)



# GRÁFICO 2: Diagnóstico Espaguete (Todos os Ativos)
plot_espaguete_facet <- ggplot() +
    geom_line(
        data = analise_long, 
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        linewidth = 0.5, 
        alpha = 0.5
    ) +
    geom_line(
        data = media_ciclos,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "189CD8",
        linewidth = 1.2
    ) +
    facet_wrap(~ Indice, scales = "free_y") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 100, linetype = "dotted", color = "black") +
    labs(
        title = "Diagnóstico dos Ciclos de Corte: Trajetórias Individuais vs. Média",
        subtitle = "Cada linha cinza representa um ciclo de corte. A linha colorida é a média.",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)",
        color = "Índice"
    ) +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_espaguete_facet)
showtext_opts(dpi = 300)
ggsave("diagnostico_ciclos_facet.png", plot_espaguete_facet, width = 12, height = 9, dpi = 300)




# GRÁFICO 3: Diagnóstico Espaguete (IBOV vs CDI)
dados_cdi_media <- media_ciclos %>% filter(Indice == "CDI")
dados_ibov_individual <- analise_long %>% filter(Indice == "IBOV")
dados_ibov_media <- media_ciclos %>% filter(Indice == "IBOV")

plot_spaghetti_ibov <- ggplot() +
    geom_line(
        data = dados_ibov_individual,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        data = dados_ibov_media,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8",
        linewidth = 1.5
    ) +
    geom_line(
        data = dados_cdi_media,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico do IBOV em Ciclos de Corte",
        subtitle = "Trajetórias Individuais (cinza) vs. Média do IBOV (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_spaghetti_ibov)
showtext_opts(dpi = 300)
ggsave("diagnostico_ibov_individual.png", plot_spaghetti_ibov, width = 10, height = 9, dpi = 300)




# GRÁFICO 4: Diagnóstico Espaguete (Carteira diversificada vs CDI)
plot_spaghetti_diversificada_com_cdi <- ggplot() +
    geom_line(
        data = analise_long_diversificada,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        data = media_ciclos_diversificada,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8", 
        linewidth = 1.5
    ) +
    geom_line(
        data = dados_cdi_media,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico da Carteira diversificada vs. CDI em Ciclos de Corte",
        subtitle = "Trajetórias Individuais (cinza), Média da Carteira (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_spaghetti_diversificada_com_cdi)
showtext_opts(dpi = 300)
ggsave("diagnostico_diversificada_com_cdi.png", plot_spaghetti_diversificada_com_cdi, width = 10, height = 9, dpi = 300)



##### === Gráficos a partir do Dia 0 até o dia 252 === #####

# GRÁFICO 5: Performance Média dos Ativos (A partir do Dia 0)
plot_fase1_d0 <- ggplot(media_ciclos_d0, aes(x = DiaRelativo, y = RetornoMedio, color = Indice)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Desempenho Médio dos Ativos a Partir do Início do Ciclo de Corte",
        subtitle = paste("Janela de 0 a", dias_depois, "dias úteis após o primeiro corte"),
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado Médio (Base 100)",
        color = "Índice"
    ) +
    scale_color_manual(
        values = c(
            "CDI" = "#252525ff",
            "IBOV" = "#189CD8",
            "IRFM" = "#929292ff",
            "IMAB" = "#4DD3C0"
        )
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_fase1_d0)
showtext_opts(dpi = 300)
ggsave("analise_performance_ciclos_d0.png", plot_fase1_d0, width = 10, height = 9, dpi = 300)




# GRÁFICO 6: Diagnóstico Espaguete (Carteira diversificada vs CDI, a partir do Dia 0)
dados_cdi_media_d0 <- media_ciclos_d0 %>% filter(Indice == "CDI")
plot_spaghetti_diversificada_com_cdi_d0 <- ggplot() +
    geom_line(
        data = analise_long_diversificada_d0,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        data = media_ciclos_diversificada_d0,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8", 
        linewidth = 1.5
    ) +
    geom_line(
        data = dados_cdi_media_d0,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico da Carteira diversificada vs. CDI a Partir do Início do Ciclo de Corte",
        subtitle = "Trajetórias Individuais (cinza), Média da Carteira (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_spaghetti_diversificada_com_cdi_d0)
showtext_opts(dpi = 300)
ggsave("diagnostico_diversificada_com_cdi_d0.png", plot_spaghetti_diversificada_com_cdi_d0, width = 10, height = 9, dpi = 300)




# GRÁFICO 7: Diagnóstico Espaguete (IBOV vs CDI, a partir do Dia 0)
dados_ibov_individual_d0 <- analise_long_d0 %>% filter(Indice == "IBOV")
dados_ibov_media_d0 <- media_ciclos_d0 %>% filter(Indice == "IBOV")

plot_spaghetti_ibov_com_cdi_d0 <- ggplot() +
    geom_line(
        data = dados_ibov_individual_d0,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        data = dados_ibov_media_d0,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8",
        linewidth = 1.5
    ) +
    geom_line(
        data = dados_cdi_media_d0,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico do IBOV vs. CDI a Partir do Início do Ciclo de Corte",
        subtitle = "Trajetórias Individuais (cinza), Média do IBOV (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_spaghetti_ibov_com_cdi_d0)
showtext_opts(dpi = 300)
ggsave("diagnostico_ibov_com_cdi_d0.png", plot_spaghetti_ibov_com_cdi_d0, width = 10, height = 9, dpi = 300)






#### Gráficos a partir do dia 0 até o dia 378 ####

# GRÁFICO 5 (Estendido): Performance Média dos Ativos (A partir do Dia 0)

# Este gráfico usa o dataframe 'media_ciclos_d0_ext'
plot_fase1_d0_ext <- ggplot(media_ciclos_d0_ext, aes(x = DiaRelativo, y = RetornoMedio, color = Indice)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Desempenho Médio dos Ativos a Partir do Início do Ciclo de Corte (Janela Estendida)",
        # Alterado para usar a variável da janela estendida
        subtitle = paste("Janela de 0 a", dias_depois_ext, "dias úteis após o primeiro corte"),
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado Médio (Base 100)",
        color = "Índice"
    ) +
    scale_color_manual(
        # Mantendo sua paleta de cores
        values = c(
            "CDI" = "#252525ff",
            "IBOV" = "#189CD8",
            "IRFM" = "#929292ff",
            "IMAB" = "#4DD3C0"
        )
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

print(plot_fase1_d0_ext)
showtext_opts(dpi = 300)
ggsave("analise_performance_ciclos_d0_ext.png", plot_fase1_d0_ext, width = 10, height = 9, dpi = 300)




# GRÁFICO 6 (Estendido): Diagnóstico Espaguete (Carteira diversificada vs CDI, a partir do Dia 0)

# Criar o benchmark de CDI a partir dos dados estendidos
dados_cdi_media_d0_ext <- media_ciclos_d0_ext %>% filter(Indice == "CDI")

plot_spaghetti_diversificada_com_cdi_d0_ext <- ggplot() +
    geom_line(
        # Usar os dados individuais estendidos
        data = analise_long_diversificada_d0_ext,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        # Usar a média estendida
        data = media_ciclos_diversificada_d0_ext,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8", 
        linewidth = 1.5
    ) +
    geom_line(
        # Usar o benchmark de CDI estendido
        data = dados_cdi_media_d0_ext,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico da Carteira diversificada vs. CDI (Janela Estendida)",
        subtitle = "Trajetórias Individuais (cinza), Média da Carteira (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

print(plot_spaghetti_diversificada_com_cdi_d0_ext)
showtext_opts(dpi = 300)
ggsave("diagnostico_diversificada_com_cdi_d0_ext.png", plot_spaghetti_diversificada_com_cdi_d0_ext, width = 10, height = 9, dpi = 300)




# GRÁFICO 7 (Estendido): Diagnóstico Espaguete (IBOV vs CDI, a partir do Dia 0)

# Filtrar dados do IBOV a partir dos dataframes estendidos
dados_ibov_individual_d0_ext <- analise_long_d0_ext %>% filter(Indice == "IBOV")
dados_ibov_media_d0_ext <- media_ciclos_d0_ext %>% filter(Indice == "IBOV")
# O objeto 'dados_cdi_media_d0_ext' já foi criado no bloco anterior

plot_spaghetti_ibov_com_cdi_d0_ext <- ggplot() +
    geom_line(
        # Usar os dados individuais estendidos
        data = dados_ibov_individual_d0_ext,
        aes(x = DiaRelativo, y = RetornoAcumulado, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        # Usar a média estendida
        data = dados_ibov_media_d0_ext,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "#189cd8", # Mantendo a cor azul do seu exemplo
        linewidth = 1.5
    ) +
    geom_line(
        # Usar o benchmark de CDI estendido
        data = dados_cdi_media_d0_ext,
        aes(x = DiaRelativo, y = RetornoMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico do IBOV vs. CDI (Janela Estendida)",
        subtitle = "Trajetórias Individuais (cinza), Média do IBOV (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Após o Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

print(plot_spaghetti_ibov_com_cdi_d0_ext)
showtext_opts(dpi = 300)
ggsave("diagnostico_ibov_com_cdi_d0_ext.png", plot_spaghetti_ibov_com_cdi_d0_ext, width = 10, height = 9, dpi = 300)




#### GRÁFICOS FINAIS: ESTRATÉGIA cautelosa (CDI ANTES DO D0 E diversificada APÓS O D0) ####

# GRÁFICO FINAL: Diagnóstico Espaguete da Estratégia cautelosa
plot_spaghetti_hibrido <- ggplot() +
    geom_line(
        data = analise_cautelosa,
        aes(x = DiaRelativo, y = PatrimonioHibrido, group = Ciclo),
        color = "gray70",
        alpha = 0.6
    ) +
    geom_line(
        data = media_cautelosa,
        aes(x = DiaRelativo, y = PatrimonioMedio),
        color = "#189cd8", 
        linewidth = 1.5
    ) +
    geom_line(
        data = media_benchmark_cdi,
        aes(x = DiaRelativo, y = PatrimonioMedio),
        color = "red",
        linetype = "dashed",
        linewidth = 1.0
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    labs(
        title = "Diagnóstico da Estratégia cautelosa (CDI->diversificada) vs. CDI",
        subtitle = "Trajetórias Individuais (cinza), Média da Estratégia (azul) e Média do CDI (vermelho tracejado)",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Retorno Acumulado (Base 100)"
    ) +
    theme_minimal(base_family = "plusjakarta") +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
print(plot_spaghetti_hibrido)
showtext_opts(dpi = 300)
ggsave("diagnostico_estrategia_cautelosa.png", plot_spaghetti_hibrido, width = 10, height = 9, dpi = 300)



# GRÁFICO 8: Simulação da Estratégia cautelosa
plot_hibrido <- ggplot(dados_plot_hibrido, aes(x = DiaRelativo, y = PatrimonioMedio, color = Estrategia)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", x = 5, y = min(dados_plot_hibrido$PatrimonioMedio), label = "Início do Corte / Troca", hjust = 0, size = 3) + # Ajustado size para melhor visualização
    labs(
        title = "Simulação de Estratégia cautelosa em Ciclos de Corte",
        subtitle = "Performance da troca de CDI para Carteira diversificada no Dia 0",
        x = "Dias Úteis Relativos ao Primeiro Corte (Dia 0)",
        y = "Patrimônio Médio Acumulado (Base 100)",
        color = "Estratégia"
    ) +
    scale_color_manual(values = c(
        "cautelosa (CDI->diversificada)" = "black",
        "CDI Puro" = "red",
        "diversificada Pura" = "#189CD8" # Cor azul para a diversificada Pura
    )) +
    theme_minimal(base_family = "plusjakarta", base_size = 14) +
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

# 7. Exibir o gráfico e salvar o arquivo
print(plot_hibrido)
showtext_opts(dpi = 300)
ggsave("simulacao_estrategia_cautelosa.png", plot_hibrido, width = 10, height = 9, dpi = 300)





### ======================================================================= ###
### EXPORTAÇÃO DOS RESULTADOS FINAIS PARA EXCEL (PONTOS PERCENTUAIS E MÉDIA) ###
### ======================================================================= ###


# Função para adicionar a linha de média em um dataframe
adicionar_media <- function(df) {
  df %>%
    # Adiciona uma nova linha com as médias de cada coluna numérica
    bind_rows(
      summarise(df,
                # A primeira coluna (Ciclo) recebe o texto "Média"
                across(where(is.Date), ~ as.Date(NA)), # Manter a coluna Date vazia
                across(where(is.character), ~ "Média"),
                # Calcula a média para todas as outras colunas (que são numéricas)
                across(where(is.numeric), ~ mean(., na.rm = TRUE)))
    )
}

# --- 1 a 7: Preparação das planilhas individuais (código inalterado) ---

# 1. Preparar dados da análise de janela completa (Ativos Originais)
final_ativos_janela_completa_pp <- analise_long %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, Indice, RetornoPP) %>%
    pivot_wider(names_from = Indice, values_from = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 2. Preparar dados da análise de janela completa (Carteira diversificada)
final_diversificada_janela_completa_pp <- analise_long_diversificada %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, RetornoPP) %>%
    rename(Retorno_diversificada_PP = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 3. Preparar dados da análise a partir do Dia 0 (Ativos Originais - 252 dias)
final_ativos_dia_0_pp <- analise_long_d0 %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, Indice, RetornoPP) %>%
    pivot_wider(names_from = Indice, values_from = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 4. Preparar dados da análise a partir do Dia 0 (Carteira diversificada - 252 dias)
final_diversificada_dia_0_pp <- analise_long_diversificada_d0 %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, RetornoPP) %>%
    rename(Retorno_diversificada_D0_PP = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 5. Preparar dados da análise cautelosa (CDI -> diversificada)
final_cautelosa_janela_completa_pp <- analise_cautelosa %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = PatrimonioHibrido - 100) %>%
    select(Ciclo, RetornoPP) %>%
    rename(Retorno_cautelosa_PP = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 6. Preparar dados da análise ESTENDIDA a partir do Dia 0 (Ativos Originais - 378 dias)
final_ativos_dia_0_ext_pp <- analise_long_d0_ext %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, Indice, RetornoPP) %>%
    pivot_wider(names_from = Indice, values_from = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# 7. Preparar dados da análise ESTENDIDA a partir do Dia 0 (Carteira diversificada - 378 dias)
final_diversificada_dia_0_ext_pp <- analise_long_diversificada_d0_ext %>%
    filter(DiaRelativo == max(DiaRelativo)) %>%
    mutate(RetornoPP = RetornoAcumulado - 100) %>%
    select(Ciclo, RetornoPP) %>%
    rename(Retorno_diversificada_D0_Ext_PP = RetornoPP) %>%
    arrange(Ciclo) %>%
    adicionar_media()

# --- 8. NOVO BLOCO: CRIAÇÃO DA TABELA CONSOLIDADA (CORRIGIDO) ---
# 8.1 Extrair médias da Janela Completa
media_jc_ativos <- final_ativos_janela_completa_pp %>% 
    filter(is.na(Ciclo)) %>% # CORREÇÃO: Trocado 'Ciclo == "Média"' por 'is.na(Ciclo)'
    pivot_longer(cols = -Ciclo, names_to = "Ativo", values_to = "Janela_Completa_PP") %>%
    select(Ativo, Janela_Completa_PP)
media_jc_diversificada <- final_diversificada_janela_completa_pp %>% 
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    transmute(Ativo = "diversificada", Janela_Completa_PP = Retorno_diversificada_PP)
media_jc_cautelosa <- final_cautelosa_janela_completa_pp %>%
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    transmute(Ativo = "cautelosa", Janela_Completa_PP = Retorno_cautelosa_PP)
df_janela_completa <- bind_rows(media_jc_ativos, media_jc_diversificada, media_jc_cautelosa)

# 8.2 Extrair médias da Janela Dia 0 (252 dias)
media_d0_ativos <- final_ativos_dia_0_pp %>%
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    pivot_longer(cols = -Ciclo, names_to = "Ativo", values_to = "Janela_D0_252d_PP") %>%
    select(Ativo, Janela_D0_252d_PP)
media_d0_diversificada <- final_diversificada_dia_0_pp %>%
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    transmute(Ativo = "diversificada", Janela_D0_252d_PP = Retorno_diversificada_D0_PP)
df_janela_d0 <- bind_rows(media_d0_ativos, media_d0_diversificada)

# 8.3 Extrair médias da Janela Dia 0 Estendida (378 dias)
media_d0_ext_ativos <- final_ativos_dia_0_ext_pp %>%
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    pivot_longer(cols = -Ciclo, names_to = "Ativo", values_to = "Janela_D0_378d_PP") %>%
    select(Ativo, Janela_D0_378d_PP)
media_d0_ext_diversificada <- final_diversificada_dia_0_ext_pp %>%
    filter(is.na(Ciclo)) %>% # CORREÇÃO
    transmute(Ativo = "diversificada", Janela_D0_378d_PP = Retorno_diversificada_D0_Ext_PP)
df_janela_d0_ext <- bind_rows(media_d0_ext_ativos, media_d0_ext_diversificada)

# 8.4 Juntar tudo em uma tabela consolidada
tabela_consolidada <- full_join(df_janela_completa, df_janela_d0, by = "Ativo") %>%
    full_join(df_janela_d0_ext, by = "Ativo")

# 9. Agrupar todos os dataframes em uma lista nomeada (bloco atualizado)
lista_para_excel_pp <- list(
    "Consolidado_Medias_PP" = tabela_consolidada, # NOVA ABA CONSOLIDADA
    "Ativos_Janela_Completa_PP" = final_ativos_janela_completa_pp,
    "diversificada_Janela_Completa_PP" = final_diversificada_janela_completa_pp,
    "cautelosa_Janela_Completa_PP" = final_cautelosa_janela_completa_pp,
    "Ativos_a_Partir_Dia_0_PP" = final_ativos_dia_0_pp,
    "diversificada_a_Partir_Dia_0_PP" = final_diversificada_dia_0_pp,
    "Ativos_D0_Estendido_PP" = final_ativos_dia_0_ext_pp,
    "diversificada_D0_Estendido_PP" = final_diversificada_dia_0_ext_pp
)

# 10. Escrever a lista para um novo arquivo .xlsx
write_xlsx(lista_para_excel_pp, "resultados_retornos_percentuais_final.xlsx")

cat("\nArquivo 'resultados_retornos_percentuais_final.xlsx' gerado com sucesso (com aba consolidada).\n")