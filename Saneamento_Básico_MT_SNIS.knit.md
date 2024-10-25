---  
title: "Relatório de Saneamento Básico"
author: "Camily Nunes dos Santos"  
output: 
  html_document:
    toc: true               # Ativa o índice
    toc_float: 
      collapsed: false      # Expande o índice automaticamente
      smooth_scroll: true   # Rolagem suave ao clicar no índice
      position: "left"      # Posição
    theme: cosmo            # Tema Bootstrap para o HTML
    highlight: tango         # Destacar a sintaxe em R
    css: "estilo.css"       # Arquivo CSS personalizado
---    

<!-- Logotipo posicionado no canto superior esquerdo -->
<div class="logo-container">
  <img src="imagens_logo_ufr.png" alt="Logotipo UFR">
</div> 

# Objetivo Final

O objetivo deste relatório é explorar e analisar os dados do Sistema Nacional de Informação sobre Saneamento (SNIS) para entender a situação do saneamento básico em Mato Grosso. Especificamente, buscamos:

-   **Avaliar a cobertura de serviços de água e esgoto**, analisando a evolução desses indicadores ao longo dos anos.
-   **Identificar desigualdades regionais** no acesso a saneamento básico e suas implicações para a saúde pública.
-   **Utilizar visualizações** e métodos estatísticos para apresentar os dados de maneira clara e acessível, facilitando a interpretação dos resultados.
-   **Contribuir para a formulação de políticas públicas** eficazes, fundamentadas em evidências, que promovam melhorias no setor de saneamento.

Com isso, esperamos que este relatório sirva como uma ferramenta útil para gestores públicos, pesquisadores e membros da sociedade civil, incentivando um debate informado sobre o tema.

## 1. Preparar o Ambiente de Trabalho

Criar uma pasta de trabalho: O código começa configurando o diretório de trabalho, onde estarão os arquivos de dados e onde os resultados serão salvos.

**Dica:** Vá para **Session \> Set Working Directory \> Choose Directory**.

## 2. Instalar e Carregar Pacotes

### 2.1 Instalar Pacotes (se necessário):

Caso os pacotes ainda não estejam instalados, você pode instalar com os comandos abaixo:  
  

``` r
# Lista de pacotes necessários
pacotes <- c("ggplot2", "dplyr", "tidyr", "plotly", "readr", "zoo")
```


``` r
# Instalar os pacotes que ainda não estão instalados
pacotes_nao_instalados <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(pacotes_nao_instalados)) install.packages(pacotes_nao_instalados)
```

### 2.2 Carregar os Pacotes


``` r
invisible(lapply(pacotes, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    stop(paste("Falha ao carregar o pacote:", pkg))
  }
}))
```

```
## Carregando pacotes exigidos: ggplot2
```

```
## Carregando pacotes exigidos: dplyr
```

```
## 
## Anexando pacote: 'dplyr'
```

```
## Os seguintes objetos são mascarados por 'package:stats':
## 
##     filter, lag
```

```
## Os seguintes objetos são mascarados por 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Carregando pacotes exigidos: tidyr
```

```
## Carregando pacotes exigidos: plotly
```

```
## 
## Anexando pacote: 'plotly'
```

```
## O seguinte objeto é mascarado por 'package:ggplot2':
## 
##     last_plot
```

```
## O seguinte objeto é mascarado por 'package:stats':
## 
##     filter
```

```
## O seguinte objeto é mascarado por 'package:graphics':
## 
##     layout
```

```
## Carregando pacotes exigidos: readr
```

```
## Carregando pacotes exigidos: zoo
```

```
## 
## Anexando pacote: 'zoo'
```

```
## Os seguintes objetos são mascarados por 'package:base':
## 
##     as.Date, as.Date.numeric
```

## 3. Carregar os Dados


``` r
url <- "https://github.com/CamilyNunes/produto_snis_mt/raw/refs/heads/main/Dados/SNIS%20-%20S%C3%A9rie%20Hist%C3%B3rica/SNISMTAgregado.csv"  

san_mt <- read_delim(url, delim = ";", locale = locale(encoding = "ISO-8859-1"))
```

```
## Rows: 517 Columns: 229
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ";"
## chr  (59): Município, Estado, Prestador, Sigla do Prestador, Abrangência, Ti...
## dbl (165): Código do Município, Ano de Referência, Código do Prestador, G05A...
## lgl   (5): GE025 - Quantidade de municípios não atendidos com abastecimento ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## 4. Desabilitar Notação Científica

Para garantir que os números não sejam representados em notação científica:


``` r
options(scipen = 999) 
```

## 5. Tratar os Dados do SNIS

Selecionar colunas de interesse:


``` r
# selecionar variaveis de referencia
san <- san_mt[,c(1:10)]
```


``` r
san$pop_atendida_abas_agua <- san_mt$`AG001 - População total atendida com abastecimento de água`

san$consumo_agua_percapita <- san_mt$`IN022 - Consumo médio percapita de água`

san$índice_coleta_esgoto <- san_mt$`IN015 - Índice de coleta de esgoto`

san$índice_tratamento_esgoto_percentual <- san_mt$`IN016 - Índice de tratamento de esgoto`

san$volume_esgoto_coletado_m3 <- san_mt$`ES005 - Volume de esgotos coletado`
```

## 6. Gráfico de Mapa de Valores Ausentes (Missing Values)


``` r
# 1. Criar uma função que calcula a proporção de valores omissos (NA) por coluna
calc_missing_proportion <- function(df) {
  missing_data <- sapply(df, function(col) {
    mean(is.na(col))
  })
  
  missing_df <- data.frame(
    Atributo = names(missing_data),
    ProporcaoOmissos = missing_data)
  
  missing_df <- missing_df %>%
    filter(ProporcaoOmissos > 0)
  
  return(missing_df)
}

# 2. Calcular a proporção de valores omissos no conjunto de dados 'san'
missing_proportions <- calc_missing_proportion(san)

# 3. Preparar os dados para a plotagem em forma de gráfico de barra empilhada
plot_data <- missing_proportions %>%
  mutate(Presentes = 1 - ProporcaoOmissos) %>%
  pivot_longer(cols = c(ProporcaoOmissos, Presentes), 
               names_to = "Status", 
               values_to = "Proporcao")

# 4. Definir as cores de acordo com seu Tema dos Gráficos
cor_omisso <- "#FF6347"
cor_presente <- "#006400"

# 5. Criar o gráfico de coluna empilhada em 100% e inverter os eixos
gg <- ggplot(plot_data, aes(x = Proporcao, 
                             y = Atributo, 
                             fill = Status)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Proporção de Valores Omissos por Atributo",
    x = "Proporção (%)",
    y = "Atributos") +
  scale_fill_manual(values = c("ProporcaoOmissos" = cor_omisso, "Presentes" = cor_presente), 
                    labels = c("Valores Presentes", "Valores Omissos")) +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 1.5, face = "bold", color = "black"),
    axis.title.y = element_text(size = 1.5, face = "bold", color = "black", angle = 90),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank())

# 6. Converter o gráfico ggplot em um gráfico interativo com plotly
interactive_plot <- ggplotly(gg)

# 7. Mostrar o gráfico interativo
interactive_plot
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-19093257594f1213ac80" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-19093257594f1213ac80">{"x":{"data":[{"orientation":"v","width":[0.99806576402321079,0.97678916827853002,0.95164410058027082,0.95938104448742745,0.96518375241779497],"base":[3.5499999999999998,0.55000000000000004,1.55,2.5499999999999998,4.5499999999999998],"x":[0.50096711798839455,0.51160541586073505,0.52417794970986464,0.52030947775628622,0.51740812379110257],"y":[0.90000000000000036,0.89999999999999991,0.90000000000000013,0.90000000000000036,0.90000000000000036],"text":["Proporcao: 0.998065764<br />Atributo: 0.9<br />Status: Presentes","Proporcao: 0.976789168<br />Atributo: 0.9<br />Status: Presentes","Proporcao: 0.951644101<br />Atributo: 0.9<br />Status: Presentes","Proporcao: 0.959381044<br />Atributo: 0.9<br />Status: Presentes","Proporcao: 0.965183752<br />Atributo: 0.9<br />Status: Presentes"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,100,0,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Presentes","legendgroup":"Presentes","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.0019342359767891683,0.02321083172147002,0.048355899419729204,0.040618955512572531,0.034816247582205029],"base":[3.5499999999999998,0.55000000000000004,1.55,2.5499999999999998,4.5499999999999998],"x":[0.00096711798839458415,0.01160541586073501,0.024177949709864602,0.020309477756286266,0.017408123791102514],"y":[0.90000000000000036,0.89999999999999991,0.90000000000000013,0.90000000000000036,0.90000000000000036],"text":["Proporcao: 0.001934236<br />Atributo: 0.9<br />Status: ProporcaoOmissos","Proporcao: 0.023210832<br />Atributo: 0.9<br />Status: ProporcaoOmissos","Proporcao: 0.048355899<br />Atributo: 0.9<br />Status: ProporcaoOmissos","Proporcao: 0.040618956<br />Atributo: 0.9<br />Status: ProporcaoOmissos","Proporcao: 0.034816248<br />Atributo: 0.9<br />Status: ProporcaoOmissos"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,99,71,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"ProporcaoOmissos","legendgroup":"ProporcaoOmissos","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":27.563304275633048,"l":217.51764217517649},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Proporção de Valores Omissos por Atributo <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,1.05],"tickmode":"array","ticktext":["0%","25%","50%","75%","100%"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0%","25%","50%","75%","100%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Proporção (%) <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":1.9925280199252804}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,5.5999999999999996],"tickmode":"array","ticktext":["consumo_agua_percapita","índice_coleta_esgoto","índice_tratamento_esgoto_percentual","pop_atendida_abas_agua","volume_esgoto_coletado_m3"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["consumo_agua_percapita","índice_coleta_esgoto","índice_tratamento_esgoto_percentual","pop_atendida_abas_agua","volume_esgoto_coletado_m3"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Atributos <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":1.9925280199252804}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"Status","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c50df3b10":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"2c5c50df3b10","visdat":{"2c5c50df3b10":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
  
## 7. Aplicação do Método de Interpolação   

  A interpolação é uma técnica usada para estimar valores desconhecidos dentro de um intervalo de dados conhecidos. Em séries temporais, a interpolação nos permite preencher valores ausentes com base nos valores antes e depois do ponto em falta, criando uma "ponte" entre os dados disponíveis. Isso é especialmente útil para manter a consistência dos dados e evitar problemas que poderiam surgir ao usar conjuntos de dados com muitas lacunas.

    

``` r
# Aplicar interpolação linear nas variáveis numéricas
san <- san %>%
  mutate(across(where(is.numeric), ~ na.approx(.)))
```
   
# Evolução Temporal do Indicador no Estado de MT

## 1. Extração da Variável de Interesse

A variável 'datavar' contém o consumo per capita de água em litros por habitante por dia.


``` r
datavar = san$consumo_agua_percapita
```

## 2. Armazenamento do Dataset Completo

A variável 'data' recebe o dataset 'san' completo. \# Isso permite o uso de todos os dados na análise subsequente.


``` r
data = san
```

## 3. Definição da Legenda para Gráficos

Define a legenda para as visualizações que utilizam a variável 'datavar'. A legenda indica que a variável representa o "Consumo Percapita de Água" em litros por habitante por dia.


``` r
legendavar = 'Consumo Percapita de Água (1/hab/dia)'
```


``` r
chave = 'media'
```

## 4. Funções de Suporte para Gráficos


``` r
# Função para limpar strings, removendo espaços, caracteres especiais e acentos
cleanStr <- function(string) {
  
  # Carregar pacotes necessários apenas uma vez
  if (!require(stringi)) install.packages("stringi", quietly = TRUE)
  if (!require(stringr)) install.packages("stringr", quietly = TRUE)
  
  # Limpar e normalizar a string
  string <- str_replace_all(string, ' ', '_')
  string <- str_replace_all(string, '-', '_')
  string <- str_replace_all(string, '%', '')
  string <- str_replace_all(string, '/', '_')
  string <- stri_trans_general(string, "latin-ascii")
  string <- tolower(string)
  
  return(string)
}
```

## 5. Definir Tema Personalizado para Gráficos (ggplot2)


``` r
#Definindo gráfico
tema_plot <- function(dataTime, legendavar, chave) {
  
  # Criar gráfico básico de linha com o tema minimalista
  plot <- ggplot(data = dataTime, aes(x = tempo, y = x, group = 1)) +
            geom_line(color = "#006400", size = 1.2) +  # Linha na cor verde-escura
            geom_label(aes(label = label), size = 3, color = "#006400") +  # Rótulos em verde-escuro
            labs(
              title = "Evolução Temporal do Indicador no Estado de MT",
              subtitle = legendavar,
              x = "Ano",  # Legenda para o eixo X
              y = legendavar  # Legenda para o eixo Y
            ) +
            scale_x_continuous(
            breaks = seq(from = min(dataTime$tempo), to = max(dataTime$tempo), by = 1),
              limits = c(min(dataTime$tempo), max(dataTime$tempo)),
              expand = c(0, 0)  # Remover margens extras
              ) +
            scale_color_distiller(palette = "Greens", direction = 1) +  # Paleta de cores Greens
            theme_minimal() %+replace%  # Manter um tema minimalista com os eixos
            theme(
              axis.title.x = element_text(size = 10, face = "bold", color = "black"),  # Legenda do eixo X
              axis.title.y = element_text(size = 10, face = "bold", color = "black", angle = 90),  # Legenda Y na vertical
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Texto do eixo X com rotação
              axis.text.y = element_text(size = 10),  # Texto do eixo Y
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Título centralizado
              plot.subtitle = element_text(size = 12, hjust = 0.5),  # Subtítulo centralizado
              panel.grid.major = element_line(color = "grey85"),  # Linhas de grid principais
              panel.grid.minor = element_blank(),  # Remover grid secundário
              legend.position = "none"  # Sem legenda adicional
            )
  
  # Agregar escala Y caso solicitado
  if (chave == 'agregar') {
    plot <- plot + scale_y_continuous(limits = range(dataTime$x, na.rm = TRUE))
  }
  
  # Salvar o gráfico em um arquivo PNG
  ggsave(
    filename = paste0('tempo_', cleanStr(legendavar), '.png'), 
    plot = plot, 
    path = 'resultados',
    width = 8,
    height = 4,
    units = 'in')
  
  # Converta o gráfico ggplot em um gráfico interativo
  plot_interativo <- ggplotly(plot)
  
  return(plot_interativo)
}
```


``` r
dev01 <- function(data, datavar, chave, legendavar) {
  
  if (chave == 'agregar') {
    # agregar valores
    dataTime = aggregate(datavar, by = list(tempo = as.numeric(data$`Ano de Referência`)), FUN = sum)
  } else if (chave == 'media') {
    # tirar media valores
    dataTime = aggregate(datavar, by = list(tempo = as.numeric(data$`Ano de Referência`)), FUN = mean)
    dataTime$x = round(dataTime$x, 2)
  } else {
    stop("Chave não encontrada! Tente 'agregar' ou 'media'.")
  }
  
  # label
  dataTime$label <- NA
  n <- min(5, nrow(dataTime)) #Seleciona no máximo 5 pontos
  idx <- seq(1, nrow(dataTime), length.out = n)
  dataTime$label[idx] <- dataTime$x[idx]
  
  # A coluna 'x' precisa ser definida aqui
  colnames(dataTime)[2] <- "x"  # Renomeia a coluna para 'x'
  
  # gráfico
  plot = tema_plot(dataTime, legendavar, chave)
  return(plot)
}
```

## 6. Processar Dados e Gerar Gráficos


``` r
# executar funcao
dev01(san, san$pop_atendida_abas_agua, 'media','Pop. com Abastecimento de Água')
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Carregando pacotes exigidos: stringi
```

```
## Carregando pacotes exigidos: stringr
```

```
## Warning: Removed 19 rows containing missing values or values outside the scale range
## (`geom_label()`).
```

```
## Warning in geom2trace.default(dots[[1L]][[1L]], dots[[2L]][[1L]], dots[[3L]][[1L]]): geom_GeomLabel() has yet to be implemented in plotly.
##   If you'd like to see this geom implemented,
##   Please open an issue with your example code at
##   https://github.com/ropensci/plotly/issues
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-c6b40f0b12851fbc3a73" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-c6b40f0b12851fbc3a73">{"x":{"data":[{"x":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[148.19,262.85000000000002,111.44,110.70999999999999,124.52,89.879999999999995,88.540000000000006,76,83.409999999999997,77.340000000000003,66.549999999999997,102.91,104.42,96.25,114.06,90.939999999999998,66.599999999999994,65.540000000000006,63.439999999999998,65.439999999999998,69.140000000000001,79.489999999999995,66.849999999999994,61.490000000000002],"text":["tempo: 1999<br />x: 148.19","tempo: 2000<br />x: 262.85","tempo: 2001<br />x: 111.44","tempo: 2002<br />x: 110.71","tempo: 2003<br />x: 124.52","tempo: 2004<br />x:  89.88","tempo: 2005<br />x:  88.54","tempo: 2006<br />x:  76.00","tempo: 2007<br />x:  83.41","tempo: 2008<br />x:  77.34","tempo: 2009<br />x:  66.55","tempo: 2010<br />x: 102.91","tempo: 2011<br />x: 104.42","tempo: 2012<br />x:  96.25","tempo: 2013<br />x: 114.06","tempo: 2014<br />x:  90.94","tempo: 2015<br />x:  66.60","tempo: 2016<br />x:  65.54","tempo: 2017<br />x:  63.44","tempo: 2018<br />x:  65.44","tempo: 2019<br />x:  69.14","tempo: 2020<br />x:  79.49","tempo: 2021<br />x:  66.85","tempo: 2022<br />x:  61.49"],"type":"scatter","mode":"lines","line":{"width":4.5354330708661417,"color":"rgba(0,100,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":42.768361007180772,"l":44.167704441677053},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Evolução Temporal do Indicador no Estado de MT <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1999,2022],"tickmode":"array","ticktext":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"tickvals":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"categoryorder":"array","categoryarray":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835198},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Ano <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[51.421999999999997,272.91800000000001],"tickmode":"array","ticktext":["100","150","200","250"],"tickvals":[100,150,200,250],"categoryorder":"array","categoryarray":["100","150","200","250"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835205},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Pop. com Abastecimento de Água <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c76ef711a":{"x":{},"y":{},"type":"scatter"},"2c5c717f6fd2":{"x":{},"y":{},"label":{}}},"cur_data":"2c5c76ef711a","visdat":{"2c5c76ef711a":["function (y) ","x"],"2c5c717f6fd2":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
dev01(san, san$consumo_agua_percapita, 'agregar','Consumo Percapita de Água (l-hab-dia)')
```

```
## Warning: Removed 19 rows containing missing values or values outside the scale range
## (`geom_label()`).
## geom_GeomLabel() has yet to be implemented in plotly.
##   If you'd like to see this geom implemented,
##   Please open an issue with your example code at
##   https://github.com/ropensci/plotly/issues
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-e91a080587b3416ed613" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-e91a080587b3416ed613">{"x":{"data":[{"x":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[114,407.28571428571428,1142.5714285714287,1348.1904761904761,1208.8095238095239,2217.4285714285716,2068.7142857142858,2382,2103,2824,3948.5,4875,4674,4861,3863,5678,5112,5732,5531,5505,5106,6684,6053,6693],"text":["tempo: 1999<br />x:  114.0000","tempo: 2000<br />x:  407.2857","tempo: 2001<br />x: 1142.5714","tempo: 2002<br />x: 1348.1905","tempo: 2003<br />x: 1208.8095","tempo: 2004<br />x: 2217.4286","tempo: 2005<br />x: 2068.7143","tempo: 2006<br />x: 2382.0000","tempo: 2007<br />x: 2103.0000","tempo: 2008<br />x: 2824.0000","tempo: 2009<br />x: 3948.5000","tempo: 2010<br />x: 4875.0000","tempo: 2011<br />x: 4674.0000","tempo: 2012<br />x: 4861.0000","tempo: 2013<br />x: 3863.0000","tempo: 2014<br />x: 5678.0000","tempo: 2015<br />x: 5112.0000","tempo: 2016<br />x: 5732.0000","tempo: 2017<br />x: 5531.0000","tempo: 2018<br />x: 5505.0000","tempo: 2019<br />x: 5106.0000","tempo: 2020<br />x: 6684.0000","tempo: 2021<br />x: 6053.0000","tempo: 2022<br />x: 6693.0000"],"type":"scatter","mode":"lines","line":{"width":4.5354330708661417,"color":"rgba(0,100,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":42.768361007180772,"l":50.809464508094663},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Evolução Temporal do Indicador no Estado de MT <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1999,2022],"tickmode":"array","ticktext":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"tickvals":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"categoryorder":"array","categoryarray":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835198},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Ano <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-214.95000000000005,7021.9499999999998],"tickmode":"array","ticktext":["0","2000","4000","6000"],"tickvals":[0,1999.9999999999998,4000,6000],"categoryorder":"array","categoryarray":["0","2000","4000","6000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835205},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Consumo Percapita de Água (l-hab-dia) <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c60583520":{"x":{},"y":{},"type":"scatter"},"2c5c1cd01f5a":{"x":{},"y":{},"label":{}}},"cur_data":"2c5c60583520","visdat":{"2c5c60583520":["function (y) ","x"],"2c5c1cd01f5a":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
dev01(san, san$índice_coleta_esgoto, 'agregar','Índice de Coleta de Esgoto')
```

```
## Warning: Removed 19 rows containing missing values or values outside the scale range
## (`geom_label()`).
## geom_GeomLabel() has yet to be implemented in plotly.
##   If you'd like to see this geom implemented,
##   Please open an issue with your example code at
##   https://github.com/ropensci/plotly/issues
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-4574e7a2e943bea4617d" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-4574e7a2e943bea4617d">{"x":{"data":[{"x":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[30.5,132,278.83333333333331,281.9666666666667,218.09999999999999,287.39999999999998,266.69999999999999,374,387.5,432,498.83333333333331,550.66666666666663,577,645,575,698,769,934,959,1072,1131,1355,1378,1681],"text":["tempo: 1999<br />x:   30.5000","tempo: 2000<br />x:  132.0000","tempo: 2001<br />x:  278.8333","tempo: 2002<br />x:  281.9667","tempo: 2003<br />x:  218.1000","tempo: 2004<br />x:  287.4000","tempo: 2005<br />x:  266.7000","tempo: 2006<br />x:  374.0000","tempo: 2007<br />x:  387.5000","tempo: 2008<br />x:  432.0000","tempo: 2009<br />x:  498.8333","tempo: 2010<br />x:  550.6667","tempo: 2011<br />x:  577.0000","tempo: 2012<br />x:  645.0000","tempo: 2013<br />x:  575.0000","tempo: 2014<br />x:  698.0000","tempo: 2015<br />x:  769.0000","tempo: 2016<br />x:  934.0000","tempo: 2017<br />x:  959.0000","tempo: 2018<br />x: 1072.0000","tempo: 2019<br />x: 1131.0000","tempo: 2020<br />x: 1355.0000","tempo: 2021<br />x: 1378.0000","tempo: 2022<br />x: 1681.0000"],"type":"scatter","mode":"lines","line":{"width":4.5354330708661417,"color":"rgba(0,100,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":42.768361007180772,"l":50.809464508094663},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Evolução Temporal do Indicador no Estado de MT <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1999,2022],"tickmode":"array","ticktext":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"tickvals":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"categoryorder":"array","categoryarray":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835198},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Ano <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-52.025000000000006,1763.5250000000001],"tickmode":"array","ticktext":["0","500","1000","1500"],"tickvals":[0,500,1000.0000000000001,1500],"categoryorder":"array","categoryarray":["0","500","1000","1500"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835205},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Índice de Coleta de Esgoto <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c5a677a":{"x":{},"y":{},"type":"scatter"},"2c5c5bc03930":{"x":{},"y":{},"label":{}}},"cur_data":"2c5c5a677a","visdat":{"2c5c5a677a":["function (y) ","x"],"2c5c5bc03930":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
dev01(san, san$índice_tratamento_esgoto_percentual, 'agregar','Índice Tratamento de Esgoto')
```

```
## Warning: Removed 19 rows containing missing values or values outside the scale range
## (`geom_label()`).
## geom_GeomLabel() has yet to be implemented in plotly.
##   If you'd like to see this geom implemented,
##   Please open an issue with your example code at
##   https://github.com/ropensci/plotly/issues
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-619270a707def9c413fe" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-619270a707def9c413fe">{"x":{"data":[{"x":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[47,237,739,830,752,1214.5,1184,1458,1394,1481,1796.6666666666667,1839.8333333333333,1745,1883,1396,2285,2205,2319,2930,2947,2789,3373,3003,3283],"text":["tempo: 1999<br />x:   47.000","tempo: 2000<br />x:  237.000","tempo: 2001<br />x:  739.000","tempo: 2002<br />x:  830.000","tempo: 2003<br />x:  752.000","tempo: 2004<br />x: 1214.500","tempo: 2005<br />x: 1184.000","tempo: 2006<br />x: 1458.000","tempo: 2007<br />x: 1394.000","tempo: 2008<br />x: 1481.000","tempo: 2009<br />x: 1796.667","tempo: 2010<br />x: 1839.833","tempo: 2011<br />x: 1745.000","tempo: 2012<br />x: 1883.000","tempo: 2013<br />x: 1396.000","tempo: 2014<br />x: 2285.000","tempo: 2015<br />x: 2205.000","tempo: 2016<br />x: 2319.000","tempo: 2017<br />x: 2930.000","tempo: 2018<br />x: 2947.000","tempo: 2019<br />x: 2789.000","tempo: 2020<br />x: 3373.000","tempo: 2021<br />x: 3003.000","tempo: 2022<br />x: 3283.000"],"type":"scatter","mode":"lines","line":{"width":4.5354330708661417,"color":"rgba(0,100,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":42.768361007180772,"l":50.809464508094663},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Evolução Temporal do Indicador no Estado de MT <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1999,2022],"tickmode":"array","ticktext":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"tickvals":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"categoryorder":"array","categoryarray":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835198},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Ano <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-119.30000000000001,3539.3000000000002],"tickmode":"array","ticktext":["0","1000","2000","3000"],"tickvals":[-1.4210854715202004e-14,1000,2000.0000000000002,3000],"categoryorder":"array","categoryarray":["0","1000","2000","3000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835205},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Índice Tratamento de Esgoto <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c1de16c3":{"x":{},"y":{},"type":"scatter"},"2c5c1c884c8c":{"x":{},"y":{},"label":{}}},"cur_data":"2c5c1de16c3","visdat":{"2c5c1de16c3":["function (y) ","x"],"2c5c1c884c8c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
dev01(san, san$volume_esgoto_coletado_m3, 'media','Volume Esgoto Coletado (m³)')
```

```
## Warning: Removed 19 rows containing missing values or values outside the scale range
## (`geom_label()`).
## geom_GeomLabel() has yet to be implemented in plotly.
##   If you'd like to see this geom implemented,
##   Please open an issue with your example code at
##   https://github.com/ropensci/plotly/issues
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-c3628ad02ee783b8f39d" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-c3628ad02ee783b8f39d">{"x":{"data":[{"x":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"y":[27.239999999999998,299.93000000000001,184.06999999999999,154.02000000000001,102.31999999999999,113.09999999999999,126.73999999999999,155.97,192.49000000000001,224.69,229.27000000000001,193.16999999999999,178.96000000000001,176.56999999999999,202.72,202.56,212.61000000000001,199.38999999999999,258.14999999999998,161.59999999999999,180.77000000000001,147,159.84999999999999,152.38],"text":["tempo: 1999<br />x:  27.24","tempo: 2000<br />x: 299.93","tempo: 2001<br />x: 184.07","tempo: 2002<br />x: 154.02","tempo: 2003<br />x: 102.32","tempo: 2004<br />x: 113.10","tempo: 2005<br />x: 126.74","tempo: 2006<br />x: 155.97","tempo: 2007<br />x: 192.49","tempo: 2008<br />x: 224.69","tempo: 2009<br />x: 229.27","tempo: 2010<br />x: 193.17","tempo: 2011<br />x: 178.96","tempo: 2012<br />x: 176.57","tempo: 2013<br />x: 202.72","tempo: 2014<br />x: 202.56","tempo: 2015<br />x: 212.61","tempo: 2016<br />x: 199.39","tempo: 2017<br />x: 258.15","tempo: 2018<br />x: 161.60","tempo: 2019<br />x: 180.77","tempo: 2020<br />x: 147.00","tempo: 2021<br />x: 159.85","tempo: 2022<br />x: 152.38"],"type":"scatter","mode":"lines","line":{"width":4.5354330708661417,"color":"rgba(0,100,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":44.825238688252384,"r":7.3059360730593621,"b":42.768361007180772,"l":44.167704441677053},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"<b> Evolução Temporal do Indicador no Estado de MT <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1999,2022],"tickmode":"array","ticktext":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"tickvals":[1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022],"categoryorder":"array","categoryarray":["1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835198},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"<b> Ano <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[13.605499999999997,313.56450000000001],"tickmode":"array","ticktext":["100","200","300"],"tickvals":[100,200.00000000000003,300],"categoryorder":"array","categoryarray":["100","200","300"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.283520132835205},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(217,217,217,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"<b> Volume Esgoto Coletado (m³) <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835198}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2c5c3bb317ae":{"x":{},"y":{},"type":"scatter"},"2c5c5dab5017":{"x":{},"y":{},"label":{}}},"cur_data":"2c5c3bb317ae","visdat":{"2c5c3bb317ae":["function (y) ","x"],"2c5c5dab5017":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
     
       
       
