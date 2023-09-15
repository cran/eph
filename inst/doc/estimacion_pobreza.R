## ---- echo = FALSE------------------------------------------------------------
LOCAL <- TRUE
# identical(Sys.getenv("LOCAL"), "TRUE")
knitr::opts_chunk$set(
  purl = LOCAL,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=LOCAL,include=FALSE------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, results = "hide")

## ----results='hide',warning=FALSE---------------------------------------------
library(eph)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(forcats)

## ---- eval=LOCAL--------------------------------------------------------------
canastas_regionales <- get_poverty_lines(regional = TRUE)
bases <- get_microdata(
  year = 2016:2019,
  period = 1:4,
  type = "individual",
  vars = c("ANO4", "TRIMESTRE", "REGION", "CODUSU", "NRO_HOGAR", "CH04", "CH06", "ITF", "PONDIH", "PP07H", "PP04D_COD")
  # ,destfile = 'bases_eph.rds'
)

## ---- eval=LOCAL--------------------------------------------------------------
# bases <- bases %>% unnest(cols = c(microdata))
bases_pobreza <- calculate_poverty(bases, canastas_regionales, print_summary = TRUE)
bases_pobreza

## ---- eval=LOCAL--------------------------------------------------------------
pobreza_oficial <- read_csv("https://raw.githubusercontent.com/holatam/data/master/eph/canasta/pobreza_oficial.csv")
pobreza_oficial <- pobreza_oficial %>%
  mutate(periodo = parse_date_time(paste0(ANO4, "-", SEMESTRE * 2), "Y.q")) %>%
  select(periodo, pobreza_oficial = tasa_pobreza, indigencia_oficial = tasa_indigencia)
pobreza_oficial

## ---- eval=LOCAL--------------------------------------------------------------
Pobreza_resumen <- bases_pobreza %>%
  group_by(ANO4, TRIMESTRE) %>%
  summarise(
    Tasa_pobreza = sum(PONDIH[situacion %in% c("pobre", "indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    Tasa_indigencia = sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(periodo = parse_date_time(paste0(ANO4, "-", TRIMESTRE), "Y.q")) %>%
  select(periodo, pobreza_estimada = Tasa_pobreza, indigencia_estimada = Tasa_indigencia)

Pobreza_resumen <- Pobreza_resumen %>%
  left_join(pobreza_oficial, by = "periodo") %>%
  pivot_longer(cols = pobreza_estimada:indigencia_oficial, names_to = c("tipo", "grupo"), values_to = "valor", names_sep = "_", values_drop_na = TRUE)

## ---- fig.height=5, fig.width=7, eval=LOCAL-----------------------------------
Pobreza_resumen <- Pobreza_resumen %>%
  group_by(tipo, grupo) %>%
  mutate(
    x = lag(periodo),
    y = lag(valor)
  )

ggplot(Pobreza_resumen, aes(periodo, valor, color = grupo)) +
  geom_step(data = Pobreza_resumen %>% filter(grupo == "estimada"), linetype = "dashed") +
  geom_segment(
    data = Pobreza_resumen %>% filter(grupo == "estimada"),
    aes(x = x, y = y, xend = periodo, yend = y), size = 1
  ) +
  geom_point(data = Pobreza_resumen %>% filter(grupo == "oficial"), size = 3) +
  facet_wrap(tipo ~ ., scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- fig.height=5, fig.width=7, eval=LOCAL-----------------------------------
pobreza_calificacion <- bases_pobreza %>%
  filter(!is.na(situacion), !is.na(PP04D_COD)) %>%
  organize_cno(.) %>%
  filter(CALIFICACION %in% c("No calificados", "Operativos", "Técnicos", "Profesionales")) %>%
  group_by(ANO4, TRIMESTRE, CALIFICACION) %>%
  summarise(
    pobreza = sum(PONDIH[situacion %in% c("pobre", "indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    indigencia = sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    .groups = "drop"
  )


pobreza_calificacion %>%
  mutate(periodo = parse_date_time(paste0(ANO4, "-", TRIMESTRE), "Y.q")) %>%
  pivot_longer(cols = pobreza:indigencia, names_to = c("tipo"), values_to = "tasa") %>%
  ggplot(aes(periodo, tasa, fill = tipo)) +
  geom_col(position = position_dodge()) +
  facet_wrap(. ~ CALIFICACION) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- fig.height=7, fig.width=7, eval=LOCAL-----------------------------------
pobreza_informalidad <- bases_pobreza %>%
  filter(!is.na(situacion), PP07H %in% 1:2) %>%
  group_by(ANO4, TRIMESTRE, PP07H) %>%
  summarise(
    pobreza = sum(PONDIH[situacion %in% c("pobre", "indigente")], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    indigencia = sum(PONDIH[situacion == "indigente"], na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    .groups = "drop"
  )


pobreza_informalidad %>%
  mutate(
    periodo = parse_date_time(paste0(ANO4, "-", TRIMESTRE), "Y.q"),
    descuento_jubilatorio = case_when(
      PP07H == 1 ~ "Si",
      TRUE ~ "No"
    )
  ) %>%
  pivot_longer(cols = pobreza:indigencia, names_to = c("tipo"), values_to = "tasa") %>%
  ggplot(aes(periodo, tasa, color = descuento_jubilatorio)) +
  # geom_col(position = position_dodge())+
  geom_point(size = 2) +
  geom_smooth() +
  facet_wrap(tipo ~ ., scales = "free", ncol = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14)
  )

## ---- eval=LOCAL--------------------------------------------------------------
relativos_cba <- canastas_regionales %>%
  select(-CBT, -codigo) %>%
  pivot_wider(names_from = region, values_from = CBA) %>%
  mutate_at(.vars = c("Cuyo", "Noreste", "Noroeste", "Pampeana", "Patagonia"), ~ .x / GBA) %>%
  mutate(
    GBA = GBA / GBA,
    periodo = parse_date_time(periodo, "Y.q")
  ) # paso a formato fecha los trimestres
relativos_cba

## ---- fig.height=5, fig.width=7, eval=LOCAL-----------------------------------
ppcc <- data.frame(
  region = c("Cuyo", "GBA", "Noreste", "Noroeste", "Pampeana", "Patagonia"),
  ppcc = c(.872, 1, .886, .865, .904, .949)
)


relativos_cba %>%
  pivot_longer(cols = Cuyo:Patagonia, names_to = "region", values_to = "relativo") %>%
  left_join(ppcc, by = c("region")) %>%
  mutate(relativo_normalizado = relativo / ppcc) %>%
  pivot_longer(names_to = "grupo", cols = c("relativo", "relativo_normalizado", "ppcc"), values_to = "valor") %>%
  mutate(grupo = fct_recode(grupo, "efecto total" = "relativo", "efecto composición" = "relativo_normalizado")) %>%
  ggplot(aes(periodo, valor, color = region, group = region)) +
  geom_line() +
  facet_wrap(. ~ grupo) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- eval=LOCAL--------------------------------------------------------------
relativos_cbt <- canastas_regionales %>%
  select(-CBA, -codigo) %>%
  pivot_wider(names_from = region, values_from = CBT) %>%
  mutate_at(.vars = c("Cuyo", "Noreste", "Noroeste", "Pampeana", "Patagonia"), ~ .x / GBA) %>%
  mutate(
    GBA = GBA / GBA,
    periodo = parse_date_time(periodo, "Y.q")
  ) # paso a formato fecha los trimestres
relativos_cbt

## ---- fig.height=5, fig.width=7, eval=LOCAL-----------------------------------
relativos_cbt %>%
  pivot_longer(cols = Cuyo:Patagonia, names_to = "region", values_to = "relativo") %>%
  left_join(ppcc, by = c("region")) %>%
  mutate(relativo_normalizado = relativo / ppcc) %>%
  pivot_longer(names_to = "grupo", cols = c("relativo", "relativo_normalizado", "ppcc"), values_to = "valor") %>%
  mutate(grupo = fct_recode(grupo, "efecto total" = "relativo", "efecto composición" = "relativo_normalizado")) %>%
  ggplot(aes(periodo, valor, color = region, group = region)) +
  geom_line() +
  facet_wrap(. ~ grupo) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- eval=LOCAL--------------------------------------------------------------
relativos_ice <- canastas_regionales %>%
  group_by(region, periodo) %>%
  mutate(ice = CBT / CBA) %>%
  select(-CBA, -CBT, -codigo) %>%
  pivot_wider(names_from = region, values_from = ice) %>%
  mutate_at(.vars = c("Cuyo", "Noreste", "Noroeste", "Pampeana", "Patagonia"), ~ .x / GBA) %>%
  mutate(
    GBA = GBA / GBA,
    periodo = parse_date_time(periodo, "Y.q")
  ) # paso a formato fecha los trimestres
relativos_ice

## ---- fig.height=5, fig.width=7, eval=LOCAL-----------------------------------
relativos_ice %>%
  pivot_longer(cols = Cuyo:Patagonia, names_to = "region", values_to = "relativo") %>%
  left_join(ppcc, by = c("region")) %>%
  ggplot(aes(periodo, relativo, color = region, group = region)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom")

