
# Limpiamos ambiente

rm(list = ls())

# Activamos paquete

library(tidyverse)
library(waffle)
library(ggtext)

# Levantamos base
# esta base fue construida a partir de datos de la empresa que gestiona el Sistema Eléctrico Interconectado
# de Colombia, la puedes descargar por consulta API mediante el siguiente comando

# Api para descargar la base
# 
# url <- "https://raw.githubusercontent.com/GomezGerardoEsteban/Day4/main/base_autogeneradores.csv"
# 
# filename <- basename(url)
# 
# download.file(url = url,
#               filename = filename,
#               mode = "wb")

base <- read.csv2("E:/Documents/Escritorio/FLACSO/Tesis/petroleo/rmd/resultados/graficos/30DayChartChallenge/base_autogeneradores.csv")

base <- base %>% 
  mutate(combustible = factor(combustible,
                              levels = c("Carbón", "Petróleo", "Gas", "Hidroeléctrica",
                                         "Bioenergía", "Solar", "Eólica"),
                              labels = c("Carbón", "Petróleo", "Gas", "Hidroeléctrica",
                                                    "Bioenergía", "Solar", "Eólica")),
         solar = factor(ifelse(combustible == "Solar", "Solar", "otro"),  # Creamos la variable solar sobre la que se quiere llamar la atención
                        levels = c("otro", "Solar"),
                        labels = c("otro", "Solar")))


# graficamos --------------------------------------------------------------


grafico <- base %>% 
  filter(year %in% c(2019,2023)) %>%
  group_by(year, solar) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_waffle(mapping = aes(fill = solar, values = n), 
              color = "white",
              show.legend = F,
              size = 1.5) +
  facet_wrap(~year, nrow = 2) +
  scale_fill_manual(values = c("black", "#ff6700")) +
  scale_y_continuous(breaks = 1:10) +
  scale_x_continuous(breaks = seq(2,56,2)) +
  labs(title = "<span style='color:#ff6700;'>**BOOM**</span> <span style='color:#4B5253;'>de autogeneración solar en el sistema eléctrico colombiano</span>",
       subtitle = "<span style='color:#4B5253;'>Número de plantas </span><span style='color:#ff6700;'>**solares**</span><span style='color:#4B5253;'> y de </span><span style='color:#000000;'>**Otras Fuentes**</span><span style='color:#4B5253;'> 2019 vs. 2023</span>",
       y = NULL,
       x = NULL,
       caption = "Fuente: XM <br> **#30DayChartChallenge #Day4** <br> **@GEstebanGomez**") +
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 11),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(plot = grafico, filename = "../rmd/resultados/graficos/30DayChartChallenge/4Day_waffle.png",
       dpi = 300, width = 9.68, height = 5.81)

  


