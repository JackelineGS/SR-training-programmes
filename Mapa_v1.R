##############################################
############# CREACIÓN DE MAPAS ##############
##############################################

install.packages("sp")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("sf")
install.packages("purrr")
install.packages("ggrepel")

library(pacman)
p_load(ggplot2,tidyverse,psych,purrr,sf, sp, ggrepel)

############ PROCEDIMIENTO ################################

#1. Cargar paquetes
#2. Cargar archivo .shp
#3. Cargar data
#4. Crear centroides en el archivo .shp
#5. Unir bases de datos
#4. Crear mapas


########### CARGAR BASES Y CREAR CENTROIDES ####################

# Se carga el archivo sf
# st_read es una función del paquete sf
mapa_sh <- st_read("BAS_LIM_DEPARTAMENTO.shp") 

# Creación del centroide en el archivo .shp
mapa_sh <- mapa_sh %>% 
  mutate(centroid = map(geometry, st_centroid), 
                        coords = map(centroid, st_coordinates), 
                        coords_x = map_dbl(coords,1), 
                        coords_y = map_dbl(coords, 2))
# Se carga la data
mapa_df <- readxl::read_xlsx("Data_map_score_10.xlsx")

# Se junta el archivo .shp y la data
# La primera columna de ambas bases debe tener el mismo nombre
datos_F <- mapa_sh %>% 
  left_join(mapa_df)

# Primer mapa con etiquetas
ggplot(data = datos_F) + 
  geom_sf(fill = "skyblue", 
         color = "black") +
  geom_text_repel(mapping = aes(coords_x, coords_y, 
                    label = NOMBDEP), 
                     size = 2.25)

# Mapa con etiqueta de las cantidades
ggplot(datos_F) +
  geom_sf(aes(fill = TOTAL_P)) +
        labs(title = "Cantidad de programas",
           caption = "Fuente: Elaboración propia",
                 x = "Longitud",
                 y = "Latitud") +
  scale_fill_continuous(guide_legend(title = "Cantidad")) +
  geom_text_repel(mapping = aes(coords_x, 
                                coords_y, 
                                label = TOTAL_P_C), 
                  size = 2.25)

###################################################################### 
############## Mapas con cantidades categorizadas ####################
######################################################################

# Mapa con rangos 

br <- c(0, 1, 6, 11, 21, 50, 108)
datos_F$mapa_df <- cut(datos_F$TOTAL_P,
                       breaks = br,
                       dig.lab = 5)

pal <- hcl.colors(7, "Inferno", rev = TRUE, alpha = 0.7)

map <- ggplot() + 
  geom_sf(data = datos_F, fill = "grey80", color = NA) + 
  geom_label_repel(mapping = aes(coords_x, coords_y, 
                                 label = TOTAL_P_C))

map <- ggplot(datos_F) + 
  geom_sf(fill = "white")

map + 
  geom_sf(data = datos_F,
          aes(fill = mapa_df), color = "white") + 
  labs(title = "",
       caption = "Instituto Peruano de Orientación Psicológica") + 
  scale_fill_manual(values = pal,
                    drop = FALSE) + 
  theme_void() + 
  theme(plot.caption = element_text(size = 7, face = "italic")) +
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40') +
  geom_label_repel(mapping = aes(coords_x, coords_y, 
                                 label = TOTAL_P_C))
  

P_mapa <- datos_F %>% 
  ggplot(aes(fill = TOTAL_P)) +
  geom_sf(colour = "white", size = 0.90)



# MAPA DEL TOTAL DE PROGRAMAS
# Programas de pregrado

datos_F <- arrange(datos_F, TOTAL_P)
datos_F$TOTAL_P <- factor(datos_F$TOTAL_P, 
                            levels = c(1, 2, 3, 4, 6, 8, 9,
                                       10, 13, 14, 108),
                            labels = c("1 programme",
                                       "2 programmes",
                                       "3 programmes",
                                       "4 programmes",
                                       "6 programmes",
                                       "8 programmes",
                                       "9 programmes",
                                       "10 programmes",
                                       "13 programmes",
                                       "14 programmes",
                                       "108 programmes"))
forcats::fct_unique(datos_F$TOTAL_P)

P_mapa <- datos_F %>% 
  ggplot(aes(fill = TOTAL_P)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Total Programmes - undergraduate",
                    values = c("1 programme" = "#EEE8AA",
                               "2 programmes" = "#EEE8AA",
                               "3 programmes" = "#EEE8AA",
                               "4 programmes" = "#EEE8AA",
                               "6 programmes" = "#B4EEB4",
                               "8 programmes" = "#B4EEB4",
                               "9 programmes" = "#B4EEB4",
                               "10 programmes" = "#B4EEB4",
                               "13 programmes" = "#FFC685",
                               "14 programmes" = "#FFC685", 
                               "108 programmes" = "#FFC685")) +
  theme_bw()+
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 10, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40') +
  geom_label_repel(mapping = aes(coords_x, coords_y, 
                     label = TOTAL_P_C), 
                  fontface = "bold", 
                     color = "gray15",
               box.padding = unit(0.30, "lines"),
             point.padding = unit(0.2, "lines"),
             segment.color = "gray50",
               show.legend = FALSE) +
   theme_classic(base_size = 12) + 
   theme(panel.grid = element_line(colour = "transparent"),
   panel.background = element_blank(),
          axis.text = element_blank(),
         axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
       axis.title.y = element_blank(),
       axis.title.x = element_blank())

# Programas de segunda especialidad

datos_F <- arrange(datos_F, TOTAL_SE)
datos_F$TOTAL_SE <- factor(datos_F$TOTAL_SE, 
                          levels = c(0, 1, 2, 3, 4, 5, 8, 34),
                          labels = c("0 programmes", 
                                     "1 programme",
                                     "2 programmes",
                                     "3 programmes",
                                     "4 programmes",
                                     "5 programmes",
                                     "8 programmes",
                                     "34 programmes"))
forcats::fct_unique(datos_F$TOTAL_SE)

SE_mapa <- datos_F %>% 
  ggplot(aes(fill = TOTAL_SE)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Total Programmes - second speciality",
                    values = c("0 programmes" = "#EEE8AA",
                               "1 programme" = "#EEE8AA",
                               "2 programmes" = "#EEE8AA",
                               "3 programmes" = "#B4EEB4",
                               "4 programmes" = "#B4EEB4",
                               "5 programmes" = "#B4EEB4",
                               "8 programmes" = "#FFC685",
                               "34 programmes" = "#FFC685")) +
  theme_bw()+
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 10, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40') +
  geom_label_repel(mapping = aes(coords_x, coords_y, 
                                 label = TOTAL_SE_C), 
                   fontface = "bold", 
                   color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE) +
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


# Unir mapas

pacman::p_load(patchwork)

plot_PSE  <- (P_mapa + SE_mapa)

ggsave("plot_PSE.png",
       plot = plot_PSE,
       width = 17, height = 9, dpi = 300)
xunion_plot_1


#############################################################
################## Mapa por programa ########################
#############################################################

# Mapa de enfermería (PRE_ENF_F)
datos_F <- arrange(datos_F, PRE_ENF_F)
datos_F$PRE_ENF_F <- factor(datos_F$PRE_ENF_F, 
                               levels = c(0, 1, 2, 3, 4, 5, 20),
                               labels = c("0 programs", 
                                          "1 program",
                                          "2 programs",
                                          "3 programs",
                                          "4 programs",
                                          "5 programs",
                                          "20 programs"))
forcats::fct_unique(datos_F$PRE_ENF_F)

F_mapa <- datos_F %>% 
  ggplot(aes(fill = PRE_ENF_F)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Nursing",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "3 programs" = "#6FC59E",
                               "4 programs" = "#43CD80",
                               "5 programs" = "#7AC5CD",
                               "20 programs" = "#4876FF")) +
  theme_bw() +
  theme(
    legend.position = "left",
      legend.margin = margin(3,3,3,3)) + 
  theme(
    legend.text = element_text(size = 10, colour = "black")) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40')+
  geom_label_repel(mapping = aes(coords_x, coords_y, label = PRE_ENF_L), 
                  fontface = "bold", color = "gray15",
               box.padding = unit(0.30, "lines"),
             point.padding = unit(0.2, "lines"),
             segment.color = "gray50",
               show.legend = FALSE)+
   theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


# Mapa de los programas de medicina (PRE_MED_F)

datos_F <- arrange(datos_F, PRE_MED_F)

datos_F$PRE_MED_F <- factor(datos_F$PRE_MED_F, 
                            levels = c(0, 1, 2, 3, 18),
                            labels = c("0 programs", 
                                       "1 program",
                                       "2 programs",
                                       "3 programs",
                                       "18 programs"))
forcats::fct_unique(datos_F$PRE_MED_F)

M_mapa <- datos_F %>% 
  ggplot(aes(fill = PRE_MED_F)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Medicine",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "3 programs" = "#6FC59E",
                               "18 programs" = "#4876FF")) +
  theme_bw()+
  theme(
    legend.position = "right",
    legend.margin = margin(3,3,3,3)
  )+ 
  theme(
    legend.text = element_text(size = 11, colour = "black")
  )+ 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40')+
  geom_label_repel(mapping = aes(coords_x, coords_y, label = PRE_MED_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE)+
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())


# Programas de psicología (PRE_PSICO_F)

datos_F <- arrange(datos_F, PRE_PSICO_F)
datos_F$PRE_PSICO_F <- factor(datos_F$PRE_PSICO_F, 
                            levels = c(0, 1, 2, 3, 4, 5, 40),
                            labels = c("0 programs", 
                                       "1 program",
                                       "2 programs",
                                       "3 programs",
                                       "4 programs",
                                       "5 programs",
                                       "40 programs"))
forcats::fct_unique(datos_F$PRE_PSICO_F)

P_mapa <- datos_F %>% 
  ggplot(aes(fill = PRE_PSICO_F)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Psychology",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "3 programs" = "#6FC59E",
                               "4 programs" = "#43CD80",
                               "5 programs" = "#7AC5CD",
                               "40 programs" = "#4876FF")) +
  theme_bw() +
  theme(
    legend.position = "rigth",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 11, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40') +
  geom_label_repel(mapping = aes(coords_x, coords_y, label = PRE_PSICO_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE) +
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Mapa de trabajo social (PRE_TRAB_PRE)

datos_F <- arrange(datos_F, PRE_TRAB_PRE)
datos_F$PRE_TRAB_PRE <- factor(datos_F$PRE_TRAB_PRE, 
                            levels = c(0, 1, 2, 3),
                            labels = c("0 programs", 
                                       "1 program",
                                       "2 programs",
                                       "3 programs"))
forcats::fct_unique(datos_F$PRE_TRAB_PRE)

T_mapa <- datos_F %>% 
  ggplot(aes(fill = PRE_TRAB_PRE)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Social Work",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "3 programs" = "#6FC59E")) +
  theme_bw() +
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 15, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40') +
  geom_label_repel(mapping = aes(coords_x, coords_y, label = PRE_TRAB_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE)+
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Juntar gráficos

pacman::p_load(patchwork)

union_plot_1 <- (M_mapa)
union_plot_2 <- (F_mapa)
union_plot_3 <- (P_mapa)
union_plot_4 <- (T_mapa)
union_plot_5 <- (M_mapa + F_mapa)
union_plot_6 <- (P_mapa + T_mapa)
union_plot_7 <- (M_mapa + F_mapa + P_mapa + T_mapa)
ggsave("union_plot_7.png",
       plot = union_plot_7,
       width = 13, height = 13, dpi = 300)
  union_plot_1

###########################################################
############### MAPAS SEGUNDA ESPECIALIDAD ################
###########################################################

# Mapa Medicina SE (SE_MED)

datos_F <- arrange(datos_F, SE_MED)
datos_F$SE_MED <- factor(datos_F$SE_MED, 
                            levels = c(0, 1, 2, 3, 4,21),
                            labels = c("0 programs", 
                                       "1 program",
                                       "2 programs",
                                       "3 programs",
                                       "4 programs",
                                       "21 programs"))
forcats::fct_unique(datos_F$SE_MED)

SEM_mapa <- datos_F %>% 
  ggplot(aes(fill = SE_MED)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Medicine",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "3 programs" = "#6FC59E",
                               "4 programs" = "#43CD80",
                               "21 programs" = "#4876FF")) +
  theme_bw() +
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 16, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40')+
  geom_label_repel(mapping = aes(coords_x, coords_y, label = SE_MED_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE)+
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Mapa enfermería SE (SE_ENF)

datos_F <- arrange(datos_F, SE_ENF)
datos_F$SE_ENF <- factor(datos_F$SE_ENF, 
                         levels = c(0, 1, 2, 8),
                         labels = c("0 programs", 
                                    "1 program",
                                    "2 programs",
                                    "8 programs"))
forcats::fct_unique(datos_F$SE_ENF)

SEE_mapa <- datos_F %>% 
  ggplot(aes(fill = SE_ENF)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Nursing",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "2 programs" = "#B7F1B2",
                               "8 programs" = "#4876FF")) +
  theme_bw()+
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  ) + 
  theme(
    legend.text = element_text(size = 16, colour = "black")
  ) + 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40')+
  geom_label_repel(mapping = aes(coords_x, coords_y, label = SE_ENF_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE)+
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Mapa Psicología SE (SE_PSIC) 

datos_F <- arrange(datos_F, SE_PSIC)
datos_F$SE_PSIC <- factor(datos_F$SE_PSIC, 
                         levels = c(0, 1, 3, 6),
                         labels = c("0 programs", 
                                    "1 program",
                                    "3 programs",
                                    "6 programs"))
forcats::fct_unique(datos_F$SE_PSIC)

SEP_mapa <- datos_F %>% 
  ggplot(aes(fill = SE_PSIC)) +
  geom_sf(colour = "white", size = 0.90) +
  scale_fill_manual("Psychology",
                    values = c("0 programs" = "#FFFF80",
                               "1 program" = "#FAE073",
                               "3 programs" = "#6FC59E",
                               "6 programs" = "#4876FF")) +
  theme_bw()+
  theme(
    legend.position = "left",
    legend.margin = margin(3,3,3,3)
  )+ 
  theme(
    legend.text = element_text(size = 16, colour = "black")
  )+ 
  geom_point(aes(coords_x, coords_y), size = 2, color = 'gray40')+
  geom_label_repel(mapping = aes(coords_x, coords_y, label = SE_PSIC_L), 
                   fontface = "bold", color = "gray15",
                   box.padding = unit(0.30, "lines"),
                   point.padding = unit(0.2, "lines"),
                   segment.color = "gray50",
                   show.legend = FALSE)+
  theme_classic(base_size = 12) + 
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Unir mapas

pacman::p_load(patchwork)

union_plot_8 <- (SEM_mapa)
union_plot_9 <- (SEE_mapa)
union_plot_10 <- (SEP_mapa)
union_plot_11 <- (SEM_mapa + SEE_mapa + SEP_mapa)
plot_PSE  <- (P_mapa + SE_mapa)

ggsave("plot_PSE.png",
       plot = plot_PSE,
       width = 20, height = 9, dpi = 300)
xunion_plot_1
