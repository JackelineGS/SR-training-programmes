
library(pacman)
p_load(tidyverse, skimr, psych)

BASE <- readxl::read_xlsx("PROGRAMA3.xlsx")
names(BASE)

#############################################
##### Cantidad de programas en total ########

Cantidad <- BASE %>% 
  filter(Type == "instituto") %>% 
  count(Institution) %>%
  mutate(
    Porcentaje = round (n/sum(n),2)) %>% 
  print(n = Inf)

Cantidad <- BASE %>%
  filter(Grade == "Pregrado") %>% 
  count(Grade, Carrer) %>% 
  group_by(Carrer) %>% 
  ungroup() %>% 
  mutate(
    Porcentaje = round (n/sum(n),2)) %>% 
  print(n = Inf)

Cantidad <- BASE %>%
  filter(Grade == "Pregrado") %>% 
  count(Carrer) %>% 
  mutate(
    Porcentaje = round (n/sum(n),2)) %>% 
  print(n = Inf)

Regiones <- BASE %>% 
  filter(Grade == "Pregrado") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n),2)) %>% 
  print(n = Inf)

RegionB <- BASE %>% 
  filter(Grade == "Especialidad") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n),2))


library(openxlsx)
openxlsx::write.xlsx(RegionB, file = "Regiones.xlsx")

###############################################################
####################### COSTOS ################################
# Medicina

#Distribución geográfica de los programas
BASE %>%
  select(Carrer, Region) %>% 
  filter(Carrer == "Medicine") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Cantidad de universidades privadas
BASE %>%
  filter(Carrer == "Medicine") %>% 
  select(Carrer, Management) %>% 
  count(Carrer, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

  
#Costo Universidad Nacional 
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

#Costo Universidad Particular
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

#Descriptivos de los costos
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  describe()

#Descriptivos de los costos quitando los casos con "0"
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Psychology

#Distribución geográfica de los programas
BASE %>%
  select(Carrer, Region) %>% 
  filter(Carrer == "Psychology") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Cantidad de universidades privadas
BASE %>%
  filter(Carrer == "Psychology") %>% 
  select(Carrer, Management) %>% 
  count(Carrer, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

# Costo en universidades nacionales
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

# Rango del costo mínimo y máximo en universidad privada
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

#Descriptivos de los costos (tomar el minimo)
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

#Descriptivos de los costos quitando los casos con "0"
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Enfermeria

#Distribución geográfica de los programas
BASE %>%
  select(Carrer, Region) %>% 
  filter(Carrer == "Nursing") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de universidades publicas y privadas
BASE %>%
  filter(Carrer == "Nursing") %>% 
  select(Carrer, Management) %>% 
  count(Carrer, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo de las universidades publicas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

#Costo de las universidades privadas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

# Descriptivos de los costos
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

#Descriptivos de los costos quitando "0" 
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Tecnica

#Distribución geográfica de los programas
BASE %>%
  select(Carrer, Region) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de instituciones publicas y privadas
BASE %>%
  filter(Carrer == "Nursing-Technician") %>% 
  select(Carrer, Management) %>% 
  count(Carrer, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo de las universidades publicas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

#Costo de las universidades privadas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Institution, Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)
#Descriptivo de los costos de instiutos privados
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmin != 0) %>% 
  describe()

#Social

#Distribución geográfica de los programas
BASE %>%
  select(Carrer, Region) %>% 
  filter(Carrer == "Social-work") %>% 
  count(Region) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de instituciones publicas y privadas
BASE %>%
  filter(Carrer == "Social-work") %>% 
  select(Carrer, Management) %>% 
  count(Carrer, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo de las universidades publicas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Social-work") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

# Costo en las universidades privadas
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Social-work") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

# Tecnologia
# Speech-therapy
# Occupational-therapy

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Carrer == "Occupational-therapy") %>% 
  select(Costo_Tmin:Costo_Tmax)

#Terapia de lenguaje(1 Cayetano / 1 Villarreal)
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Carrer == "Speech-therapy") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)


Tipo <- BASE_1 %>% 
  count(Institution, Management) %>% 
  mutate(
    Porcentaje = n/sum(n))

########################################################
########## Totales de programas de SE ##################

# Cantidad de programas
BASE %>% 
  filter(Grade == "Especialidad") %>%
  select(Grade, Public) %>% 
  count(Grade, Public) %>%
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

########################################################
########## Conteos de programas de SE ##################

#Distribución geográfica Medicina
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Medicos") %>%
  count(Region, Public) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de universidades publicas y privadas
#En cuanto a Grade se excluye a todos los de pregrado
#Para considerar a los de segunda especialidad y subespecialidad
BASE %>%
  filter(Grade != "Pregrado") %>% 
  filter(Public == "Medicos") %>%
  count(Public, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo SE de Medicos
BASE %>%  
  filter(Grade == "Especialidad") %>%
  filter(Public == "Medicos") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

BASE %>% 
  filter(Grade == "Especialidad") %>%
  filter(Public == "Medicos") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  print(n = Inf)

#Costo SUBESPECIALIDAD en Medicina
BASE %>%
  filter(Grade == "EspecialidadS") %>%
  filter(Public == "Medicos") %>%
  select(Costo_Tmax) %>% 
  print(n = Inf)

#Distribución geográfica Enfermería
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Medicos") %>%
  count(Region, Public) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Universidades publicas y privadas en Enfermería
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Enfermeras") %>%
  count(Public, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo SE en enfermería
BASE %>%  
  filter(Grade == "Especialidad") %>%
  filter(Public == "Enfermeras") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

BASE %>% 
  filter(Grade == "Especialidad") %>%
  filter(Public == "Enfermeras") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  print(n = Inf)

#Distribución geográfica Psicología
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Psicologos") %>%
  count(Region, Public) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de universidades publicas y privadas en Psicología
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Psicologos") %>%
  count(Public, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo SE de Psicología
BASE %>%  
  filter(Grade == "Especialidad") %>%
  filter(Public == "Psicologos") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

BASE %>% 
  filter(Grade == "Especialidad") %>%
  filter(Public == "Medicos") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  print(n = Inf)

#Distribución geográfica Multidisciplinario

BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Multidisciplinario") %>%
  count(Region, Public) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100)) %>% 
  print(n = Inf)

#Cantidad de universidades publicas y privadas Multidisciplinario
BASE %>%
  filter(Grade == "Especialidad") %>% 
  filter(Public == "Multidisciplinario") %>%
  count(Public, Management) %>% 
  mutate(
    Porcentaje = round (n/sum(n)*100))

#Costo SE Multidisciplinario
BASE %>%  
  filter(Grade == "Especialidad") %>%
  filter(Public == "Multidisciplinario") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

BASE %>% 
  filter(Grade == "Especialidad") %>%
  filter(Public == "Multidisciplinario") %>%
  select(Costo_Tmin: Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  print(n = Inf)

########################################################
####################### Gráficos #######################

pacman::p_load(tidyverse, openxlsx, labelled, ggplot2,scales, jcolors)


LIMA <- filter(BASE_3, Region == "Lima")

SINLIMA <- filter(BASE_3, Region != "Lima")

###########################################################
################### BARRAS FINALES ########################
###########################################################

BARRA <- BASE_3 %>% 
  count(Carrer, Region) %>% 
  mutate(
    Carrer = factor(Carrer),
    Region = factor(Region),
    Carrer = fct_reorder(Carrer, n)
  ) %>% 
  ggplot(aes(y = Region, x = n,
             fill = Carrer)) +
  geom_col() +
  geom_text(aes(label = n),position = position_stack(vjust = 0.5)) +
    labs(
      title = "Undergraduate programmes by region (n = 251)",
      x = "Number of programmes",
      y = "Regions"
    ) +
    scale_fill_manual(values = c("#BCEE68",
                                 "#9BCD9B",
                                 "#4EEE94", 
                                 "#79CDCD",
                                 "#EEDC82",
                                 "#FFA07A", 
                                 "#9AC0CD"))

ggsave("BARRA.png",
       plot = BARRA,
       width = 11, height = 8, dpi = 300)
xunion_plot_1


##################################################################
################ SOLO LIMA #######################################

ggplot(LIMA,
       aes(y = Carrer, fill = Carrer)) +
  geom_bar() 


BARRA_LL <- LIMA %>% 
  count(Carrer) %>% 
  mutate(
    Carrer = factor(Carrer),
    Carrer = fct_reorder(Carrer, n)
  ) %>% 
  ggplot(aes(y = Carrer, x = n, fill = Carrer)) +
  geom_col() + 
  guides(fill = FALSE) +
  geom_text(aes(label = n),position = position_stack(vjust = 0.5)) +
  labs(
    title = "Undergraduate programmes in Lima",
    x = "Number of programmes",
    y = "Programs"
  ) +
  scale_fill_manual(values = c("#BCEE68",
                               "#9BCD9B",
                               "#4EEE94", 
                               "#79CDCD",
                               "#EEDC82",
                               "#FFA07A", 
                               "#9AC0CD"))

ggsave("BARRASL.png",
       plot = BARRA_L,
       width = 20, height = 9, dpi = 300)
xunion_plot_1

pacman::p_load(patchwork)


union_plot_8 <- (SEM_mapa)
union_plot_9 <- (SEE_mapa)
union_plot_10 <- (SEP_mapa)
union_plot_11 <- (SEM_mapa + SEE_mapa + SEP_mapa)

##################################################################
################## Segunda Especialidad ##########################

BASE_4 <- filter(BASE, Grade != "Pregrado")
names(BASE_4)

UNI <- BASE_4 %>% 
  count(Institution) %>% 
  mutate(
    Porcentaje = n/sum(n))


CARRERA <- BASE_4 %>% 
  count(Carrer) %>% 
  mutate(
    Porcentaje = n/sum(n))

PUBLICO <- BASE_4 %>% 
  count(Public) %>% 
  mutate(
    Porcentaje = n/sum(n))

REG <- BASE_4 %>% 
  count(Region,Public) %>% 
  mutate(
    Porcentaje = n/sum(n))

REGPUBLIC <- BASE_4 %>% 
  count(Region,Carrer) %>% 
  mutate(
    Porcentaje = n/sum(n))



LIMASE <- filter(BASE_4, Region == "Lima")
SINLIMASE <- filter(BASE_4, Region != "Lima")

##################################################
########## BARRAS SEGUNDA ESPECIALIDAD ###########

BARRASE <- BASE_4 %>% 
  count(Carrer, Region) %>% 
  mutate(
    Carrer = factor(Carrer),
    Region = factor(Region),
    Carrer = fct_reorder(Carrer, n)
  ) %>% 
  ggplot(aes(y = Region, x = n,
             fill = Carrer)) +
  geom_col() +
  geom_text(aes(label = n),position = position_stack(vjust = 0.5)) +
  labs(
    title = "Second speciality programmes by region",
    x = "Number of programmes",
    y = "Regions"
  ) +
  scale_fill_manual(values = c("#BCEE68",
                               "#9BCD9B",
                               "#4EEE94", 
                               "#79CDCD",
                               "#EEDC82",
                               "#FFA07A", 
                               "#9AC0CD",
                               "#DDA0DD",
                               "#CDB7B5",
                               "#EED8AE"))

ggsave("BARRASE.png",
       plot = BARRASE,
       width = 11, height = 8, dpi = 300)

BARRA_L <- LIMASE %>% 
  count(Carrer) %>% 
  mutate(
    Carrer = factor(Carrer),
    Carrer = fct_reorder(Carrer, n)
  ) %>% 
  ggplot(aes(y = Carrer, x = n, fill = Carrer)) +
  geom_col() +
  guides(fill = FALSE) +
  geom_text(aes(label = n),position = position_stack(vjust = 0.5)) +
  labs(
    title = "Second speciality programmes in Lima",
    x = "Number of programmes",
    y = "Programs"
  ) +
  scale_fill_manual(values = c("#BCEE68",
                               "#9BCD9B",
                               "#4EEE94", 
                               "#79CDCD",
                               "#EEDC82",
                               "#FFA07A", 
                               "#9AC0CD",
                               "#C6E2FF",
                               "#B9D3EE",
                               "#9AFF9A"))
ggsave("union_plot1.png",
       plot = union_plott,
       width = 20, height = 9, dpi = 300)

pacman::p_load(patchwork)

union_plott <- (BARRA_LL+BARRA_L)
union_plot <- (BARRA + BARRASE) 
union_plot_8 <- (SEM_mapa)
union_plot_9 <- (SEE_mapa)
union_plot_10 <- (SEP_mapa)
union_plot_11 <- (SEM_mapa + SEE_mapa + SEP_mapa)

library(openxlsx)
openxlsx::write.xlsx(Cantidad, file = "Cantidad.xlsx")

pacman::p_load(psych, skimr, summarytools)








