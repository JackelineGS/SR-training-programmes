
library(pacman)
p_load(tidyverse, skimr, psych)

BASE <- readxl::read_xlsx("PROGRAMA2.xlsx")
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
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)
  
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Medicine") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Psychology
BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)


BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Psychology") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Enfermeria

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 1) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)


BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmax != 0) %>% 
  describe()

#Tecnica

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmin != 0) %>% 
  describe()

#Social

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Social-work") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Social-work") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmin != 0) %>% 
  describe()

# Tecnologia
# Speech-therapy
# Occupational-therapy

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Occupational-therapy") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  print(n = Inf)

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Occupational-therapy") %>% 
  select(Costo_Tmin:Costo_Tmax) %>% 
  describe()

BASE %>%
  filter(Grade == "Pregrado") %>%
  filter(Management == 0) %>% 
  filter(Carrer == "Nursing-Technician") %>% 
  select(Costo_Tmin:Costo_Tmax) %>%
  filter(Costo_Tmin != 0) %>% 
  describe()



library(openxlsx)
openxlsx::write.xlsx(Distribucion, file = "Distribution.xlsx")

############ Gestion de universidad #####################
#########################################################

BASE_1 <- filter(BASE, Type != "instituto")
BASE_1 <- BASE_1 %>% 
  count(Type)
BASE_1 <- BASE_1 %>% 
  count(Management)
BASE_1 <- BASE_1 %>% 
  count(Management) %>% 
  mutate(
    Porcentaje = n/sum(n))

BASE_1 <- filter(BASE, Grade != "Especialidad")

UNIS <- BASE_1 %>% 
  count(Institution)

Tipo <- BASE_1 %>% 
  count(Institution, Management) %>% 
  mutate(
    Porcentaje = n/sum(n))


library(openxlsx)
openxlsx::write.xlsx(UNIS, file = "UNIS.xlsx")

##########################################################
###### Hallando la cantidad de Institutos
##########################################################

BASE_2 <- filter(BASE, Grado != "universidad")

Instituciones <- BASE_2 %>% 
  count(Institucion) %>% 
  mutate(
    Porcentaje = n/sum(n))

########################################################
######## Cantidad total de programas ####################

BASE_3 <- filter(BASE, Grade != "Especialidad")

CARRERA <- BASE_3 %>% 
  count(Carrer) %>% 
  mutate(
    Porcentaje = n/sum(n))

library(openxlsx)
openxlsx::write.xlsx(CARRERA, file = "Carrera.xlsx")

BASE_1 <- BASE %>% 
  filter(Management == "0") %>% 
  filter(Grade == "Pregrado") %>% 
  filter(Type == "universidad") %>% 
  select (Institution, Carrer, Type,Management,
          Grade, Costo_Tmin:Costo_Tmax)

Costo <- BASE_1 %>%
  filter(Carrer == "Social-work") %>% 
  skim()

BASE_2 <- BASE %>% 
  filter(Management == "0") %>% 
  filter(Grade == "Pregrado") %>% 
  filter(Type == "instituto") %>% 
  select (Institution, Carrer, Type,Management,
          Grade, Costo_Tmin:Costo_Tmax)

Costo_I <- BASE_2 %>%
  select(Institution, Costo_Tmin:Costo_Tmax) %>% 
  count(Institution, Costo_Tmax)

Privada <- BASE_2 %>% 
  skim()

library(openxlsx)
openxlsx::write.xlsx(Costo, file = "Costo.xlsx")


##############################################
############ Programs by region ##############

BASE_3 <- filter(BASE, Grade != "Especialidad")

REGIONES <- BASE_3 %>% 
  count(Region, Carrer) %>% 
  mutate(
    Porcentaje = n/sum(n)) %>% 
  print(n = Inf)

BASE_4 <- filter(BASE, Grade != "Pregrado")
names(BASE_4)

REGIONES <- BASE_4 %>% 
  count(Region, Carrer) %>% 
  mutate(
    Porcentaje = n/sum(n)) %>% 
  print(n = Inf)


library(openxlsx)
openxlsx::write.xlsx(REGIONES, file = "Regiones.xlsx")


########################################################
################ Crear el grafico de barras ############

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








