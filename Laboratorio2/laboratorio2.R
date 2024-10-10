library(tidyverse)
data(starwars)
#Ejemplos usando select
# Seleccionar todas las columnas menos el nombre
starwars %>% select(-name)
#Seleccionar sólo las columnas que tienen subraya (_)
starwars %>% select(contains("_"))
#Seleccionar sólo las columnas que empiezan con "s"
starwars %>% select(starts_with("s"))
#Crear un data frame con los nombres y planeta de origen (homeworld)
homeworld <- starwars %>% select(name, homeworld)
#Filtrar datos 
#Filtrar por especies: sólo humanos
human <- starwars %>% filter(species == "Human")
#Filtrar por especies: sólo humanos del planeta Tatooine
starwars %>% filter(species == "Human", homeworld == "Tatooine")
#Crear un nuevo datframe con todas las especies menos los Droides
starwars_nodroids <- starwars %>% filter(species != "Droid")
 
#cumplen todas las condiciones 77obs. 

#Usamos group_by y tally
starwars %>% group_by(species) %>% tally()

#Añadiendo otra variable
starwars %>% group_by(species, gender) %>% tally()

#Si lo quieres guardar en el environment recuerda asignarle un nombre
table_gender <- starwars %>% group_by(species, gender) %>% tally()
starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))

#¿Cómo calcularías la desviación estándar (sd) de esos parámetros?
starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),sd_mass = sd(mass,na.rm = T))


#Hacer un gráfico de la altura vs. la masa de los personajes
ggplot(starwars, aes(height, mass)) + geom_point()

#Puedes modificar el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")

#Modificando el color y el punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)

#Modificando el color y el fondo 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()

#Al crear los gráficos puedes observar que hay un punto que corresponde a un personaje con una masa muy grande. Inspecciona el datset, filtra usando las funciones de tidyverse, crea un nuevo dataframe sin ese personaje y crea de nuevo el gráfico final.
starwars_filtrado <- starwars %>%
  filter(mass != 1358)

ggplot(starwars_filtrado, aes(height, mass)) +
  geom_point(color = "blue", size = 3) 

toy <- read_csv("toy.csv")

# Calcular las medias de las variables agrupadas por sexo
resumen_mediaW <- toy %>%
  filter(Sex == "Women") %>%  # Agrupar por la columna "sexo"
  summarise(
    media_peso = mean(Weight_Kg, na.rm = T),       # Media del peso
    media_altura = mean(Height_cm, na.rm = T),   # Media de la altura
    media_imc = mean(IMC, na.rm = T),         # Media del IMC
    media_ias = mean(IAS, na.rm = T),         # Media del IAS
    media_ccintura = mean(Ccintura, na.rm = T) # Media del CCintura
  )

# Mostrar el resumen de las medias
print(resumen_mediaW)

# Calcular las medias de las variables agrupadas por sexo
resumen_mediaM <- toy %>%
  filter(Sex == "Men") %>%  # Agrupar por la columna "sexo"
  summarise(
    media_peso = mean(Weight_Kg, na.rm = T),       # Media del peso
    media_altura = mean(Height_cm, na.rm = T),   # Media de la altura
    media_imc = mean(IMC, na.rm = T),         # Media del IMC
    media_ias = mean(IAS, na.rm = T),         # Media del IAS
    media_ccintura = mean(Ccintura, na.rm = T) # Media del CCintura
  )

#Haz una tabla sólo con los pacientes femeninos ¿Cuántos registros cumplen las condiciones? ¿De estos cuantos tienen Sobrepeso (Overweight)?  Usa select y filter.
sobrepeso <- toy %>%
  filter(Sex == "Women") %>% filter(IMC_clas=="Overweight")
#9 mujeres tienen sobrepeso

#Haz un gráfico usando ggplot relacionando el IMC (Indice de masa corporal) con el peso (Weight_Kg) de todos los pacientes.
#Repítelo filtrando sólo los pacientes categorizados como "Overweight" y "Obesity".

ggplot(toy, aes(Weight_Kg, IMC)) + geom_point(colour = "red")
obesity_overweight <- toy %>%
  filter(IMC_clas=="Overweight" | IMC_clas=="Obesity")


ggplot(obesity_overweight, aes(Weight_Kg, IMC)) + geom_point(colour = "red")


#nueva actividad
#Utiliza los comandos adecuados para instalar los paquetes de R ape phangorn y phytools que utilizaremos en el laboratorio de la siguiente semana. Carga las librerías y envia un print de pantalla con el output, demostrando que la instalación fue exitosa o si hubo algún problema.
