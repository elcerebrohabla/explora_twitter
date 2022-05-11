# Instalar el paquete tweetbotornot (Tuvimos que subirlo a Github porque el código de origen tenía un error)
library(devtools)
install_github("elcerebrohabla/tweetbotornot")

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)
# Paquetes
library(pacman)
p_load(readxl, tidyverse, dplyr, cowplot, janitor, lmtest, 
     sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, 
     hexbin, janitor, mosaicData, scales, ggthemes, tweetbotornot, rtweet,
     lubridate, hms, tidytext, wordcloud2, tm, SnowballC, htmlTable, kableExtra,
     magick, magrittr, scales)

#ÃPI de Twitter
token <- create_token(
  "My Application Name",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = ""
)

get_token()

#FUNCIONES

# Stopwords ----
custom_stop_words <- as_data_frame(tm::stopwords("es")) %>% 
  bind_rows(as_data_frame(c(
    "si","dijo","así","sólo", "dice", "pues","entonces",
    "ahí","digo","creo","que","en","la","ah","bueno", "bla","tan",
    "te", "iba", "he", "él", "t", "+", "de", "cómo", "su", "https", "t.co"
  ))) %>% 
  dplyr::rename(palabra = value)

stopwords <- as.matrix(custom_stop_words)

#datos_revocacion <- read_excel("01_datos/datos_revocacion.xlsx")

#Tweets de revocación
datos_revocacion <- search_tweets(q = "Revocación", n = 300000)

#Seleccionar columnas
revocacion <- datos_revocacion %>% 
  select(screen_name, text, source, favorite_count, retweet_count, followers_count, account_created_at)
 
revocacion 

etapa1 <- revocacion %>% 
  select(text, screen_name, source, account_created_at, followers_count) %>%
  #Agrupamos por cadena de texto para encontrar los tuits que están siendo replicados
  #por varias cuentas y què cuentas los estàn replicando.
  group_by(text) %>% 
  mutate(n= n()) %>% 
  arrange(desc(n)) %>% 
  #Los tuits deben haber sido replicados màs de 3 veces
  filter(n > 3) %>% 
  #Los usuarios deben tener menos de 30 followers
  filter(followers_count < 30) %>% 
  #Y su cuenta debió crearse este año
  filter(account_created_at > "2022-01-01") %>% 
  ungroup()

follow <- revocacion %>% 
  group_by(screen_name, followers_count) %>% 
  summarize(n=n()) %>% 
  arrange(desc(followers_count)) %>% 
  filter(n > 1) 
follow

personas <- revocacion %>% 
  group_by(screen_name) %>% 
  summarize(count = n()) %>% 
  filter(count > 0) %>% 
  arrange(desc(count))

personas

#Lista de los tuits más replicados
textos_etapa_1 <- revocacion %>% 
  group_by(text) %>% 
  summarize(count = n()) %>% 
  filter(count > 0) %>% 
  arrange(desc(count))

view(textos_etapa_1)

etapa2 <- etapa1 %>% 
  select(screen_name, account_created_at) %>%  
  group_by(screen_name) %>% 
  #Agrupamos por usuario y contamos cuàntas veces dichos usuarios replicaron
  #los tuits anteriores
  mutate(n= n()) %>% 
  distinct(screen_name, .keep_all=TRUE) %>% 
  arrange(desc(n))

etapa2

#Para finalizar, utilizamos un paquete llamado botornot que te arroja la probabilidad
#de que una cuenta sea un bot. No es perfecto, pero es útil utilizarlo después
#de haber hecho los pasos anteriores.


#Seleccionamos la columna con los nombres de usuario
etapa3 <- etapa2 %>% 
  select(screen_name)

#Convertimos el tibble en una matriz para que el paquete pueda procesar las 
#cuentas
lista <- as.matrix(etapa3)

#Ejecutamoos el paquete
bots <- tweetbotornot(lista)

#Ordenamos los resultados por probabilidad de mayor a menor, y convertimos
#el resultado en porcentaja para mayor legibilidad
bots_final <- bots %>% 
  mutate(prob_bot = percent(prob_bot) ) %>% 
  arrange(desc(prob_bot))


#Analizar cuentas

glimpse(tmls)
view(tmls)
tmls

tmls <- get_timeline(c("elcerebrohabla"), n = 20000, include_rts = FALSE)

cuentas <- read_csv("01_datos/callo.csv")


ruzzarin <- read_csv("01_datos/ruzzarin.csv")
master <- read_csv("01_datos/carlosmunoz.csv")



cuentas <- rbind(ruzzarin, master)
glimpse(cuentas)


cuentas

procesado <- cuentas %>% 
  filter(is.na(mencionados)) %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE) %>% 
  dplyr::select(date, time, Text, User, like, tuiteado, Datetime) %>% 
  mutate(date = as.Date(date))

procesado

promedio <- procesado %>% 
  group_by(date) %>% 
  summarize(promedio_likes = mean(favorite_count))

procesado %>% 
  ggplot(aes(date, like, color=User)) +
  geom_point()+
  #geom_smooth(se = F) +
  labs(title = 'Promedio likes por tweet\nDiego Ruzzarin vs Carlos Muñoz',
       subtitle = '',
       caption = "Desarrollado por @elcerebrohabla\nFuente: API de TWitter",
       x = "Fecha",
       y = "Likes")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  scale_x_date(labels = date_format("%m-%Y"))+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size= 14))
  #scale_y_continuous(trans='log10')

procesado

promedio %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(date, promedio_likes)) +
  geom_smooth(se = FALSE)+
  geom_point()

glimpse(promedio)  

#BIGRAMAS

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
  }

bigramas <- cuentas %>% 
  dplyr::select(Text) %>% 
  #mutate(texto = limpiar(cuentas)) %>%
  #select(texto) %>%
  unnest_tokens(input = Text, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  unite(palabra1, palabra2,col="bigrama",sep=" ") %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(n)) 

bigramas %>% 
  top_n(100) %>% 
  kbl() %>%
  kable_styling()

#Wordcloud
cuentas %>% 
  select(Text) %>% 
  unnest_tokens(input = Text, output = "text",
                token = "ngrams",n = 1, drop = TRUE) %>% 
  group_by(text) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!text %in% stopwords) %>% 
  arrange(desc(n)) %>% 
  wordcloud2(size=2.5, color='random-dark')

cuentas %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE)  %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  count() %>% 
  ggplot(aes(date, n)) +
  geom_line()+
  geom_point()

total <- cuentas %>% 
  group_by(User) %>% 
  count() %>% 
  arrange(desc(n))

cuentas

mas_populares <- cuentas %>% 
  arrange(desc(like)) %>% 
  dplyr::select(Text, User, like) %>% 
  top_n(100) %>% 
  kbl() %>%
  kable_styling()


mas_populares

tidyHtmlTable(mas_populares)

