library (pacman) #carrega as bibliotecas de forma mais simples

p_load(tidyverse,
       lubridate,
       channelAttribuiton, #atribuição
       plotly,
       tidyquant,
       dtplyr,
       rgdal,
       raster,
       tmap,
       maptools,
       tidyverse,
       broom,
       knitr,
       kableExtra,
       RColorBrewer,
       readxl, #leitura do arquivo xlsx
       stringr,
       dplyr,
       sf, #simple feature
       rgeos,
       adehabitatHR,
       knitr,
       data.table
       )


db_customers <- read_csv("olist_customers_dataset.csv") #Dados dos clientes
db_geo <- read_csv("olist_geolocation_dataset.csv")#data frame localização - Simple Feature?
db_orderitems <- read_csv("olist_order_items_dataset.csv")#pedido carrinho
db_opay <- read_csv("olist_order_payments_dataset.csv")#ordem de pagamento
db_orders <- read_csv("olist_orders_dataset.csv")#processo de compra
db_products <- read_csv("olist_products_dataset.csv")#informações de produtos
db_sellers <- read_csv("olist_sellers_dataset.csv")#Dados dos vendedores
db_category <- read_csv("product_category_name_translation.csv")#sem dados
db_ibge<-read_excel("Lista-de-Municípios-com-IBGE-Brasil.xlsx")

# https://www.youtube.com/watch?v=yw16C8Bq_r0&t=1692s

class(db_customers)
class(db_geo)
class(db_orderitems)
class(db_opay)
class(db_orders)
class(db_products)
class(db_sellers)
class(db_category)

head(db_customers)
head(db_geo)
head(db_orderitems)
head(db_opay)
head(db_orders)
head(db_products)
head(db_sellers)
head(db_category)

#validar a informação de que os proximos tem resultados semelhantes

#Clientes + Geo + Produtos

#1- padronizando

# Funcao para remover acentos de string
RemoveAcentos <- function(textoComAcentos) {

  # Se nao foi informado texto
  if(!is.character(textoComAcentos)){
    on.exit()
  }

  # Letras com acentos
  letrasComAcentos <- "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨'"

  # Letras equivalentes sem acentos
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC      "

  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  )

  # Retorno da funcao
  return(textoSemAcentos)
}

db_ibge$`ConcatUF+Mun`<-RemoveAcentos(db_ibge$`ConcatUF+Mun`) #Removendo acentos da base

#minusculas

db_ibge$`ConcatUF+Mun`<-tolower(db_ibge$`ConcatUF+Mun`)

#Concatenar Estado+Cidade do db_customers

db_customers$customer_state<-tolower(db_customers$customer_state)
db_customers$`ConcatUF+Mun`<- paste(db_customers$customer_state,db_customers$customer_city,sep="")

#2- Unindo as tabelas

db_customers_ibge<-left_join(db_customers,db_ibge,by="ConcatUF+Mun")

db_custfull <- left_join(db_customers_ibge,db_orders,by="customer_id")

db_custfull<-left_join(db_custfull,db_orderitems,by="order_id")

# Selecionando somente os do estado de AC

db_geo_AC<- db_custfull[db_custfull$UF=="AC",] %>% na.omit(db_geo_AC)

#transformando em SF
sf_geo_AC <- st_as_sf(x = db_geo_AC,
                         coords = c("Longitude", "Latitude"),
                         crs = 4326)
class(sf_geo_AC)

#Visualizando no Mapa

tmap_mode("view")
tm_shape(shp = sf_geo_AC) +
  tm_dots(size = 1)

# Selecionando somente os do estado de SP

db_geo_SP<- db_custfull[db_custfull$UF=="SP",] %>% na.omit(db_geo_SP)

#transformando em SF
sf_geo_SP <- st_as_sf(x = db_geo_SP,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)
class(sf_geo_SP)

#Carregando o shapefile do estado de SP
shp_saopaulo <- readOGR("shapefile_sp", "estado_sp")

# Visualização gráfica do objeto shp_saopaulo:
tm_shape(shp = shp_saopaulo) +
  tm_borders()

# Combinando o objeto shp_saopaulo com o objeto db_geo_SP: Mostrar pontos onde aconteceram compras
tm_shape(shp = shp_saopaulo) +
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_geo_SP) +
  tm_dots(size = 0.2)

#A qualidade não ficou boa


#Retomando


db_filtro_sp <- data.table(db_geo_SP)
cidades_SP<-db_filtro_sp$customer_city
preco_SP<-db_filtro_sp$price
lat_sp<-db_filtro_sp$Latitude
lon_sp<-db_filtro_sp$Longitude

db_cidade_valor_sp<-aggregate(preco_SP ~ cidades_SP,data=db_filtro_sp,FUN=sum)

### Cmo incluir as tatitudes e longitude

db_cidade_lat_sp <- aggregate(lat_sp ~ cidades_SP,data=db_filtro_sp,FUN=range)
db_cidade_lon_sp <- aggregate(lon_sp ~ cidades_SP,data=db_filtro_sp,FUN=range)
db_ibge7_sp <- aggregate(IBGE7 ~ cidades_SP,data=db_filtro_sp,FUN=range)

db_sp <- left_join(db_cidade_valor_sp,db_cidade_lat_sp,by="cidades_SP")
db_sp <- left_join(db_sp,db_cidade_lon_sp,by="cidades_SP")
db_sp <- left_join(db_sp,db_ibge7_sp,by="cidades_SP")
db_sp <- rename(db_sp,Latitude="lat_sp",Longitude="lon_sp",CD_GEOCMU="IBGE7",NM_MUNICIP="cidades_SP")


# Carregando o shapefile de SP:
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

# Converter para maiusculo para fazer o left_join

db_sp$NM_MUNICIP <-toupper(db_sp$NM_MUNICIP)

shp_sp@data$NM_MUNICIP <- RemoveAcentos(shp_sp@data$NM_MUNICIP)

shp_sp@data %>%
  left_join(db_sp, by = "NM_MUNICIP") -> shp_sp@data

#Plotando

tm_shape(shp = shp_sp) +
  tm_fill(col = "preco_SP",
          style = "quantile",
          n = 5,
          palette = "plasma",
          legend.hist = TRUE) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)













