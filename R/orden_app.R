#' @title Ordena los elementos de un json en base a la distancia de estos respecto a unas coordenadas de reerencia
#'
#' @description Ordena los elementos de un json en base a la distancia de estos respecto a unas coordenadas de reerencia
#'
#' @param mi_posicion,elemento
#'
#' @return json
#'
#' @examples  orden_app("40.03695469525709,-6.081439474677546","Alojamientos")
#'
#' @import httr
#' jsonlite
#'
#' @export

orden_app <- function(mi_posicion, elemento){

  mi_posicion <- "40.03695469525709,-6.081439474677546"
  mi_posicion <- as.numeric(c(gsub(",.*","",mi_posicion), gsub(".*,","",mi_posicion)))
  elemento <- as.character(elemento)

  id_activo_app <- "b3af8a80-e590-11ec-b4eb-0d97eeef399c"

  # ==============================================================================
  # PETICIÓN TOKENs THB
  # ==============================================================================

  cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
  post <- httr::POST(url = "https://plataforma.plasencia.es/api/auth/login",
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)


  # ==============================================================================
  # PETICIÓN DATOS ATRIBUTO
  # ==============================================================================

  url_thb_fechas <- paste("https://plataforma.plasencia.es/api/plugins/telemetry/ASSET/",id_activo_app,"/values/attributes/SERVER_SCOPE",sep = "")
  peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  # Tratamiento datos. De raw a dataframe
  df_atr <- jsonlite::fromJSON(rawToChar(peticion$content))
  df_puntos <-as.data.frame(df_atr$value$Puntos[df_atr$key == elemento])


  # ==============================================================================
  # CÁLCULO DITANCIAS EUCLIDIANAS
  # ==============================================================================

  distancia_euclidea <- c()
  for (i in 1:nrow(df_puntos)) {
    vector_punto <- df_puntos[i,c(2,3)]
    distancia_euclidea <- c(distancia_euclidea,dist(rbind(mi_posicion,df_puntos[i,c(2,3)])))
  }
  df_puntos$distancia <- distancia_euclidea
  df_puntos <- df_puntos[order(df_puntos$distancia,decreasing = FALSE),]


  # ==============================================================================
  # DEVOLUCIÓN JSON
  # ==============================================================================
  df_puntos_output <- df_puntos$Punto
  json_output <- toJSON(df_puntos_output)
  json_output <- paste('{"Resultado":',json_output,'}',sep = "")

  return(json_output)
}

