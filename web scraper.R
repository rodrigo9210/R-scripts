library(rvest)
library(pdftools)
library(stringr)

#1. Obtener links del 2020
link <- "https://www.cenace.gob.mx/Paginas/SIM/EstadoOperativoSEN.aspx"
pagina <- read_html(link)
#las siguientes 3 lineas se usan para sacar los links del año en curso
# links2020 <- pagina %>%
#   html_nodes(xpath = "//td/a") %>% 
#   html_attr("href")

# 1.1 Obtener links del 2019
pgsession <- html_session(link)
pgform <- html_form(pgsession)[[1]]
pgform$fields[[1]]$type <- "button"
filled_form <- set_values(pgform,"ctl00$ContentPlaceHolder1$DrpAnio" = "2018")

pagina <- submit_form(session=pgsession,form=filled_form, POST=url)

links2019 <- pagina %>%
  html_nodes(xpath = "//td/a") %>% 
  html_attr("href")

#links2019 <- head(links2019, -2) #quita los ultimos dos que son de 2018 y 2016

#1.2 Juntar links del 2019 y 2020
#links <- c(links2020, links2019) #usar solo cuando se obtienen los links del año en curso
links <- c(links2019) #usar para sacar solo un año especifico
#links <- head(links, 1100) #recorta links hasta 2020/06/30

#Obtener fechas
fechas <- regmatches(links, regexpr("\\d{4} \\d{2} \\d{2}", links))
fechas <- gsub(" ", "/", fechas, fixed = TRUE)
#fechas <- head(fechas, 1100) #recorta fechas hasta 2020/06/30

#2. Dar formato a URLs relativos
for(i in (1:length(links))){
  links[i] <- paste("https://www.cenace.gob.mx", links[i], sep = "")
  links[i] <- gsub(" ", "%20", links[i], fixed = TRUE)
}


#3. Descargar pdf y obtener texto
#x <- 1030
#y <- 1035
#links_de_prueba <- links[x:y]


textos <- vector(mode="character", length=length(links))
for(i in (1:length(links))){
  download.file(links[i],"tmp.pdf",method="auto",mode="wb")
  textos[i] <- pdf_text("tmp.pdf")
}

#4. Obtener descripciones y formatearlas
Descripcion <- textos

for(i in (1:length(textos))){
  Descripcion[i] <- sub(".*por", '', Descripcion[i])
  #Descripcion[i] <- sub(".*estuvo", '', Descripcion[i])
  Descripcion[i] <- gsub("\r\n", " ", Descripcion[i], fixed = TRUE)
  Descripcion[i] <- sub("www.*", '', Descripcion[i])
  #Descripcion[i] <- sub(". .*", '', Descripcion[i])
}

#4. Dar formato a textos

for(i in (1:length(textos))){
  textos[i] <- gsub("\r\n", " ", textos[i], fixed = TRUE)
  textos[i] <- gsub("por.*","",textos[i]) #descarta el texto despues del estado operativo
}

# 4.1 Obten ID
ID <- gsub("%20", " ", str_sub(links, start= -14), fixed = TRUE)
ID <- gsub(".pdf", "", ID, fixed = TRUE)

#5. Busca Alertas o Emergencias y Regiones
Estado_Operativo <- c()
Control_Regional_1 <- c()
Control_Regional_2 <- c()
for(i in (1:length(textos))){
  
  # Busca coincidencias de alerta o emergencia
  if (grepl("Alerta", textos[i], fixed = TRUE)){
    Estado_Operativo[i] <- "Alerta"
  }
  else {
    Estado_Operativo[i] <- "Emergencia"
  }
  
  contador_region <- 1
  
  #Busca coincidencias de controles regionales
  if (grepl("Baja California", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Baja California"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Baja California"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Baja California Sur", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Baja California Sur"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Baja California Sur"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Norte", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Norte"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Norte"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Noroeste", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Noroeste"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Noroeste"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Noreste", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Noreste"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Noreste"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Occidental", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Occidental"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Occidental"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Central", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Central"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Central"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Oriental", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Oriental"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Oriental"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Peninsular", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Peninsular"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Peninsular"
      contador_region <- contador_region + 1
    }
  }
  
  if (grepl("Mulegé", textos[i], fixed = TRUE)){
    if (contador_region == 1){
      Control_Regional_1[i] <- "Mulegé"
      contador_region <- contador_region + 1
    }
    else if (contador_region == 2){
      Control_Regional_2[i] <- "Mulegé"
      contador_region <- contador_region + 1
    }
  }
  
  #descomentar solo si la longitud de los controles regionales no coincide con la del resto de los arreglos
  # if (contador_region == 1){
  #   Control_Regional_1[i] <- ""
  #   contador_region <- 2
  # }
  
  #Si solo encuentra un control regional deja vacio el espacio correspondiente de la segunda columna
  if (contador_region == 2){
    Control_Regional_2[i] <- ""
  }
  
}

# 6. Preara datos para exportarlos 
datos_finales <- data.frame(fechas, ID, Estado_Operativo, Control_Regional_1, Control_Regional_2, Descripcion)
write.csv(datos_finales,'2018.csv')
