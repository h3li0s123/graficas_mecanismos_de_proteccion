#### Función para hacer las gráficas 

### Paquetes ----
library(pacman)
p_load(tidyverse, ggplot2, janitor, readxl, viridis)


#### Tema PDP ----

tema_pdp_col <- function(){
  theme(
    plot.title = element_text(hjust = .5,
                              colour = "#002F6C",
                              face = "bold"),
    plot.subtitle = element_text(hjust = .5,
                                 colour = "#002F6C",
                                 face = "bold"),
    plot.caption = element_text(colour = "#002F6C",
                                face = "bold"),
    #panel.background = element_rect(fill = "#A7C6ED"),
    panel.grid = element_line(linetype = 3,
                              color = "black"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    legend.spacing.y = unit(.2, "cm"),
    legend.title = element_text(hjust = .5),
    legend.position = "bottom"
    )
}



tema_pdp_mapa <- function(){
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #legend.position = c(.8, .68),
    plot.title = element_text(hjust = .5,
                              size = 14),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 12)
  )
}





##### Función ----

funcion_graficas <- function(contra_parte, tipo_de_grafica, fecha_de_los_datos, tipo_de_datos){
  #####Opcion si queremos contra parte el MF ----
  
  if (contra_parte == "MF") {
    
    
    #### Opción si queremos un mapa del MF ----
    
    if (tipo_de_grafica == "Mapa"){
      
      #cargamos el mapa 
      mapa <- readRDS(file = "01_datos/mapa2.RDS") %>% 
        clean_names()
      
      #se neceista que el archivo se llama personas_beneficiarias_MF August 2022.xlsx por ejemplo
      bd_edos <- read_excel(paste0("01_datos/personas_beneficiarias_MF ",fecha_de_los_datos,".xlsx")) %>% 
        clean_names()
      
      #limpiamos la bd para que coincida con el mapa
      bd_edos <- bd_edos %>% 
        mutate(entidad = str_replace_all(estado, "Ciudad de México", "CDMX"),
               entidad = str_replace_all(entidad, "Coahuila", "Coahuila de Zaragoza"),
               entidad = str_replace_all(entidad, "Michoacán", "Michoacán de Ocampo"),
               entidad = str_replace_all(entidad, "Veracruz", "Veracruz de Ignacio de la Llave"),
               total = as.numeric(total))
      
      #opción del mapa de cada estado si tiene más hombres o mujeres
      if (tipo_de_datos == "mas genero por entidad"){
        
        #Determinar cuál genero hay más en cada estado
        mayoria_sexo_entidad <- bd_edos %>% 
          mutate(mayoria_sexo = if_else(mujeres>hombres, "Women","Men"))
        
        #Unir bases de datos
        mapa_sexo_mayoria <- merge(mapa, mayoria_sexo_entidad)
        
        
        #Mapa de entidades con más hombres o mujeres beneficiarios
        sf::st_cast(mapa_sexo_mayoria) %>% 
          ggplot(aes(fill = mayoria_sexo))+
          geom_sf()+
          scale_fill_manual(values =  c("#A7C6ED", "#002F6C"))+
          labs(title = "States with more beneficiaries women or men of the NMP",
               caption = paste0("Source: NPM data. ", fecha_de_los_datos),
               fill = "")+
          tema_pdp_mapa()+
          theme(legend.position = c(.1,.2))
        
      }
      
      #Opción del mapa de personas beneficiarias por estado
      else if (tipo_de_datos == "personas beneficiarias por estado"){
        
        #Unir bases de datos
        mapa_personas_benef <- merge(mapa, bd_edos)
        
        #Mapa de personas beneficiarias por estado
        sf::st_cast(mapa_personas_benef) %>% 
          ggplot(aes(fill = total))+
          geom_sf()+
          scale_fill_gradient(low = "#A7C6ED",
                              high = "#002F6C")+
          labs(title = "Número de personas beneficiarias del Mecanismo Federal por estado",
               caption = "Fuente: Datos del MFP de diciembre del 2022.", #paste0("Source: NPM data. ", fecha_de_los_datos),
               fill = "")+
          tema_pdp_mapa()
        
      }
    }
    
    #### Opción si queremos una gráfica del MF de barra ----
    else if (tipo_de_grafica == "Col"){
      
      #Si queremos columanas de num de medidas extra ordinarias
      if (tipo_de_datos == "medidas extra MF"){
        
        #Cargamos la base de datos. El nombre del archivo tiene que ser: num_medidas_MF_anual August 2022.xlsx
        num_medidas_extra_MF <- read_excel(paste0("01_datos/num_medidas_MF_anual ",fecha_de_los_datos,".xlsx"), 
                                           sheet = "medidas_extra", col_types = c("text", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "skip")) %>% 
          clean_names()
        
        #Hacemos la bd a formato long para poder graficar los datos que queremos
        num_medidas_extra_MF_2 <- num_medidas_extra_MF %>% 
          filter(ano_de_ingreso != "Total") %>%
          pivot_longer(cols = -ano_de_ingreso,
                       names_to = "tipo_medidas",
                       values_to = "n")
        
        #Obtenemos el máximo para la grafica
        lim_superior <- num_medidas_extra_MF_2 %>% 
          group_by(ano_de_ingreso) %>% 
          summarise(suma = sum(n)) %>% 
          summarise(maximo = max(suma)) %>% 
          mutate(maximo = maximo +100)
        
        #Graficamos
        num_medidas_extra_MF_2 %>% 
          ggplot(aes(as_factor(ano_de_ingreso), n, fill = tipo_medidas))+
          geom_col(col = "black")+
          scale_y_continuous(expand = c(0,0),
                             limits = c(0, lim_superior$maximo[1]))+
          scale_fill_viridis(discrete = T, option = "C")+
          labs(title = "Number of extraordinary protection measures provided by the NPM",
               subtitle = "2013-2022",
               caption = paste0("Source: Data from NPM. ",fecha_de_los_datos),
               x = "Year",
               y = "Number of protection measures",
               fill = "Type of protection measure")+
          theme_bw()+
          tema_pdp_col()+
          theme(legend.position = "right")+
          guides(fill = guide_legend(byrow = TRUE))
        
        
        
      }
      
      #Opción si queremos grafica de medidas ordinarias para el MF
      else if (tipo_de_datos == "medidas ordi MF"){
        
        #Cargamos la base de datos. El nombre de ser num_medidas_MF_anual August 2022.xlsx
        num_medidas_ordinarias_MF <- read_excel(paste0("01_datos/num_medidas_MF_anual ",fecha_de_los_datos,".xlsx"), 
                                                sheet = "medidas_ordinarias", col_types = c("text", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "skip")) %>% 
          clean_names()
        
        #Arreglamos la bd
        num_medidas_ordinarias_MF <- num_medidas_ordinarias_MF %>% 
          filter(ano_de_ingreso != "Total") %>%
          pivot_longer(cols = -ano_de_ingreso,
                       names_to = "tipo_medidas",
                       values_to = "n") %>%
          filter(tipo_medidas != "otras_1")
        
        #Obtenemos el máximo para la grafica
        lim_superior <- num_medidas_ordinarias_MF %>% 
          group_by(ano_de_ingreso) %>% 
          summarise(suma = sum(n)) %>% 
          summarise(maximo = max(suma)) %>% 
          mutate(maximo = maximo +100)
        
        
        #Graficamos
        num_medidas_ordinarias_MF %>% 
          ggplot(aes(as_factor(ano_de_ingreso), n, fill = tipo_medidas))+
          geom_col(col = "black")+
          scale_y_continuous(expand = c(0,0),
                             limits = c(0, lim_superior$maximo[1]))+
          scale_fill_viridis(discrete = T, option = "C")+
          labs(title = "Number of ordinary protection measures provided by the NPM",
               subtitle = "2013-2022",
               caption = paste0("Source: Data from NPM. ", fecha_de_los_datos),
               x = "Year",
               y = "Number of protection measures",
               fill = "Type of protection measure")+
          theme_bw()+
          tema_pdp_col()+
          theme(legend.position = "right")+
          guides(fill = guide_legend(byrow = TRUE))
        
         
      }
      
      #Opción si queremos una grafica de las medidas totales emitidas por el MF
      else if (tipo_de_datos == "total medidas MF"){
        
        #Cargamos la base de datos. El nombre del archivo tiene que ser: num_medidas_MF_anual August 2022.xlsx
        num_medidas_extra_MF <- read_excel(paste0("01_datos/num_medidas_MF_anual ",fecha_de_los_datos,".xlsx"), 
                                           sheet = "medidas_extra", col_types = c("text", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric", "skip")) %>% 
          clean_names()
        
        #Hacemos la bd a formato long para poder graficar los datos que queremos
        num_medidas_extra_MF_2 <- num_medidas_extra_MF %>% 
          filter(ano_de_ingreso != "Total") %>%
          pivot_longer(cols = -ano_de_ingreso,
                       names_to = "tipo_medidas",
                       values_to = "n")
        
        #BD de medidas ordinarias
        num_medidas_ordinarias_MF <- read_excel(paste0("01_datos/num_medidas_MF_anual ",fecha_de_los_datos,".xlsx"), 
                                                sheet = "medidas_ordinarias", col_types = c("text", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", 
                                                                                            "skip")) %>% 
          clean_names()
        
        #Modificamos la
        num_medidas_ordinarias_MF <- num_medidas_ordinarias_MF %>% 
          filter(ano_de_ingreso != "Total") %>%
          pivot_longer(cols = -ano_de_ingreso,
                       names_to = "tipo_medidas",
                       values_to = "n") %>%
          filter(tipo_medidas != "otras_1")
        
        #Modificamos las BD para lo queremos
        total_num_medidas_extra_MF <- num_medidas_extra_MF_2 %>% 
          group_by(ano_de_ingreso) %>% 
          summarise(total_extra = sum(n)) %>% 
          ungroup()
        
        #Modificamos la BD para lo que queremos
        total_num_medidas_ordinarias_MF <- num_medidas_ordinarias_MF %>% 
          group_by(ano_de_ingreso) %>% 
          summarise(total_ordi = sum(n)) %>% 
          ungroup()
        
        #Unimos las dos BD y modificamos los datos
        total_medidas_MF <- merge(total_num_medidas_extra_MF, total_num_medidas_ordinarias_MF) %>% 
          rename(medidas_extraordinarias = "total_extra", 
                 medidas_ordinarias = "total_ordi") %>% 
          mutate(medidas_extraordinarias = as.numeric(medidas_extraordinarias),
                 medidas_ordinarias = as.numeric(medidas_ordinarias)) %>% 
          pivot_longer(cols = -ano_de_ingreso,
                       values_to = "total_medidas",
                       names_to = "tipo_medidas")
        
        #Obtenemos el limite superior para la grafica
        lim_superior <- total_medidas_MF %>% 
          group_by(ano_de_ingreso) %>% 
          summarise(suma = sum(total_medidas)) %>% 
          summarise(maximo = max(suma)) %>% 
          mutate(maximo = maximo +100)
        
        #Graficamos  
        total_medidas_MF %>% 
        ggplot(aes(as_factor(ano_de_ingreso), total_medidas, fill = tipo_medidas))+
          geom_col(col = "black")+
          scale_y_continuous(expand = c(0,0),
                             limits = c(0, lim_superior$maximo[1]))+
          scale_fill_manual(values = c("#002F6C", "#A7C6ED"),
                            labels = c("Medidas extraordinarias", 
                                       "Medidas ordinarias"))+
          labs(title = "Number of protection measures provided by the NPM",
               subtitle = "2013-2022",
               caption = paste0("Source: Data from NPM. ", fecha_de_los_datos),
               x = "Year",
               y = "Number of protection measures",
               fill = "")+
          theme_bw()+
          tema_pdp_col()+
          guides(fill = guide_legend(byrow = TRUE))
        
        
      }
      
      # Opción si queremos una grafica del perfil de beneficiarios actuales
      else if (tipo_de_datos == "perfil benef MF"){
        
        #Cargamos la bd. Nombre del archivo: benef_actuales_por_año_y_sexo Agust 2022.xlsx
        benef_actuales_sexo <- read_excel(paste0("01_datos/benef_actuales_por_año_y_sexo ",fecha_de_los_datos,".xlsx"), 
                                          col_types = c("text", "numeric", "numeric", 
                                                        "numeric", "numeric"))
        #Obtenemos el total de beneficarios de ese año
        num_beneficiarios_MF <- benef_actuales_sexo %>% 
          mutate(total = `mujeres defensoras`+`hombres periodistas`+`mujeres periodistas`+`hombres defensores`) %>% 
          summarise(total_todo = sum(total))
        
        #Obtenemos el limite superior para la grafica
        lim_superior <- benef_actuales_sexo %>% 
          mutate(total = `mujeres defensoras`+`hombres periodistas`+`mujeres periodistas`+`hombres defensores`) %>% 
          summarise(maximo = max(total)) %>% 
          mutate(maximo = maximo +100)
        
        ##Arreglamos la bd como se necesita y graficamos
        benef_actuales_sexo %>% 
          pivot_longer(cols = -año,
                       values_to = "n",
                       names_to = "sexo_trabajo") %>% 
          ggplot(aes(as_factor(año), n, fill = sexo_trabajo))+
          geom_col(col = "black")+
          scale_y_continuous(expand = c(0,0),
                             limits = c(0, lim_superior$maximo[1]))+
          scale_fill_viridis(discrete = T, option = "C")+
          labs(title = "Profile of the current beneficiaries of the NPM",
               subtitle = paste0("Total current beneficiaries: ",num_beneficiarios_MF$total_todo[1]),
               caption = paste0("Source: Data from NPM. ",fecha_de_los_datos),
               x = "Year of incorporation",
               y = "Number of beneficiaries",
               fill = "")+
          theme_bw()+
          tema_pdp_col()+
          theme(legend.position = "right")+
          guides(fill = guide_legend(byrow = TRUE))
          
      }
      
    }
    
  }
  
  #### Opcion si queremos contra parte el MPI ----
  else if (contra_parte == "MPI"){
    
    ##Solo tenemos graficas de col en el MPI
    if (tipo_de_grafica == "Col"){
      
      #Opcion si queremos el num de incorporaciones del MPI al año
      if (tipo_de_datos == "num incor MPI"){
        
        #Cargamos la BD. Nombre: beneficiarios_anual_mpi August 2022.xlsx
        mpi <- read_excel(paste0("01_datos/beneficiarios_anual_mpi ",fecha_de_los_datos,".xlsx")) %>% 
          clean_names()
        
        #Ordenamos la bd
        num_incorporaciones_anual <- mpi %>% 
          filter(ano != "Totales") %>% 
          mutate(ano = as_factor(ano))
        
        #Obtenemos el limite superior para la grafica
        lim_superior <- num_incorporaciones_anual %>% 
          select(total_todo) %>% 
          summarise(maximo = max(total_todo)) %>% 
          mutate(maximo = maximo + 10)
        
        #Graficamos
        num_incorporaciones_anual %>% 
          filter(ano != "2016" & ano != "2017") %>% 
          ggplot(aes(ano, total_todo, fill = ano))+
          geom_text(aes(label = total_todo, y = total_todo),
                    vjust = -1)+
          geom_col(fill = "#002F6C")+
          scale_y_continuous(expand = c(0, 0),
                             limits = c(0, lim_superior$maximo[1]))+
          labs(title = "Number of Incorporations of the MPI",
               subtitle = "2018-2023",
               caption = paste0("Source: Data from MPI. ", fecha_de_los_datos),
               x = "Year",
               y = "Beneficiaries")+
          theme_bw()+
          tema_pdp_col()+
          theme(legend.position = "")
        
      }
      
      #Opción si queremos las medidas emitidas por el MPI
      else if (tipo_de_datos == "total medidas MPI"){
        
        print("Ver la grafica ya hecha en lo que sale el nuevo informe")
        
        
      }
      
    }
    
    else if(tipo_de_grafica == "Mapa") {
      print("Ingresar alguna otra opción. No hay mapas para el MPI")
    }
  }
  
}


  #### Ejemplos MF ---- 

#Mapa de personas beneficiarias MF por estado 

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Mapa",
                 fecha_de_los_datos = "December 2022",
                 tipo_de_datos = "personas beneficiarias por estado")

#Mapa de estados con mayor sexo beneficarios del MF

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Mapa",
                 fecha_de_los_datos = "December 2022",
                 tipo_de_datos = "mas genero por entidad")

#Grafica de medidas extra ordinarias del MF

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "December 2022",
                 tipo_de_datos = "medidas extra MF")

#Grafica de medidas ordinarias del MF

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "August 2022",
                 tipo_de_datos = "medidas ordi MF")

#Grafica del total de  medidas del MF

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "December 2022",
                 tipo_de_datos = "total medidas MF")

#Grafica del total de  medidas del MF

funcion_graficas(contra_parte = "MF", 
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "December 2022",
                 tipo_de_datos = "perfil benef MF")

#Grafica del numero de incorporaciones del MPI

funcion_graficas(contra_parte = "MPI",
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "March 2023",
                 tipo_de_datos = "num incor MPI")

#Grafica del numero de incorporaciones del MPI

funcion_graficas(contra_parte = "MPI",
                 tipo_de_grafica = "Col",
                 fecha_de_los_datos = "December 2021",
                 tipo_de_datos = "total medidas MPI")

