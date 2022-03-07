library(shiny)
library(shinyWidgets)
library(pdftools)
library(tidyverse)
library(tokenizers)
library(tm)

femap <- read_file("femartpron.txt")
femapw <- tokenize_words(femap, strip_numeric = TRUE)
femapw <- unlist(femapw)

masap <- read_file("mascartpron.txt")
masapw <- tokenize_words(masap, strip_numeric = TRUE)
masapw <- unlist(masapw)

femasus <- read_file('femasus.txt')
femasuspw <- tokenize_words(femasus, strip_numeric = TRUE)
femasuspw <- unlist(femasuspw)

massus <- read_file('massus.txt')
masssuspw <- tokenize_words(massus, strip_numeric = TRUE)
masssuspw <- unlist(masssuspw)

neutrosus <-read_file('neutrosus.txt')
neutrosuspw <- tokenize_words(neutrosus, strip_numeric = TRUE)
neutrosuspw <- unlist(neutrosuspw)


estatutoPersonalAcademico <- read_file('./documentos/epa2019.txt')
estatutoAlumnos <- read_file('./documentos/estalumnos.txt')
estatutoGeneral <- read_file('./documentos/estgeneral.txt')
leyAutonomiaUV <- read_file('./documentos/leyautonomia.txt')
leyOrganica <- read_file('./documentos/leyorganica.txt')
reglamentoAcademias <- read_file('./documentos/racademias.txt')
reglamentoAdquisiciones <- read_file('./documentos/radquisiciones.txt')
reglamentoBibliotecas <- read_file('./documentos/rbibliotecas.txt')
reglamentoCentroIdiomas <- read_file('./documentos/rcenidiomas.txt')
reglamentoConsejoUniversitario <- read_file('./documentos/rCGU.txt')
reglamentoComiteAdquisiciones <- read_file('./documentos/rcomiteadq.txt')
reglamentoComiteObras <- read_file('./documentos/rcomiteobras.txt')
reglamentoControlInmuebles <- read_file('./documentos/rcontrolinmuebles.txt')
reglamentoDefensoriaDerechos <- read_file('./documentos/rdefensoria.txt')
reglamentoDesarrolloPersonal <- read_file('./documentos/rdesapers.txt')
reglamentoEditorial <- read_file('./documentos/reditorial.txt')
reglamentoEntregaRecepcion <- read_file('./documentos/rentregarec.txt')
reglamentoRevalidacion <- read_file('./documentos/requivreva.txt')
reglamentoIgualdadGenero <- read_file('./documentos/rgenero.txt')
reglamentoIdentidadInstitucional <- read_file('./documentos/ridentidad.txt')
reglamentoEstablecimientoDocente <- read_file('./documentos/rincorpestab.txt')
reglamentoIngresosEgresos <- read_file('./documentos/ringresosegresos.txt')
reglamentoJuntaGobierno <- read_file('./documentos/rjuntadegob.txt')
reglamentoMeritoUniversitario <- read_file('./documentos/rmeritouniv.txt')
reglamentoMovilidad <- read_file('./documentos/rmovilidad.txt')
reglamentoObras <- read_file('./documentos/robras.txt')
reglamentoPlaneacionEvaluacion <- read_file('./documentos/rplaneacion.txt')
reglamentoPlanesProgramas <- read_file('./documentos/rplanesyprog.txt')
reglamentoPosgrado <- read_file('./documentos/rposgrado.txt')
reglamentoProMejoras <- read_file('./documentos/rpromejoras.txt')
reglamentoSeguridadInformacion <- read_file('./documentos/rseginfo.txt')
reglamentoServicioSocial <- read_file('./documentos/rservsocial.txt')
reglamentoSustentabilidad <- read_file('./documentos/rsustenta.txt')
reglamentoTabaco <- read_file('./documentos/rtabaco.txt')
reglamentoTransparencia <- read_file('./documentos/rtransparencia.txt')
reglamentoTutorias <- read_file('./documentos/rtutorias.txt')


# Define UI ----
ui <- fluidPage(
  
  
  titlePanel("Analizador de uso de lenguaje con perspectiva de género"),
  
  
    fluidRow(
      
      tabsetPanel(id="tabs", type = "tabs",
        tabPanel('Analizador de textos', id ='text',
          
          column(4,
                 fileInput("documento_pdf", "Escoge documento de texto", accept = ".pdf"),
                 
          ),
          
          column(8, 
                 tableOutput("contents"),
                 tableOutput('contentsSus'),
                 tableOutput("contentsNeutro")
                 
                 
          ),

          
          fluidRow(
            column(2 ,
                   tableOutput("FemTotal"),
                   
            ),
            column(5, 
                   tableOutput("MasTotal"),
                   
            )
          ),
          

          
          fluidRow(
            column(2 ,
                   tableOutput("FemTotalSus"),
                   
            ),
            column(5, 
                   tableOutput("MasTotalSus"),
                   
            )
          ),
          

          fluidRow(
            column(2, 
                   tableOutput('neutroTotal'))
          )
          
        ),
        
        tabPanel('Documentos Universidad Veracruzana', id = 'uvDocu',
          
            
            column(4,
              pickerInput('Documento', 'Documentos Universidad Veracruzana',
                          choices = list(
                            'Estatuto del Personal Académico' = estatutoPersonalAcademico,
                            'Estatuto de los Alumnos' = estatutoAlumnos,
                            'Estatuto General' = estatutoGeneral,
                            'Ley de Autonomía' = leyAutonomiaUV,
                            'Ley Orgánica' = leyOrganica,
                            'Reglamento de Academias por Área de Conocimiento, por Programa Académico y de Investigación' = reglamentoAcademias,
                            'Reglamento para las Adquisiciones, Arrendamientos y Servicios de la Universidad Veracruzana' = reglamentoAdquisiciones,
                            'Reglamento General del Sistema Bibliotecario' = reglamentoBibliotecas,
                            'Reglamento General de Centros de Idiomas y de Autoacceso'= reglamentoCentroIdiomas,
                            'Reglamento del Consejo Universitario General' = reglamentoConsejoUniversitario,
                            'Reglamento del Comité de Adquisiciones, Arrendamientos y Servicios'= reglamentoComiteAdquisiciones,
                            'Reglamento del Comité de Obras' = reglamentoComiteObras,
                            'Reglamento para el Control de Bienes Muebles e Inmuebles' = reglamentoControlInmuebles,
                            'Reglamento de la Defensoría de los Derechos Universitarios' = reglamentoDefensoriaDerechos,
                            'Reglamento del Programa de Desarrollo del Personal Académico a través de Estudios de Posgrado' = reglamentoDesarrolloPersonal,
                            'Reglamento Editorial' = reglamentoEditorial,
                            'Reglamento para el Proceso de Entrega-Recepción' = reglamentoEntregaRecepcion,
                            'Reglamento de Equivalencia y Revalidación de Estudios'= reglamentoRevalidacion,
                            'Reglamento para la Igualdad de Género' = reglamentoIgualdadGenero,
                            'Reglamento de los Elementos de Identidad Institucional' = reglamentoIdentidadInstitucional,
                            'Reglamento de incorporación de establecimientos docentes' = reglamentoEstablecimientoDocente,
                            'Reglamento de Ingresos y Egresos' = reglamentoIngresosEgresos,
                            'Reglamento de la Junta de Gobierno de la Universidad Veracruzana' = reglamentoJuntaGobierno,
                            'Reglamento de Reconocimiento al Mérito Universitario' = reglamentoMeritoUniversitario,
                            'Reglamento de Movilidad' = reglamentoMovilidad,
                            'Reglamento de Obras de la Universidad Veracruzana' = reglamentoObras,
                            'Reglamento de Planeación y Evaluación' = reglamentoPlaneacionEvaluacion,
                            'Reglamento de Planes y Programas de Estudio' = reglamentoPlanesProgramas,
                            'Reglamento General de Estudios de Posgrado 2010' = reglamentoPosgrado,
                            'Reglamento de Comités Pro-Mejoras de las Entidades Académicas' = reglamentoProMejoras,
                            'Reglamento para la Seguridad de la Información' = reglamentoSeguridadInformacion,
                            'Reglamento de Servicio Social' = reglamentoServicioSocial,
                            'Reglamento para la Gestión de la Sustentabilidad' = reglamentoSustentabilidad,
                            'Reglamento de Espacios Universitarios Cien por Ciento Libres de Humo de Tabaco' = reglamentoTabaco,
                            'Reglamento de Transparencia, Acceso a la Información y Protección de Datos Personales' = reglamentoTransparencia,
                            'Reglamento del Sistema Institucional de Tutorías' = reglamentoTutorias
                            
                          ),
                          selected = NULL,
                          multiple = TRUE,
                          options = pickerOptions( 'liveSearch'=T, 'liveSearchStyle'="startsWith", 'virtualScroll'=T, 'maxOptions' = 1),
                          
                )
            ),
            column(8, 
                   tableOutput("contentsUvDoc"),
                   tableOutput('contentsUvDocSus'),
                   tableOutput("contentsUVDocNeutro")
                   
            ),
            fluidRow(
              column(3 ,
                     tableOutput("FemTotalUvDoc"),
                     
              ),
              column(6, 
                     tableOutput("MasTotalUvDoc"),
                     
              )
            ),
            


            
            fluidRow(
              column(3 ,
                     tableOutput("FemTotalUvDocSus"),
                     
              ),
              column(6, 
                     tableOutput("MasTotalUvDocSus"),
                     
              )
            ),
            
            fluidRow(
              column (3,
                      tableOutput("NeutroUvDocs"))
            )
          
        ),
        
        
        tabPanel('Créditos', id = 'uvDocu',

                 fluidRow(
                   column (8,

                           h4(strong("Aplicación desarrollada para el Observatorio de Igualdad de Género de la Universidad Veracruzana")),
                           br(),
                           p("Lic. Célida Paola Buenrostro Grajeda - Universidad Veracruzana"),
                           p(a("Dra. Rocío López-Lara - Universidad Veracruazna", href = "https://scholar.google.com/citations?user=7vVcs2cAAAAJ&hl=es")),
                           p(a("Dr. Rubén Flores González - Universidad Veracruzana", href="https://uv-mx.academia.edu/RubénFloresGonzález")),
                           p("C. José Arturo García López - Ingeniería de Software"),
                           p("C. Rafael Andrade Méndez - Ingeniería de Software"),
                           
                           
                           )
                 )
                 
                 )

        
        
      )
  )
)
  


# Define server logic ----
server <- function(input, output) {
  
  
  observeEvent(input$Documento, {
    documento_txt <- input$Documento
    
    
    
    epaw <- tokenize_words(documento_txt, strip_numeric = TRUE)
    epaw <- unlist(epaw)
    
    totalFem <- contar_palabrasFem(epaw)
    totalMas <- contar_palabrasMas(epaw)
    updateTableFemUvDoc(epaw)
    updateTableMasUvDoc(epaw)
    
    totalFemsus <- contar_palabrasFemSus(epaw)
    totalMassus <- contar_palabrasMasSus(epaw)
    
    totalNeutro <- contar_palabrasNeutros(epaw)

    
    updateTableMasUvDocSus(epaw)
    updateTableFemUvDocSus(epaw)
      
    
    updateTableNeutroUvDocs(epaw)
    
    dataframeSus <- merge(totalFemsus, totalMassus)
    output$contentsUvDocSus <- renderTable(dataframeSus)
    
    dataFrame <- merge(totalFem, totalMas)
    output$contentsUvDoc <- renderTable(dataFrame)
    
    output$contentsUVDocNeutro <-renderTable(totalNeutro)
    
  })
  
  
  
  contar_palabrasFem <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femapw)) %>% 
      dplyr::filter(!is.na(x)) %>%
      summarise("Total de pronombres Femenino" = sum(freq))
    
  }
  
  contar_palabrasFemSus <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femasuspw)) %>% 
      dplyr::filter(!is.na(x)) %>%
      summarise("Total de sustantivos Femenino" = sum(freq))
    
  }
  
  contar_palabrasMas <- function(documento){
    
    sumaMas <- plyr::count(match(documento,masapw)) %>% 
      dplyr::filter(!is.na(x)) %>%
      
      summarise("Total de pronombres masculinos" = sum(freq))
    
  }
  
  contar_palabrasMasSus <- function(documento){
    
    sumaMas <- plyr::count(match(documento,masssuspw)) %>% 
      dplyr::filter(!is.na(x)) %>%
      
      summarise("Total de sustantivos masculinos" = sum(freq))
    
  }
  
  
  contar_palabrasNeutros <- function(documento){
    
    sumaMas <- plyr::count(match(documento,neutrosuspw)) %>% 
      dplyr::filter(!is.na(x)) %>%
      
      summarise("Total de neutros" = sum(freq))
    
  }
  
  updateTableFem <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femapw)) %>%
      dplyr::filter(!is.na(x))
    
    sumaFem_position <- as.data.frame(t(sumaFem[1]))
    femapw_id <- tibble::rowid_to_column(as.data.frame( femapw), 'ID')
    femafilter <- filter(femapw_id, ID %in% sumaFem_position )
    femaTotal <- data.frame(femafilter, sumaFem[2])
    femaDesc <- arrange(femaTotal, desc(freq))
    femaTopDiez <- head(femaDesc, 10)
    
    tablefilter <- subset(femaTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Pronombres Femeninos"
    names(tablefilter)[2] <- "Frecuencia"
    
    
    output$FemTotal <- renderTable(tablefilter)
  }
  
  updateTableFemSus <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femasuspw)) %>%
      dplyr::filter(!is.na(x))
    
    sumaFem_position <- as.data.frame(t(sumaFem[1]))
    femapw_id <- tibble::rowid_to_column(as.data.frame( femasuspw), 'ID')
    femafilter <- filter(femapw_id, ID %in% sumaFem_position )
    femaTotal <- data.frame(femafilter, sumaFem[2])
    femaDesc <- arrange(femaTotal, desc(freq))
    femaTopDiez <- head(femaDesc, 10)
    
    tablefilter <- subset(femaTopDiez, select = -c(ID))

    names(tablefilter)[1] <- "sustantivos Femeninos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$FemTotalSus <- renderTable(tablefilter)
  }
  
  updateTableFemUvDoc <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femapw)) %>%
      dplyr::filter(!is.na(x))
    
    sumaFem_position <- as.data.frame(t(sumaFem[1]))
    femapw_id <- tibble::rowid_to_column(as.data.frame( femapw), 'ID')
    femafilter <- filter(femapw_id, ID %in% sumaFem_position )
    femaTotal <- data.frame(femafilter, sumaFem[2])
    femaDesc <- arrange(femaTotal, desc(freq))
    femaTopDiez <- head(femaDesc, 10)
    
    tablefilter <- subset(femaTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Pronombres Femeninos"
    names(tablefilter)[2] <- "Frecuencia"
    
    
    output$FemTotalUvDoc <- renderTable(tablefilter)
  }
  
  updateTableFemUvDocSus <- function(documento){
    
    sumaFem <- plyr::count(match(documento,femasuspw)) %>%
      dplyr::filter(!is.na(x))
    
    sumaFem_position <- as.data.frame(t(sumaFem[1]))
    femapw_id <- tibble::rowid_to_column(as.data.frame( femasuspw), 'ID')
    femafilter <- filter(femapw_id, ID %in% sumaFem_position )
    femaTotal <- data.frame(femafilter, sumaFem[2])
    femaDesc <- arrange(femaTotal, desc(freq))
    femaTopDiez <- head(femaDesc, 10)
    
    tablefilter <- subset(femaTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Sustantivos Femeninos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$FemTotalUvDocSus <- renderTable(tablefilter)
    
  }
  
  updateTableMas <- function (documento){
    sumaMas <- plyr::count(match(documento,masapw)) %>% 
      dplyr::filter(!is.na(x))
    
    sumaMas_position <- as.data.frame(t(sumaMas[1]))
    masapw_id <- tibble::rowid_to_column(as.data.frame( masapw), 'ID')
    masfilter <- filter(masapw_id, ID %in% sumaMas_position )
    masTotal <- data.frame(masfilter, sumaMas[2])
    masDesc <- arrange(masTotal, desc(freq))
    masTopDiez <- head(masDesc, 10)
    
    tablefilter <- subset(masTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Pronombres Masculinos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$MasTotal <-renderTable(tablefilter)
  }
  
  updateTableMasSus <- function (documento){
    sumaMas <- plyr::count(match(documento,masssuspw)) %>% 
      dplyr::filter(!is.na(x))
    
    sumaMas_position <- as.data.frame(t(sumaMas[1]))
    masapw_id <- tibble::rowid_to_column(as.data.frame( masssuspw), 'ID')
    masfilter <- filter(masapw_id, ID %in% sumaMas_position )
    masTotal <- data.frame(masfilter, sumaMas[2])
    masDesc <- arrange(masTotal, desc(freq))
    masTopDiez <- head(masDesc, 10)
    
    tablefilter <- subset(masTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Sustantivos Masculinos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$MasTotalSus <- renderTable(tablefilter)
  }
  
  updateTableMasUvDoc <- function (documento){
    sumaMas <- plyr::count(match(documento,masapw)) %>% 
      dplyr::filter(!is.na(x))
    
    sumaMas_position <- as.data.frame(t(sumaMas[1]))
    masapw_id <- tibble::rowid_to_column(as.data.frame( masapw), 'ID')
    masfilter <- filter(masapw_id, ID %in% sumaMas_position )
    masTotal <- data.frame(masfilter, sumaMas[2])
    masDesc <- arrange(masTotal, desc(freq))
    masTopDiez <- head(masDesc, 10)
    
    tablefilter <- subset(masTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Pronombres Masculinos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$MasTotalUvDoc <- renderTable(tablefilter)
  }
  
  updateTableMasUvDocSus <- function (documento){
    sumaMas <- plyr::count(match(documento,masssuspw)) %>% 
      dplyr::filter(!is.na(x))
    
    sumaMas_position <- as.data.frame(t(sumaMas[1]))
    masapw_id <- tibble::rowid_to_column(as.data.frame( masssuspw), 'ID')
    masfilter <- filter(masapw_id, ID %in% sumaMas_position )
    masTotal <- data.frame(masfilter, sumaMas[2])
    masDesc <- arrange(masTotal, desc(freq))
    masTopDiez <- head(masDesc, 10)
    
    tablefilter <- subset(masTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Sustantivios Masculinos"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$MasTotalUvDocSus <- renderTable(tablefilter) 
    }
  
  updateTableNeutroUvDocs <- function(documento){
    sumaNeutro <- plyr::count(match(documento,neutrosuspw)) %>% 
      dplyr::filter(!is.na(x))
    

    sumaNeutro_position <- as.data.frame(t(sumaNeutro[1]))
    
    neutropw_id <- tibble::rowid_to_column(as.data.frame( neutrosuspw), 'ID')
    neutrofilter <- filter(neutropw_id, ID %in% sumaNeutro_position )
    neutroTotal <- data.frame(neutrofilter, sumaNeutro[2])
    neutroDesc <- arrange(neutroTotal, desc(freq))
    neutroTopDiez <- head(neutroDesc, 10)
    
    tablefilter <- subset(neutroTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Neutro"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$NeutroUvDocs <- renderTable(tablefilter)
  }
  
  updateTableNeutro <- function(documento){
    sumaNeutro <- plyr::count(match(documento,neutrosuspw)) %>% 
      dplyr::filter(!is.na(x))
    
    sumaNeutro_position <- as.data.frame(t(sumaNeutro[1]))
    neutropw_id <- tibble::rowid_to_column(as.data.frame( neutrosuspw), 'ID')
    neutrofilter <- filter(neutropw_id, ID %in% sumaNeutro_position )
    neutroTotal <- data.frame(neutrofilter, sumaNeutro[2])
    neutroDesc <- arrange(neutroTotal, desc(freq))
    neutroTopDiez <- head(neutroDesc, 10)
    
    tablefilter <- subset(neutroTopDiez, select = -c(ID))
    
    names(tablefilter)[1] <- "Neutro"
    names(tablefilter)[2] <- "Frecuencia"
    
    output$neutroTotal <- renderTable(tablefilter)
  }
  
  output$contents <- renderTable({
    file <- input$documento_pdf
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "pdf", "Debe Seleccionar un archivo pdf"))
    
    documento_txt <- pdftools::pdf_text(file$datapath)
    
    epaw <- tokenize_words(documento_txt, strip_numeric = TRUE)
    epaw <- unlist(epaw)
    
    totalFem <- contar_palabrasFem(epaw)
    totalMas <- contar_palabrasMas(epaw)
    updateTableFem(epaw)
    updateTableMas(epaw)
    
    totalFemsus <- contar_palabrasFemSus(epaw)
    totalMassus <- contar_palabrasMasSus(epaw)
    totalNeutro <- contar_palabrasNeutros(epaw)
    
    
    updateTableMasSus(epaw)
    updateTableFemSus(epaw)
    updateTableNeutro(epaw)
    dataframeSus <- merge(totalFemsus, totalMassus)
    output$contentsSus <- renderTable(dataframeSus)
    output$contentsNeutro <-renderTable(totalNeutro)
    dataFrame <- merge(totalFem, totalMas)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
