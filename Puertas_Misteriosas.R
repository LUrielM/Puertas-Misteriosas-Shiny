library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Usar shinyjs para animaciones
  titlePanel("Puertas Misteriosas"),
  
  # Instrucciones del juego
  h3("Bienvenido a las Puertas Misteriosas: Descubre si puedes ganar un auto, un viaje o dinero"),
  p("Elige el número de puertas y luego selecciona una de ellas. El presentador abrirá algunas puertas vacías antes de preguntarte si deseas cambiar tu elección."),
 
  # Leyenda con el primer paso
  h4("1. Selecciona el número de puertas"),
   
  # Control para editar el número de puertas
  fluidRow(
    column(6, numericInput("num_puertas", "Número de puertas:", 10, min = 3, max = 20)),
    column(6, actionButton("actualizar", "Actualizar", style = "background-color:#3498db; color:white; font-size:16px;"))
  ),
  
  # Texto "Selecciona cualquier puerta"
  h4("2. Selecciona cualquier puerta"),
  
  # Espacio para las imágenes y botones de las puertas (inicialmente vacío)
  fluidRow(
    column(12, 
           uiOutput("puertas_ui")
    )
  ),
  
  # Texto "Gira la ruleta"
  h4("3. Gira la ruleta"),
  
  # Ruleta (Selección de 1, 2 o 3 puertas)
  fluidRow(
    column(12, 
           actionButton("girar_ruleta", "Girar Ruleta", style = "background-color:#9b59b6; color:white; font-size:18px;"),
           h4("La ruleta ha decidido abrir:"),
           textOutput("ruleta_resultado")
    )
  ),
  
  # Botón para revelar puertas
  fluidRow(
    column(12, 
           actionButton("revelar_puertas", "Revelar puertas", style = "background-color:#f39c12; color:white; font-size:18px; display:none;")
    )
  ),
  
  # Mensaje de pregunta para cambiar o continuar
  fluidRow(
    column(12, textOutput("pregunta_cambio")),
    column(12, actionButton("cambiar", "Cambiar elección", style = "background-color:#f39c12; color:white; font-size:18px; display:none;")),
    column(12, actionButton("continuar", "Continuar con mi elección", style = "background-color:#2ecc71; color:white; font-size:18px; display:none;"))
  ),
  
  # Mensajes de juego y puntuación
  fluidRow(
    column(12, 
           h4("Resultado de la ronda:"),
           textOutput("estado_juego"),
           textOutput("mensaje_ganador"),
           actionButton("reiniciar", "Jugar de nuevo", style = "background-color:#e74c3c; color:white; font-size:18px;")
    )
  ),
  
  # Estilo para el texto de la ruleta
  tags$style(HTML("
    #ruleta_resultado {
      font-size: 20px;
      font-weight: bold;
    }
    #estado_juego, #mensaje_ganador {
      font-size: 24px;
      font-weight: bold;
    }
  "))
)

server <- function(input, output, session) {
  
  # Estado inicial de los premios
  premios <- reactiveVal(NULL)
  puerta_elegida <- reactiveVal(NULL)
  puertas_a_abrir <- reactiveVal(0)
  num_puertas <- reactiveVal(0)  # Inicializamos el número de puertas a 0
  
  # Crear las puertas dinámicamente
  output$puertas_ui <- renderUI({
    num_puertas_value <- num_puertas()
    
    # Solo mostrar las puertas si el número de puertas es mayor que 0
    if (num_puertas_value > 0) {
      fluidRow(
        column(12, 
               lapply(1:num_puertas_value, function(i) {
                 div(id = paste0("puerta_div_", i), style = "display: inline-block; margin: 5px;", 
                     actionButton(paste0("puerta_", i), paste("Puerta", i),
                                  style = "width:90px; height:120px; background-color:#3498db; color:white; font-size:16px;"))
               })
        )
      )
    }
  })
  
  # Evento de actualización del número de puertas
  observeEvent(input$actualizar, {
    num_puertas(input$num_puertas)
    
    # Generar los premios solo si es la primera vez o si se reinicia el juego
    premios(sample(c("🚗 Mustang", "🌴 Viaje", "💰 Dinero", rep("🚪 Nada", num_puertas() - 3))))
    
    # Resetear las puertas
    lapply(1:num_puertas(), function(i) {
      shinyjs::runjs(sprintf("$('#puerta_div_%s button').html('Puerta %s'); $('#puerta_div_%s button').css({'background-color': '#3498db'});", i, i, i))
    })
    
    # Ocultar las opciones de cambio
    shinyjs::hide("cambiar")
    shinyjs::hide("continuar")
    
    output$estado_juego <- renderText("Elige una puerta para comenzar.")
    output$mensaje_ganador <- renderText("")
  })
  
  # Evento para cada una de las puertas
  lapply(1:20, function(i) {
    observeEvent(input[[paste0("puerta_", i)]], {
      if (!is.null(puerta_elegida())) { # Asegurarse de que la puerta elegida no sea NULL
        # Si ya se había elegido otra puerta, quitarle el verde
        if (puerta_elegida() != i) {
          shinyjs::runjs(sprintf("$('#puerta_div_%s button').css({'background-color': '#3498db'});", puerta_elegida()))
        }
        
        puerta_elegida(i)
        
        # Marcar la nueva puerta seleccionada en verde
        shinyjs::runjs(sprintf("$('#puerta_div_%s button').css({'background-color': '#2ecc71'});", i))
        
        # Preguntar si girar la ruleta
        output$pregunta_cambio <- renderText("¿Quieres continuar con tu elección o prefieres cambiarla?")
        shinyjs::show("girar_ruleta")
      } else {
        puerta_elegida(i)
        
        # Marcar la puerta elegida en verde
        shinyjs::runjs(sprintf("$('#puerta_div_%s button').css({'background-color': '#2ecc71'});", i))
        
        # Preguntar si girar la ruleta
        output$pregunta_cambio <- renderText("¿Quieres continuar con tu elección o prefieres cambiarla?")
        shinyjs::show("girar_ruleta")
      }
    })
  })
  
  # Girar la ruleta
  observeEvent(input$girar_ruleta, {
    num_puertas_a_abrir <- sample(1:3, 1)
    puertas_a_abrir(num_puertas_a_abrir)
    
    output$ruleta_resultado <- renderText({
      paste(num_puertas_a_abrir, "puerta(s) vacía(s)")
    })
    
    shinyjs::show("revelar_puertas")
  })
  
  # Revelar las puertas
  observeEvent(input$revelar_puertas, {
    puertas_vacias <- which(premios() == "🚪 Nada" & seq_along(premios()) != puerta_elegida())
    puertas_a_abrir <- min(puertas_a_abrir(), length(puertas_vacias))  # No puede haber más puertas que vacías
    
    puertas_a_revelar <- sample(puertas_vacias, puertas_a_abrir)
    lapply(puertas_a_revelar, function(puerta) {
      # Marcar las puertas vacías reveladas en rojo
      shinyjs::runjs(sprintf("$('#puerta_div_%s button').html('🚪 Nada'); $('#puerta_div_%s button').css({'background-color': '#e74c3c'});", puerta, puerta))
    })
    
    # Mostrar botones para cambiar o continuar
    shinyjs::show("cambiar")
    shinyjs::show("continuar")
  })
  
  # Opción de cambiar elección
  observeEvent(input$cambiar, {
    # Cuando se cambia, la puerta elegida se pone en azul
    shinyjs::runjs(sprintf("$('#puerta_div_%s button').css({'background-color': '#3498db'});", puerta_elegida()))
    
    puerta_elegida(NULL)
    shinyjs::hide("cambiar")
    shinyjs::hide("continuar")
    output$pregunta_cambio <- renderText("Elige una nueva puerta.")
  })
  
  # Opción de continuar con la elección
  observeEvent(input$continuar, {
    premio <- premios()[puerta_elegida()]
    if (premio == "🚪 Nada") {
      output$estado_juego <- renderText(paste("Elegiste la Puerta", puerta_elegida(), "y detrás de ella había:", premio))
      output$mensaje_ganador <- renderText("¡Perdiste! No hay nada detrás de esta puerta.")
      
      # Animación de pérdida
      shinyjs::runjs('$("#mensaje_ganador").effect("shake");')
    } else {
      output$estado_juego <- renderText(paste("Elegiste la Puerta", puerta_elegida(), "y detrás de ella había:", premio))
      output$mensaje_ganador <- renderText(paste("¡Felicidades! Has ganado un", premio))
      
      # Animación de victoria
      shinyjs::runjs('$("#mensaje_ganador").effect("bounce", {times:3}, 500);')
    }
    shinyjs::hide("cambiar")
    shinyjs::hide("continuar")
  })
  
  # Botón para reiniciar el juego
  observeEvent(input$reiniciar, {
    premios(NULL)
    puerta_elegida(NULL)
    puertas_a_abrir(0)
    num_puertas(0)
    
    premios(sample(c("🚗 Mustang", "🌴 Viaje", "💰 Dinero", rep("🚪 Nada", num_puertas() - 3))))
    
    lapply(1:num_puertas(), function(i) {
      shinyjs::runjs(sprintf("$('#puerta_div_%s button').html('Puerta %s'); $('#puerta_div_%s button').css({'background-color': '#3498db'});", i, i, i))
    })
    
    output$pregunta_cambio <- renderText("")
    shinyjs::hide("cambiar")
    shinyjs::hide("continuar")
    
    output$estado_juego <- renderText("Elige una puerta para comenzar.")
    output$mensaje_ganador <- renderText("")
  })
}

shinyApp(ui = ui, server = server)
