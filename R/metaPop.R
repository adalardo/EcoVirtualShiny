####################
## Meta Population
#####################
metapopUI <- function(id) {
  # O NS vem do pacote shiny
    ns <- shiny::NS(id)
    # Todas as funções de layout e widgets são do namespace shiny
    shiny::tagList(
               shiny::titlePanel("Dinâmica de Metapopulação"),
               shiny::sidebarLayout(
                          shiny::sidebarPanel(
                                     shiny::selectInput(ns("model"), "Escolha o Modelo:",
                                                 choices = c("Chuva de Propágulo" = "rain",
                                                             "Colonização Interna" = "internal",
                                                             "Efeito Resgate" = "rescue",
                                                             "Resgate e Colonização Interna" = "both")),
                                     shiny::uiOutput(ns("metaPars")),
                                     shiny::sliderInput(ns("f0"), "Ocupância inicial:", min = 0, max = 1, value = 0.2, step = 0.01),
                                     shiny::sliderInput(ns("tmax"), "Tempo máximo:", min = 10, max = 1000, value = 20, step =10),
                                     shiny::uiOutput(ns("animaUI")),
                                     shiny::hr(),
                                     h5("Configurações da Paisagem"),
                                     shiny::sliderInput(ns("cl"), "Colunas na paisagem:", value = 10, min = 5, max = 50, step = 1),
                                     shiny::sliderInput(ns("rw"), "Linhas na paisagem:", value = 10, min = 5, max = 50, step = 1)
                                      # animate = shiny::animationOptions(interval = 300, loop = FALSE)),
                                    
                                 ),
                          shiny::mainPanel(
                                     shiny::fluidRow(align = "center",
                                                     #style = "display: flex; align-items: center; margin: 0; padding: 0;",
                                                     shiny::column(width = 6, style = "display: flex; ; align-items: center; justify-content: center;", # align-items: flex-end;",
                                                              shiny::plotOutput(ns("plotLand"), width = "75%", height = "350px")),
                                     
                                                shiny::column(width = 6, #style = "padding-right: 2px; padding-left: 2px;",
                                                              shiny::plotOutput(ns("plotOcupa"), width = "100%", height = "400px"))
                                                ),
                                     shiny::tags$br(),
                                     shiny::tags$hr(),
                                     shiny::fluidRow(
                                                shiny::column(width = 12,
                                                              style = "display: flex; justify-content: center; align-items: center; gap: 10px;",
                                                              shiny::actionButton(
                                                                ns("playSim"),  "", 
                                                                icon = shiny::icon("play"), 
                                                                class = "btn-success btn-lg",
                                                                style = "width: 80px; height: 50px;"
                                                                )
                                                              ) 
                                            )
                                 )
                      )
           )
}
metapopServer <- function(id){
    shiny::moduleServer(id, function(input, output, session){
        ns <- session$ns
        anima <- shiny::reactiveVal(FALSE)
        animaInt <- shiny::reactive({
            #print(input$tmax)
            shiny::req(input$tmax)
            tt <- input$tmax
            if(length(tt) == 0 || is.na(tt)){
                return(500)
            }
            if(tt <=50){
                return(500)
            }
            if(tt > 50 && tt <= 100){
                return(200)
            }
            if(tt > 100 ){
                return(100)
            }
            ## if(tt > 1000){
            ##     return(100)
            ## }
        })
        output$metaPars <- shiny::renderUI({
            shiny::req(input$model)
            colInput <- shiny::sliderInput(ns("pi"), "Prob. Colonização (pi):", 
                                  min = 0, max = 1, value = 0.2, step = 0.01)
            extInput <- shiny::sliderInput(ns("pe"), "Prob. Extinção (pe):", 
                                  min = 0, max = 1, value = 0.2, step = 0.01)
            if(input$model %in% c("internal", "both"))
            {
                colInput <- shiny::sliderInput(ns("ci"), "Coeficiente de Colonização (ci):", 
                                    min = 0, max = 5, value = 0.4, step = 0.01)
            }
            if(input$model %in% c("rescue", "both"))
            {
                extInput <- shiny::sliderInput(ns("ce"), "Coeficiente de Extinção (ce):", 
                                                   min = 0, max = 2,value = 0.4, step = 0.01)
            }
            # RETORNA os dois para a UI
            shiny::tagList(colInput, extInput)
        })
        output$animaUI <- shiny::renderUI({
            shiny::req(input$tmax)
            ms <- animaInt()
            shiny::sliderInput(ns("timeNow"), "Progresso da Animação:",
                               min = 1, 
                               max = input$tmax, 
                               value = 1, 
                               step = 1,
                               animate = shiny::animationOptions(interval = ms, loop = FALSE)
                               )
        })
        simData <- shiny::reactive({

            print(paste("pi =", input$pi))
            print(paste("pe =", input$pe))
            print(paste("ci =", input$ci))
            print(paste("ce =", input$ce))

            ## varCol <- if(!is.null(input$ci)) input$ci else input$pi
            ## varExt <- if(!is.null(input$ce)) input$ce else input$pe

            ## print(paste("varCol =", varCol))
            ## print(paste("varExt =", varExt))
            switch(input$model,
                   "rain"     = EcoVirtual::metaPop(cl = input$cl, rw = input$rw, f0 = input$f0, pi = input$pi, pe = input$pe, tmax = input$tmax),
                   "internal" = EcoVirtual::metaCi(cl = input$cl, rw = input$rw, f0 = input$f0, ci = input$ci, pe = input$pe, tmax = input$tmax),
                   "rescue"   = EcoVirtual::metaEr(cl = input$cl, rw = input$rw, f0 = input$f0, pi = input$pi, ce = input$ce, tmax = input$tmax),
                   "both"     = EcoVirtual::metaCiEr(cl = input$cl, rw = input$rw, f0 = input$f0, ci = input$ci, ce = input$ce, tmax = input$tmax)
                   )
        })
        simData <- simData |> shiny::debounce(500)
        shiny::observeEvent(input$model, {
            shiny::req(input$timeNow)
            #print(input$timeNow)
            shiny::req(input$timeNow)
            shiny::updateSliderInput(session, "timeNow", value = 1)
        })
        output$plotLand <- renderPlot({
            shiny::req(simData(), input$timeNow)
            if (length(input$timeNow) == 0 || is.null(input$timeNow)) return(NULL)
            image(simData(), times = input$timeNow)
        })
        output$plotOcupa <- renderPlot({
            shiny::req(simData(), input$timeNow)
            if (length(input$timeNow) == 0 || is.null(input$timeNow)) return(NULL)
            plot(simData(), times = input$timeNow)
        })
        shiny::observeEvent(input$playSim, {
            if (input$timeNow >= input$tmax && !anima()) {
                shiny::updateSliderInput(session, "timeNow", value = 1)
                anima(TRUE)
                shiny::updateActionButton(session, "playSim", 
                                          label = "", 
                                          icon = shiny::icon("pause"))
            }else{
                anima(!anima())
                if(anima())
                {
                    shiny::updateActionButton(session, "playSim", label = "", icon = shiny::icon("pause"))
                }
                if(!anima())
                {
                    shiny::updateActionButton(session, "playSim", label = "", icon = shiny::icon("play"))
                }
            }
        })
        shiny::observe({
            if(anima())
            {
                ms0 <- animaInt()
                shiny::invalidateLater(ms0, session)
                timePlus <- shiny::isolate(input$timeNow + 1)
                if(timePlus <= input$tmax){
                    shiny::updateSliderInput(session, "timeNow", value = timePlus)
                }
                else
                {
                    # Para a animação ao chegar no fim
                    shiny::updateActionButton(session, "playSim", label = "", icon = shiny::icon("sync"))
                    anima(FALSE)
                }
            }
        })
      
    })
}
        
