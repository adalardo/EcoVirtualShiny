ecoVirtualServer <- function(input, output, session) {
    shiny::observe({
    # Este bloco monitora o objeto 'input'
    # Sempre que houver mudança, ele imprime no console do ESS
    ## cat("\n--- Atualização de Parâmetros ---\n")
    ## print(shiny::reactiveValuesToList(input))
  })
    # Ativando os módulos com seus IDs específicos
    popExpServer("popExp_1")
    popLogisticServer("popLog_1")
    bifMapServer("bifMap_1")
    demograStochasticServer("demoStoc_1")
    environmentStochasticServer("envStoc_1")
    popStructuredServer("popStr_1")
    metapopServer("metapop_1")
    #mod_meta_server("meta_1")
}
