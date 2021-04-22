library(magrittr)
library(scraperlojas)

links <- cea_hierarquia %>%
  purrr::imap_dfr(function(categorias, departamento) {
    purrr::imap_dfr(categorias, function(.x, categoria) {
      pag <- furrr::future_map_dfr(1:50, .progress=TRUE, function(page_number) {
        pag <- cea_url(page_number, departamento, categoria) %>%
          httr::GET() %>%
          cea_produtos_pagina() %>%
          dplyr::mutate(departamento = departamento, categoria = categoria)
        pag
      })
      rlang::inform(glue::glue("Terminou {departamento} & {categoria}."))
      pag
    })
  })


links <- readr::read_rds("data-raw/cea.rds")
b <- chromote::ChromoteSession$new()
b$Browser$setWindowBounds(windowId = b$Browser$getWindowForTarget()$windowId,
                          bounds = list(height = 2000L))
links2 <- links %>%
  dplyr::mutate(links_imgs = purrr::map(links, ~cea_imagens_produto(b, .x)))

readr::write_rds(links2, "data-raw/cea2.rds")

