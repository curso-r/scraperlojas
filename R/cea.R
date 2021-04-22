cea_url <- function(page_number, departamento, categoria) {
  glue::glue("https://www.cea.com.br/{departamento}/{categoria}?filters=[]&pageNumber={page_number}")
}

cea_hierarquia <- list(
  "moda/feminina" = list(
    "acessorios" = list(),
    "blusas" = list()
  )
)

cea_produtos_pagina <- function(pag) {

  pag <- pag %>%
    httr::content()

  produtos <- pag %>%
    xml2::xml_find_all('//div[contains(@id, "ResultItems_")]/div/ul')

  links <- produtos %>%
    xml2::xml_find_all("//figure/a") %>%
    xml2::xml_attr("href")

  id <- produtos %>%
    xml2::xml_find_all("//div[@class='product-image']") %>%
    xml2::xml_attr("data-productid")

  tibble::tibble(
    id = id,
    links = links
  )
}


get_img <- function(b) {
  tryCatch({
    b$DOM$getDocument() %>%
      { b$DOM$querySelector(.$root$nodeId, "div.cea-cea-store-theme-0-x-product-thumbs.w-100.relative") } %>%
      { b$DOM$getOuterHTML(.$nodeId) } %>%
      { .$outerHTML} %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//img") %>%
      xml2::xml_attr("src")
  }, error = function(e) {
    c()
  })
}

cea_imagens_produto <- function(b, url) {
  b$Page$navigate(url)
  #print(b$view())

  #Sys.sleep(5)
  # while (b$Runtime$evaluate("document.readyState")$result$value != "complete") {
  #   Sys.sleep(0.1)
  # }

  iter <- 0
  while(length(get_img(b)) < 1 && iter < 20) {
    Sys.sleep(1)
    cat(".")
    iter <- iter + 1
  }

  if (iter >= 20) {
    cat(url)
  }

  cat("\n")
  get_img(b)
}









