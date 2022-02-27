# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

library(httr)
options(digits=5)


# Google API Places - documentacao: https://developers.google.com/maps/documentation/places/web-service/search-nearby
# main URL:       https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-16.680808%2C-49.256245&radius=1500&keyword=restaurant&key=YOUR_API_KEY
# pagination URL: https://maps.googleapis.com/maps/api/place/nearbysearch/json?pagetoken=CpQCAgEAAFxg8o-eU7_uKn7Yqjana-HQIx1hr5BrT4zBaEko29ANsXtp9mrqN0yrKWhf-y2PUpHRLQb1GT-mtxNcXou8TwkXhi1Jbk-ReY7oulyuvKSQrw1lgJElggGlo0d6indiH1U-tDwquw4tU_UXoQ_sj8OBo8XBUuWjuuFShqmLMP-0W59Vr6CaXdLrF8M3wFR4dUUhSf5UC4QCLaOMVP92lyh0OdtF_m_9Dt7lz-Wniod9zDrHeDsz_by570K3jL1VuDKTl_U1cJ0mzz_zDHGfOUf7VU1kVIs1WnM9SGvnm8YZURLTtMLMWx8-doGUE56Af_VfKjGDYW361OOIj9GmkyCFtaoCmTMIr5kgyeUSnB-IEhDlzujVrV6O9Mt7N4DagR6RGhT3g1viYLS4kO5YindU6dm3GIof1Q&key=YOUR_API_KEY

# Tabela de log das requisicoes
columns_log <- c("id", "url", "records", "acum_records", "col_names")
log <- data.frame(matrix(nrow = 0, ncol = length(columns_log))) 
colnames(log) <- columns_log

# Tabela de log das requisicoes paginadas
columns_log <- c("id", "url", "records", "acum_records", "col_names")
log_pag <- data.frame(matrix(nrow = 0, ncol = length(columns_log))) 
colnames(log_pag) <- columns_log

keyword <- "restaurant"
api_token <- "YOUR_API_KEY"

# Cria query_url
query <- list()
for(x in 1:nrow(grid)){
  query[[x]] <- list(
    location = paste(grid[x,1], grid[x,2], sep = ','),
    radius = 1500, type = "", keyword = keyword, key = api_token
  )
}

columns <- c("lat", "lng", "lat_ref", "long_ref", "place_id", "name", "rating", "user_ratings_total", "price_level", "business_status")
dat <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(dat) <- c("lat", "lng", "lat_ref", "long_ref", "place_id", "name", "rating", "user_ratings_total", "price_level", "business_status")

columns_c <- c("price_level", "business_status")

base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json" 
for(x in 1:length(query)) {
  print(paste0("--- consuming ", x, " of ", length(query), " urls."))
  log[x,"id"] <- x
  # Requisicao REST GET
  log[x,"url"] <- paste0(base_url,"?location=",query[[x]]$location,"&radius=",query[[x]]$radius,"&type=",query[[x]]$type,"&keyword=",query[[x]]$keyword,"&key=",query[[x]]$key)
  response <- GET(base_url, query = query[[x]])
  # Obtem response to json
  json <- content(response, 'parsed', simplify = TRUE)
  # Cria database
  as_tibble(json$results$geometry$location) %>% 
    mutate(lat_ref = grid[1,1],
           long_ref = grid[1,2],
           place_id = json$results$place_id,
           name = json$results$name,
           rating = json$results$rating,
           user_ratings_total = json$results$user_ratings_total,
           price_level = json$results$price_level,
           business_status = json$results$business_status
    ) -> dattemp_
  # Criar coluna no df caso API nao tenha retornado valor
  dattemp <- fix_df(dattemp_, columns_c)
  # Grava log
  log[x,"records"] <- nrow(dattemp)
  log[x,"col_names"] <- paste(names(dattemp), collapse = " ") 
  next_page_token <- json$next_page_token
  Sys.sleep(2.8)
  if(!is.null(next_page_token)){
    has_pagination <- TRUE
    next_page_token <- json$next_page_token
    while(has_pagination){
      print(paste0("   >> pagination "))
      Sys.sleep(2.8)  
      log_pag[x, "id"] <- x
      # Requisicao REST GET
      urlp = paste0(base_url,"?pagetoken=",next_page_token,"&key=",api_token)
      log_pag[x,"url"] <- urlp
      response_pagination <- GET(urlp)
      Sys.sleep(2.8)
      json_pagination <- content(response_pagination, 'parsed', simplify = TRUE)
      as_tibble(json_pagination$results$geometry$location) %>% 
        mutate(lat_ref = grid[1,1],
               long_ref = grid[1,2],
               place_id = json_pagination$results$place_id,
               name = json_pagination$results$name,
               rating = json_pagination$results$rating,
               user_ratings_total = json_pagination$results$user_ratings_total,
               price_level = json_pagination$results$price_level,
               business_status = json_pagination$results$business_status
        ) -> dattemp_pag_
      dattemp_pag <- fix_df(dattemp_pag_, columns_c)
      Sys.sleep(2.8)
      # Criar coluna no df caso API nao tenha retornado valor
      dattemp <- rbind(dattemp, dattemp_pag)
      # Grava log
      log_pag[x,"records"] <- nrow(dattemp_pag)
      log_pag[x,"col_names"] <- paste(names(dattemp_pag), collapse = " ")
      log_pag[x,"acum_records"] <- nrow(dattemp)
      # Zera df
      dattemp_pag <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
      dattemp_pag <- colnames(dattemp_pag) <- columns
      
      save.image("dados_google_places.RData")
      
      next_page_token <- json_pagination$next_page_token
      if(!is.null(next_page_token)){
        has_pagination <- TRUE
      } else {
        has_pagination <- FALSE
      }
    }
  }
  dat <- rbind(dat, dattemp)  
  log[x,"acum_records"] <- nrow(dat)
  print(paste0("# =============================== rows fetched ", nrow(dat)))
  print(paste0("# =============================== unique restaurant name's fetched: ", dat %>% select(name) %>% unique() %>% count() %>% as.character()))
  print(tail(dat))
  save.image("dados_google_places.RData")
  
  # Zera df
  dattemp = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(dattemp) <- columns
} 

dat %>% unique() -> dat_unique

rm(list=c('columns_log', 'keyword', 'api_token', 'query', 'columns', 'columns_c', 'response', 'json', 'dattemp_', 'dattemp', 'next_page_token', 'has_pagination', 'urlp', 'response_pagination', 'json_pagination', 'dattemp_pag_', 'dattemp_pag'))
save.image("dados_google_places.RData", 'base_url', 'x')
