getStateName <- function(place_id){
  
  api <- 'https://api.inaturalist.org/v1/places'
  call_url <- str_glue('{api}/{place_id}?admin_level=10')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  
  if(get_json_call$total_results == 0) {
    place <- tibble(place_id = place_id,
                    place_state_name = NA)
  } else {
    place <- tibble(place_id = results$id,
                    place_state_name = results$name)
  }
  return(place$place_state_name)
}
getCountryName <- function(place_id){
  
  api <- 'https://api.inaturalist.org/v1/places'
  call_url <- str_glue('{api}/{place_id}?admin_level=0')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  if(get_json_call$total_results == 0) {
    place <- tibble(place_id = place_id,
                    place_country_name = NA)
  } else {
    place <- tibble(place_id = results$id,
                    place_country_name = results$name)
  }
  return(place$place_country_name)
}
getProjectData <- function(project_ids){
  
  project_ids <- str_c(project_ids, collapse=',')
  
  projectData <- tibble(observation_id = numeric(),
                        quality_grade = character(),
                        captive = character(),
                        taxon_name = character(),
                        taxon_rank = character(),
                        taxon_id = numeric(),
                        observations_count = numeric(),
                        conservation_status = character(),
                        threatened = character(),
                        endemic = character(),
                        introduced = character(),
                        iconic_taxa = character(),
                        taxon_common_name = character(),
                        created_at = date(), 
                        observed_on = date(), 
                        time_observed_at = date(),
                        uri = character(),
                        user_login = character(),
                        user_name = character(),
                        user_created_at = character(),
                        user_site_id = numeric(),
                        user_observations_count = numeric(),
                        latitude = numeric(), 
                        longitude = numeric(),
                        place_ids = numeric(),
                        geoprivacy = character())
  
  api <- 'https://api.inaturalist.org/v1/observations'
  
  total_results <- GET(url = str_glue('{api}/?',
                                      'project_id={project_ids}&',
                                      'page=1&',
                                      'per_page=1')) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  total_results <- total_results$total_results
  cat(str_glue('{total_results} observations found in total'), 
      '\n downloading ...\n')
  
  per_page = 200 
  for(page in 1:ceiling(total_results/per_page)) {
    cat(str_glue('page {page} of {ceiling(total_results/per_page)} done'), '\n')
    
    call_url <- str_glue('{api}/?',
                         'project_id={project_ids}&',
                         'page={page}&',
                         'per_page={per_page}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    results <- as_tibble(get_json_call$results)
    projectData_i <- tibble(observation_id = results$id,
                            quality_grade = results$quality_grade,
                            captive = results$captive,
                            taxon_name = results$taxon.name,
                            taxon_rank = results$taxon.rank,
                            taxon_id = results$taxon.id,
                            observations_count = results$taxon.observations_count,
                            #conservation status doesn't exist, is NA
                            conservation_status = 
                              ifelse(exists('taxon.conservation_status.status', 
                                            where=results),
                                     results$taxon.conservation_status.status, NA),
                            threatened = results$taxon.threatened,
                            endemic = results$taxon.endemic,
                            introduced = results$taxon.introduced,
                            iconic_taxa = results$taxon.iconic_taxon_name,
                            taxon_common_name = results$taxon.preferred_common_name,
                            created_at = results$created_at,
                            observed_on = results$observed_on,
                            time_observed_at = results$time_observed_at,
                            uri = results$uri,
                            user_login = results$user.login,
                            user_name = results$user.name,
                            user_created_at = results$user.created_at,
                            user_site_id = results$user.site_id,
                            user_observations_count = results$user.observations_count,
                            geojson.coordinates = results$geojson.coordinates,
                            place_ids = results$place_ids,
                            geoprivacy = results$geoprivacy) %>%
      unnest_wider(geojson.coordinates, names_sep = "_") %>%
      rename(longitude=geojson.coordinates_1, 
             latitude=geojson.coordinates_2) %>% 
      unnest_wider(place_ids, names_sep = "_") %>%
      select(-(num_range('place_ids_', c(3:15)))) 
    
    projectData <- rbind(projectData, projectData_i)
    Sys.sleep(2)
  }
  
  projectData <- projectData %>% 
    mutate(user_site_id = case_when(user_site_id == 28 ~ 'NaturalistaUY',
                                    user_site_id == 1 ~ 'iNaturalist',
                                    user_site_id == 2 ~ 'iNaturalistMX',
                                    user_site_id == 3 ~ 'iNaturalistNZ',
                                    user_site_id == 5 ~ 'iNaturalist.ca',
                                    user_site_id == 6 ~ 'NaturalistaCO',
                                    user_site_id == 8 ~ 'BioDiversity4All',
                                    user_site_id == 9 ~ 'iNaturalistAU',
                                    user_site_id == 13 ~ 'iNaturalistPa',
                                    user_site_id == 14 ~ 'iNaturalistEc',
                                    user_site_id == 15 ~ 'iNaturalistil',
                                    user_site_id == 16 ~ 'ArgentiNat',
                                    user_site_id == 17 ~ 'NaturalistaCR',
                                    user_site_id == 18 ~ 'iNaturalistCL',
                                    user_site_id == 20 ~ 'iNaturalistFi',
                                    user_site_id == 21 ~ 'iNaturalist.Se',
                                    user_site_id == 22 ~ 'Natusfera',
                                    user_site_id == 23 ~ 'iNaturalistGR',
                                    user_site_id == 24 ~ 'iNaturalistGT',
                                    user_site_id == 25 ~ 'iNaturalistUK',
                                    user_site_id == 26 ~ 'iNaturalist.LU',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 29 ~ 'iNaturalistsAfr',
                                    .default = NA))
  
  countryAndState <- projectData %>% 
    distinct(place_ids_1, place_ids_2) %>% 
    mutate(country_name = map_chr(place_ids_1, getCountryName),
           state_name = map_chr(place_ids_2, getStateName))
  
  projectData <- left_join(projectData, countryAndState,
                           by = join_by(place_ids_1, place_ids_2)) %>% 
    select(-c(place_ids_1, place_ids_2))
  
  return(projectData)
}
