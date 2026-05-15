# ------------------------------------------------------------------ #
# |   Brazilian Institute for Applied Economic Research - Ipea     | #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# |   Author: Luiz Eduardo S. Gomes                                | #
# |   Coordinator: Erivelton P. Guedes                             | #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# |   An R package for Ipeadata API database                       | #
# |   Version: 0.2.0                                               | #
# |   January 09, 2026                                             | #
# ------------------------------------------------------------------ #

# Available territories -------------------------------------------------------

#' @title Available territorial divisions
#'
#' @description Returns a list of available Brazilian territorial divisions
#' from the Ipeadata API.
#'
#' @usage available_territories(language = c("en", "br"))
#'
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#'
#' @return A data frame containing the unit type, code, name, and area (in km²)
#'   of Brazilian territorial divisions.
#' 
#' @export
#' 
#' @importFrom magrittr %>%

available_territories <- function(language = c("en", "br")) {
  
  # Check language arg
  language <- match.arg(language)
  
  # URL for territories
  url <- 'https://www.ipeadata.gov.br/api/odata4/Territorios/'
  
  # Output NULL
  territories <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    tryCatch({
      
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Remove NA >
      ##           Sort by uname >
      ##           Rename variables >
      ##           Add subtitles
      territories <- jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
        dplyr::as_tibble() %>%
        dplyr::select(
          NIVNOME, TERCODIGO, TERNOME, TERAREA
        ) %>%
        dplyr::filter(!is.na(TERAREA)) %>%
        dplyr::arrange(TERCODIGO)
      
    }, error = function(e) {
    rlang::abort(
      "Failed to retrieve data from the Ipeadata API.",
      class = "ipeadata_api_error",
      parent = e
    )
  })
    
    # Setting labels in selected language
    if (!is.null(territories)) {
      
      # Setting labels in selected language
      if (language == 'en') {
        
        territories <- territories %>%
          dplyr::mutate(
            NIVNOME = iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT'),
            NIVNOME = factor(
              NIVNOME,
              levels = df_nivnome$nivnome,
              labels = df_nivnome$nivnome_en
            )
          ) %>%
          dplyr::arrange(NIVNOME) %>%
          purrr::set_names(c('uname', 'tcode', 'tname', 'area')) %>%
          sjlabelled::set_label(c(
            'Territorial Unit Name', 'Territorial Code',
            'Territorial Name', 'Area (Km2)'
          ))
        
      } else {
        
        aux_ter <- territories %>% 
          dplyr::select(NIVNOME, TERCODIGO) %>% 
          purrr::set_names(c('NIVNOME0', 'TERCODIGO'))
        
        territories <- territories %>%
          dplyr::mutate(
            NIVNOME = iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT'),
            NIVNOME = factor(NIVNOME, levels = df_nivnome$nivnome)
          ) %>%
          dplyr::arrange(NIVNOME) %>% 
          dplyr::left_join(
            aux_ter, by = "TERCODIGO",
            relationship = "many-to-many"
          ) %>% 
          dplyr::select(NIVNOME0, TERCODIGO, TERNOME, TERAREA) %>% 
          dplyr::mutate(NIVNOME0 = as.factor(NIVNOME0)) %>% 
          purrr::set_names(c('uname', 'tcode', 'tname', 'area')) %>%
          sjlabelled::set_label(c(
            'Nome da Unidade Territorial', 'Codigo Territorial',
            'Nome do Territorio', 'Area (Km2)'
          ))
        
      }
      
    }
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  # Output
  return(territories)
}

# Available subjects ----------------------------------------------------------

#' @title Available subjects
#'
#' @description Returns a list of available subjects from the Ipeadata API.
#'
#' @usage available_subjects(language = c("en", "br"))
#'
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#'
#' @return A data frame containing the code and name of available subjects.
#'
#' @export

available_subjects <- function(language = c("en", "br")) {
  
  # Check language arg
  language <- match.arg(language)
  
  # URL for themes
  url <- 'https://www.ipeadata.gov.br/api/odata4/Temas/'
  
  # Output NULL
  subjects <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    tryCatch({
      
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Sort by code >
      ##           Transform in chr
      subjects <- jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
        dplyr::as_tibble() %>%
        dplyr::select(TEMCODIGO, TEMNOME) %>%
        dplyr::arrange(TEMCODIGO) %>%
        dplyr::mutate(TEMNOME = as.character(TEMNOME))
      
    }, error = function(e) {
      rlang::abort(
        "Failed to retrieve data from the Ipeadata API.",
        class = "ipeadata_api_error",
        parent = e
      )
    })
    
    # Setting labels in selected language
    if (!is.null(subjects)) {
      
      # Setting labels in selected language
      if (language == 'en') {
        
        subjects <- subjects %>%
          dplyr::mutate(
            TEMNOME = iconv(TEMNOME, 'UTF-8', 'ASCII//TRANSLIT'),
            TEMNOME = factor(
              TEMNOME,
              levels = df_temnome$temnome,
              labels = df_temnome$temnome_en
            ),
            TEMNOME = as.character(TEMNOME)
          ) %>%
          purrr::set_names(c('scode', 'sname')) %>%
          sjlabelled::set_label(c('Subject Code', 'Subject Name'))
        
      } else {
        
        subjects <- subjects %>%
          purrr::set_names(c('scode', 'sname')) %>%
          sjlabelled::set_label(c('Codigo do Tema', 'Nome do Tema'))
        
      }
      
    }
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  # Output
  return(subjects)
}

# Available series ------------------------------------------------------------

#' @title Available series
#'
#' @description Returns a list of available series from the Ipeadata API.
#'
#' @usage available_series(language = c("en", "br"))
#'
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#'
#' @return A data frame containing the Ipeadata code, name, theme, source,
#'   frequency, last update, and activity status of available series.
#'
#' @note The original language of the available series names is preserved.
#'
#' @export

available_series <- function(language = c("en", "br")) {

  # Check language arg
  language <- match.arg(language)

  # URL for metadata
  url <- 'https://www.ipeadata.gov.br/api/odata4/Metadados/'
  
  # Output NULL
  series <- NULL
  
  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    tryCatch({
      
      ## Starting: Extract from JSON >
      ##           Transform to tbl >
      ##           Select variables >
      ##           Sort by source, freq and code >
      ##           Transform in factor >
      ##           Transform in date >
      series <- jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
        dplyr::as_tibble() %>%
        dplyr::select(
          SERCODIGO, SERNOME, BASNOME, FNTSIGLA, PERNOME, 
          SERATUALIZACAO, SERSTATUS
        ) %>%
        dplyr::arrange(
          BASNOME, FNTSIGLA, PERNOME, SERCODIGO
        ) %>%
        dplyr::mutate(
          FNTSIGLA = as.factor(FNTSIGLA),
          SERATUALIZACAO = lubridate::as_date(SERATUALIZACAO),
          SERSTATUS = as.character(SERSTATUS),
          SERSTATUS = dplyr::if_else(is.na(SERSTATUS), '', SERSTATUS)
        )
      
    }, error = function(e) {
      rlang::abort(
        "Failed to retrieve data from the Ipeadata API.",
        class = "ipeadata_api_error",
        parent = e
      )
    })
    
    # Setting labels in selected language
    if (!is.null(series)) {
      
      if (language == 'en') {
        
        series <- series %>% 
          dplyr::mutate(
            SERSTATUS = factor(
              SERSTATUS,
              levels = c('A', 'I', ''),
              labels =  c('Active', 'Inactive', '')
            ),
            PERNOME = iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT'),
            PERNOME = factor(
              PERNOME,
              levels = df_pernome$pernome,
              labels = df_pernome$pernome_en
            ),
            BASNOME = iconv(BASNOME, 'UTF-8', 'ASCII//TRANSLIT'),
            BASNOME = factor(
              BASNOME,
              levels = c('Macroeconomico', 'Regional', 'Social'),
              labels = c('Macroeconomic', 'Regional', 'Social')
            )
          ) %>%
          purrr::set_names(c(
            'code', 'name', 'theme', 'source', 
            'freq', 'lastupdate', 'status'
          )) %>%
          sjlabelled::set_label(c(
            'Ipeadata Code', 'Serie (PT-BR)', 'Theme', 
            'Source', 'Frequency', 'Last Update', 'Status'
          ))
        
      } else {
        
        series <- series %>% 
          dplyr::mutate(
            SERSTATUS = factor(
              SERSTATUS,
              levels = c('A', 'I', ''),
              labels =  c('Ativa', 'Inativa', '')
            ),
            BASNOME = factor(BASNOME),
            PERNOME = factor(PERNOME)
          ) %>%
          purrr::set_names(c(
            'code', 'name', 'theme', 'source',
            'freq', 'lastupdate', 'status')
          ) %>%
          sjlabelled::set_label(c(
            'Codigo Ipeadata', 'Nome da Serie', 'Base', 'Fonte',
            'Frequencia', 'Ultima Atualizacao', 'Status'
          ))
        
      }
      
    }
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  # Output
  return(series)
}

# Metadata --------------------------------------------------------------------

#' @title Metadata for a requested series
#'
#' @description Returns metadata information for the requested Ipeadata series.
#'
#' @usage metadata(code, language = c("en", "br"), quiet = FALSE)
#'
#' @param code A character vector of Ipeadata series codes.
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#' @param quiet Logical. If \code{FALSE} (default), a progress bar is displayed.
#'
#' @return A data frame containing the Ipeadata code, name, short comment,
#'   last update, theme name, source name and full name, source URL,
#'   frequency, unit, multiplier factor, status, subject code, and
#'   country code of the requested series.
#'
#' @examples
#' \dontrun{
#' # Metadata for:
#' # "PRECOS12_IPCA12": Brazil’s official consumer price inflation index (IPCA)
#' meta <- metadata(code = "PRECOS12_IPCA12")
#' }
#'
#' @note The original language of the series names and comments is preserved.
#'   The Ipeadata codes may be obtained using \code{available_series()}.
#'
#' @seealso \code{\link{available_series}},
#'   \code{\link{available_subjects}},
#'   \code{\link{available_territories}}
#'
#' @references This package uses the Ipeadata API.
#'   For more information, see \url{https://www.ipeadata.gov.br/}.
#'
#' @export

metadata <- function(code, language = c("en", "br"), quiet = FALSE) {

  # Check language arg
  language <- match.arg(language)

  # Output
  metadata <- tibble::tibble()

  # Progress Bar settings
  n <- length(code)
  pb <- NULL
  use_cli <- FALSE
  if (!quiet && n >= 2) {
    rlang::inform("Requesting Ipeadata API <https://www.ipeadata.gov.br/api/>") 
    
    if (rlang::is_installed("cli")) {
      use_cli <- TRUE
      pb <- cli::cli_progress_bar("Processing", total = n)
    } else {
      pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    }
    
  }
  update.step <- max(2L, floor(n / 100))

  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    tryCatch({
      
      # Retrieve metadata 1 by 1
      for (i in seq_len(n)) {
        
        # Check
        code0 <- gsub(" ", "_", toupper(code[i]))
        
        # URL for metadata
        url <- paste0(
          "https://www.ipeadata.gov.br/api/odata4/Metadados('", code0,"')"
        )
        
        # Extract from JSON
        Sys.sleep(.01)
        metadata.aux <- jsonlite::fromJSON(url, flatten = TRUE)[[2]]
        
        if (length(metadata.aux) > 0) {
          
          ## Starting: Transform to tbl >
          ##           Select variables
          metadata.aux <- metadata.aux %>% 
            dplyr::as_tibble() %>%
            dplyr::select(- SERNUMERICA)
          
          # Concatenate rows
          metadata <- dplyr::bind_rows(metadata, metadata.aux)
          
        } else {
          
          rlang::warn(
            paste0("Series code not found: '", code[i], "'."),
            class = "ipeadata_series_not_found"
          )
          
        }
        
        # Progress Bar
        if (!quiet && n >= 2 && (i %% update.step == 0L || i == n)) {
          if (use_cli) {
            cli::cli_progress_update(id = pb, set = i)
          } else {
            utils::setTxtProgressBar(pb, i)
          }
        }
      }
      
    }, error = function(e) {
      rlang::abort(
        "Failed to retrieve data from the Ipeadata API.",
        class = "ipeadata_api_error",
        parent = e
      )
    })
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }
  
  # Progress Bar closes
  if (!quiet && n >= 2) {
    if (use_cli) {
      cli::cli_progress_done(id = pb)
    } else {
      close(pb)
    }
  }
  
  # Setting labels in selected language
  if (nrow(metadata) != 0) {
    
    ## Starting: Transform in date >
    ##           Transform in factor >
    ##           Transform in chr >
    ##           Replace missing status
    metadata <- metadata %>%
      dplyr::mutate(
        SERATUALIZACAO = lubridate::as_date(SERATUALIZACAO), 
        FNTSIGLA = as.factor(FNTSIGLA),
        SERSTATUS = as.character(SERSTATUS),
        SERSTATUS = dplyr::if_else(is.na(SERSTATUS), '', SERSTATUS)
      )
    
    # Setting labels in selected language
    if (language == 'en') {
      
      metadata <- metadata %>%
        dplyr::mutate(
          BASNOME = iconv(BASNOME, 'UTF-8', 'ASCII//TRANSLIT'),
          BASNOME = factor(
            x = BASNOME,
            levels = c('Macroeconomico', 'Regional', 'Social'),
            labels = c('Macroeconomic', 'Regional', 'Social')
          ),
          UNINOME = iconv(UNINOME, 'UTF-8', 'ASCII//TRANSLIT'),
          UNINOME = factor(
            UNINOME,
            levels = df_uninome$uninome,
            labels = df_uninome$uninome_en
          ),
          PERNOME = iconv(PERNOME, 'UTF-8', 'ASCII//TRANSLIT'),
          PERNOME = factor(
            PERNOME,
            levels = df_pernome$pernome,
            labels = df_pernome$pernome_en
          ),
          MULNOME = iconv(MULNOME, 'UTF-8', 'ASCII//TRANSLIT'),
          MULNOME = factor(
            MULNOME,
            levels = df_mulnome$mulnome,
            labels = df_mulnome$mulnome_en
          ),
          SERSTATUS = factor(
            SERSTATUS,
            levels = c('A', 'I', ''),
            labels =  c('Active', 'Inactive', '')
          ),
          TEMCODIGO = as.integer(TEMCODIGO)
        ) %>%
        purrr::set_names(c(
          'code', 'name', 'comment', 'lastupdate', 'theme', 'source', 
          'sourcename', 'sourceurl', 'freq', 'unity', 'mf', 'status',
          'scode', 'ccode'
        )) %>%
        sjlabelled::set_label(c(
          'Ipeadata Code', 'Serie (PT-BR)', 'Comment (PT-BR)', 
          'Last Update', 'Theme', 'Source', 'Source Full Name', 'Source URL',
          'Frequency', 'Unity', 'Multiplier Factor', 'Status',
          'Subject Code', 'Country Code'
        ))

    } else {
      
      metadata <- metadata %>%
        dplyr::mutate(
          BASNOME = factor(BASNOME),
          UNINOME = factor(UNINOME),
          PERNOME = factor(PERNOME),
          MULNOME = factor(MULNOME),
          SERSTATUS = factor(
            SERSTATUS,
            levels = c('A', 'I', ''),
            labels =  c('Ativa', 'Inativa', '')
          ),
          TEMCODIGO = as.integer(TEMCODIGO)
        ) %>%
        purrr::set_names(c(
          'code', 'name', 'comment', 'lastupdate', 'theme', 'source', 
          'sourcename', 'sourceurl', 'freq', 'unity', 'mf', 'status',
          'scode', 'ccode'
        )) %>%
        sjlabelled::set_label(c(
          'Codigo Ipeadata', 'Nome da Serie (PT-BR)', 'Comentario', 
          'Ultima Atualizacao', 'Base', 'Fonte', 'Nome da Fonte', 
          'URL da Fonte', 'Frequencia', 'Unidade', 'Fator Multiplicador', 
          'Status', 'Codigo do Tema', 'Codigo de Pais'
        ))
    }
    
  }
  
  # Output
  return(metadata)
}

# Ipeadata --------------------------------------------------------------------

#' @title Data for a requested series
#'
#' @description Returns the data associated with the requested Ipeadata series.
#'
#' @usage ipeadata(code, language = c("en", "br"), quiet = FALSE)
#'
#' @param code A character vector of Ipeadata series codes.
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#' @param quiet Logical. If \code{FALSE} (default), a progress bar is displayed.
#'
#' @return A data frame containing the Ipeadata code, date, value,
#'   territorial unit name, and territorial code of the requested series.
#'
#' @examples
#' \dontrun{
#' # Data for:
#' # "PRECOS12_IPCA12": Brazil’s official consumer price inflation index (IPCA)
#' data <- ipeadata(code = "PRECOS12_IPCA12", language = "en")
#' }
#'
#' @note The Ipeadata codes may be obtained using \code{available_series()}.
#'
#' @seealso \code{\link{available_series}},
#'   \code{\link{available_territories}}
#'
#' @references This package uses the Ipeadata API.
#'   For more information, see \url{https://www.ipeadata.gov.br/}.
#'
#' @export

ipeadata <- function(code, language = c("en", "br"), quiet = FALSE) {

  # Check language arg
  language <- match.arg(language)

  # Output
  values <- tibble::tibble()

  # Progress Bar settings
  n <- length(code)
  pb <- NULL
  use_cli <- FALSE
  if (!quiet && n >= 2) {
    rlang::inform("Requesting Ipeadata API <https://www.ipeadata.gov.br/api/>") 
    
    if (rlang::is_installed("cli")) {
      use_cli <- TRUE
      pb <- cli::cli_progress_bar("Processing", total = n)
    } else {
      pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    }
    
  }
  update.step <- max(2L, floor(n / 100))
  
  # Test internet connection
  if (curl::has_internet()) {
    
    Sys.sleep(.01)
    tryCatch({
      
      # Retrieve metadata 1 by 1
      for (i in seq_along(code)) {
        
        # Check
        code0 <- gsub(" ", "_", toupper(code[i]))
        
        # URL for metadata
        url <- paste0(
          "https://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='", 
          code0, "')"
        )
        
        # Extract from JSON
        Sys.sleep(.01)
        values.aux <- jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>% 
          dplyr::as_tibble()
        
        if (length(values.aux) > 0) {
          
          # Sorting by tcode and date
          values.aux <- values.aux %>%
            dplyr::mutate(
              TERCODIGO = dplyr::if_else(
                condition = TERCODIGO == "", true = "0", false = TERCODIGO
              ),
              TERCODIGO = as.integer(TERCODIGO),
              NIVNOME = dplyr::if_else(
                condition = NIVNOME == "", true = "Brasil", false = NIVNOME
              ),
              VALDATA = lubridate::as_date(VALDATA)
            ) %>%
            dplyr::arrange(TERCODIGO, VALDATA)
          
          # Concatenate rows
          values <- dplyr::bind_rows(values, values.aux)
          
        } else {
          
          rlang::warn(paste0("code '", code[i], "' not found"))
          
        }
        
        # Progress Bar
        if (!quiet && n >= 2 && (i %% update.step == 0L || i == n)) {
          if (use_cli) {
            cli::cli_progress_update(id = pb, set = i)
          } else {
            utils::setTxtProgressBar(pb, i)
          }
        }
        
      }
      
    }, error = function(e) {
      rlang::abort(
        "Failed to retrieve data from the Ipeadata API.",
        class = "ipeadata_api_error",
        parent = e
      )
    })
    
  } else {
    rlang::abort(
      "Internet connection is unavailable.",
      class = "ipeadata_no_internet"
    )
  }

  # Progress Bar closes
  if (!quiet && n >= 2) {
    if (use_cli) {
      cli::cli_progress_done(id = pb)
    } else {
      close(pb)
    }
  }
  
  # Setting labels in selected language
  if (nrow(values) != 0) {
    
    ## Starting: Remove NA >
    ##           Rename variables >
    ##           Add subtitles >
    ##           Remove duplicates
    values <- values %>%
      dplyr::filter(!is.na(VALVALOR)) %>%
      dplyr::distinct()
    
    # Setting labels in selected language
    if (language == 'en') {
      
      values <- values %>%
        dplyr::mutate(
          NIVNOME = iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT'),
          NIVNOME = factor(
            NIVNOME,
            levels = df_nivnome$nivnome,
            labels = df_nivnome$nivnome_en
          ),
          TERCODIGO = as.character(TERCODIGO)
        ) %>% 
        purrr::set_names(c('code', 'date', 'value', 'uname', 'tcode')) %>%
        sjlabelled::set_label(c(
          'Ipeadata Code', 'Date', 'Value', 'Territorial Unit Name', 
          'Territorial Code'
        ))
      
    } else {
      
      values <- values %>%
        dplyr::mutate(
          NIVNOME = iconv(NIVNOME, 'UTF-8', 'ASCII//TRANSLIT'),
          NIVNOME = factor(NIVNOME, levels = df_nivnome$nivnome),
          TERCODIGO = as.character(TERCODIGO)
        ) %>%
        purrr::set_names(c('code', 'date', 'value', 'uname', 'tcode')) %>%
        sjlabelled::set_label(c(
          'Codigo Ipeadata', 'Data', 'Valor', 'Nome da Unidade Territorial',
          'Codigo Territorial'
        ))
      
    }
  }
  
  # Output
  return(values)
}

# Searched series -------------------------------------------------------------

#' @title Searched series
#'
#'   from the Ipeadata API.
#'
#' @usage search_series(terms = NULL, language = c("en", "br"))
#'
#' @param terms A character vector of search terms.
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#'
#' @return A data frame containing the Ipeadata code, name, theme, source,
#'   frequency, last update, and activity status of the matched series.
#'
#' @examples
#' \dontrun{
#' # Search for "rural"
#' search <- search_series(terms = "rural", language = "en")
#' }
#'
#' @note The original language of the series names is preserved.
#'
#' @export

search_series <- function(terms = NULL, language = c("en", "br")) {
  
  # Check language arg
  language <- match.arg(language)
  
  # Check terms
  if (!is.null(terms) && !is.character(terms)) {
    rlang::abort(
      "`terms` must be a character vector or NULL.",
      class = "ipeadata_invalid_terms"
    )
  }
  
  if (!is.null(terms)) {
    if (!is.character(terms)) {
      rlang::abort(
        "`terms` must be a character vector or NULL.",
        class = "ipeadata_invalid_terms"
      )
    }
    
    if (any(!nzchar(terms))) {
      rlang::abort(
        "`terms` must not contain empty strings.",
        class = "ipeadata_invalid_terms"
      )
    }
  }
  
  if (!is.null(terms) && anyNA(terms)) {
    rlang::abort(
      "`terms` must not contain NA values.",
      class = "ipeadata_invalid_terms"
    )
  }
  
  # Getting all series
  all_series <- available_series(language = language)
  
  # Searching
  users_search <- dplyr::tibble()
  
  if (!is.null(terms)) {
    
    for (i in seq_along(terms)) {
      
      users_search <- dplyr::bind_rows(
          users_search, 
          all_series %>%
            dplyr::filter(dplyr::if_any(
              dplyr::everything(), ~ grepl(
                terms[i], as.character(.x), ignore.case = TRUE
              )
            ))
        ) %>%
        dplyr::distinct()
      
    }
    
  } else {
    
    users_search <- all_series
    
  }
  
  # Setting labels in selected language
  if (language == 'en') {
    users_search <- users_search %>%
      purrr::set_names(c(
        'code', 'name', 'theme', 'source', 'freq', 'lastupdate', 'status'
      )) %>%
      sjlabelled::set_label(c(
        'Ipeadata Code', 'Serie (PT-BR)', 'Theme',
        'Source', 'Frequency', 'Last Update', 'Status'
      ))
  } else {
    users_search <- users_search %>%
      purrr::set_names(c(
        'code', 'name', 'theme', 'source', 'freq', 'lastupdate', 'status'
      )) %>%
      sjlabelled::set_label(c(
        'Codigo Ipeadata', 'Nome da Serie', 'Base', 'Fonte',
        'Frequencia', 'Ultima Atualizacao', 'Status'
      ))
  }
  
  # Output
  return(users_search)
    
}

# Change frequency ------------------------------------------------------------
# WIP (!!)

# Available countries (deprecated) --------------------------------------------

#' @title Available countries
#'
#' @description Returns a list of available countries from the Ipeadata API.
#'
#' @usage available_countries(language = c("en", "br"))
#'
#' @param language A string specifying the language. Available options are
#'   English (\code{"en"}, default) and Brazilian Portuguese (\code{"br"}).
#'
#' @return A data frame containing the three-letter country code and name
#'   of available countries.

# available_countries <- function(language = c("en", "br")) {
#   
#   # Check language arg
#   language <- match.arg(language)
#   
#   # URL for countries
#   url <- 'https://www.ipeadata.gov.br/api/odata4/Paises/'
#   
#   # Output NULL
#   countries <- NULL
#   
#   # Test internet connection
#   if (curl::has_internet()) {
#     
#     Sys.sleep(.01)
#     tryCatch({
#       
#       ## Starting: Extract from JSON >
#       ##           Transform to tbl >
#       ##           Sort by code
#       countries <-
#         jsonlite::fromJSON(url, flatten = TRUE)[[2]] %>%
#         dplyr::as_tibble() %>%
#         dplyr::arrange(PAICODIGO)
#       
#     }, error = function(e) {
#       rlang::abort(
#         "Failed to retrieve data from the Ipeadata API.",
#         class = "ipeadata_api_error",
#         parent = e
#       )
#     })
#     
#     # Setting labels in selected language
#     if (!is.null(countries)) {
#       
#       # Setting labels in selected language
#       if (language == 'en') {
#         
#         countries %<>%
#           dplyr::mutate(PAINOME = iconv(PAINOME, 'UTF-8', 'ASCII//TRANSLIT')) %>%
#           dplyr::mutate(PAINOME = factor(
#             PAINOME,
#             levels = c(
#               'Angola', 'Emirados Arabes Unidos', 'Argentina', 
#               'Sudeste Asiatico', 'Australia',
#               'Austria', 'Belgica', 'Bahamas', 'Bolivia', 'Brasil', 'Canada',
#               'Suica', 'Chile',
#               'China', 'Congo', 'Colombia', 'Cabo Verde', 'Republica Tcheca',
#               'Alemanha',
#               'Dinamarca', 'Republica Dominicana', 'Argelia', 'Equador', 
#               'Egito', 'Espanha',
#               'Uniao Europeia', 'Finlandia', 'Franca', 
#               'Gra-Bretanha (Reino Unido, UK)',
#               'Guine-Bissau', 'Grecia', 'Hong Kong', 'Haiti', 'Hungria', 
#               'Indonesia',
#               'India', 'Paises industrializados', 'Irlanda', 'Ira', 'Iraque', 
#               'Islandia',
#               'Israel', 'Italia', 'Japao', 'Coreia do Sul', 'America Latina', 
#               'Santa Lucia',
#               'Luxemburgo', 'Macau', 'Marrocos', 'Mexico', 'Myanma (Ex-Burma)', 
#               'Mocambique',
#               'Malasia', 'Nigeria', 'Holanda', 'Noruega', 'Nova Zelandia', 
#               'Peru', 'Filipinas',
#               'Polonia', 'Portugal', 'Paraguai', 'Qatar', 
#               'Leste Europeu e Russia', 'Romenia',
#               'Federacao Russa', 'Arabia Saudita', 'Cingapura', 
#               'Sao Tome e Principe', 'Eslovenia',
#               'Suecia', 'Tailandia', 'Timor Leste (Ex-East Timor)', 
#               'Trinidad and Tobago', 'Taiwan',
#               'Paises em desenvolvimento', 'Uruguai', 'Estados Unidos', 
#               'Venezuela', 'Mundial',
#               'Iemen', 'Africa do Sul', 'Zona do Euro'),
#             labels = c(
#               'Angola', 'United Arab Emirates', 'Argentina', 'Southeast Asia', 
#               'Australia', 'Austria',
#               'Belgium', 'Bahamas', 'Bolivia', 'Brazil', 'Canada', 
#               'Switzerland', 'Chile',
#               'China', 'Congo', 'Colombia', 'Cape Verde', 'Czech republic', 
#               'Germany',
#               'Denmark', 'Dominican Republic', 'Algeria', 'Ecuador', 'Egypt', 
#               'Spain',
#               'European Union', 'Finland', 'France', 
#               'Great Britain (United Kingdom, UK)', 'Guinea Bissau',
#               'Greece', 'Hong Kong', 'Haiti', 'Hungary', 'Indonesia', 
#               'India', 'Developed countries',
#               'Ireland', 'Iran', 'Iraq', 'Iceland', 'Israel', 'Italy', 
#               'Japan', 'South Korea',
#               'Latin America', 'Saint Lucia', 'Luxembourg', 'Macao', 
#               'Morocco', 'Mexico', 'Myanmar',
#               'Mozambique', 'Malaysia', 'Nigeria', 'Netherlands', 'Norway', 
#               'New Zealand', 'Peru',
#               'Philippines', 'Poland', 'Portugal', 'Paraguay', 'Qatar', 
#               'Eastern Europe and Russia',
#               'Romania', 'Russian Federation', 'Saudi Arabia', 'Singapore',
#               'Sao Tome and Principe',
#               'Slovenia', 'Sweden', 'Thailand', 'East Timor', 
#               'Trinidad and Tobago', 'Taiwan',
#               'Developing countries', 'Uruguay', 'United States of America', 
#               'Venezuela', 'World',
#               'Yemen', 'South Africa', 'Euro Area'))) %>%
#           dplyr::mutate(PAINOME = as.character(PAINOME)) %>%
#           purrr::set_names(c('tcode', 'tname')) %>%
#           sjlabelled::set_label(c('Country Code', 'Country Name'))
#         
#       } else {
#         
#         countries %<>%
#           purrr::set_names(c('tcode', 'tname')) %>%
#           sjlabelled::set_label(c('Codigo do Pais', 'Nome do Pais'))
#         
#       }
#       
#     }
#     
#   } else {
#     rlang::abort(
#       "Internet connection is unavailable.",
#       class = "ipeadata_no_internet"
#     )
#   }
#   
#   # Output
#   return(countries)
# }
