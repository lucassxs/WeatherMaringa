station_data <- function(x){

  # find line with variables names
  rowheader <- x %>%
    # toUTF8()
    stringr::str_detect("Data;Hora;") %>%
    which()
  # variable names
  h <- x[rowheader]

  # extract header and fix it
  h_fix <- h %>%
    stringr::str_replace("VelocidadeVentoInsolacao;", "VelocidadeVento;Insolacao;") %>%
    stringr::str_split(";") %>%
    unlist()

  to_discard <- h_fix %>%
    magrittr::equals("") %>%
    which()

  h_fix <- h_fix[-to_discard]

  ## replace original vnames by the new ones
  new_vnames <- c("codigo", "data","hora",
                  "prec", "tair", "tw", "tmax", "tmin", "urmax",
                  "patm", "pnmm", "wd", "wsmax", "n", "cc", "evap", "tcomp", "ur", "ws")
  vnames <-  doBy::recodeVar(as.character(h_fix),
                             src = as.list(as.character(h_fix)),
                             tgt = as.list(new_vnames))

  x_clean <- x %>%
    magrittr::extract((rowheader+1) : (length(x)-1)) %>%
    stringr::str_replace(";$", "")

  station_data <- read.csv2(text = x_clean,
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      na.strings = "")

  # stop if there is conflict between ncol(x) and length(hvec)
  if (ncol(station_data) != length(vnames)) {
    print(head(station_data))
    cat("ncol(x) = ", ncol(station_data), "\n",
        "hvec = ", vnames, "\n", "\n")

    stop("num. of data columns does not match the num. of variables")
  } else {
    names(station_data) <- vnames
  }# end if


  # coercion to numeric due to na.strings = ""
  sel_vars <- names(station_data)[!names(station_data) %in% c("codigo","data", "hora")]
  station_data <- station_data %>%
    dplyr::mutate_at(sel_vars, dplyr::funs(as.numeric))

  ## date conversion
  station_data <- station_data %>%
    dplyr::mutate(hora = doBy::recodeVar(as.character(hora),
                                         src = as.list(c("1800","0","1200")),
                                         tgt = as.list(c("18:00","00:00","12:00"))
    ),
    date = as.POSIXct(paste(as.Date(data,
                                    format = "%d/%m/%Y"),
                            hora,
                            sep = " "),
                      tz = "UTC"),
    data = NULL,
    hora = NULL,
    id = as.character(codigo),
    codigo = NULL)
  # reorder columns
  station_data <- station_data %>%
    dplyr::select(date, id, prec:ws, -tcomp)

  # duplicated rows
  bdmepd <- dplyr::distinct(bdmepd)

  return(bdmepd)

}
set_bdmep_user <- function(lnk, email, passwd){
  txt <- httr::GET(lnk)
  attrs_name_passwd_bt <- txt %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_nodes("form") %>%
    rvest::html_nodes("input") %>%
    magrittr::extract(c(3:4, 6)) %>%
    rvest::html_attr("name")

  vals_name_passwd_bt <- txt %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_nodes("form") %>%
    rvest::html_nodes("input") %>%
    magrittr::extract(c(3:4, 6)) %>%
    rvest::html_attr('value')

  # put values in a named list
  l <- vals_name_passwd_bt %>%
    seq_along() %>%
    lapply(function(i) vals_name_passwd_bt[i]) %>%
    setNames(attrs_name_passwd_bt)
  # add email and passwd
  l <- purrr::update_list(l, mCod = email, mSenha = passwd)
  return(l)
}

<<<<<<< HEAD
station_data_import_station <- function(.id = "A835" , #maringa
=======
station_data_import_station <- function(.id = "83488" ,
>>>>>>> bdf1923b4a2ff28e01531324cb196d5a766ba488
                                 .sdate = "01/01/1961",
                                 .edate = format(Sys.Date(), '%d/%m/%Y'),
                                 .email = "your-email",
                                 .passwd = "your-password",
                                 .verbose = TRUE,
                                 .destdir = NULL,
                                 ...){
  # step 1 - login
  link <- "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"
  station_data_form_l <- set_station_data_user(link, .email, .passwd)
  r <- httr::POST(link, body = bdmep_form_l, encode = "form")

  if (httr::status_code(r) == 200 & .verbose) {
    message("\n", "------------------------------", "\n",
            "station: " , .id)
  }
  # visualize(r)
  # step 2 - get data
  url_data <- "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=XXXXX&btnProcesso=serie&mRelDtInicio=dd/mm/yyyy&mRelDtFim=DD/MM/YYYY&mAtributos=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,"
  # link to station data
  url_data <-  url_data %>%
    stringr::str_replace("XXXXX", as.character(.id)) %>%
    stringr::str_replace("dd/mm/yyyy", .sdate) %>%
    stringr::str_replace("DD/MM/YYYY", .edate)

  # request data
  r2 <- httr::GET(url_data)

  msg <- httr::http_status(r2)$message

  #httr::stop_for_status(r2)
  if (.verbose) {
    httr::message_for_status(r2)
    cat("\n")
  }

  # column to inform request status
  if (httr::status_code(r2) != 200) {
    xtidy <- bdmep_template(.id , msg)
    return(xtidy)
  }

  x <- r2 %>%
    httr::content('text') %>%
    textConnection(local = TRUE) %>%
    readLines()

  # tidy data and output
  xtidy <- bdmep_read(x)
  # column with status
  xtidy <- dplyr::mutate(xtidy, request_status = msg)

  if (!is.null(.destdir)) {
    #if(!stringr::str_detect(.destfile, "\\.[a-z]{3,}")){
    .file <- file.path(.destdir, paste0(.id, ".csv"))
    if (.verbose) message("Data saved in ", .file)
    #readr::write_csv(x = dplyr::mutate(xtidy, date = as.character(date)),
    readr::write_csv(x = xtidy,
                     path = .file,
                     ...)
  }
  return(xtidy)
}


station_data_data_import <- function(id = c("83936", "83967") ,
                         sdate = "01/01/1961",
                         edate = format(Sys.Date(), '%d/%m/%Y'),
                         email = "your@email.com",
                         passwd = "your-password",
                         verbose = TRUE,
                         destdir = NULL,
                         ...){
  id <- as.character(id)
  # check arguments precondition ----------------------------------------------
  stopifnot(unique(nchar(id)) == 5,
            all(id %in% inmetr::bdmep_meta$id),
            length(unlist(stringr::str_extract_all(sdate, "/"))) == 2,
            length(unlist(stringr::str_extract_all(edate, "/"))) == 2,
            stringr::str_detect(email, "@"),
            is.character(passwd),
            is.logical(verbose),
            is.null(destdir) | is.character(destdir)
  )
  if(!is.null(destdir)) stopifnot(dir.exists(destdir))
  # import data ---------------------------------------------------------------
  purrr::map_df(id, ~bdmep_import_station(.x,
                                          .sdate = sdate,
                                          .edate = edate,
                                          .email = email,
                                          .passwd = passwd,
                                          .verbose = verbose,
                                          .destdir = destdir,
                                          ...))
}


station_data_data_template <- function(.id, .req_status){
  varnames <- bdmep_description()[, "varname"]
  templ_df <- as.data.frame(t(rep(NA, length(varnames))), stringsAsFactors = FALSE)
  templ_df <- templ_df %>%
    setNames(varnames) %>%
    dplyr::mutate(id = as.character(.id),
                  request_status = as.character(.req_status))
  templ_df
}

