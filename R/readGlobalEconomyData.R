#' Global economy data indicators
#' aggregation of over 500 indicators for over 200 countries from 1960 - 2024.
#' sources: central banks, national statistical institutes, and multiple international organizations.
#' https://www.theglobaleconomy.com/
#'
#' @returns MagPIE object with global economy data indicators
#'
#' @author Renato Rodrigues
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select rename filter
#' @importFrom utils read.csv
#'
readGlobalEconomyData <- function() {

  files <- list.files(path = "./", pattern = "\\.csv$", full.names = TRUE)

  data <- do.call(
    rbind,
    lapply(files, function(file) {
      df <- utils::read.csv(file, stringsAsFactors = FALSE)
      df <- df %>%
        pivot_longer(cols = setdiff(names(df), c("Country", "Code", "ContinentCode", "Year")),
                     names_to = "driver", values_to = "value") %>%
        mutate(type = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file)),
               driver = gsub(".", "_", .data$driver, fixed = TRUE)) %>%
        select("Code", "Year", "type", "driver", "value") %>%
        rename(region = "Code", period = "Year")
    })
  ) %>%
    mutate(region = ifelse(.data$region == "CRC", "CRI", # fixing Costa Rica iso3 code
                           ifelse(.data$region == "PS", "PSE", .data$region))) %>% # fixing Palestine iso3 code
    filter(!(.data$region %in% c("XC"))) # removing euro area data

  out <- data %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  return(out)
}
