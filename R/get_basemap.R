#' Download IPBES World Countries Boundaries
#'
#' @description
#' This function downloads shapefile of world countries boundaries as defined by
#' IPBES (available at \url{https://zenodo.org/record/3928281}). Zip file is
#' downloaded in the folder `path` ('data/ipbes-regions' by default) and files
#' are extracted in the same folder. If this folder doesn't exist it will be
#' created.
#'
#' **Note:** if you delete the ZIP file (after files extraction) and re-run this
#' function, the ZIP file will be downloaded again. It this is what you want do
#' forget to use `force = TRUE` to erase previous files.
#'
#' Original Projection System: WGS84
#'
#' @param path the folder to download and extract zip file
#'
#' @param force a logical value. If `TRUE` will erase previously downloaded
#'   files. Default is `FALSE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Download and extract ZIP ----
#' get_basemap()
#'
#' ## Check ----
#' list.files(here::here("data", "ipbes-regions"))
#'
#' ## Re-run function ----
#' get_basemap()              # No download (ZIP already present)
#' get_basemap(force = TRUE)  # Erase previous files
#' }

get_basemap <- function(path = here::here("data", "ipbes-regions"),
                        force = FALSE) {
  
  filename <- "ipbes_regions_subregions_shape_1.1.zip"
  
  url <- paste0("https://zenodo.org/record/3928281/files/",
                filename, "?download=1")
  
  if (force) {
    if (dir.exists(path)) {
      invisible(unlink(path, recursive = TRUE))
    } else {
      dir.create(path, recursive = TRUE)
    }
  } else {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
  
  
  if (!(filename %in% list.files(path))) {
    
    utils::download.file(url, destfile = file.path(path, filename))
    utils::unzip(zipfile = file.path(path, filename), exdir = path)
  }
  
  invisible(NULL)
}
