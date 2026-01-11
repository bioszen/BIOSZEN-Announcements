args <- commandArgs(trailingOnly = TRUE)
message <- NULL
url_value <- NULL

i <- 1
while (i <= length(args)) {
  arg <- args[i]
  if (grepl("^--url=", arg)) {
    url_value <- sub("^--url=", "", arg)
    i <- i + 1
    next
  }
  if (arg %in% c("--url", "-u")) {
    if (i == length(args)) {
      stop("Falta valor para --url")
    }
    url_value <- args[i + 1]
    i <- i + 2
    next
  }
  if (grepl("^--message=", arg)) {
    message <- sub("^--message=", "", arg)
    i <- i + 1
    next
  }
  if (arg %in% c("--message", "-m")) {
    if (i == length(args)) {
      stop("Falta valor para --message")
    }
    message <- args[i + 1]
    i <- i + 2
    next
  }
  if (startsWith(arg, "-")) {
    stop(paste0("Argumento desconocido: ", arg))
  }
  if (is.null(message)) {
    message <- arg
  } else {
    message <- paste(message, arg)
  }
  i <- i + 1
}

if (is.null(message)) {
  message <- readline("Mensaje: ")
}

message <- gsub("[\r\n]+", " ", trimws(message))
if (!nzchar(message)) {
  stop("Mensaje vacio.")
}

base_dir <- "announcements"
items_dir <- file.path(base_dir, "items")
index_path <- file.path(base_dir, "index.dcf")

if (!file.exists(index_path)) {
  stop("No se encontro announcements/index.dcf")
}
if (!dir.exists(items_dir)) {
  stop("No se encontro announcements/items")
}

index_lines <- readLines(index_path, warn = FALSE)
latest_idx <- grep("^latest\\s*:", index_lines)
if (length(latest_idx) == 0) {
  stop("announcements/index.dcf no tiene campo latest")
}

current_latest <- trimws(sub("^latest\\s*:", "", index_lines[latest_idx[1]]))
if (!nzchar(current_latest)) {
  stop("announcements/index.dcf tiene latest vacio")
}

template_path <- file.path(items_dir, paste0(current_latest, ".dcf"))
if (!file.exists(template_path)) {
  stop(paste0("Falta el item actual: ", template_path))
}

template_lines <- readLines(template_path, warn = FALSE)

make_title <- function(msg, max_len = 60) {
  msg <- trimws(msg)
  if (!nzchar(msg)) {
    return("Aviso")
  }
  if (nchar(msg) <= max_len) {
    return(msg)
  }
  prefix <- substr(msg, 1, max_len)
  cut <- regexpr("\\s+\\S*$", prefix, perl = TRUE)
  if (cut > 1) {
    prefix <- substr(prefix, 1, cut - 1)
  }
  paste0(prefix, "...")
}

make_slug <- function(msg, max_len = 40) {
  slug <- tolower(msg)
  slug <- iconv(slug, "UTF-8", "ASCII//TRANSLIT", sub = "")
  if (is.na(slug)) {
    slug <- ""
  }
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-|-$)", "", slug)
  slug <- gsub("-{2,}", "-", slug)
  if (!nzchar(slug)) {
    slug <- "announcement"
  }
  substr(slug, 1, max_len)
}

format_field <- function(key, value) {
  if (is.na(value) || !nzchar(value)) {
    return(paste0(key, ":"))
  }
  paste0(key, ": ", value)
}

set_field <- function(lines, key, value) {
  formatted <- format_field(key, value)
  idx <- grep(paste0("^", key, "\\s*:"), lines)
  if (length(idx) == 0) {
    return(c(lines, formatted))
  }
  lines[idx[1]] <- formatted
  lines
}

start_date <- format(Sys.Date(), "%Y-%m-%d")
slug <- make_slug(message)
base_id <- paste(start_date, slug, sep = "-")

existing_ids <- tools::file_path_sans_ext(
  list.files(items_dir, pattern = "\\.dcf$", full.names = FALSE)
)

new_id <- base_id
counter <- 2
while (new_id %in% existing_ids) {
  new_id <- paste0(base_id, "-", counter)
  counter <- counter + 1
}

title <- make_title(message)

new_lines <- template_lines
new_lines <- set_field(new_lines, "id", new_id)
new_lines <- set_field(new_lines, "title", title)
new_lines <- set_field(new_lines, "message", message)
new_lines <- set_field(new_lines, "start", start_date)
if (!is.null(url_value)) {
  url_value <- trimws(url_value)
  new_lines <- set_field(new_lines, "url", url_value)
}

new_path <- file.path(items_dir, paste0(new_id, ".dcf"))
if (file.exists(new_path)) {
  stop(paste0("Ya existe el item: ", new_path))
}

writeLines(new_lines, new_path, useBytes = TRUE)

index_lines[latest_idx[1]] <- paste0("latest: ", new_id)
writeLines(index_lines, index_path, useBytes = TRUE)

cat("Nuevo item:", new_path, "\n")
cat("Index actualizado:", index_path, "\n")
