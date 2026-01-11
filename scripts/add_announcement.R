args <- commandArgs(trailingOnly = TRUE)
message <- NULL
url_value <- NULL
title_value <- NULL
end_value <- NULL
min_version_value <- NULL
max_version_value <- NULL
enabled_value <- NULL

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
  if (grepl("^--title=", arg)) {
    title_value <- sub("^--title=", "", arg)
    i <- i + 1
    next
  }
  if (grepl("^--end=", arg)) {
    end_value <- sub("^--end=", "", arg)
    i <- i + 1
    next
  }
  if (grepl("^--min-version=", arg)) {
    min_version_value <- sub("^--min-version=", "", arg)
    i <- i + 1
    next
  }
  if (grepl("^--max-version=", arg)) {
    max_version_value <- sub("^--max-version=", "", arg)
    i <- i + 1
    next
  }
  if (grepl("^--enabled=", arg)) {
    enabled_value <- sub("^--enabled=", "", arg)
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
  if (arg %in% c("--title", "-t")) {
    if (i == length(args)) {
      stop("Falta valor para --title")
    }
    title_value <- args[i + 1]
    i <- i + 2
    next
  }
  if (arg == "--end") {
    if (i == length(args)) {
      stop("Falta valor para --end")
    }
    end_value <- args[i + 1]
    i <- i + 2
    next
  }
  if (arg == "--min-version") {
    if (i == length(args)) {
      stop("Falta valor para --min-version")
    }
    min_version_value <- args[i + 1]
    i <- i + 2
    next
  }
  if (arg == "--max-version") {
    if (i == length(args)) {
      stop("Falta valor para --max-version")
    }
    max_version_value <- args[i + 1]
    i <- i + 2
    next
  }
  if (arg == "--enabled") {
    if (i < length(args) && !startsWith(args[i + 1], "-")) {
      enabled_value <- args[i + 1]
      i <- i + 2
    } else {
      enabled_value <- "true"
      i <- i + 1
    }
    next
  }
  if (arg == "--disabled") {
    enabled_value <- "false"
    i <- i + 1
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

message <- gsub("[\r\n\t]+", " ", trimws(message))
if (!nzchar(message)) {
  stop("Mensaje vacio.")
}

base_dir <- "announcements"
items_dir <- file.path(base_dir, "items")
index_path <- file.path(base_dir, "index.dcf")
latest_json_path <- file.path(base_dir, "latest.json")

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

get_json_field <- function(lines, key) {
  idx <- grep(paste0("\"", key, "\"\\s*:"), lines)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  line <- lines[idx[1]]
  sub(paste0(".*\"", key, "\"\\s*:\\s*\"([^\"]*)\".*"), "\\1", line)
}

json_escape <- function(value) {
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub("\"", "\\\\\"", value)
  value <- gsub("\r", "\\\\r", value)
  value <- gsub("\n", "\\\\n", value)
  value <- gsub("\t", "\\\\t", value)
  value
}

normalize_bool <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  raw <- tolower(trimws(as.character(value)))
  if (raw %in% c("true", "1", "yes", "y")) return("true")
  if (raw %in% c("false", "0", "no", "n")) return("false")
  stop(paste0("Valor invalido para enabled: ", value))
}

set_json_field <- function(lines, key, value) {
  idx <- grep(paste0("\"", key, "\"\\s*:"), lines)
  if (length(idx) == 0) {
    stop(paste0("announcements/latest.json no tiene campo ", key))
  }
  line <- lines[idx[1]]
  indent <- sub("^([[:space:]]*).*", "\\1", line)
  has_comma <- grepl(",\\s*$", line)
  escaped <- json_escape(value)
  lines[idx[1]] <- paste0(
    indent,
    "\"",
    key,
    "\": \"",
    escaped,
    "\"",
    if (has_comma) "," else ""
  )
  lines
}

set_json_scalar_field <- function(lines, key, value) {
  idx <- grep(paste0("\"", key, "\"\\s*:"), lines)
  if (length(idx) == 0) {
    stop(paste0("announcements/latest.json no tiene campo ", key))
  }
  line <- lines[idx[1]]
  indent <- sub("^([[:space:]]*).*", "\\1", line)
  has_comma <- grepl(",\\s*$", line)
  lines[idx[1]] <- paste0(
    indent,
    "\"",
    key,
    "\": ",
    value,
    if (has_comma) "," else ""
  )
  lines
}

if (!file.exists(latest_json_path)) {
  stop("No se encontro announcements/latest.json")
}

latest_json_lines <- readLines(latest_json_path, warn = FALSE)
current_latest <- get_json_field(latest_json_lines, "id")
if (is.na(current_latest) || !nzchar(current_latest)) {
  stop("announcements/latest.json tiene id vacio")
}

current_url <- get_json_field(latest_json_lines, "url")

items_available <- dir.exists(items_dir)
index_available <- file.exists(index_path)

template_lines <- NULL
template_from_default <- FALSE
if (items_available) {
  template_path <- file.path(items_dir, paste0(current_latest, ".dcf"))
  if (file.exists(template_path)) {
    template_lines <- readLines(template_path, warn = FALSE)
  }
}
if (is.null(template_lines)) {
  template_from_default <- TRUE
  template_lines <- c(
    "id:",
    "title:",
    "message:",
    "url:",
    "start:",
    "end:",
    "min_version:",
    "max_version:",
    "once: true"
  )
}

index_lines <- NULL
latest_idx <- integer(0)
if (index_available) {
  index_lines <- readLines(index_path, warn = FALSE)
  latest_idx <- grep("^latest\\s*:", index_lines)
  if (length(latest_idx) == 0) {
    stop("announcements/index.dcf no tiene campo latest")
  }
}

start_date <- format(Sys.Date(), "%Y-%m-%d")
time_stamp <- format(Sys.time(), "%H%M%S")
slug <- make_slug(message)
base_id <- paste(start_date, time_stamp, slug, sep = "-")

existing_ids <- character(0)
if (items_available) {
  existing_ids <- tools::file_path_sans_ext(
    list.files(items_dir, pattern = "\\.dcf$", full.names = FALSE)
  )
}
existing_ids <- unique(c(existing_ids, current_latest))

new_id <- base_id
counter <- 2
while (new_id %in% existing_ids) {
  new_id <- paste0(base_id, "-", counter)
  counter <- counter + 1
}

if (!is.null(title_value)) {
  title_value <- gsub("[\r\n\t]+", " ", trimws(title_value))
  if (!nzchar(title_value)) {
    title_value <- NULL
  }
}
if (is.null(title_value)) {
  title_value <- readline("Titulo (Enter para sugerido): ")
  title_value <- gsub("[\r\n\t]+", " ", trimws(title_value))
  if (!nzchar(title_value)) {
    title_value <- NULL
  }
}

title <- if (is.null(title_value)) make_title(message) else title_value

end_value <- if (is.null(end_value)) "" else gsub("[\r\n\t]+", " ", trimws(end_value))
min_version_value <- if (is.null(min_version_value)) "" else gsub("[\r\n\t]+", " ", trimws(min_version_value))
max_version_value <- if (is.null(max_version_value)) "" else gsub("[\r\n\t]+", " ", trimws(max_version_value))
enabled_value <- normalize_bool(if (is.null(enabled_value)) "true" else enabled_value)

new_lines <- template_lines
new_lines <- set_field(new_lines, "id", new_id)
new_lines <- set_field(new_lines, "title", title)
new_lines <- set_field(new_lines, "message", message)
new_lines <- set_field(new_lines, "start", start_date)
new_lines <- set_field(new_lines, "end", end_value)
new_lines <- set_field(new_lines, "min_version", min_version_value)
new_lines <- set_field(new_lines, "max_version", max_version_value)
if (!is.null(url_value)) {
  url_value <- gsub("[\r\n\t]+", " ", trimws(url_value))
  new_lines <- set_field(new_lines, "url", url_value)
} else if (template_from_default && !is.na(current_url) && nzchar(current_url)) {
  new_lines <- set_field(new_lines, "url", current_url)
}

if (items_available) {
  new_path <- file.path(items_dir, paste0(new_id, ".dcf"))
  if (file.exists(new_path)) {
    stop(paste0("Ya existe el item: ", new_path))
  }

  writeLines(new_lines, new_path, useBytes = TRUE)
  cat("Nuevo item:", new_path, "\n")
} else {
  cat("Sin items/: se omitio el item DCF.\n")
}

if (index_available) {
  index_lines[latest_idx[1]] <- paste0("latest: ", new_id)
  writeLines(index_lines, index_path, useBytes = TRUE)
  cat("Index actualizado:", index_path, "\n")
}

latest_json_lines <- set_json_field(latest_json_lines, "id", new_id)
latest_json_lines <- set_json_field(latest_json_lines, "title", title)
latest_json_lines <- set_json_field(latest_json_lines, "message", message)
latest_json_lines <- set_json_field(latest_json_lines, "start", start_date)
latest_json_lines <- set_json_field(latest_json_lines, "end", end_value)
latest_json_lines <- set_json_field(latest_json_lines, "min_version", min_version_value)
latest_json_lines <- set_json_field(latest_json_lines, "max_version", max_version_value)
if (!is.null(url_value)) {
  latest_json_lines <- set_json_field(latest_json_lines, "url", url_value)
}
latest_json_lines <- set_json_scalar_field(latest_json_lines, "enabled", enabled_value)
writeLines(latest_json_lines, latest_json_path, useBytes = TRUE)
cat("Latest actualizado:", latest_json_path, "\n")
