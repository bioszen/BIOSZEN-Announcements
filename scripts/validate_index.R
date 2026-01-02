index_path <- file.path("announcements", "index.dcf")
if (!file.exists(index_path)) {
  stop("Missing announcements/index.dcf")
}

index <- tryCatch(
  read.dcf(index_path),
  error = function(e) stop("Failed to read announcements/index.dcf")
)

if (nrow(index) < 1) {
  stop("announcements/index.dcf is empty")
}

if (!"latest" %in% colnames(index)) {
  stop("announcements/index.dcf is missing field: latest")
}

latest <- index[1, "latest"]
if (is.na(latest) || !nzchar(latest)) {
  stop("announcements/index.dcf has empty latest")
}

item_path <- file.path("announcements", "items", paste0(latest, ".dcf"))
if (!file.exists(item_path)) {
  stop(paste0("Missing announcement item: ", item_path))
}

item <- tryCatch(
  read.dcf(item_path),
  error = function(e) stop(paste0("Failed to read item file: ", item_path))
)

if (nrow(item) < 1) {
  stop(paste0("Item file is empty: ", item_path))
}

if (!"id" %in% colnames(item)) {
  stop(paste0("Item file missing field: id (", item_path, ")"))
}

item_id <- item[1, "id"]
if (is.na(item_id) || !nzchar(item_id)) {
  stop(paste0("Item file has empty id: ", item_path))
}

if (!identical(item_id, latest)) {
  stop(paste0("Item id does not match latest: ", item_id, " vs ", latest))
}

invisible(TRUE)
