
photo_dir <- "C:/Users/miral/Downloads/gamecam_photos"
location_prefix <- "loc_inlet"

do_rename <- TRUE   # Change to TRUE only after checking the preview CSV

# -------------------------------------------------------------------
# Find photos
# -------------------------------------------------------------------

photos <- list.files(
  photo_dir,
  pattern = "\\.(jpg|jpeg)$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Skip old outputs, test crops, and temporary files from previous attempts
photos <- photos[
  !grepl("^loc_inlet_\\d{8}_\\d{4}", basename(photos), ignore.case = TRUE) &
    !grepl("^test_", basename(photos), ignore.case = TRUE) &
    !grepl("^__TEMP_RENAME__", basename(photos), ignore.case = TRUE)
]

if (length(photos) == 0) {
  stop("No original JPG/JPEG photos found in the folder.")
}

# Sort by camera number when possible, e.g. RCNX0426.JPG
cam_num <- suppressWarnings(
  as.integer(sub(".*?(\\d+).*", "\\1", basename(photos)))
)

photos <- photos[order(cam_num, basename(photos), na.last = TRUE)]

# -------------------------------------------------------------------
# PowerShell script to read EXIF dates from all photos
# -------------------------------------------------------------------

file_list <- tempfile(fileext = ".txt")
ps_file <- tempfile(fileext = ".ps1")
meta_csv <- tempfile(fileext = ".csv")

writeLines(
  normalizePath(photos, winslash = "\\", mustWork = TRUE),
  file_list
)

ps_code <- c(
  'param(',
  '  [string]$file_list,',
  '  [string]$out_csv',
  ')',
  '',
  'Add-Type -AssemblyName System.Drawing',
  '',
  '$rows = foreach($path in Get-Content -LiteralPath $file_list) {',
  '  $dt = ""',
  '  $tag = ""',
  '  $img = $null',
  '',
  '  try {',
  '    $img = [System.Drawing.Image]::FromFile($path)',
  '',
  '    # 0x9003 = DateTimeOriginal',
  '    # 0x0132 = DateTime',
  '    # 0x9004 = DateTimeDigitized',
  '    foreach($id in @(0x9003, 0x0132, 0x9004)) {',
  '      if($img.PropertyIdList -contains $id) {',
  '        $bytes = $img.GetPropertyItem($id).Value',
  '        $dt = [System.Text.Encoding]::ASCII.GetString($bytes).Trim([char]0)',
  '        $tag = $id.ToString("X")',
  '        break',
  '      }',
  '    }',
  '  } catch {',
  '    $dt = ""',
  '    $tag = "ERROR"',
  '  } finally {',
  '    if($img -ne $null) { $img.Dispose() }',
  '  }',
  '',
  '  [PSCustomObject]@{',
  '    file = $path',
  '    exif_datetime = $dt',
  '    exif_tag = $tag',
  '  }',
  '}',
  '',
  '$rows | Export-Csv -LiteralPath $out_csv -NoTypeInformation'
)

writeLines(ps_code, ps_file)

ps_output <- system2(
  "powershell",
  args = c(
    "-NoProfile",
    "-ExecutionPolicy", "Bypass",
    "-File", shQuote(normalizePath(ps_file, winslash = "\\")),
    "-file_list", shQuote(normalizePath(file_list, winslash = "\\")),
    "-out_csv", shQuote(normalizePath(meta_csv, winslash = "\\"))
  ),
  stdout = TRUE,
  stderr = TRUE
)

# Uncomment this if you want to see PowerShell messages:
# cat(paste(ps_output, collapse = "\n"))

meta <- read.csv(meta_csv, stringsAsFactors = FALSE)

# -------------------------------------------------------------------
# Parse EXIF dates and create preview table
# -------------------------------------------------------------------

meta$old_file <- basename(meta$file)

# EXIF date usually looks like: 2025:04:08 10:00:00
dt <- as.POSIXct(
  meta$exif_datetime,
  format = "%Y:%m:%d %H:%M:%S",
  tz = "UTC"
)

meta$parsed_datetime <- as.character(dt)

meta$new_file <- ifelse(
  is.na(dt),
  NA,
  paste0(
    location_prefix, "_",
    format(dt, "%Y%m%d_%H%M"),
    ".JPG"
  )
)

results <- meta[, c(
  "old_file",
  "exif_datetime",
  "exif_tag",
  "parsed_datetime",
  "new_file"
)]

preview_path <- file.path(photo_dir, "rename_preview_metadata.csv")
write.csv(results, preview_path, row.names = FALSE)

cat("Preview written to:\n", preview_path, "\n\n")
print(results)

# -------------------------------------------------------------------
# Safety checks
# -------------------------------------------------------------------

ok <- !is.na(results$new_file)

if (any(!ok)) {
  cat("\nThese files did not have a readable EXIF date:\n")
  print(results$old_file[!ok])
  cat("\nDo not rename yet. These may be copied/exported versions without metadata.\n")
}

if (any(duplicated(results$new_file[ok]))) {
  cat("\nDuplicate final filenames detected:\n")
  print(results[ok & (
    duplicated(results$new_file) |
      duplicated(results$new_file, fromLast = TRUE)
  ), ])
  stop("Duplicate final filenames found. Check the preview before renaming.")
}

# -------------------------------------------------------------------
# Rename files, only if do_rename is TRUE
# -------------------------------------------------------------------

if (do_rename) {
  
  if (any(!ok)) {
    stop("Some files are missing EXIF dates. Fix/remove those before renaming.")
  }
  
  old_paths <- file.path(photo_dir, results$old_file[ok])
  final_paths <- file.path(photo_dir, results$new_file[ok])
  
  # Stop if final filenames already exist from a previous run
  existing_final <- final_paths[file.exists(final_paths)]
  
  if (length(existing_final) > 0) {
    cat("\nThese target filenames already exist:\n")
    print(basename(existing_final))
    stop("Move/delete old renamed files first, then rerun.")
  }
  
  # Two-step rename prevents accidental name collisions during the rename
  temp_paths <- file.path(
    photo_dir,
    paste0("__TEMP_RENAME__", sprintf("%05d", seq_along(old_paths)), ".JPG")
  )
  
  if (any(file.exists(temp_paths))) {
    stop("Temporary rename files already exist. Delete files beginning with __TEMP_RENAME__ and rerun.")
  }
  
  step1 <- file.rename(old_paths, temp_paths)
  
  if (!all(step1)) {
    stop("Some files could not be renamed to temporary names.")
  }
  
  step2 <- file.rename(temp_paths, final_paths)
  
  if (!all(step2)) {
    stop("Some temporary files could not be renamed to final names. Check the folder.")
  }
  
  cat("\nRenaming complete.\n")
}
