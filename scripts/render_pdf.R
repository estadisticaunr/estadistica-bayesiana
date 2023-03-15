OUTPUT_BASE_DIR <- file.path(".output")
INPUT_DIRS <- c("practica", "trabajos_practicos")
OUTPUT_DIRS <- file.path(OUTPUT_BASE_DIR, INPUT_DIRS)

enquote <- function(x) {
    return(paste0("'", x, "'"))
}

get_pdf_filename <- function(x) {
    pdf_filename <- paste0(sub(".qmd", "", basename(x)), ".pdf")
    return(pdf_filename)
}

make_command <- function(input, output) {
    output_dir <- dirname(output)
    output_file <- basename(output)
    command_components <- c(
        "quarto render",
        enquote(input),
        "--to pdf",
        "--output-dir",
        enquote(output_dir),
        "--output",
        enquote(output_file),
        "--pdf-engine xelatex"
    )
    command <- paste(command_components, collapse = " ")
    return(command)
}

render_pdf <- function(input, output, verbose = FALSE) {
    command <- make_command(input, output)
    if (verbose) cat(command)
    return(system(command))
}

render_pdf_dir <- function(input_dir, output_dir) {
    # Obtener los .qmd que se corresponden a la practica
    input_files <- list.files(input_dir, pattern = "qmd", full.names = TRUE)

    # Obtener los nombres de salida
    output_files <- file.path(
        output_dir,
        vapply(input_files, get_pdf_filename, character(1), USE.NAMES = FALSE)
    )

    # Renderizarlos como pdf
    exit_codes <- mapply(
        render_pdf,
        input_files,
        output_files,
        MoreArgs = list(verbose = TRUE)
    )
    return(exit_codes)
}

# Sino existe el directorio de salida, crearlo
if (!dir.exists(OUTPUT_BASE_DIR)) {
    dir.create(OUTPUT_BASE_DIR)
}

# Crear sub-directorios de salida
for (dir in OUTPUT_DIRS) {
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
}

exit_codes <- mapply(render_pdf_dir, INPUT_DIRS, OUTPUT_DIRS)

# Codigos de salida
cat("Codigos de salida:\n")
for (code in exit_codes) {
    cat(code, "\n")
}
