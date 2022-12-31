OUTPUT_DIR <- file.path(".output", "practica")
PRACTICA_DIR <- file.path("practica")

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

# Sino existe el directorio de salida, crearlo
if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR)
}

# Obtener los .qmd que se corresponden a la practica
practica_files <- list.files(PRACTICA_DIR, pattern = "qmd", full.names = TRUE)

# Obtener los nombres de salida
output_files <- file.path(
    OUTPUT_DIR,
    vapply(practica_files, get_pdf_filename, character(1), USE.NAMES = FALSE)
)

# Renderizarlos como pdf
exit_codes <- mapply(
    render_pdf,
    practica_files,
    output_files,
    MoreArgs = list(verbose = TRUE)
)

# Codigos de salida
cat(exit_codes, "\n")