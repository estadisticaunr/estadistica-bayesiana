OUTPUT_DIR <- "."
PRACTICA_DIR <- "practica"
ROOT <- here::here("estadistica-bayesiana")

enquote <- function(x) {
    return(paste0("'", x, "'"))
}

get_pdf_filename <- function(x) {
    pdf_filename <- paste0(sub(".qmd", "", basename(x)), ".pdf")
    return(pdf_filename)
}

make_command <- function(input, output) {
    command_components <- c(
        "quarto render",
        enquote(input),
        "--to pdf",
        "--output",
        enquote(output),
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

# Obtener los .qmd que se corresponden a la practica
practica_dir <- file.path(ROOT, PRACTICA_DIR)
output_dir <- file.path(ROOT, OUTPUT_DIR)
practica_files <- list.files(practica_dir, pattern = "qmd", full.names = TRUE)

# Obtener los nombres de salida
output_files <- vapply(practica_files, get_pdf_filename, character(1), USE.NAMES = FALSE)

# Renderizarlos como pdf
exit_codes <- mapply(render_pdf, practica_files, output_files, MoreArgs = list(verbose = TRUE))

