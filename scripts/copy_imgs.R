imgs_origen <- list.files(here::here("estadistica-bayesiana", "practica", "imgs"), full.names = TRUE)
imgs_destino <- file.path(here::here("estadistica-bayesiana", "_site", "practica", "imgs"), basename(imgs_origen))

exit_codes <- mapply(file.copy, imgs_origen, imgs_destino, overwrite = TRUE)

cat("Resultado de copiar las imagenes...\n")
cat(exit_codes, "\n")