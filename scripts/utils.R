make_unsplash_caption <- function(author, handle, code) {
    template <- paste(
        "Foto de <a href='https://unsplash.com/%s'>%s</a>", 
        "en <a href='https://unsplash.com/photos/%s'>Unsplash</a>"
    )
    return(sprintf(template, handle, author, code))
}