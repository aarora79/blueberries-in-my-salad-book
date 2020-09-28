library(bookdown)
bookdown::render_book("index.Rmd", "bookdown::pdf_book")
bookdown::render_book("index.Rmd", "bookdown::epub_book")
bookdown::render_book("index.Rmd")
