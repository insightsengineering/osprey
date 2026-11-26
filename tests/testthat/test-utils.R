library(ggplot2)

test_that("Check as_pdf to transform plots to pdf", {
  ADAE <- osprey::rADAE
  plot_list <- list(
    ggplotGrob(ggplot(ADAE, aes(RACE)) +
      geom_bar()),
    ggplotGrob(ggplot(ADAE, aes(SEX)) +
      geom_bar())
  )
  pdfpath <- tempfile()
  pdffile <- paste0(pdfpath, ".pdf")
  as_pdf(plot_list, pdffile)
  expect_true(file.exists(pdffile))
})

test_that("Check grobs2pdf function to transform plots to pdf", {
  ADAE <- osprey::rADAE
  plot_list <- list(
    ggplotGrob(ggplot(ADAE, aes(RACE)) +
      geom_bar()),
    ggplotGrob(ggplot(ADAE, aes(SEX)) +
      geom_bar())
  )
  pdfpath <- tempfile()
  pdffile <- paste0(pdfpath, ".pdf")
  grobs2pdf(
    plot_list,
    titles = "Visualization of ADAE Data",
    footnotes = "This is a footnote",
    progpath = "~/example_prog.R",
    outpath = pdffile
  )
  expect_true(file.exists(pdffile))
})
