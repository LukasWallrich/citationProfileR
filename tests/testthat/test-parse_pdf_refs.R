f <- system.file("test-data", "Wallrich_et al_2020.pdf", package = "CitationProfileR")
refs <- parse_pdf_refs(f)

test_that("parse_pdfs", {
    # Most basic test - correct number of references returned
    expect_equal(nrow(refs), 47)
})
