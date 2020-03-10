test_that("backendInitialize,MsBackendHmdbXml works", {
    fls <- dir(system.file("xml", package = "MsBackendHmdb"),
               full.names = TRUE, pattern = "xml$")
    be <- MsBackendHmdbXml()

    ## Import a single file.
    res <- backendInitialize(be, fls[1])
    expect_identical(length(res), 1L)
    expect_identical(res$dataStorage, "<memory>")
    expect_identical(res$dataOrigin, normalizePath(fls[1]))
    expect_identical(res$msLevel, 2L)
    expect_identical(res$compound_id, "HMDB0000001")
    expect_true(lengths(res$mz) == 5)

    ## Import multiple files.
    res_all <- backendInitialize(be, fls)
    expect_true(length(res_all) == length(fls))
    expect_identical(res_all[1]$mz, res$mz)
    expect_true(all(res_all$msLevel == 2L))
    expect_identical(res_all$dataOrigin, normalizePath(fls))
    expect_true(is.integer(res_all@spectraData$msLevel))

    ## Import with failing file.
    fls <- dir(system.file("xml", package = "MsBackendHmdb"),
               full.names = TRUE, pattern = "xml$", recursive = TRUE)
    fls <- fls[length(fls):1]
    expect_error(res <- backendInitialize(be, fls))

    ## Import with failing file and nonStop = TRUE
    expect_warning(res <- backendInitialize(be, fls, nonStop = TRUE))
    res <- res[4:1]
    expect_identical(res$compound_id, res_all$compound_id)
    expect_identical(res$dataOrigin, res_all$dataOrigin)

    ## errors
    expect_error(backendInitialize(be), "'files' is mandatory")
    expect_error(backendInitialize(be, 4), "expected to be a character")
    expect_error(backendInitialize(be, "a"), "a not found")
})
