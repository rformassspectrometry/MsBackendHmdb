test_that(".import_hmdb_ms_ms_spectrum works", {
    fl <- system.file("xml/HMDB0000001_ms_ms_spectrum_2_experimental.xml",
                      package = "MsBackendHmdbXml")

    expect_error(.import_hmdb_ms_ms_spectrum(), "is missing")
    expect_error(.import_hmdb_ms_ms_spectrum(4), "character with length")

    res <- MsBackendHmdbXml:::.import_hmdb_ms_ms_spectrum(fl)
    cns <- c("msLevel", "spectrum_id", "compound_id", "polarity",
             "collisionEnergy", "predicted", "splash", "instrument_type",
             "dataOrigin", "mz", "intensity")
    expect_equal(colnames(res), cns)
    expect_true(is(res$mz, "NumericList"))
    expect_true(is(res$intensity, "NumericList"))
    expect_equal(length(res$intensity[[1]]), length(res$mz[[1]]))
    expect_equal(length(res$mz[[1]]), 7)

    expect_equal(res$compound_id[1], "HMDB0000001")

    ## One that should fail.
    fl <- system.file("xml/fail/HMDB0001875_ms_ms_spectrum_1768_experimental.xml",
                      package = "MsBackendHmdbXml")
    expect_error(.import_hmdb_ms_ms_spectrum(fl), "No mz and intensity")
    expect_warning(.import_hmdb_ms_ms_spectrum(fl, nonStop = TRUE))
})
