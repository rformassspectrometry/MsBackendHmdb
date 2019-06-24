#' @description
#'
#' Utility function to parse the data from a single spectrum xml file from
#' HMDB.
#'
#' @param x `character(1)` with the file path/name of the xml file.
#'
#' @param nonStop `logical(1)` whether content-related errors should be
#'     reported as a `warning`.
#'
#' @return `DataFrame`
#'
#' @author Johannes Rainer
#'
#' @return
#'
#' `DataFrame` with one row and columns:
#' - msLevel (`integer`): the MS level (2).
#' - spectrum_id (`character`): the HMDB-internal ID of the spectrum.
#' - compound_id (`character`): the HMDB ID the spectrum is associated with.
#' - polarity (`integer`): 0 for negative, 1 for positive, `NA` for not set.
#' - collision_energy (`numeric`): collision energy voltage.
#' - predicted (`logical`): whether the spectrum is predicted or experimentally
#'   verified.
#' - splash (`character`): the SPLASH key of the spectrum.
#' - instrument_type (`character`): the type of instrument on which the
#'   spectrum was measured.
#' - mz [NumericList()]: m/z values of the spectrum.
#' - intensity [NumericList()]: intensity of the spectrum.
#'
#' @md
#'
#' @noRd
#'
#' @importFrom xml2 read_xml xml_text xml_find_first xml_find_all xml_double
#'
#' @importFrom S4Vectors DataFrame
#'
#' @importClassesFrom S4Vectors DataFrame
#'
#' @importFrom IRanges NumericList
#'
#' @importClassesFrom IRanges NumericList
.import_hmdb_ms_ms_spectrum <- function(x, nonStop = FALSE) {
    if (!is.character(x) || length(x) != 1)
        stop("'x' has to be of type character with length 1")
    x_ml <- read_xml(x)
    id <- xml_text(xml_find_first(x_ml, "id"))
    cmp_id <- xml_text(xml_find_first(x_ml, "database-id"))
    if (id == "" || cmp_id == "") {
        msg <- paste0("Could not extract the HMDB ID from ", basename(x),
                      "! Is the file a spectrum xml file from HMDB?")
        if (nonStop) {
            warning(msg)
            return(DataFrame())
        } else stop(msg)
    }
    plrty <- xml_text(xml_find_first(x_ml, "ionization-mode"))
    ## 0: negative, +1: positive, NA: not set.
    if (plrty == "")
        plrty <- NA_integer_
    else plrty <- ifelse(length(grep("pos", tolower(plrty))), yes = 1L, no = 0L)
    cev <- xml_double(xml_find_first(x_ml, "collision-energy-voltage"))
    prd <- xml_text(xml_find_first(x_ml, "predicted"))
    if (prd == "")
        prd <- NA
    else prd <- ifelse(prd == "false", yes = FALSE, no = TRUE)
    splsh <- xml_text(xml_find_first(x_ml, "splash-key"))
    itype <- xml_text(xml_find_first(x_ml, "instrument-type"))
    if (itype == "")
        itype <- NA_character_
    mz <- xml_double(xml_find_all(x_ml, "ms-ms-peaks/ms-ms-peak/mass-charge"))
    int <- xml_double(xml_find_all(x_ml, "ms-ms-peaks/ms-ms-peak/intensity"))
    if (!length(mz) || !length(int) || length(mz) != length(int)) {
        msg <- paste0("No mz and intensity values found in file ", basename(x))
        if (nonStop) {
            warning(msg)
            return(DataFrame())
        } else stop(msg)
    }
    if (is.unsorted(mz)) {
        idx <- order(mz)
        mz <- mz[idx]
        int <- int[idx]
    }
    res <- DataFrame(
        msLevel = 2L,
        spectrum_id = id,
        compound_id = cmp_id,
        polarity = plrty,
        collisionEnergy = cev,
        predicted = prd,
        splash = splsh,
        instrument_type = itype,
        dataOrigin = x
        )
    res$mz <- NumericList(mz, compress = FALSE)
    res$intensity <- NumericList(int, compress = FALSE)
    res
}
