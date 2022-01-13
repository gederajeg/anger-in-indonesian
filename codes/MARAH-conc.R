library(corplingr)
leipzig_path <- collogetr::leipzig_corpus_path

# 1000 concordance lines for "marah", "kemarahan", and "amarah"
marah <- corplingr::concord_leipzig(leipzig_path, "(?<!-)\\bmarah\\b(?!-+)")
marah <- dplyr::mutate(marah, cases = 1:nrow(marah))
marah_1K <- dplyr::slice_sample(marah, n = 1000)
marah_1K <- dplyr::select(marah_1K, cases, dplyr::everything())
sort_words <- stringr::str_extract(marah_1K$left, "([^\\s]+)([^a-zA-Z])?$")
marah_1K$sort_words <- sort_words
marah_1K <- dplyr::arrange(marah_1K, sort_words)
readr::write_tsv(dplyr::select(marah_1K, -start, -end, -sort_words), "data/leipzig-marah1K-conc.txt")

kemarahan <- corplingr::concord_leipzig(leipzig_path, "(?<!-)\\bkemarahan\\b(?!-+)")
kemarahan <- dplyr::mutate(kemarahan, cases = 1:nrow(kemarahan))
kemarahan_1K <- dplyr::slice_sample(kemarahan, n = 1000)
kemarahan_1K <- dplyr::select(kemarahan_1K, cases, dplyr::everything())
sort_words <- stringr::str_extract(kemarahan_1K$left, "([^\\s]+)([^a-zA-Z])?$")
kemarahan_1K$sort_words <- sort_words
kemarahan_1K <- dplyr::arrange(kemarahan_1K, sort_words)
readr::write_tsv(dplyr::select(kemarahan_1K, -start, -end, -sort_words), "data/leipzig-kemarahan1K-conc.txt")

amarah <- corplingr::concord_leipzig(leipzig_path, "(?<!-)\\bamarah\\b(?!-+)")
amarah <- dplyr::mutate(amarah, cases = 1:nrow(amarah))
amarah_1K <- dplyr::slice_sample(amarah, n = 1000)
amarah_1K <- dplyr::select(amarah_1K, cases, dplyr::everything())
sort_words <- stringr::str_extract(amarah_1K$left, "([^\\s]+)([^a-zA-Z])?$")
amarah_1K$sort_words <- sort_words
amarah_1K <- dplyr::arrange(amarah_1K, sort_words)
readr::write_tsv(dplyr::select(amarah_1K, -start, -end, -sort_words), "data/leipzig-amarah1K-conc.txt")

save(marah, marah_1K, kemarahan, kemarahan_1K, amarah, amarah_1K, file = "data/leipzig-MARAH-conc.RData")
