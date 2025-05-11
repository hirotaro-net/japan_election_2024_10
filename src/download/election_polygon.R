# 日本全国のポリゴンデータ
polygon_data_url <- "https://gtfs-gis.jp/senkyoku2022/senkyoku2022.zip"
destfile <- paste(getwd(), "/data/raw/election.zip", sep = "")

tryCatch(
    download.file(
        url = polygon_data_url,
        destfile = destfile,
        method = "curl"
    ),
    error = function (e) {
        next
    }
)

unzip(
    zipfile = destfile,
    overwrite = FALSE,
    exdir = paste(getwd(), "/data/processed/polygon/", sep = "")
)
