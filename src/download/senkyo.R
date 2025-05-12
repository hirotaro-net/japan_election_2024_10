# ファイルが存在しない場合は以下の処理を行う
if (!file.exists(paste(getwd(), "/data/processed/election_results.csv", sep = ""))) {
    # 選挙結果
    data <- tibble::tibble()
    target_numbers <- c(979538, 979539, 979540, 979541, 979542, 979543, 979544, 979545, 979546, 979547, 979548, 979549, 979551, 979558, 979550, 979553, 979555, 979556, 979561, 979562, 979563, 979564, 979568, 979569, 979570, 979571, 979573, 979574, 979575, 979578, 979579, 979580, 979581, 979582, 979585, 979586, 979587, 979588, 979589, 979590, 979592, 979594, 987140, 979596, 979597, 979598, 979552)
    for (n in target_numbers) {
        file_url <- paste("https://www.soumu.go.jp/main_content/000", n, ".xlsx", sep = "")
        destfile <- paste(getwd(), "/data/raw/results/", n, ".xlsx", sep = "")
        download.file(
            file_url,
            destfile = destfile,
            method = "curl",
            cacheOK = TRUE
        )
        
        tryCatch(
            sheets <- readxl::excel_sheets(paste(getwd(), "/data/raw/results/", n, ".xlsx", sep = "")),
            error = function (e) {
                next
            }
        )
        
        for (sheet_name in sheets) {
            excel <- readxl::read_xlsx(
                paste(getwd(), "/data/raw/results/", n, ".xlsx", sep = ""),
                sheet = sheet_name,
                skip = 3
            ) |> 
                dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) |> 
                dplyr::select(-`得票数計`)
            
            excel <- tibble::as_tibble(t(excel), rownames = "候補者名") |> 
                janitor::row_to_names(row_number = 1)
            
            excel <- excel |> 
                tidyr::pivot_longer(
                    cols = -c(`市区町村名＼政党名`, `候補者名`),
                    names_to = "市区町村名",
                    values_to = "得票数"
                ) |> 
                dplyr::rename(`政党名` = `市区町村名＼政党名`) |> 
                dplyr::mutate(`小選挙区` = sheet_name) |> 
                dplyr::mutate(`都道府県名` = stringr::str_extract(`小選挙区`, "(北海道|青森県|岩手県|宮城県|秋田県|山形県|福島県|茨城県|栃木県|群馬県|埼玉県|千葉県|東京都|神奈川県|新潟県|富山県|石川県|福井県|山梨県|長野県|岐阜県|静岡県|愛知県|三重県|滋賀県|京都府|大阪府|兵庫県|奈良県|和歌山県|鳥取県|島根県|岡山県|広島県|山口県|徳島県|香川県|愛媛県|高知県|福岡県|佐賀県|長崎県|熊本県|大分県|宮崎県|鹿児島県|沖縄県)")) |> 
                dplyr::mutate(`選挙区` = stringr::str_extract(`小選挙区`, "第[０１２３４５６７８９]+区")) |> 
                dplyr::mutate(`市区町村名` = stringr::str_replace_all(`市区町村名`, "第[０１２３４５６７８９]+区", "")) |> 
                dplyr::mutate(`市区町村名` = stringr::str_replace_all(`市区町村名`, "[\\s　]+", "")) |> 
                dplyr::select(`小選挙区`, `都道府県名`, `市区町村名`, `選挙区`, `政党名`, `候補者名`, `得票数`)
            data <- dplyr::bind_rows(data, excel)
        }
    }
    
    write.table(data, file = paste(getwd(), "/data/processed/election_results.csv", sep = ""), sep = ",", row.names = FALSE)
}
