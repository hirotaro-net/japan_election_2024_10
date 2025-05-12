# 必要なデータをダウンロードする
source("./src/download/senkyo.R")
source("./src/download/election_polygon.R")

# Custom Function ----
prefList <- c("北海道" = 1,　"青森" = 2,　"岩手" = 3,　"宮城" = 4,　"秋田" = 5,　"山形" = 6,　"福島" = 7,　"茨城" = 8,　"栃木" = 9,　"群馬" = 10,　"埼玉" = 11,　"千葉" = 12,　"東京" = 13,　"神奈川" = 14,　"新潟" = 15,　"富山" = 16,　"石川" = 17,　"福井" = 18,　"山梨" = 19,　"長野" = 20,　"岐阜" = 21,　"静岡" = 22,　"愛知" = 23,　"三重" = 24,　"滋賀" = 25,　"京都" = 26,　"大阪" = 27,　"兵庫" = 28,　"奈良" = 29,　"和歌山" = 30,　"鳥取" = 31,　"島根" = 32,　"岡山" = 33,　"広島" = 34,　"山口" = 35,　"徳島" = 36,　"香川" = 37,　"愛媛" = 38,　"高知" = 39,　"福岡" = 40,　"佐賀" = 41,　"長崎" = 42,　"熊本" = 43,　"大分" = 44,　"宮崎" = 45,　"鹿児島" = 46,　"沖縄" = 47)

colorFunc <- function(party) {
    party_colors <- c(
        "自由民主党" = "#d22319",
        "立憲民主党" = "#2391ff",
        "日本維新の会" = "#e19a00",
        "公明党" = "#eb61be",
        "国民民主党" = "#0010a5",
        "日本共産党" = "#6e41e1",
        "れいわ新選組" = "#f0a0a7",
        "参政党" = "#eb640a",
        "（日本保守党）" = "#9696f0",
        "社会民主党" = "#05555a",
        "みんなでつくる党" = "#b6c81b",
        "その他" = "#808080"
    )
    
    if (is.na(party)) {
        return("#808080")  # NAの場合の色
    } else {
        return(party_colors[party])
    }
}

prefFunc <- function(election_area) {
    pref <- stringr::str_extract(election_area, "(北海道|青森|岩手|宮城|秋田|山形|福島|茨城|栃木|群馬|埼玉|千葉|東京|神奈川|新潟|富山|石川|福井|山梨|長野|岐阜|静岡|愛知|三重|滋賀|京都|大阪|兵庫|奈良|和歌山|鳥取|島根|岡山|広島|山口|徳島|香川|愛媛|高知|福岡|佐賀|長崎|熊本|大分|宮崎|鹿児島|沖縄)")
    if (is.na(pref)) {
        return("Unknown")
    } else {
        return(prefList[pref])
    }
    
}

# Data Component ----
election_results <- readr::read_csv(paste(getwd(), "/data/processed/election_results.csv", sep = "")) |> 
    dplyr::group_by(`小選挙区`, `政党名`, `候補者名`) |> 
    dplyr::summarise(`合計得票数` = sum(`得票数`)) |> 
    dplyr::group_by(`小選挙区`) |> 
    dplyr::arrange(-`合計得票数`) |> 
    dplyr::mutate(`次点との票差` = `合計得票数` - dplyr::lead(`合計得票数`)) |> 
    dplyr::mutate(`次点の票数に対する倍数` = `合計得票数` / dplyr::lead(`合計得票数`)) |> 
    dplyr::mutate(`小選挙区` = stringr::str_replace_all(`小選挙区`, "[都府県]第", "")) |> 
    dplyr::mutate(`小選挙区` = stringr::str_replace_all(`小選挙区`, "第", "")) |> 
    dplyr::mutate(`小選挙区` = stringi::stri_trans_nfkc(`小選挙区`)) |> 
    dplyr::mutate(`政党名` = tidyr::replace_na(`政党名`, "その他")) |> 
    dplyr::ungroup() |>
    dplyr::mutate(pref = as.numeric(sapply(`小選挙区`, prefFunc)))

election_polygon <- sf::read_sf(paste(getwd(), "/data/processed/polygon/senkyoku2022.shp", sep = "")) |>
    dplyr::rename(`小選挙区` = kuname) |> 
    dplyr::left_join(y = election_results, by = "小選挙区") |>
    dplyr::group_by(`小選挙区`) |>
    dplyr::filter(`合計得票数` == max(`合計得票数`)) |> 
    dplyr::mutate(fillColor = sapply(`政党名`, colorFunc))


# Define UI ----
ui <- bslib::page_sidebar(
    title = "第50回衆議院議員総選挙",
    sidebar = bslib::sidebar(
        "フィルター",
        shiny::selectInput(
            "prefCode",
            "都道府県",
            choices = prefList
        ),
        shiny::selectInput(
            "electionArea", 
            "小選挙区", 
            choices = NULL
        )
    ),
    bslib::card(
        bslib::card_header("都道府県内の得票状況"),
        leaflet::leafletOutput("map"),
        height = "60%"
    ),
    bslib::card(
        bslib::card_header("候補者別得票数"),
        DT::DTOutput("table"),
        height = "40%"
    )
)

# Define server logic ----
server <- function(input, output, session) {
    selected_polygon <- shiny::reactive({
        election_polygon |> 
            dplyr::filter(ken == input$prefCode) |> 
            dplyr::select(-c(UserID, ken, ku, kucode))
    })
    
    output$map <- leaflet::renderLeaflet({
        polygon_data <- selected_polygon()
        
        mapview::mapview(
            x = polygon_data,
            zcol = "政党名",
            layer.name = "政党名",
            color = polygon_data$fillColor,
            lwd = 2,
            col.regions = polygon_data$fillColor,
            alpha.regions = "次点の票数に対する倍数",
            map.types = "CartoDB.Positron"
        )@map
    })
    
    selected_table <- shiny::reactive({
        election_results |> 
            dplyr::filter(pref == input$prefCode) |> 
            dplyr::filter(`小選挙区` == input$electionArea) |> 
            dplyr::select(-c(pref))
    })

    output$table <- DT::renderDT({
        selected_table()
    })
    
    election_areas <- reactive({
        election_results |> 
            dplyr::filter(pref == input$prefCode) |> 
            dplyr::distinct(`小選挙区`) |> 
            dplyr::arrange(`小選挙区`)
    })
    
    observeEvent(input$prefCode, {
        shiny::updateSelectInput(session, "electionArea", choices = election_areas())
    })
}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)
