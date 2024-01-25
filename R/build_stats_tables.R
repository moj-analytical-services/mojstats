build_stats_tables <- function(data,
                          dimensions,
                          indicator = "count",
                          title,
                          cover_df,
                          notes_df,
                          tables_df,
                          outfile_s3bucket,
                          outfile_filename) {

  data_tables <- to_table_list(data,dimensions,indicator)

  contents_df <- dplyr::bind_rows(
    c(`Sheet name` = "Notes",`Sheet title` = "Notes used in this workbook"),
    tables_df[,1:2])

  a11ytable <-
    a11ytables::create_a11ytable(
      tab_titles = c(
        "Cover",
        "Contents",
        "Notes",
        paste0("Table_", c(1:length(data_tables)))
      ),
      sheet_types = c(
        "cover",
        "contents",
        "notes",
        rep("tables",length(data_tables))
      ),
      sheet_titles = c(
        title,
        "Table of contents",
        "Notes",
        paste0(tables_df$`Sheet name`,": ",tables_df$`Sheet title`," ",tables_df$Notes)
      ),
      blank_cells = c(
        rep(NA_character_,length(data_tables)+3)
      ),
      sources = c(
        NA_character_,
        NA_character_,
        NA_character_,
        tables_df$Sources
      ),
      tables = c(list(
        cover_df,
        contents_df,
        notes_df),
        data_tables
      )
    )

  wb <- a11ytables::generate_workbook(a11ytable)

  sheet_array <- c(1:length(data_tables)+3)

  data_length <- sapply(data_tables,nrow)

  options("openxlsx.numFmt" = "#,##0")

  lapply(sheet_array,function(x) {openxlsx::readWorkbook(wb,x,startRow = 4)}) %>%
    lapply(type.convert,as.is=TRUE) %>%
    mapply(openxlsx::writeData,
           sheet = sheet_array,
           x = .,
           MoreArgs = list(wb = wb,startRow = 5, colNames = FALSE))

  mapply(function(x,y){
    openxlsx::addStyle(wb=wb,
                       sheet=x,
                       style=openxlsx::createStyle(halign = "left"),
                       cols = 1,
                       rows = 4:(y+3),
                       stack = TRUE)
      },
      sheet_array,
      data_length
    )

  mapply(function(x,y){
    openxlsx::addStyle(wb=wb,
                       sheet=x,
                       style=openxlsx::createStyle(numFmt = "GENERAL",
                                         halign = "left"),
                       cols = 1,
                       rows = 5:(y+4))
    },
    sheet_array,
    data_length
  )

  mapply(function(x,y){
    openxlsx::setRowHeights(wb=wb,
                            sheet=x,
                            rows=2:(y+4),
                            heights = 17)
    },
    sheet_array,
    data_length
  )

  mapply(function(x,y){

    if("Quarter" %in% names(y)) {

      openxlsx::setRowHeights(wb=wb,
                              sheet=x,
                              rows=stringr::str_which(y$Quarter,"Jan to Mar")+4,
                              heights = 34)
    }
  },
  sheet_array,
  data_tables
  )

  lapply(sheet_array,function(x) {openxlsx::setRowHeights(wb,x,c(4:5),34)})

 # openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)

  # save file to AWS S3 bucket
 # s3_bucket <- "alpha-forward-look"
  #filename  <- "Forward Look.xlsx"

  Rs3tools::write_using(wb, openxlsx::saveWorkbook, paste0(outfile_s3bucket, "/", outfile_filename), overwrite=TRUE)

}
