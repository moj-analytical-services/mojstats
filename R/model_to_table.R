model_to_table <- function() {

Query_dim <- "SELECT * FROM awsdatacatalog.sirius_derived_dev_dbt.opg_family_court_stats"

con <- Rdbtools::connect_athena()
data <- Rdbtools::dbGetQuery(con, Query_dim)
Rdbtools::dbDisconnect(con)

df_list <- data %>%
  split(interaction(.$attribute_name, .$period_type)) %>%
  lapply(function(x){dplyr::arrange(x,period_name)}) %>%
  lapply(function(x){dplyr::select(x,c(period_name,attribute_value,count))}) %>%
  lapply(function(x){tidyr::pivot_wider(x,names_from = attribute_value,values_from= count)}) %>%
  lapply(function(x){replace(x,is.na(x),0)})

cover_list <- list(
  "Section 1" = c("First row of Section 1.", "Second row of Section 1."),
  "Section 2" = "The only row of Section 2.",
  "Section 3" = c(
    "[Website](https://co-analysis.github.io/a11ytables/)",
    "[Email address](mailto:fake.address@a11ytables.com)"
  )
)

contents_df <- data.frame(
  "Sheet name" = c("Notes", "Table_1", "Table_2"),
  "Sheet title" = c(
    "Notes used in this workbook",
    "First Example Sheet",
    "Second Example Sheet"
  ),
  check.names = FALSE
)

a11y_table <- a11ytables::create_a11ytable(
  tab_titles = c("cover","contents",paste0("Table_",1:length(df_list))),
  sheet_types = c("cover","contents",rep("tables",length(df_list))),
  sheet_titles = c("Cover","Contents",paste("Power of Attorney applications:",stringr::str_replace(names(df_list),"\\."," by "))),
  blank_cells = rep(NA_character_,length(df_list)+2),
  sources = c(rep(NA_character_,2),rep("Sirius case management system",length(df_list))),
  tables = append(append(df_list,list(contents_df),0),list(cover_list),0)
)

a11y_workbook <- a11ytables::generate_workbook(a11y_table)

Rs3tools::write_using(a11y_workbook, openxlsx::saveWorkbook,  "alpha-data-modelling-iceberg/stats-table.xlsx", overwrite=TRUE)

#openxlsx::saveWorkbook(a11y_workbook,"workbook.xlsx")

}
