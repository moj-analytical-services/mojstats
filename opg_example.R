cover_df <- read.csv("cover.csv",check.names=FALSE)
notes_df <- read.csv("notes.csv",check.names=FALSE)
tables_df <- read.csv("contents.csv",check.names=FALSE)

SQL <- "SELECT * FROM sirius_derived_dev_dbt.opg_family_court_stats_published"

data <- query_athena(SQL)

extract_date <- trimws(format(unique(data$extract_date), "%e %B %Y"))

build_stats_tables(data,
                   c("case_type","case_subtype","gender","age_band"),
                   "count",
                   title = "Office of the Public Guardian Statistics",
                   cover_df,
                   notes_df,
                   tables_df,
                   "output.xlsx")
