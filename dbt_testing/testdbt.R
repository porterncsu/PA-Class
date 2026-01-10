library(duckdb)
con <- dbConnect(duckdb(), dbdir = "/Users/kristinporter/Documents/NCSU/my_dbt/dev.duckdb")

# See your tables
dbListTables(con)

# Look at the bank data
bank <- dbGetQuery(con, 'SELECT * FROM "bank-full" LIMIT 100')
head(bank)