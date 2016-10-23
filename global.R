library(shiny)
#suppressPackageStartupMessages(library(googleVis))
library(googleVis)
#library(RPostgreSQL)
require("RPostgreSQL")

#Connect to the database
drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname = "heracles",
#                  host = "172.22.25.178", port = 5432,
#                  user = "postgres", password = 601680) 


  con <- dbConnect(drv, dbname = "DMS",
                 host = "172.22.25.178", port = 5432,
                 user = "postgres", password = 601680) 
  #on.exit(dbDisconnect(con))
#rm(pw) # removes the password

# query the data from postgreSQL 
# df_postgres <- dbGetQuery(con, "SELECT * from cartable")
  melt_db <- dbGetQuery(con, "SELECT * from dms_melt")
  by_project_db <- dbGetQuery(con, "SELECT * from manhour_by_project")

  #postgresqlpqExec(con, "SET client_encoding = 'windows-1252'")
  #postgresqlpqExec(con, "SET client_encoding = 'GBK'")
  #postgresqlpqExec(con, "SET client_encoding = 'big5'")
  monthly_expense <- dbGetQuery(con, "SELECT * from expense_table")