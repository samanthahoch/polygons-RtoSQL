savePolygonsToSQL <- function(polygons_spdf, dest_tbl, dest_db, dest_server = "camboscatmgt01\\catmgt", simplify = F){
  library(sp)
  library(rgdal)
  library(wicket)
  library(rmapshaper)
  library(RODBC)
  options(stringsAsFactors = F, scipen = 9999)
  
  
  # projects polygons to WGS 84
  polygons_spdf <- spTransform(polygons_spdf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  print("Polygons projected to WGS 84")
  
  polygons_spdf$wkt <- NA
  
  # converts each polygon to WKT 
  for (i in 1:nrow(polygons_spdf@data))  {
    polygons_spdf[i, "wkt"] <- sp_convert(polygons_spdf[i, ]) 
  }
  
  
  # simplify polygons to be less than 8000 characters if condition is true
  if (simplify) {
    for (i in 1:nrow(polygons_spdf@data)) {
      length_wkt <- nchar(polygons_spdf@data[i, "wkt"])
      
      if (length_wkt >= 8000) {
        # determines percentage of points to keep to be under 8000 chars
        # * adds 0.05 buffer
        keep <- 1 - (((length_wkt - 8000) / length_wkt) + 0.05)
        
        # simplifies polygon by specified amount
        polygons_spdf@polygons[i] <- ms_simplify(polygons_spdf[i, ], keep = keep, keep_shapes = TRUE)@polygons
        print(paste("Polygon", i, "simplified to contain", format(round(keep * 100, 2), nsmall = 2), "% of points"))
        
        # converts the new simplified polygon to WKT
        polygons_spdf[i, "wkt"] <- sp_convert(polygons_spdf[i, ]) 
      }
    } # end simplify polygons
  }
  
  
  # creates the string need to connect to the server
  conText <- paste("DRIVER=SQL Server",";Database=", dest_db, ";Server=", dest_server,
                   ";Port=;PROTOCOL=TCPIP",";trusted_connection=true",sep="")
  
  con <- odbcDriverConnect(conText, readOnlyOptimize = TRUE)
  
  # name of intermediate table to store data before conversion
  intrTable <- paste0(dest_tbl, "Intr")
  
  # copies data to intermediate table
  sqlSave(con, polygons_spdf@data, intrTable, safer = F, rownames = FALSE, varTypes = c(wkt = "NVARCHAR(MAX)"))
  
  sqlDrop(con, dest_tbl)
  
  # uploads polygons to dest_tbl
  tryCatch(expr = {
    
    # transforms data from intrTable to SQL geometry
    sql_query <- paste0("SELECT *, geography::STGeomFromText(wkt, 4326) as Geom into ",
                        dest_db, ".dbo.", dest_tbl, " FROM ", dest_db, ".dbo.", intrTable)
    print(paste("Attempting to execute:", sql_query, "..."))
    
    sqlQuery(con, sql_query, rows_at_time = 1)
    print(paste("Upload to", dest_tbl, "successful"))
    
    
    # makes sure the geometries in the dest_tbl are valid
    sql_query <- paste0("UPDATE ",  dest_db, ".dbo.", dest_tbl,
                        " SET Geom = Geom.MakeValid() WHERE Geom.STIsValid() = 0;")
    
    sqlQuery(con, sql_query)
    print(paste("Validating geometries in", dest_tbl, "successful"))
    
  }, error = function(e){
    
    print(paste("Upload to", dest_tbl, "unsuccessful"))
    
  }) # end catch upload to dest_tbl
  
  
  sqlDrop(con, intrTable) 
  odbcClose(con)
  
  
} # end function