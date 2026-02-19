# This R script stores useful functions for processing the climate extremes

find_date <- function(nc_file, start, end) {
  
  print(paste("Processing file:", nc_file))  # Debugging line
  
  nc_data <- nc_open(nc_file)
  time <- ncvar_get(nc_data, "time")
  tunits  <- ncatt_get(nc_data, "time", "units")
  
  # Convert time to CFtime class
  cf <- CFtime(tunits$value, calendar = "standard", time)
  timestamps <- as_timestamp(cf)
  time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
  time_cf$time_index <- row_number(time_cf)
  
  start_month  <- time_cf %>% filter(year == start) %>% pull(month) %>% min()
  end_month  <- time_cf %>% filter(year == end) %>% pull(month) %>% max()
  
  # Print the time range for this file for debugging
  print(paste("Time range in the file:", min(time_cf$year), "-", max(time_cf$year)))  # Debugging line
  
  # Find the time indices for the start and end dates
  startdate <- time_cf %>%
    filter(year == start, month == start_month) %>%
    pull(time_index) %>%
    first()
  
  enddate <- time_cf %>%
    filter(year == end, month == end_month) %>%
    pull(time_index) %>%
    first()
 
   # Save startdate and enddate to the calling environment
  assign("startdate", startdate, envir = .GlobalEnv)
  assign("enddate", enddate, envir = .GlobalEnv)
  # Return the start and end indices
  return(list(startdate = startdate, enddate = enddate))
}



process_rx5day_data <- function(nc_file, output_dir) {
  # Open the NetCDF file using terra
  
  for (time_index in startdate:enddate) { 
    start_time <- Sys.time()
    print(paste("Processing time index:", time_index))
    
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    lon <- ncvar_get(nc_data, "lon")
    lat <- ncvar_get(nc_data, "lat")
    time <- ncvar_get(nc_data, "time")
    tunits  <- ncatt_get(nc_data, "time", "units")
    
    fillvalue <- ncatt_get(nc_data, "rx5day", "_FillValue")$value
    missingvalue <- ncatt_get(nc_data, "rx5day", "missing_value")$value
    
    # Experiment ID and member info
    experiment_id <- ncatt_get(nc_data, 0, "experiment_id")$value
    member <- ncatt_get(nc_data, 0, "parent_source_id")$value
    
    # Convert time to CFtime class
    cf <- CFtime(tunits$value, calendar = "standard", time)
    timestamps <- as_timestamp(cf)
    time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
    time_cf$time_index <- row_number(time_cf)
    
    start_month  <- time_cf %>% filter(year == start) %>% pull(month) %>% min()
    end_month  <- time_cf %>% filter(year == end) %>% pull(month) %>% max()
    
    spi_rast <- rast(nc_file)
    # Extract the layers for the current time index across all members
    spi_layers <- spi_rast[[grep(paste0("_", time_index, "$"), names(spi_rast))]]
    
    # Replace fill values and missing values with NA
    replace_values <- function(x) {
      x[x == fillvalue] <- NA
      x[x == missingvalue] <- NA
      return(x)
    }
    
    # Apply the function to all layers
    spi_layers <- app(spi_layers, replace_values)
    
    # Project to the same CRS
    spi_layers <- project(spi_layers, "EPSG:4326")
    
    std_extent <- ext(-180, 180, -90, 90)
    
    # Crop the raster to the standard extent
    spi_layers <- crop(spi_layers, std_extent)
    
    # Compute the quantiles and min/max for each pixel across all models
    quantiles <- function(x) {
      if (all(is.na(x))) {
        return(c(NA, NA, NA))
      } else {
        return(quantile(x, probs = c(0.10, 0.50, 0.90), na.rm = TRUE))
      }
    }
    
    quantile_rasters <- app(spi_layers, quantiles)
    
    
    quantile_rasters$time_index <- time_index
    
    rx5day_values <- as.data.frame(quantile_rasters, xy = T)
    names(rx5day_values) <- c('lon', 'lat', "rx5day_10th", "rx5day_median", "rx5day_90th", 'time_index')
    
    # Save the results of the current time step to Parquet files
    write_parquet(rx5day_values, file.path(output_dir, paste0("rx5day_grid_timestep", time_index, ".parquet")))
    
    end_time <- Sys.time()
    duration <- end_time - start_time
    
    cat(green(time_index, "processed and saved in", round(duration, 2), "seconds!\n"))
  }
}



process_spei6_data <- function(nc_file, output_dir) {
  # Open the NetCDF file using terra
  
  for (time_index in startdate:enddate) { 
    start_time <- Sys.time()
    print(paste("Processing time index:", time_index))
    
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    lon <- ncvar_get(nc_data, "lon")
    lat <- ncvar_get(nc_data, "lat")
    time <- ncvar_get(nc_data, "time")
    tunits  <- ncatt_get(nc_data, "time", "units")
    
    fillvalue <- ncatt_get(nc_data, "spei6", "_FillValue")$value
    missingvalue <- ncatt_get(nc_data, "spei6", "missing_value")$value
    
    # Experiment ID and member info
    experiment_id <- ncatt_get(nc_data, 0, "experiment_id")$value
    member <- ncatt_get(nc_data, 0, "parent_source_id")$value
    
    # Convert time to CFtime class
    cf <- CFtime(tunits$value, calendar = "standard", time)
    timestamps <- as_timestamp(cf)
    time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
    time_cf$time_index <- row_number(time_cf)
    
    
    start_month  <- time_cf %>% filter(year == start) %>% pull(month) %>% min()
    end_month  <- time_cf %>% filter(year == end) %>% pull(month) %>% max()
    
    spi_rast <- rast(nc_file)
    # Extract the layers for the current time index across all members
    spi_layers <- spi_rast[[grep(paste0("_", time_index, "$"), names(spi_rast))]]
    
    # Replace fill values and missing values with NA
    replace_values <- function(x) {
      x[x == fillvalue] <- NA
      x[x == missingvalue] <- NA
      return(x)
    }
    
    # Apply the function to all layers
    spi_layers <- app(spi_layers, replace_values)
    
    # Project to the same CRS
    spi_layers <- project(spi_layers, "EPSG:4326")
    
    std_extent <- ext(-180, 180, -90, 90)
    
    # Crop the raster to the standard extent
    spi_layers <- crop(spi_layers, std_extent)
    
    # Compute the quantiles and min/max for each pixel across all models
    quantiles <- function(x) {
      if (all(is.na(x))) {
        return(c(NA, NA, NA))
      } else {
        return(quantile(x, probs = c(0.10, 0.50, 0.90), na.rm = TRUE))
      }
    }
    
    quantile_rasters <- app(spi_layers, quantiles)
    
    
    quantile_rasters$time_index <- time_index
    
    spei6_values <- as.data.frame(quantile_rasters, xy = T)
    names(spei6_values) <- c('lon', 'lat', "spei6_10th", "spei6_median", "spei6_90th", 'time_index')
    
    # Save the results of the current time step to Parquet files
    write_parquet(spei6_values, file.path(output_dir, paste0("spei6_grid_timestep", time_index, ".parquet")))
    
    end_time <- Sys.time()
    duration <- end_time - start_time
    
    cat(green(time_index, "processed and saved in", round(duration, 2), "seconds!\n"))
  }
}



process_spi6_data <- function(nc_file, output_dir) {
  # Open the NetCDF file using terra
  
  for (time_index in startdate:enddate) { 
    start_time <- Sys.time()
    print(paste("Processing time index:", time_index))
    
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    lon <- ncvar_get(nc_data, "lon")
    lat <- ncvar_get(nc_data, "lat")
    time <- ncvar_get(nc_data, "time")
    tunits  <- ncatt_get(nc_data, "time", "units")
    
    fillvalue <- ncatt_get(nc_data, "spi6", "_FillValue")$value
    missingvalue <- ncatt_get(nc_data, "spi6", "missing_value")$value
    
    # Experiment ID and member info
    experiment_id <- ncatt_get(nc_data, 0, "experiment_id")$value
    member <- ncatt_get(nc_data, 0, "parent_source_id")$value
    
    # Convert time to CFtime class
    cf <- CFtime(tunits$value, calendar = "standard", time)
    timestamps <- as_timestamp(cf)
    time_cf <- CFparse(cf, timestamps) %>% dplyr::select(year, month)
    time_cf$time_index <- row_number(time_cf)
    
    start_month  <- time_cf %>% filter(year == start) %>% pull(month) %>% min()
    end_month  <- time_cf %>% filter(year == end) %>% pull(month) %>% max()
    
    spi_rast <- rast(nc_file)
    
    # Extract the layers for the current time index across all members
    spi_layers <- spi_rast[[grep(paste0("_", time_index, "$"), names(spi_rast))]]
    
    # Replace fill values and missing values with NA
    replace_values <- function(x) {
      x[x == fillvalue] <- NA
      x[x == missingvalue] <- NA
      return(x)
    }
    
    # Apply the function to all layers
    spi_layers <- app(spi_layers, replace_values)
    
    # Project to the same CRS
    spi_layers <- project(spi_layers, "EPSG:4326")
    
    std_extent <- ext(-180, 180, -90, 90)
    
    # Crop the raster to the standard extent
    spi_layers <- crop(spi_layers, std_extent)
    
    # Compute the quantiles and min/max for each pixel across all models
    quantiles <- function(x, probs) {
      if (all(is.na(x))) {
        return(c(rep(NA, length(probs)), NA, NA))
      } else {
        return(c(quantile(x, probs = probs, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
      }
    }
    
    probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    quantile_rasters <- app(spi_layers, quantiles, probs = probs)
    
    names(quantile_rasters) <- c("spi6_10th", "spi6_25th", "spi6_median", "spi6_75th", "spi6_90th", "spi6_min", "spi6_max")
    
    
    # Aggregate to 1 degree to match CMIP6
    #spi_layer <- aggregate(spi_layer, fact = 4, fun = mean, na.rm  = T)
      
    
    # Load country borders for the current year using cshapes
    #country_borders <- st_read("data/extremes/countries/cshp20poldat_mod.shp") #Change path here
    #country_borders <- st_transform(country_borders, crs = 4326)
    #library(lubridate)
    #country_borders <- country_borders  %>%
    #  filter(year(start) <= 2019 & year(end) >= 2019)
    #country_borders <- country_borders %>% dplyr::select(gwcode, cntry_n)
    #names(country_borders) <- c('gwcode', 'country_name', 'geometry')
    #saveRDS(country_borders, 'data/extremes/countries/countries.Rda')
    country_borders <- readRDS('data/extremes/countries/countries.Rda')
    
    # Extract raster values by country polygons with lon lat (to check) and coverage fraction
    spi6_values <- tryCatch({
      exactextractr::exact_extract(quantile_rasters, country_borders, force_df = TRUE, include_xy = TRUE, coverage_area = FALSE, include_area = TRUE, include_cols = c('gwcode', 'country_name'))
    }, error = function(e) {
      message(paste("Error extracting raster values for time index:", time_index))
      return(NULL)
    })
    
    if (is.null(spi6_values)) {
      next
    }
    
    
    
    # Add the country_id and bind_rows to combine all data frames
    spi6_values <- bind_rows(lapply(seq_along(spi6_values), function(i) {
      spi6_values[[i]] %>% mutate(id = i)
    }))
    
    spi6_values$id <- NULL
    
    
    names(spi6_values) <- c('country_id', 'country_name', 'spi6_10th', 'spi6_25th', 'spi6_median', 'spi6_75th', 'spi6_90th', 'spi6_min', 'spi6_max', 'lon', 'lat', 'coverage_area', 'coverage_fraction')
    
    spi6_values$time_index <- time_index
    
    # Keep only the obs with the highest coverage fraction for each grid cell to avoid cells 
    # being assigned to more than one country
    
    spi6_values <- spi6_values %>%
      group_by(lon, lat, time_index) %>%
      slice_max(coverage_fraction, n = 1) %>%
      ungroup()
    
    
    # Save the results of the current time step to Parquet files
    write_parquet(spi6_values, file.path(output_dir, paste0("spi6_grid_timestep", time_index, ".parquet")))
    
    end_time <- Sys.time()
    duration <- end_time - start_time
    
    cat(green(time_index, "processed and saved in", round(duration, 2), "seconds!\n"))
  }
}



# Main processing function, now updated to use start and end time indices

# Main processing function that uses the identified start and end time indices
process_tx90_quantiles <- function(nc_files, start_time_index, end_time_index, output_dir) {
  
  
  # Outer loop for all time indices
    # Inner loop through each time index between start and end indices
  for (time_index in seq(start_time_index, end_time_index)) {
    cat("Processing time index:", time_index, "\n")
      
      # Create an empty list to hold rasters from all models for the current time index
      model_rasters <- list()
      
      # Inner loop through each model file for the current time index
      for (nc_file in nc_files) {
        cat("Reading file:", nc_file, "\n")
        # Open the NetCDF file and extract the necessary variables
        
        nc_data <- nc_open(nc_file)
        lon <- ncvar_get(nc_data, "lon")
        lat <- ncvar_get(nc_data, 'lat')
        
        # Extract the fill value for this specific file
        fillvalue <- ncatt_get(nc_data, "tx90pETCCDI", "_FillValue")$value
        # Read the raster corresponding to the current time index
        tx90p_rast <- rast(nc_file)[[time_index]]
        
        
        if(max(lon) > 200){
          tx90p_rast <- rotate(tx90p_rast)
          }
        
        # Check if the raster exists
        if (is.null(tx90p_rast)) {
          cat("No raster found for time index:", time_index, "\n")
          nc_close(nc_data)
          next
        }
        
        # Replace missing values (FillValue) with NA
        replace_fill_value <- function(x) {
          x[x == fillvalue] <- NA
          return(x)
        }
        tx90p_rast <- app(tx90p_rast, replace_fill_value)
        target_raster <- rast(nrows = 180, ncols = 360, ext = ext(-180, 180, -90, 90), crs = "EPSG:4326")
        ext(tx90p_rast) <- ext(target_raster)
        tx90p_rast <- project(tx90p_rast, "EPSG:4326")
        
        # Resample to a standard 1x1 degree grid
        resampled_raster <- resample(tx90p_rast, target_raster)
       
        # Add the raster to the list for this time index
        model_rasters[[nc_file]] <- resampled_raster
        
        # Close the NetCDF file for the current model
        nc_close(nc_data)
      }
      
      # If there are no rasters for the current time index, skip to the next
      if (length(model_rasters) == 0) {
        cat("No valid rasters for time index:", time_index, "\n")
        next
      }
      
      # Stack all the rasters for the current time index
      stacked_rasters <- rast(model_rasters)
      
      # Compute the quantiles (10th, 50th, and 90th percentiles) for each pixel
      quantiles <- function(x) {
        if (all(is.na(x))) {
          return(c(NA, NA, NA))
        } else {
          return(quantile(x, probs = c(0.10, 0.50, 0.90), na.rm = TRUE))
        }
      }
  
      # Apply the quantile function across the stacked rasters
      quantile_rasters <- app(stacked_rasters, quantiles)
      
      # Convert to dataframe
      quantile_df <- as.data.frame(quantile_rasters, xy = TRUE, na.rm = TRUE)
      colnames(quantile_df) <- c("lon", "lat", "tx90p_10th", "tx90p_median", "tx90p_90th")
      
      # Add the time_index as a column to the dataframe
      quantile_df$time_index <- time_index
      
      # Save the dataframe as a Parquet file for the current time index
      parquet_file <- file.path(output_dir, paste0("tx90p_quantiles_time_index_", time_index, ".parquet"))
      write_parquet(quantile_df, parquet_file)
      
      cat(green("Quantile computation done and saved for time index:", time_index, "\n"))
    }
  
  cat("All files processed and saved!")
}



#Process rx5day quantiles


# Function to read and combine Parquet files
read_combine_parquet <- function(file_pattern) {
  files <- list.files(output_dir, pattern = file_pattern, full.names = TRUE)
  combined_data <- lapply(files, read_parquet) %>% bind_rows()
  return(combined_data)
}


# List all TIFF files in the directory
process_crop_tiff <- function(directory) {
      
      tiff_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)
      
      # Read all the TIFF files into a list of SpatRaster objects
      tiff_rasters <- lapply(tiff_files, rast)
      
      # Combine all the raste rs into a single SpatRaster object (stack them)
      combined_raster <- do.call(c, tiff_rasters)
      
      # Print the resolution of the current rasters (should be ~10km)
      print(res(combined_raster))
      
      # Remove layers that contain "_I" or "_R" in their names
      valid_layer_names <- names(combined_raster)[!grepl("_I$|_R$", names(combined_raster))]
      
      # Subset the raster to include only the valid layers
      combined_raster <- combined_raster[[valid_layer_names]]
      names(combined_raster) <- tolower(gsub("_A$", "", names(combined_raster)))
      
      # Set the desired resolution to 1x1 degrees (roughly 111km x 111km at the equator)
      # Aggregate by summing to reach the new resolution
      agg_raster <- aggregate(combined_raster, fact = 1/res(combined_raster), fun = sum, na.rm = TRUE)
      
      agg_raster <- project(agg_raster, "EPSG:4326")
      
      std_extent <- ext(-180, 180, -90, 90)
      
      # Crop the raster to the standard extent
      agg_raster <- crop(agg_raster, std_extent)
      
      # Print the new resolution to verify it's 1x1 degree
      print(res(agg_raster))
      
      plot(agg_raster)
      
      # Convert the raster to a dataframe
      # Assign the new names back to the raster object
      crop_data <- terra::as.data.frame(agg_raster, xy = TRUE)
      
      # Convert the dataframe to long format
      crop_data <- crop_data %>%
        pivot_longer(cols = -c(x, y),  # Specify the crop columns
                     names_to = "crop",  
                     values_to = "crop_value", 
                     values_drop_na = F)  
      
      
      crop_data$x <-round(crop_data$x, 2)
      crop_data$y <-round(crop_data$y, 2)
      # Assign the data frame to the global environment
      assign("crop_data", crop_data, envir = .GlobalEnv)
      
      return(crop_data)
}


# Define a function to process each NetCDF file
process_gs <- function(nc_file) {
  
  # Extract crop name from file name
  crop_name <- gsub("\\.nc4$", "", basename(nc_file))
  
  # Open the NetCDF file
  nc_data <- nc_open(nc_file)
  on.exit(nc_close(nc_data))  # Ensure the NetCDF file is closed when done
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  
  # Get the _FillValue for the variable 'planting_day'
  fillvalue <- ncatt_get(nc_data, "planting_day", "_FillValue")$value
  
  # Load the raster data
  calendar <- rast(nc_file)
  
  # Replace fill values with NA
  replace_values <- function(x) {
    x[x == fillvalue] <- NA
    return(x)
  }
  
  calendar <- app(calendar, replace_values)
  
  # Project to the same CRS
  calendar <- project(calendar, "EPSG:4326")
  
  # Define the target grid (1x1 degree global extent)
  target_extent <- ext(-180, 180, -90, 90)  # Global extent
  target_raster <- rast(nrows = 180, ncols = 360, ext = target_extent, crs = "EPSG:4326")
  
  # Resample the raster to the 1x1 degree grid
  calendar <- resample(calendar, target_raster, method = "near")
  
  # Convert the raster to a dataframe
  df <- as.data.frame(calendar, xy = TRUE)
  
  # Add the crop name as a column
  df$crop <- crop_name
  
  return(df)
}


library(zoo)  # For rolling mean calculations
library(dplyr)  # For data manipulation


