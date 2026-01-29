context("pData ordering")

test_that("set_pdata_order sets factor levels correctly", {
  library(ggcyto)
  data(GvHD)
  
  # Create a subset of the data
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Check original pData column types
  expect_type(pData(fs)$Patient, "character")
  expect_type(pData(fs)$Visit, "character")
  
  # Set custom order for Patient
  fs_ordered <- set_pdata_order(fs, Patient = c("7", "6", "5"))
  
  # Check that Patient is now a factor with correct levels
  expect_s3_class(pData(fs_ordered)$Patient, "factor")
  expect_equal(levels(pData(fs_ordered)$Patient), c("7", "6", "5"))
  
  # Set order for multiple columns
  fs_multi <- set_pdata_order(fs, 
                               Patient = c("7", "6", "5"),
                               Visit = c("6", "5"))
  
  expect_s3_class(pData(fs_multi)$Patient, "factor")
  expect_s3_class(pData(fs_multi)$Visit, "factor")
  expect_equal(levels(pData(fs_multi)$Patient), c("7", "6", "5"))
  expect_equal(levels(pData(fs_multi)$Visit), c("6", "5"))
  
  # Test automatic ordering (NULL)
  fs_auto <- set_pdata_order(fs, Patient = NULL)
  expect_s3_class(pData(fs_auto)$Patient, "factor")
  # Levels should be in order of appearance
  expect_equal(levels(pData(fs_auto)$Patient), unique(as.character(pData(fs)$Patient)))
})

test_that("set_pdata_order handles errors correctly", {
  data(GvHD)
  fs <- GvHD[1:3]
  
  # Error when no columns specified
  expect_error(set_pdata_order(fs), "At least one column must be specified")
  
  # Error when column doesn't exist
  expect_error(set_pdata_order(fs, NonExistentColumn = c("a", "b")), 
               "Column 'NonExistentColumn' not found in pData")
  
  # Warning when levels don't match all values
  expect_warning(set_pdata_order(fs, Patient = c("1", "2")),
                 "Some values.*are not in the provided levels")
})

test_that("factor ordering is preserved in fortify", {
  library(ggcyto)
  data(GvHD)
  
  # Create a subset
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Set custom order
  fs <- set_pdata_order(fs, Patient = c("7", "6", "5"))
  
  # Fortify the data
  df <- fortify(fs)
  
  # Check that Patient column in fortified data is a factor with correct levels
  expect_s3_class(df$Patient, "factor")
  expect_equal(levels(df$Patient), c("7", "6", "5"))
})

test_that("factor ordering works with ggcyto plots", {
  library(ggcyto)
  data(GvHD)
  
  # Create a subset
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Set custom order (reverse alphabetical)
  fs <- set_pdata_order(fs, Patient = c("7", "6", "5"))
  
  # Create a plot
  p <- ggcyto(fs, aes(x = `FSC-H`)) + geom_histogram() + facet_grid(Patient~Visit)
  
  # Convert to ggplot to get the data
  p_gg <- as.ggplot(p)
  
  # Check that the data has the correct factor levels
  expect_s3_class(p_gg$data$Patient, "factor")
  expect_equal(levels(p_gg$data$Patient), c("7", "6", "5"))
})
