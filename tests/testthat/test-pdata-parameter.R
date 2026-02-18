test_that("ggcyto accepts custom pData with factors for ordering", {
  data(GvHD)
  
  # Create a subset of the data
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Create custom pData with Patient as factor in custom order
  custom_pd <- pData(fs)
  custom_pd$Patient <- factor(custom_pd$Patient, levels = c("7", "6", "5"))
  custom_pd$Visit <- factor(custom_pd$Visit, levels = c("6", "5"))
  
  # Create plot with custom pData
  p <- ggcyto(fs, aes(x = `FSC-H`), pData = custom_pd) + geom_histogram()
  
  # Verify that original pData in fs is NOT modified (should still be character)
  expect_type(pData(p$data)$Patient, "character")
  expect_type(pData(p$data)$Visit, "character")
  
  # Verify custom_pdata is stored in plot object, not as attribute on fs
  expect_null(attr(p$data, "custom_pdata"))
  expect_false(is.null(p[["custom_pdata"]]))
  
  # Convert to ggplot to check the data
  p_gg <- as.ggplot(p)
  
  # Check that Patient column in plot data is a factor with correct levels
  expect_s3_class(p_gg$data$Patient, "factor")
  expect_equal(levels(p_gg$data$Patient), c("7", "6", "5"))
  expect_equal(levels(p_gg$data$Visit), c("6", "5"))
})

test_that("ggcyto validates pData rownames match", {
  data(GvHD)
  fs <- GvHD[1:3]
  
  # Create pData with wrong rownames
  wrong_pd <- data.frame(
    name = c("a", "b", "c"),
    Patient = c("1", "2", "3"),
    row.names = c("wrong1", "wrong2", "wrong3")
  )
  
  # Should error because rownames don't match
  expect_error(
    ggcyto(fs, aes(x = `FSC-H`), pData = wrong_pd),
    "rownames must match"
  )
})

test_that("ggcyto warns about missing columns in custom pData", {
  data(GvHD)
  fs <- GvHD[1:3]
  
  # Create pData with only some columns
  partial_pd <- data.frame(
    name = sampleNames(fs),
    row.names = sampleNames(fs)
  )
  
  # Should warn about missing columns
  expect_warning(
    ggcyto(fs, aes(x = `FSC-H`), pData = partial_pd),
    "missing in supplied pData"
  )
})

test_that("autoplot.flowSet accepts custom pData", {
  data(GvHD)
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Create custom pData with factors
  custom_pd <- pData(fs)
  custom_pd$Patient <- factor(custom_pd$Patient, levels = c("7", "6", "5"))
  
  # Create plot with autoplot
  p <- autoplot(fs, x = "FSC-H", pData = custom_pd)
  
  # Convert to ggplot and check
  p_gg <- as.ggplot(p)
  expect_s3_class(p_gg$data$Patient, "factor")
  expect_equal(levels(p_gg$data$Patient), c("7", "6", "5"))
})

test_that("custom pData ordering is preserved in faceted plots", {
  data(GvHD)
  fs <- GvHD[subset(pData(GvHD), Patient %in% 5:7 & Visit %in% c(5:6))[["name"]]]
  
  # Create custom pData with reverse alphabetical order
  custom_pd <- pData(fs)
  custom_pd$Patient <- factor(custom_pd$Patient, levels = c("7", "6", "5"))
  
  # Create faceted plot
  p <- ggcyto(fs, aes(x = `FSC-H`), pData = custom_pd) + 
    geom_histogram() + 
    facet_grid(Patient~Visit)
  
  # Convert and check
  p_gg <- as.ggplot(p)
  expect_s3_class(p_gg$data$Patient, "factor")
  expect_equal(levels(p_gg$data$Patient), c("7", "6", "5"))
})

test_that("pData reordering matches flowSet sample order", {
  data(GvHD)
  fs <- GvHD[1:3]
  
  # Create pData with samples in different order
  custom_pd <- pData(fs)
  # Reverse the order of rows
  custom_pd <- custom_pd[rev(rownames(custom_pd)), , drop = FALSE]
  custom_pd$Test <- factor(rownames(custom_pd), levels = rownames(custom_pd))
  
  # Should reorder to match flowSet
  p <- ggcyto(fs, aes(x = `FSC-H`), pData = custom_pd)
  
  # The pData in the flowSet should match the original order
  expect_equal(rownames(pData(p$data)), sampleNames(fs))
})
