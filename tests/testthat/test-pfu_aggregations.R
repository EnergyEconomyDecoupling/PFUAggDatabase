################################################################################

test_that("calculate_p_ex_total() works", {

  test_data <- Recca::UKEnergy2000mats  %>%
    tidyr::pivot_wider(names_from = matrix.name,
                       values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_p_ex_total(p_industry_prefixes = list(c("Resources", "Imports")))

  expect_equal(unique(test_data$Gross.Net), NA)
  expect_equal(nrow(test_data), 2)
  expect_equal(unique(test_data$Aggregation.by), "Total")
  expect_equal(unique(test_data$E.product), "All")
  expect_equal(unique(test_data$Flow), "All")
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 93000)
  expect_equal(test_data$EX[[2]], 98220)

})

test_that("calculate_p_ex_product() works", {

  test_data <- Recca::UKEnergy2000mats  %>%
    tidyr::pivot_wider(names_from = matrix.name,
                       values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_p_ex_product(p_industry_prefixes = list(c("Resources", "Imports")))

  expect_equal(unique(test_data$Gross.Net), NA)
  expect_equal(nrow(test_data), 4)
  expect_equal(unique(test_data$Aggregation.by), "Product")
  expect_equal(unique(test_data$E.product), c("Crude", "NG"))
  expect_equal(unique(test_data$Flow), "All")
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 50000)
  expect_equal(test_data$EX[[2]], 43000)
  expect_equal(test_data$EX[[3]], 53500)
  expect_equal(test_data$EX[[4]], 44720)

})

test_that("calculate_p_ex_flow() works", {

  test_data <- Recca::UKEnergy2000mats  %>%
    tidyr::pivot_wider(names_from = matrix.name,
                       values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_p_ex_flow(p_industry_prefixes = list(c("Resources", "Imports")))


  expect_equal(unique(test_data$Gross.Net), NA)
  expect_equal(nrow(test_data), 4)
  expect_equal(unique(test_data$Aggregation.by), "Flow")
  expect_equal(unique(test_data$E.product), "All")
  expect_equal(unique(test_data$Flow), c("Resources - Crude", "Resources - NG"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 50000)
  expect_equal(test_data$EX[[2]], 43000)
  expect_equal(test_data$EX[[3]], 53500)
  expect_equal(test_data$EX[[4]], 44720)

})

test_that("calculate_primary_ex_data() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_primary_ex_data(p_industry_prefixes = list(c("Resources", "Imports")))

  expect_equal(unique(test_data$Gross.Net), NA)
  expect_equal(nrow(test_data), 10)
  expect_equal(unique(test_data$Aggregation.by), c("Total", "Flow", "Product"))
  expect_equal(unique(test_data$E.product), c("All", "Crude", "NG"))
  expect_equal(unique(test_data$Flow), c("All", "Resources - Crude", "Resources - NG"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 93000)
  expect_equal(test_data$EX[[2]], 98220)
  expect_equal(test_data$EX[[3]], 50000)
  expect_equal(test_data$EX[[4]], 43000)
  expect_equal(test_data$EX[[5]], 53500)
  expect_equal(test_data$EX[[6]], 44720)
  expect_equal(test_data$EX[[7]], 50000)
  expect_equal(test_data$EX[[8]], 43000)
  expect_equal(test_data$EX[[9]], 53500)
  expect_equal(test_data$EX[[10]], 44720)

})

################################################################################

test_that("calculate_fu_ex_total() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_fu_ex_total(fd_sectors = c("Residential"))

  expect_equal(unique(test_data$Gross.Net), c("Net", "Gross"))
  expect_equal(nrow(test_data), 8)
  expect_equal(unique(test_data$Aggregation.by), "Total")
  expect_equal(unique(test_data$E.product), "All")
  expect_equal(unique(test_data$Sector), "All")
  expect_equal(unique(test_data$Stage), c("Final", "Services", "Useful"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 31000)
  expect_equal(test_data$EX[[2]], 31000)
  expect_equal(test_data$EX[[3]], 500075000000000)
  expect_equal(test_data$EX[[4]], 500075000000000)
  expect_equal(test_data$EX[[5]], 4200.4)
  expect_equal(test_data$EX[[6]], 4200.4)
  expect_equal(test_data$EX[[7]], 500075000000000)
  expect_equal(test_data$EX[[8]], 500075000000000)

})

test_that("calculate_fu_ex_product() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_fu_ex_product(fd_sectors = c("Residential"))

  expect_equal(unique(test_data$Gross.Net), c("Gross", "Net"))
  expect_equal(nrow(test_data), 32)
  expect_equal(unique(test_data$Aggregation.by), "Product")
  expect_equal(unique(test_data$E.product), c("Diesel - Dist.",
                                              "Elect - Grid",
                                              "NG - Dist.",
                                              "Petrol - Dist.",
                                              "Freight [tonne-km/year]",
                                              "Illumination [lumen-hrs/yr]",
                                              "Passenger [passenger-km/yr]",
                                              "Space heating [m3-K]",
                                              "LTH",
                                              "Light",
                                              "MD - Car engines",
                                              "MD - Truck engines"))
  expect_equal(unique(test_data$Sector), "All")
  expect_equal(unique(test_data$Stage), c("Final", "Services", "Useful"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 0)
  expect_equal(test_data$EX[[32]], 75000000000)

})


test_that("calculate_fu_ex_sector() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_fu_ex_sector(fd_sectors = c("Residential"))

  expect_equal(unique(test_data$Gross.Net), c("Gross", "Net"))
  expect_equal(nrow(test_data), 8)
  expect_equal(unique(test_data$Aggregation.by), "Sector")
  expect_equal(unique(test_data$E.product), "All")
  expect_equal(unique(test_data$Sector), "Residential")
  expect_equal(unique(test_data$Stage), c("Final", "Services", "Useful"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 31000)
  expect_equal(test_data$EX[[8]], 500075000000000)

})

test_that("calculate_finaluseful_ex_data() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_finaluseful_ex_data(fd_sectors = c("Residential"))

  expect_equal(unique(test_data$Gross.Net), c("Net", "Gross"))
  expect_equal(nrow(test_data), 48)
  expect_equal(unique(test_data$Aggregation.by), c("Total", "Sector", "Product"))
  expect_equal(unique(test_data$E.product), c("All",
                                              "Diesel - Dist.",
                                              "Elect - Grid",
                                              "NG - Dist.",
                                              "Petrol - Dist.",
                                              "Freight [tonne-km/year]",
                                              "Illumination [lumen-hrs/yr]",
                                              "Passenger [passenger-km/yr]",
                                              "Space heating [m3-K]",
                                              "LTH",
                                              "Light",
                                              "MD - Car engines",
                                              "MD - Truck engines"))
  expect_equal(unique(test_data$Sector), c("All", "Residential"))
  expect_equal(unique(test_data$Stage), c("Final", "Services", "Useful"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$EX[[1]], 31000)
  expect_equal(test_data$EX[[48]], 75000000000)

})

################################################################################

test_that("calculate_finaluseful_ex_data() works", {

  test_data <- Recca::UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    dplyr::mutate(Method = "PCM") %>%
    calculate_pfu_aggregations()

  expect_equal(unique(test_data$Gross.Net), c(NA, "Net", "Gross"))
  expect_equal(nrow(test_data), 114)
  expect_equal(unique(test_data$Aggregation.by), c("Total", "Sector", "Product"))
  expect_equal(unique(test_data$E.product), c("All", "Crude", "NG", "Crude - Dist.",
                                              "Crude - Fields", "Diesel", "Diesel - Dist.", "Elect",
                                              "Elect - Grid", "NG - Dist.", "NG - Wells", "Petrol",
                                              "Petrol - Dist.", "Freight [tonne-km/year]", "Illumination [lumen-hrs/yr]", "Light",
                                              "LTH", "MD - Car engines", "MD - Truck engines", "Passenger [passenger-km/yr]",
                                              "Space heating [m3-K]"))
  expect_equal(unique(test_data$Sector), c("All", "Resources - Crude", "Resources - NG", "Oil refineries", "Residential"))
  expect_equal(unique(test_data$Stage), c("Primary", "Final", "Services", "Useful"))
  expect_equal(unique(test_data$Energy.type), c("E", "X"))
  expect_equal(test_data$E.dot[[1]], 93000)
  expect_equal(test_data$E.dot[[114]], 75000000000)

})

