library(testthat)

# Define a mock database function that captures the input
mock_database <- function(df) {
    assign("database_input", df, envir = .GlobalEnv)
}

# Define the test case
test_that("certificate_df creates a data frame with the correct columns", {
    # Call the function with sample input
    x <- list(data_formatted = "example data", rules = "example rules")
    df <- certificate_df(x, database_true = FALSE)
    
    # Check if the output is a data frame with the correct columns
    expect_is(df, "data.frame")
    expect_equal(names(df), c("time", "data", "rules", "package_version", "web_hash"))
    
    # Check if the "data" and "rules" columns contain the expected values
    expect_equal(df$data, digest("example data"))
    expect_equal(df$rules, digest("example rules"))
})

# Define another test case for database functionality
test_that("certificate_df inserts the data frame into a database when requested", {
    # Set up a mock database and call the function with database_true = TRUE
    assign("database", list(insert = mock_database), envir = .GlobalEnv)
    x <- list(data_formatted = "example data", rules = "example rules")
    df <- certificate_df(x, database_true = TRUE)
    
    # Check if the mock database captured the input
    expect_is(database_input, "data.frame")
    expect_equal(database_input, df)
})
