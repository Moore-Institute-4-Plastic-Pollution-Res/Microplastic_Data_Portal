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


#Test the validate data function. 


# Test that validate_data correctly identifies valid file types
test_that("validate_data correctly identifies valid file types", {
    result <- validate_data("example.csv", file_rules = "rules.csv")
    expect_equal(result$status, NULL)
})

# Test that validate_data returns an error when given an invalid file type
test_that("validate_data returns an error when given an invalid file type", {
    result <- validate_data("example.txt", file_rules = "rules.csv")
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Data type not supported!")
    expect_match(result$message$text, "Uploaded data type is not currently supported")
})

# Test that validate_data correctly reads a valid CSV rules file
test_that("validate_data correctly reads a valid CSV rules file", {
    result <- validate_data("example.csv", file_rules = "rules.csv")
    expect_equal(result$status, NULL)
})

# Test that validate_data returns an error when given an invalid CSV rules file
test_that("validate_data returns an error when given an invalid CSV rules file", {
    result <- validate_data("example.csv", file_rules = "invalid_rules.csv")
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Data type not supported!")
    expect_match(result$message$text, "Uploaded rules format is not currently supported")
})

# Test that validate_data correctly reads a valid XLSX rules file
test_that("validate_data correctly reads a valid XLSX rules file", {
    result <- validate_data("example.csv", file_rules = "rules.xlsx")
    expect_equal(result$status, NULL)
})

# Test that validate_data returns an error when given an invalid XLSX rules file
test_that("validate_data returns an error when given an invalid XLSX rules file", {
    result <- validate_data("example.csv", file_rules = "invalid_rules.xlsx")
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Data type not supported!")
    expect_match(result$message$text, "Uploaded rules format is not currently supported")
})

# Test that validate_data correctly identifies malicious rules
test_that("validate_data correctly identifies malicious rules", {
    malicious_rules <- data.frame(name = "Malicious Rule", description = "This rule contains the word 'config'", severity = "High", rule = "This rule contains the word 'config'")
    result <- validate_data("example.csv", file_rules = malicious_rules)
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Rule not supported!")
    expect_match(result$message$text, "unable to support any rules with the words config or secret in them")
})

# Test that validate_data correctly identifies invalid rules
test_that("validate_data correctly identifies invalid rules", {
    invalid_rules <- data.frame(name = "Invalid Rule", description = "This rule has an invalid column name", severity = "Low", invalid_column_name = "This rule has an invalid column name")
    result <- validate_data("example.csv", file_rules = invalid_rules)
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Data type not supported!")
    expect_match(result$message$text, "provide a rules file with column names, \"name\", \"description\", \"severity\", \"rule\"")
})

# Test that validate_data correctly identifies mixed data types
test_that("validate_data correctly identifies mixed data types", {
    result <- validate_data(c("example.csv", "example.xlsx"), file_rules = "rules.csv")
    expect_equal(result$status, "error")
    expect_equal(result$message$title, "Mixed Data Types")
})
