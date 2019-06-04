context("darleq3_taxa")

test_that("TaxonID and TaxonName should have unique IDs for all rows", {

  expect_true(length(darleq3_taxa$TaxonId[duplicated(darleq3_taxa$TaxonId)]) == 0)
  expect_true(length(darleq3_taxa$TaxonName[duplicated(darleq3_taxa$TaxonName)]) == 0)

})


test_that("NBScode should be unique or NA values for each row in table", {

  expect_true(all(is.na(darleq3_taxa$NBSCode[duplicated(darleq3_taxa$NBSCode)])))


})
