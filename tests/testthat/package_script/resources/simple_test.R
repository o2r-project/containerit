cat("Hello from containerit!!\n")
cat("What is 9999 * 1234 ?\n")
9999 * 1234
cat("Printing a table:\n")
# paths must be relative to work dir
read.csv("package_script/resources/test_table.csv")
cat("Printing a resource:\n")
readLines("package_script/resources/test_subfolder/testresource")
