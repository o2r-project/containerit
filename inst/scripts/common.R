# Common objects for Addin script files
file_dialog_volumes <- c("Working directory" = getwd(), "Home directory" = "~")

save_button <- function() {
  shinyFiles::shinySaveButton(id = "save",
                              label = "Save as...",
                              title = "Save file as ..."
                              # , filetype = list(dockerfile = "Dockerfile")
                              )
}

# Output docker instructions to console
print_docker_instructions <- function(file) {
  cat("You can now use the Docker CLI to build the Dockerfile and run the image:",
      "\n>> cd ", dirname(file),
      "\n>> docker build . -t [tag] -f", basename(file),
      "\n>> docker run -t [tag]")
}
