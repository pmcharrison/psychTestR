suppressWarnings(file.remove("..pdf"))
system("R CMD Rd2pdf .")
file.rename("..pdf", "psychTestR.pdf")
