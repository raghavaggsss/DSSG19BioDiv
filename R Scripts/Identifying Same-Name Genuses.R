overlaptester = function(data) {
  for (gen in unique(data$genus)) {
    gd = data[data$genus == gen,]
    if (length(unique(gd$family)) > 1) {
      print(paste("Genus", gen, "belongs to families", paste(unique(gd$family), collapse = " and ")))
    }
    if (length(unique(gd$order)) > 1) {
      print(paste("Genus", gen, "belongs to orders", paste(unique(gd$order), collapse = " and ")))
    }
    if (length(unique(gd$class)) > 1) {
      print(paste("Genus", gen, "belongs to classes", paste(unique(gd$class), collapse = " and ")))
    }
    if (length(unique(gd$phylum)) > 1) {
      print(paste("Genus", gen, "belongs to phylums", paste(unique(gd$phylum), collapse = " and ")))
    }    
    if (length(unique(gd$kingdom)) > 1) {
      print(paste("Genus", gen, "belongs to kingdoms", paste(unique(gd$kingdom), collapse = " and ")))
    }
  }
}
