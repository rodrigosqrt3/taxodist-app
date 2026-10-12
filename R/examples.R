# Example taxa shared by app.R and build_cache.R. Shiny sources files in R/
# automatically before app.R runs.

shuffle_pool <- c(
  "Tyrannosaurus", "Triceratops", "Carnotaurus", "Velociraptor", "Spinosaurus", "Stegosaurus", "Brachiosaurus", "Diplodocus",
  "Struthio", "Gallus", "Anas", "Columba", "Falco", "Corvus", "Ara", "Spheniscus", "Aptenodytes", "Tyto", "Bubo",
  "Homo", "Panthera", "Canis", "Felis", "Equus", "Bos", "Sus", "Ovis", "Capra", "Mus", "Rattus", "Loxodonta",
  "Crocodylus", "Alligator", "Chelonia", "Varanus", "Python", "Boa", "Iguana", "Rana", "Bufo", "Ambystoma",
  "Octopus", "Loligo", "Carcharodon", "Sphyrna", "Salmo", "Oncorhynchus", "Thunnus", "Hippocampus", "Danio",
  "Drosophila", "Apis", "Bombus", "Atta", "Camponotus", "Anopheles", "Aedes", "Culex", "Danaus", "Manduca",
  "Quercus", "Pinus", "Ginkgo", "Araucaria", "Eucalyptus", "Ficus", "Zea", "Oryza", "Triticum", "Solanum",
  "Saccharomyces", "Amanita", "Aspergillus", "Penicillium", "Candida", "Neurospora", "Pleurotus",
  "Escherichia", "Bacillus", "Staphylococcus", "Streptococcus", "Pseudomonas", "Salmonella", "Lactobacillus",
  "Plasmodium", "Trypanosoma", "Leishmania", "Giardia", "Euglena", "Tetrahymena", "Amoeba"
)

# Clickable pairs shown under "Try these" in the pairwise tab.
example_pairs <- list(
  c("Tyrannosaurus", "Velociraptor"),
  c("Homo", "Quercus"),
  c("Nomingia", "Huanansaurus")
)

clade_examples <- list(
  list(clade = "Theropoda",  taxa = c("Tyrannosaurus", "Velociraptor", "Spinosaurus", "Carnotaurus", "Triceratops", "Brachiosaurus")),
  list(clade = "Dinosauria", taxa = c("Tyrannosaurus", "Triceratops", "Brachiosaurus", "Velociraptor", "Homo", "Quercus")),
  list(clade = "Amniota",    taxa = c("Homo", "Panthera", "Canis", "Tyrannosaurus", "Drosophila", "Quercus"))
)

example_taxa <- function() {
  c("Tyrannosaurus", "Velociraptor", "Triceratops", "Brachiosaurus", "Homo", "Quercus")
}

# Every name the app suggests; build_cache.R pre-fetches them all.
all_example_taxa <- function() {
  unique(c(
    shuffle_pool,
    unlist(example_pairs),
    unlist(lapply(clade_examples, `[[`, "taxa")),
    vapply(clade_examples, `[[`, character(1), "clade"),
    example_taxa(),
    "Spinosaurus", "Allosaurus", "Nereis"
  ))
}
