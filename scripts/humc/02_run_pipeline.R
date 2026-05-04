# I used AI to help write this code
# Run pipeline -----------------------------------------------------

source(here::here("scripts", "humc", "01_setup_functions.R"))

out <- refresh_out()

saveRDS(out, out_path)

message("Saved normalized analysis object to: ", out_path)

# Optional quick check --------------------------------------------

# View(out$lemma_counts)
# View(out$category_counts)