#!/usr/bin/env Rscript
# Generate individual reference pages from .Rd files

library(tools)

# Create reference directory
ref_dir = "site/reference"
if (!dir.exists(ref_dir)) dir.create(ref_dir, recursive = TRUE)

# Get all .Rd files
rd_files = list.files("man", pattern = "\\.Rd$", full.names = TRUE)

# Function to extract metadata from .Rd file
get_rd_info = function(rd_file) {
  rd = parse_Rd(rd_file)

  title_idx = which(sapply(rd, attr, "Rd_tag") == "\\title")
  title = if (length(title_idx) > 0) {
    gsub("^\\s+|\\s+$", "", paste(unlist(rd[[title_idx[1]]]), collapse = ""))
  } else NA

  alias_idx = which(sapply(rd, attr, "Rd_tag") == "\\alias")
  aliases = if (length(alias_idx) > 0) {
    sapply(alias_idx, function(i) paste(unlist(rd[[i]]), collapse = ""))
  } else character(0)

  list(
    file = basename(rd_file),
    name = sub("\\.Rd$", "", basename(rd_file)),
    title = title,
    aliases = aliases
  )
}

# Generate HTML for each .Rd file
cat("Generating individual reference pages...\n")
all_info = list()

for (rd_file in rd_files) {
  info = get_rd_info(rd_file)
  all_info[[length(all_info) + 1]] <- info

  out_file = file.path(ref_dir, paste0(info$name, ".html"))

  temp_html = tempfile(fileext = ".html")
  Rd2HTML(rd_file, out = temp_html, package = "data.table")

  html_content = readLines(temp_html, warn = FALSE)
  navbar_html = readLines("site/_navbar.html", warn = FALSE)
  navbar_html = gsub('href="index.html"', 'href="../index.html"', navbar_html)
  navbar_html = gsub('href="news.html"', 'href="../news.html"', navbar_html)
  navbar_html = gsub('href="manual.html"', 'href="../manual.html"', navbar_html)
  navbar_html = gsub('href="articles/', 'href="../articles/', navbar_html)
  navbar_html = gsub('href="assets/', 'href="../assets/', navbar_html)

  wrapped_html = c(
    '<!DOCTYPE html>',
    '<html>',
    '<head>',
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">',
    '<meta name="generator" content="litedown">',
    sprintf('<title>%s — %s • data.table</title>', paste(info$aliases, collapse = ", "), info$title),
    '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css">',
    '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css">',
    '<link rel="stylesheet" href="../assets/style.css">',
    '<style>',
    'body { max-width: none !important; margin: 0 !important; padding: 0 !important; }',
    '.frontmatter, .body { max-width: 1320px !important; width: 100% !important; margin: 0 auto !important; padding: 0 clamp(1.5rem, 3vw, 4rem) !important; }',
    '.body { padding-top: 1.5rem; }',
    'h2 { margin-top: 1.5em; border-bottom: 1px solid #dee2e6; padding-bottom: 0.3em; }',
    'table { border-collapse: collapse; }',
    'table td, table th { border: 1px solid #dee2e6; padding: 0.5rem; }',
    '</style>',
    '<link rel="icon" type="image/png" sizes="16x16" href="../assets/favicon/favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="../assets/favicon/favicon-32x32.png"><link rel="apple-touch-icon" sizes="60x60" href="../assets/favicon/apple-touch-icon-60x60.png"><link rel="apple-touch-icon" sizes="76x76" href="../assets/favicon/apple-touch-icon-76x76.png"><link rel="apple-touch-icon" sizes="120x120" href="../assets/favicon/apple-touch-icon-120x120.png"><link rel="apple-touch-icon" sizes="152x152" href="../assets/favicon/apple-touch-icon-152x152.png"><link rel="apple-touch-icon" sizes="180x180" href="../assets/favicon/apple-touch-icon-180x180.png"><link rel="apple-touch-icon" sizes="180x180" href="../assets/favicon/apple-touch-icon.png"><link rel="icon" sizes="any" href="../assets/favicon/favicon.ico">',
    '</head>',
    '<body>',
    navbar_html,
    '<div class="body">',
    html_content,
    '</div>',
    '<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>',
    '</body>',
    '</html>'
  )

  writeLines(wrapped_html, out_file)
  unlink(temp_html)

  cat(sprintf("  Generated %s\n", out_file))
}

all_info = all_info[order(sapply(all_info, function(x) x$name), method = "radix")]

saveRDS(all_info, "site/reference_index.rds")

cat(sprintf("\nGenerated %d reference pages in %s/\n", length(all_info), ref_dir))