#!/usr/bin/env Rscript
# Generate individual reference pages from .Rd files

library(tools)
library(xml2)

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

all_info = lapply(setNames(nm = rd_files), get_rd_info)
topic_links = character()
for (topic in all_info) topic_links[topic$aliases] = paste0('reference/', topic$name, '.html')

html_meta = xfun::yaml_load(readLines('site/_litedown.yml'))$output$html$meta

for (rd_file in rd_files) {
  info = all_info[[rd_file]]

  out_file = file.path(ref_dir, paste0(info$name, ".html"))

  temp_html = tempfile(fileext = ".html")
  Rd2HTML(rd_file, out = temp_html, package = "data.table", Links = topic_links, Links2 = character())
  html_root = read_html(temp_html)

  if (!is.null(html_meta$lang))
    xml_attr(html_root, 'lang') = html_meta$lang

  html_title = xml_find_first(html_root, '/html/head/title')
  xml_text(html_title) <- sprintf('%s — %s • data.table', paste(info$aliases, collapse = ", "), info$title)

  html_head = xml_find_first(html_root, '//head')

  if (!is.null(add_head <- html_meta$header_includes)) {
    add_head = xml_find_first(read_html(paste0('<head>', add_head, '</head>')), '//head')
    for (tag in xml_children(add_head))
      xml_add_child(html_head, tag)
  }

  for (css in html_meta$css2) {
    link = xml_add_child(html_head, "link")
    xml_set_attrs(link, list(
      rel = "stylesheet",
      href = css
    ))
  }

  for (js in html_meta$js2) {
    script = xml_add_child(html_head, "script")
    xml_set_attr(script, 'src', js)
  }

  body = xml_find_first(html_root, '//body')
  new_body = xml_add_child(html_root, 'body')
  if (!is.null(include <- html_meta$include_before)) {
    include = read_html(file.path('site', include))
    include = xml_find_first(include, '//body')
    for (node in xml_children(include))
      xml_add_child(new_body, node)
  }

  new_body_div = xml_add_child(new_body, 'div')
  xml_set_attr(new_body_div, 'class', 'body')
  for (node in xml_children(body))
    xml_add_child(new_body_div, node)

  if (!is.null(include <- html_meta$include_after)) {
    include = read_html(include)
    include = xml_find_first(include, '//body')
    for (node in xml_children(include))
      xml_add_child(new_body, node)
  }

  xml_remove(body)

  for (link in xml_find_all(html_root, '//a|//link')) {
    href = xml_attr(link, 'href')
    if (!is.na(href))
      if (startsWith(href, '../../')) # \link[package]{topic}
        xml_attr(link, 'href') <- NULL
      else if (!grepl('^https?://', href))
        xml_attr(link, 'href') <- paste0('../', href)
  }

  write_html(html_root, out_file)
  unlink(temp_html)

  cat(sprintf("  Generated %s\n", out_file))
}

all_info = all_info[order(sapply(all_info, function(x) x$name), method = "radix")]

saveRDS(all_info, "site/reference_index.rds")

cat(sprintf("\nGenerated %d reference pages in %s/\n", length(all_info), ref_dir))