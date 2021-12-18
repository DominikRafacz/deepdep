# OFFLINE TESTS ----
test_that("incorrect object type results in an error", {
  expect_error(plot_dependencies("Wrong type"))
})

# SETUP ----
# Tests below make HTTP calls, so we use vcr to record them
skip_if_not_installed("vcr")

# Write cassettes first
# vcr::use_cassette("plot-dd-shiny-1", {
#   dd_shiny_1 <- deepdep("shiny", downloads = TRUE)
# })
vcr::use_cassette("plot-dd-shiny-2", {
  dd_shiny <- deepdep("shiny", depth = 2)
})
vcr::use_cassette("plot-dd-rlang", {
  dd_rlang <- deepdep("rlang")
  plt_rlang <- plot_dependencies(dd_rlang)
})
vcr::use_cassette("plot-dd-datatable", {
  dd_dt <- deepdep("data.table", depth = 2, dependency_type = "all")
})

# LAYER CLASSES ----
test_that("deepdep plot has correct layers and objects", {
  plt_shiny <- plot_dependencies(dd_shiny)
  
  expect_length(plt_shiny$layers, 4)
  # 1st layer is circles
  expect_s3_class(plt_shiny$layers[[1]]$geom, "GeomPath")
  expect_s3_class(plt_shiny$layers[[1]]$stat, "StatIdentity")
  # 2nd layer is edges
  expect_s3_class(plt_shiny$layers[[2]]$geom, "GeomEdgePath")
  expect_s3_class(plt_shiny$layers[[2]]$stat, "StatEdgeLink")
  # 3rd layer is node points
  expect_s3_class(plt_shiny$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(plt_shiny$layers[[3]]$stat, "StatFilter")
  # 4th layer is node labels
  expect_s3_class(plt_shiny$layers[[4]]$geom, "GeomLabel")
  expect_s3_class(plt_shiny$layers[[4]]$stat, "StatFilter")
})

# SAME LEVEL & REVERSE EDGES ----
test_that("deepdep plot omits same level and reverse dependencies", {
  plt_dt <- plot_dependencies(dd_dt)
  
  edge_attrs <- igraph::edge_attr(plt_dt$plot_env$G)
  # Origin level is always smaller than target level
  expect_true(all(edge_attrs$origin_level < edge_attrs$dest_level))
  # To be more precise, origin is always exactly 1 smaller than target level
  expect_true(all(edge_attrs$origin_level == edge_attrs$dest_level - 1))
})

test_that("deepdep plot can keep same level dependencies", {
  plt_dt <- plot_dependencies(dd_dt, same_level = TRUE)
  
  edge_attrs <- igraph::edge_attr(plt_dt$plot_env$G)
  # Origin level isn't greater than target level
  expect_true(all(edge_attrs$origin_level <= edge_attrs$dest_level))
  # Not always true - if deepdep has no same-level dependencies
  expect_true(any(edge_attrs$origin_level == edge_attrs$dest_level))
})

test_that("deepdep plot can keep reverse dependencies", {
  plt_dt <- plot_dependencies(dd_dt, reverse = TRUE)
  
  edge_attrs <- igraph::edge_attr(plt_dt$plot_env$G)
  # Origin level isn't equal to target level
  expect_true(all(edge_attrs$origin_level != edge_attrs$dest_level))
  # Not always true - if deepdep has no reverse dependencies
  expect_true(any(edge_attrs$origin_level > edge_attrs$dest_level))
})

test_that("deepdep plot can keep all dependencies", {
  plt_dt <- plot_dependencies(dd_dt, same_level = TRUE, reverse = TRUE)
  
  # No dependency is dropped from deepdep source object
  expect_equal(nrow(dd_dt), igraph::ecount(plt_dt$plot_env$G))
})

# CAPTION ----
test_that("deepdep plot has a caption by default", {
  plt_shiny <- plot_dependencies(dd_shiny)
  
  expect_match(
    plt_shiny$labels$caption,
    "Plot made with deepdep v[\\d.]+ on \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    perl = TRUE
  )
  
  plt_shiny <- plot_dependencies(dd_shiny, show_stamp = TRUE)
  
  expect_match(
    plt_shiny$labels$caption,
    "Plot made with deepdep v[\\d.]+ on \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    perl = TRUE
  )
})

test_that("deepdep plot has no caption when specified", {
  plt_shiny <- plot_dependencies(dd_shiny, show_stamp = FALSE)
  
  expect_null(plt_shiny$labels$caption)
})

# VERSION & DOWNLOADS ----
{
  # Extracting download count requires calling a HTTP address that vcr cannot yet record
  # due to ":" character
  skip_on_cran()
  
  dd_shiny_1 <- deepdep("shiny", downloads = TRUE)
  
  test_that("no version or download count by default (resulting in one-line label)", {
    plt_shiny_1 <- plot_dependencies(dd_shiny_1)
    
    expect_match(
      plt_shiny_1$data$label,
      "(?s)^[^\\n]+$",
      perl = TRUE
    )
  })
  
  test_that("version code is placed in another line within braces", {
    plt_shiny_1 <- plot_dependencies(dd_shiny_1, show_version = TRUE)
    
    # Labels may or may not contain version code
    expect_match(
      plt_shiny_1$data$label,
      "(?s)^[^\\n]+(\\n\\(.+\\))?$",
      perl = TRUE
    )
    
    # Package was selected so that there are version requirements in there
    expect_true(any(grepl(
      "(?s)^[^\\n]+\\n\\(.+\\)$",
      plt_shiny_1$data$label,
      perl = TRUE
    )))
  })
  
  test_that("download count is placed in the last line", {
    plt_shiny_1 <- plot_dependencies(dd_shiny_1, show_downloads = TRUE)
    
    # Checked package is always without version or download count
    expect_match(
      plt_shiny_1$data$label[1],
      "shiny",
      fixed = TRUE
    )
    
    # Download count is always shown for other packages
    expect_match(
      plt_shiny_1$data$label[-1],
      "(?s)^[^\\n]+\\n\\d+$",
      perl = TRUE
    )
    
    plt_shiny_1 <- plot_dependencies(dd_shiny_1, show_version = TRUE, show_downloads = TRUE)
    
    expect_match(
      plt_shiny_1$data$label[1],
      "shiny",
      fixed = TRUE
    )
    
    # Labels may or may not contain version code
    expect_match(
      plt_shiny_1$data$label[-1],
      "(?s)^[^\\n]+(\\n\\(.+\\))?\\n\\d+$",
      perl = TRUE
    )
    
    # Package was selected so that there are version requirements in there
    expect_true(any(grepl(
      "(?s)^[^\\n]+\\n\\(.+\\)\\n\\d+$",
      plt_shiny_1$data$label,
      perl = TRUE
    )))
  })
}

test_that("showing versions don't break plots with depth >= 2", {
  plt_shiny <- plot_dependencies(dd_shiny, show_version = TRUE)
  
  # Labels may or may not contain version code
  expect_match(
    plt_shiny$data$label,
    "(?s)^[^\\n]+(\\n\\(.+\\))?$",
    perl = TRUE
  )
  
  # Package was selected so that there are version requirements in there
  expect_true(any(grepl(
    "(?s)^[^\\n]+\\n\\(.+\\)$",
    plt_shiny$data$label,
    perl = TRUE
  )))
})

# LABEL PERCENTAGE ----
{
  # Extracting download count requires calling a HTTP address that vcr cannot yet record
  # due to ":" character
  skip_on_cran()
  
  dd_shiny_1 <- deepdep("shiny", downloads = TRUE)
  
  test_that("by default label percentage is equal to 1", {
    plt_shiny_1 <- plot_dependencies(dd_shiny_1)
    
    expect_true(all(plt_shiny_1$data$labeled == TRUE))
  })
  
  test_that("actual label percentage is equal to or greater than argument value", {
    for (l_perc in c(0.05, 0.17, 0.25, 0.47, 0.81, 0.99)) {
      plt_shiny_1 <- plot_dependencies(dd_shiny_1, label_percentage = l_perc)
      
      expect_gte(mean(plt_shiny_1$data$labeled[-1] == TRUE), l_perc)
    }
  })
  
  test_that("label percentage equal to 0 means no labels except for the central package", {
    plt_shiny_1 <- plot_dependencies(dd_shiny_1, label_percentage = 0)
    
    expect_equal(plt_shiny_1$data$labeled[1], TRUE)
    expect_true(all(plt_shiny_1$data$labeled[-1] == FALSE))
  })
}

# NO DEPENDENCIES ----
test_that("deepdep plot with no dependencies has only a subset of layers", {
  skip_if_not(
    nrow(dd_rlang) == 0,
    "rlang has dependencies now - use different package to test plotting empty deepdep"
  )
  
  # No circles, no edges
  # 1st layer is node points
  expect_s3_class(plt_rlang$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(plt_rlang$layers[[1]]$stat, "StatFilter")
  # 2nd layer is node labels
  expect_s3_class(plt_rlang$layers[[2]]$geom, "GeomLabel")
  expect_s3_class(plt_rlang$layers[[2]]$stat, "StatFilter")
})

# DECLUTTER ----
test_that("declutter removes all Suggests and Enhances packages from outer layers", {
  plt_dt <- plot_dependencies(dd_dt, declutter = TRUE)
  
  # Node labels are equal to the names appearing in the filtered data
  filtered_dd <- dd_dt[dd_dt$origin_level == 0 |
                         !dd_dt$type %in% c("Suggests", "Enhances"), ]
  packages_plotted <- unique(c(filtered_dd$origin, filtered_dd$name))
  expect_setequal(packages_plotted, plt_dt$data$name)
  
  # If edge has type Suggests or Enhances, it starts in the center
  edge_attrs <- igraph::edge_attr(attr(plt_dt$data, "graph"))
  expect_true(
    all(edge_attrs$origin_level[edge_attrs$type %in% c("Suggests", "Enhances")] == 0)
  )
})
