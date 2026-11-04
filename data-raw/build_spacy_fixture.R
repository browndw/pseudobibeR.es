# Run from the package root with RETICULATE_PYTHON pointing to r-spacyr.
# Requires an installed Spanish model; never installs or downloads anything.
local({
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python) || !file.exists(python)) {
    stop("Set RETICULATE_PYTHON to the Python executable in r-spacyr.")
  }
  model <- Sys.getenv("PSEUDOBIBER_ES_SPACY_MODEL", "es_core_news_sm")
  spacyr::spacy_initialize(model = model)
  on.exit(spacyr::spacy_finalize(), add = TRUE)
  texts <- c(
    agentless = "La tarea fue realizada.",
    agent = "El informe fue redactado por el equipo.",
    perfect = "Yo he terminado el informe.",
    perfect_passive = "El informe ha sido revisado.",
    existential = "Hay tres libros.",
    direct = "\u00bfQu\u00e9 compr\u00f3 Ana?",
    indirect = "Pregunt\u00f3 qu\u00e9 compr\u00f3 Ana.",
    modal = "El informe puede ser revisado.",
    necessity = "Debe revisar el informe.",
    future = "Voy a trabajar.",
    progressive = "Ella est\u00e1 leyendo.",
    adjective = "El trabajo est\u00e1 terminado.",
    negation = "No vino nadie.",
    reflexive = "Se venden casas.",
    conditional = "Si llueve, todos esperan.",
    multi = "Yo trabajo. Nosotros leemos.",
    tokens = "Fue al pueblo para verlo.",
    pronominal = "Ella se dio cuenta del error.",
    split_misparse = "Puede siempre revisar el informe.",
    coordination_misparse = "Ella canta y \u00e9l baila.",
    wh_misparse = "Pregunt\u00f3 d\u00f3nde vive Ana."
  )
  tokens <- spacyr::spacy_parse(
    texts, dependency = TRUE, tag = TRUE, pos = TRUE, entity = FALSE,
    additional_attributes = "morph"
  )
  # Freeze native annotations as strings, not session-bound Python objects.
  tokens$morph <- vapply(tokens$morph, as.character, character(1))
  spacy <- reticulate::import("spacy")
  metadata <- reticulate::py_to_r(spacy$util$get_model_meta(
    spacy$util$get_package_path(model)
  ))
  saveRDS(
    list(
      model = model, model_version = metadata$version,
      spacy_version = spacy$`__version__`,
      spacyr_version = as.character(utils::packageVersion("spacyr")),
      texts = texts, tokens = tokens
    ),
    "tests/testthat/fixtures/spanish-spacy-regressions.rds", version = 3
  )
})
