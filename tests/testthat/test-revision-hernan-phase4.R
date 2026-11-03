# test-revision-hernan-phase4.R
#
# Phase 4: dictionaries and syntactic control. Covers what is implemented in code
# (without regenerating dict.rda):
#   - f_11: syntactic control (only the independent pronominal function; excludes
#     the determiner use). It becomes code-only so that the surface branch does not
#     override the filter.
#
# Note f_04/f_05: detection of multiword phrases ALREADY works (the quanteda
# branch compacts all the dict phrases via multiword_patterns, which the
# build constructs as the union of all the '_' entries of the dictionary). What
# remains is dict COVERAGE (e.g. 'en mitad de' is missing) and traced
# evidence, not the mechanism.
#
# nolint start: line_length_linter, object_name_linter

f11_count <- function(text) as.numeric(run_biber(text)$f_11_indefinite_pronouns)

test_that("f_11 cuenta indefinidos en función pronominal independiente", {
  skip_if_not_installed("udpipe")
  expect_equal(f11_count("Elige cualquiera de los libros."), 1)
  expect_equal(f11_count("No vino nadie a la fiesta."), 1)
  expect_equal(f11_count("Algo falló en el motor."), 1)
  expect_equal(f11_count("Alguien llamó pero nadie respondió nada."), 3)
  expect_equal(f11_count("Lo quiero todo ahora."), 1)   # pronominal 'todo' (obj)
})

test_that("f_11 excluye el uso NO pronominal (adjetival y determinante)", {
  skip_if_not_installed("udpipe")
  expect_equal(f11_count("Es un libro cualquiera."), 0)      # ADJ/amod
  expect_equal(f11_count("Todo el día estuvo lloviendo."), 0) # DET/det ('todo el día')
})

test_that("f_46/f_47: 'casi' cuenta en f_47 (hedges), no en f_46 (downtoners)", {
  skip_if_not_installed("udpipe")
  r <- run_biber("Casi termino el informe.")
  expect_equal(as.numeric(r$f_47_hedges), 1)
  expect_equal(as.numeric(r$f_46_downtoners), 0)
  # 'apenas' stays in f_46:
  expect_equal(as.numeric(run_biber("Apenas se nota el cambio.")$f_46_downtoners), 1)
})

test_that("f_04/f_05: locuciones añadidas al diccionario se cuentan", {
  skip_if_not_installed("udpipe")
  expect_equal(as.numeric(run_biber("Lo dejó en mitad de la sala.")$f_04_place_adverbials), 1)
  expect_equal(as.numeric(run_biber("El pueblo está más allá del río.")$f_04_place_adverbials), 1)
  expect_equal(as.numeric(run_biber("Hace poco llegaron los datos.")$f_05_time_adverbials), 1)
})

# nolint end
