linters <- all_linters(
  undesirable_function_linter(modify_defaults(
    default_undesirable_functions,
    ifelse = "Use fifelse instead.",
    system.time = "Only run timings in benchmark.Rraw."
  ))
)
