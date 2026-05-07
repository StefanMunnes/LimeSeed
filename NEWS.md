# Recent Development

- codebook options: named `fields` labels, `rm_hidden`, and `rm_vars`
- bulk question option changes with `set_question_options()` and
  `seed_to_tsv(..., question_options = list(...))`


# LimeSeed 0.0.6

- create codebooks in different formats with quarto from seed with `seed_to_codebook()` or on the fly with `seed_to_tsv(..., codebook = "codebook.html")`
- new option for better testing of survey: change default values of survey settings and questions

# LimeSeed 0.0.5

- add template generator functions (lst_) for settings, questions, quota, and seed

# LimeSeed 0.0.4

- add helper functions (lsh_) for survey settings, question types and question options

# LimeSeed 0.0.3

- new naming convetion: remove sprout, all seed
- standalone `validate_seed()` function with line by line reportign and df export

# LimeSeed 0.0.2

- new code base: remove dependency from limonaid
- explicit definitions for every question, options, and quota in `survey_defs.R` vor validation and helper functions 

# LimeSeed 0.0.1

Initial version !
