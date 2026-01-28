
- [x] provide one or two files separated by metadata & structure

- [ ] implement direct import of survey via API (create new, replace existing, modify) 

- [ ] change yaml default handler (for in text {}) ! seems no problem
- how to deal with Y N input as boolean? (change default handler?)
- [ ] implement and test new posit yaml function

- [ ] auto add html_entity_decode() for variable insert {varname}

- [ ] transform TSV export from given Survey to YAML (minimal or with all default values)

- [ ] function to create YAML templates (minimal or with all default values)

- [ ] function to create codebooks and formatted and printable survey preview

### Documentation

- [ ] fill readme with:
  - [ ] general idea
  - [ ] workflow (diagrams)
  - [ ] examples
  - [ ] installation

- [ ] create documentation page with multiple and in-depth examples
  - [ ] one or multiple input files
  - [ ] multiple language support
  - [ ] deal with long and short text inputs
  - [ ] manipulate loaded YAML content with code (e.g. for FSE)
  
  - [ ] Question-Types: multiple choice needs subquestions instead of answerOptions

### Checks

- [ ] unique question codes (will be renamed while import)
- [ ] variable names without "_"
- [ ] expressions are correct (e.g. in default)
- [ ] check if default values match type (numeric input != default = pos/neg)
- [ ] got an error because missed indentation for question because no group for first question
- [ ] all necessary inputs (e.g. multiple choice needs subquestions instead of answerOptions)

### limonaid
- [x] language specific default
- [x] add policy_notice
- [x] add_quota: condition/member valid question code (& type) and answer code
- [ ] do I have to specify type instead of lsType?
  `Error in if (type %in% c("array dual scale", "array (dual scale)")) ...: ! argument is of length zero`

### documentation
- [ ] multiline input (newline) https://stackoverflow.com/questions/3790454/how-do-i-break-a-string-in-yaml-over-multiple-lines
- [ ] multiple languages
- [ ] different ways to add text: simple -> full path


### function name ideas
- lime_grow <- function() {}
- lime_germ()
- lime_prune()
- lime_inspect()
- lime_trim()
- lime_validate()
- lime_deploy()
- lime_plant()
- lime_grow("survey.yaml")
- lime_grow("survey.yaml", deploy = TRUE)
- lime_seed() |>
-   lime_prepare() |>
-   lime_sprout() |>
-   lime_prune() |>
-   lime_compile()
