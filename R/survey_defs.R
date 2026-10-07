# survey_defs.R — LimeSurvey constants: type definitions, options, column names
# ─────────────────────────────────────────────────────────────────────────────
#
# Human-maintained:
#   LS_QUESTION_TYPES  — LS type code as key; each entry has `labels` (synonyms)
#                        plus structural metadata (requires, quota, description)
#   LS_Q_OPTIONS       — flat option registry with ls_type, valid, default
#
# Derived at load time (do not edit manually):
#   LS_LABEL_TO_CODE   — all label synonyms → LS type code (case-insensitive)
#   LS_ALL_TYPE_CODES  — all known LS type codes
#   LS_TYPES           — unified lookup keyed by LS type code
#   LS_COLUMNS         — full TSV column header
#
# Also defined here (shared by build and validate):
#   LS_SETTINGS     - survey seed keys written as S rows
#   LS_SL_SETTINGS  - survey seed keys written as SL rows
#   resolve_question_type()

# ══ Question type definitions ═════════════════════════════════════════════════
#
# KEY : the single-character LimeSurvey type code
#   labels:      character vector of human-readable synonyms accepted in YAML `type:`
#   requires:    YAML structural keys that must be present ("answerOptions", "subquestions")
#   quota :      TRUE when this type officially supports quota membership
#   description: optional named list(de=, en=) for documentation

# ══ Type group definitions ════════════════════════════════════════════════════
# Used by lsh_types("all") to group output instead of listing all types in one flat block.

LS_TYPE_GROUPS <- list(
  "List / Radio" = c("5", "!", "L", "O"),
  "Text" = c("S", "T", "U", "Q"),
  "Array" = c("1", "A", "B", "C", "E", "F", "H", ";", ":"),
  "Multiple Choice" = c("M", "P"),
  "Special / Masked" = c("|", "D", "G", "*", "Y", "K", "R", "I", "X", "N")
)

# TODO check labels and descriptions

LS_QUESTION_TYPES <- list(
  # ── List / radio types ───────────────────────────────────────────────────
  "5" = list(
    labels = c("5 point choice", "5 point", "5-Punkte-Auswahl", "5 Punkte")
  ),
  "!" = list(
    labels = c(
      "dropdown",
      "list dropdown",
      "Liste (Klappbox)",
      "Klappbox",
      "Dropdown-Liste",
      "Dropdown"
    ),
    description = list(
      de = "Einfachauswahl (mit einer klappbaren Liste).",
      en = "Single-choice dropdown list."
    ),
    requires = "answerOptions",
    quota = TRUE
  ),
  "L" = list(
    labels = c(
      "radio",
      "list",
      "single choice",
      "list with radio",
      "radiobuttons",
      "radio buttons",
      "Liste",
      "Liste (Optionsfelder)",
      "Einfachauswahl",
      "Optionsfelder",
      "Radiobuttons"
    ),
    description = list(
      de = "Einfachauswahl (Radiobuttons).",
      en = "Single-choice radio list."
    ),
    requires = "answerOptions",
    quota = TRUE
  ),
  "O" = list(
    labels = c(
      "list with comment",
      "list (with comment)",
      "Liste mit Kommentar",
      "Liste (mit Kommentar)",
      "Liste mit Freitext"
    ),
    description = list(
      de = "Liste mit Kommentar",
      en = "List with comment"
    ),
    requires = "answerOptions",
    quota = TRUE
  ),

  # ── Text types ───────────────────────────────────────────────────────────
  "U" = list(
    labels = c(
      "huge text",
      "huge free text",
      "free text (huge)",
      "text (huge)",
      "Riesiger Freitext",
      "Freitext (riesig)",
      "Ausführlicher Text",
      "Ausführlicher Freitext",
      "Text (riesig)"
    ),
    description = list(
      de = "Textangabe mit mehreren Zeilen.",
      en = "Text input with multiple lines."
    )
  ),
  "S" = list(
    labels = c(
      "short text",
      "short free text",
      "free text (short)",
      "text (short)",
      "Kurzer Freitext",
      "Kurzer freier Text",
      "Freitext (kurz)",
      "Text (kurz)"
    ),
    description = list(
      de = "Kurze Freitexteingabe.",
      en = "Short free-text input."
    )
  ),
  "T" = list(
    labels = c(
      "long text",
      "long free text",
      "free text (long)",
      "text (long)",
      "Langer Text",
      "Langer Freitext",
      "Langer freier Text",
      "Freitext (lang)",
      "Text (lang)"
    ),
    description = list(
      de = "Langer Freitextangabe",
      en = "Long free text input."
    )
  ),
  "Q" = list(
    labels = c(
      "multiple short text",
      "short text (multiple)",
      "Mehrfache kurze Texte",
      "Mehrere kurze Texte",
      "Mehrere kurze Freitexte",
      "Mehrere Texte",
      "Kurzer Text (mehrfach)"
    ),
    description = list(
      de = "Mehrere kurze Textangaben pro Frage",
      en = "More than one text entry per question"
    ),
    requires = "subquestions"
  ),

  # ── Masked / special types ───────────────────────────────────────────────
  "|" = list(
    labels = c(
      "file upload",
      "file",
      "upload",
      "Datei-Upload",
      "Datei",
      "Hochladen"
    ),
    description = list(
      de = "Datei hochladen.",
      en = "Upload file."
    )
  ),
  "D" = list(
    labels = c(
      "date",
      "time",
      "date/time",
      "Datum",
      "Datum/Zeit"
    ),
    description = list(
      de = "Datum, Uhrzeit oder eine Kombination aus Datum und Uhrzeit.",
      en = "Date, time or a combination of both date and time."
    )
  ),
  "G" = list(
    labels = c(
      "gender",
      "Geschlecht"
    ),
    description = list(
      de = "Diese Frage erfasst das Geschlecht ('männlich' oder 'weiblich').",
      en = "This question collects gender ('male' or 'female')."
    ),
    quota = TRUE
  ),
  "*" = list(
    labels = c(
      "equation",
      "Gleichung"
    ),
    description = list(
      de = "Gleichung erstellen und diese in einer Datenbankvariablen speichern.",
      en = "Creates an enquation and saves it in a database variable."
    ),
    quota = TRUE
  ),
  "Y" = list(
    labels = c(
      "yes/no",
      "yes no",
      "yes, no",
      "Ja/Nein",
      "Ja Nein",
      "Ja, Nein"
    ),
    description = list(
      de = "Erfasst 'Ja' oder 'Nein'.",
      en = "Collects 'Yes' or 'No' responses."
    ),
    quota = TRUE
  ),
  "K" = list(
    labels = c(
      "multiple numerical",
      "multiple numerical input",
      "slider",
      "Mehrfache numerische Eingabe",
      "Mehrfach numerisch",
      "Schieberegler"
    ),
    description = list(
      de = "Mehrere Textboxen mit jeweils numerischen Eingaben. Schiebregler möglich.",
      en = "Multiple text boxes, each for numerical input. Allows slider."
    ),
    requires = "subquestions"
  ),
  "R" = list(
    labels = c(
      "ranking",
      "Reihenfolge",
      "Rangfolge"
    ),
    description = list(
      de = "Optionsliste, die angeordnet werden kann.",
      en = "List of possible answers/options, which can be ranked."
    ),
    requires = "answerOptions"
  ),
  "I" = list(
    labels = c(
      "language switch",
      "language",
      "change language",
      "Sprachauswahl",
      "Sprache",
      "Sprachwechsel"
    ),
    description = list(
      de = "Sprache der Befragung wechseln.",
      en = "Changes the language of the survey."
    ),
    quota = TRUE
  ),
  "X" = list(
    labels = c(
      "Text display",
      "Just text",
      "Boilerplate",
      "Textanzeige",
      "Nur Text"
    ),
    description = list(
      de = "Nur Text, keine Eingabe.",
      en = "Read-only text display."
    )
  ),
  "N" = list(
    labels = c(
      "numerical input",
      "Zahleneingabe",
      "Numerische Eingabe"
    ),
    description = list(
      de = "Erfasst eine Zahl.",
      en = "Collects a single number."
    )
  ),

  # ── Array types ───────────────────────────────────────────────────────────
  "1" = list(
    labels = c(
      "array dual scale",
      "array (dual scale)",
      "Matrix (Zwei Skalen)",
      "Matrix duale Skala",
      "Dual Matrix"
    ),
    requires = c("subquestions", "answerOptions"),
    description = list(
      de = "Matrixfrage mit zwei Skalen für jede Teilfrage.",
      en = "Matrix / array question with two scales for each subquestion."
    )
  ),
  "F" = list(
    labels = c(
      "array",
      "array flexible",
      "array (flexible labels)",
      "Matrix",
      "Matrix (flexibel)"
    ),
    requires = c("subquestions", "answerOptions"),
    description = list(de = "Matrixfrage.", en = "Matrix / array question.")
  ),
  "B" = list(
    labels = c(
      "array 10 point",
      "array (10 point choice)",
      "10 point array",
      "Matrix (10-Punkte-Auswahl)",
      "10-Punkte-Matrix"
    ),
    requires = "subquestions",
    description = list(
      de = "Matrixfrage mit vordefinierter Skala von 1 bis 10.",
      en = "Matrix / array question prefilled with a scale from 1 to 10."
    ),
    quota = TRUE
  ),
  "A" = list(
    labels = c(
      "array 5 point",
      "array (5 point choice)",
      "5 point array",
      "Matrix (5-Punkte-Auswahl)",
      "5-Punkte-Matrix"
    ),
    requires = "subquestions",
    description = list(
      de = "Matrixfrage mit vordefinierter Skala von 1 bis 5.",
      en = "Matrix / array question prefilled with a scale from 1 to 5."
    ),
    quota = TRUE
  ),
  "C" = list(
    labels = c(
      "array yes no uncertain",
      "array (yes/no/uncertain)",
      "yes/no/uncertain array",
      "Matrix (Ja/Nein/Unsicher)",
      "Ja/Nein/Unsicher-Matrix",
      "Matrix Ja Nein Unsicher"
    ),
    requires = "subquestions",
    description = list(
      de = "Matrixfrage mit vordefinierten Antworten ('Ja', 'Nein', 'Unsicher').",
      en = "Matrix / array question prefilled with answers ('yes', 'no', 'uncertain')."
    )
  ),
  ";" = list(
    labels = c(
      "array text",
      "array texts",
      "array (texts)",
      "array (flexible labels)",
      "array (multiple texts",
      "multiple texts array",
      "Matrix (Text)",
      "Matrix (Texte)",
      "Matrix (Flexible Beschriftungen)",
      "Matrix (Mehrere Texteingabefelder",
      "Text-Matrix"
    ),
    requires = c("subquestions", "answerOptions"),
    description = list(
      de = "Matrixfrage mit Textfeldern.",
      en = "Matrix / array with text boxes."
    )
  ),
  ":" = list(
    labels = c(
      "array numbers",
      "array (flexible labels) multiple drop down",
      "array numbers multiflexible",
      "multiple dropdown array",
      "Matrix (Zahlen)",
      "Matrix (Flexible Beschriftungen) Dropdown-Liste",
      "Dropdown-Matrix",
      "Zahlen-Matrix"
    ),
    requires = c("subquestions", "answerOptions"),
    description = list(
      de = "Matrixfrage mit nummerierten Dropdown-Boxen.",
      en = "Matrix / array question with numbered dropdown boxes."
    )
  ),
  "E" = list(
    labels = c(
      "array increase same decrease",
      "array (increase/same/decrease)",
      "increase/same/decrease array",
      "Matrix (Zunahme/Gleich/Abnahme)",
      "Matrix Zunahme Gleich Abnahme",
      "Zunahme Gleich Abnahme Matrix"
    ),
    requires = "subquestions",
    description = list(
      de = "Matrixfrage mit vordefinierten Antwortmöglichkeiten - 'Zunahme', 'Gleich' und 'Abnahme'.",
      en = "Matrix / array question prefilled with the answers 'increase', 'same' and 'decrease'."
    )
  ),
  "H" = list(
    labels = c(
      "array by column",
      "array (flexible labels) by column",
      "Matrix (nach Spalte)",
      "Spalten-Matrix",
      "Matrix nach Spalte"
    ),
    requires = c("subquestions", "answerOptions"),
    description = list(
      de = "Matrixfrage mit Teilfragen als Spalten und Antwortoptionen als Zeilen.",
      en = "Matrix / array question with subquestions in the columns and answers in the rows."
    )
  ),

  # ── Multiple types ────────────────────────────────────────────────────────
  "M" = list(
    labels = c(
      "multiple choice",
      "checkboxes",
      "checkbox",
      "Mehrfachauswahl",
      "Checkboxen",
      "Kontrollkästchen"
    ),
    description = list(
      de = "Mehrfachauswahl (Checkboxen).",
      en = "Multiple-choice checkboxes."
    ),
    requires = "subquestions",
    quota = TRUE
  ),
  "P" = list(
    labels = c(
      "multiple choice with comments",
      "multiple choice (with comments)",
      "Mehrfachauswahl mit Kommentar",
      "Mehrfachauswahl (mit Kommentar)"
    ),
    requires = "subquestions"
  )
)


# ══ Question options ══════════════════════════════════════════════════════════
#
# Fields per entry:
#   description  named list(de=, en=)
#   ls_type      LS type codes this option applies to; absent → all types
#   default      written when user omits it; NULL → only written when provided
#   valid        allowed values: character/numeric vector OR a function for
#                complex predicates; NULL → any value accepted (no validation)
#   language     TRUE → multilingual value (get_text_fb() in builder)
#
# Validation in survey_validate.R handles both vectors and functions:
#   vector  → as.character(v) %in% as.character(valid)
#   function → isTRUE(valid(v))
#   NULL    → skip validation
#
# ?UNCERTAIN marks cases where only one example value was observed.

.ls_valid_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
attr(.ls_valid_character, "lsh_label") <- "character"

.ls_valid_numeric <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  if (is.numeric(x)) {
    return(is.finite(x))
  }
  grepl("^-?[0-9]+(\\.[0-9]+)?$", trimws(as.character(x)))
}
attr(.ls_valid_numeric, "lsh_label") <- "numeric"

.ls_valid_integer <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  grepl("^-?[0-9]+$", trimws(as.character(x)))
}
attr(.ls_valid_integer, "lsh_label") <- "integer"

.ls_valid_positive_integer <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  grepl("^[1-9][0-9]*$", trimws(as.character(x)))
}
attr(.ls_valid_positive_integer, "lsh_label") <- "positive integer"

.ls_valid_nonnegative_integer <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  grepl("^[0-9]+$", trimws(as.character(x)))
}
attr(.ls_valid_nonnegative_integer, "lsh_label") <- "non-negative integer"

.ls_valid_number_or_expression <- function(x) {
  length(x) == 1L &&
    !is.na(x) &&
    ((is.numeric(x) && is.finite(x)) ||
      (is.character(x) && nzchar(trimws(x))))
}
attr(.ls_valid_number_or_expression, "lsh_label") <- "number or expression"

.ls_valid_input_width <- function(x) {
  length(x) == 1L &&
    !is.na(x) &&
    (identical(as.character(x), "") || as.character(x) %in% as.character(1:12))
}
attr(.ls_valid_input_width, "lsh_label") <- "default or 1-12"

.ls_valid_label_width <- function(x) {
  length(x) == 1L &&
    !is.na(x) &&
    (as.character(x) %in% c("", "hidden", as.character(1:12)))
}
attr(.ls_valid_label_width, "lsh_label") <- "default, hidden, or 1-12"

LS_Q_OPTIONS <- list(
  # ══ Options available from the 4. Version on ══════════════════════════════

  "answer_order" = list(
    description = list(
      de = "Reihenfolge der Antwortoptionen: normal, random, alphabetical oder random_alphabetical.",
      en = "Answer option order: normal, random, alphabetical, or random_alphabetical."
    ),
    ls_type = c("!", "L", "O"),
    valid = c("normal", "random", "alphabetical", "random_alphabetical"),
    version = "5.5.0"
  ),

  "encrypted" = list(
    description = list(
      de = "Datenverschlüsselung für entsprechende Antworten ermöglichen. Standard = ausgeschaltet",
      en = "Enable data encryption for corresponding responses. Standard = off"
    ),
    ls_type = c(
      "5", "!", "L", "O", "D", "G", "Y", "K", "R", "I", "N",
      "1", "F", "B", "A", "C", ";", ":", "E", "H", "M", "P",
      "Q", "S", "T", "U", "|", "*"
    ),
    default = "N",
    valid = c("Y", "N"),
    version = "4"
  ),

  "subquestion_order" = list(
    description = list(
      de = "Reihenfolge der Teilfragen: normal, random, alphabetical oder random_alphabetical.",
      en = "Subquestion order: normal, random, alphabetical, or random_alphabetical."
    ),
    ls_type = c("M", "P", "R"),
    default = "normal",
    valid = c("normal", "random", "alphabetical", "random_alphabetical"),
    version = "6.16.0"
  ),

  "question_theme_name" = list(
    description = list(
      de = "Name des Fragenthemas (LS 6+), z.B. 'bootstrap_buttons'.",
      en = "Question theme name (LS 6+ only), e.g. 'bootstrap_buttons'."
    ),
    valid = .ls_valid_character,
    version = 5
  ),

  # ══ Options valid for ALL types ═══════════════════════════════════════════

  "text" = list(
    description = list(
      de = "Fragetext",
      en = "Question text."
    ),
    valid = .ls_valid_character
  ),

  "mandatory" = list(
    description = list(
      de = "Pflichtfrage (N = nein, Y = ja, S = weiche Pflichtfrage).",
      en = "Mandatory question (N = no, Y = yes, S = soft mandatory)."
    ),
    default = "N",
    valid = c("Y", "N", "S")
  ),

  "relevance" = list(
    description = list(
      de = "Anzeige-Bedingung (ExpressionScript, ohne {}). Standard: 1 = immer zeigen.",
      en = "Display condition (ExpressionScript, without {}). Default: 1 = always show."
    ),
    default = "1", # free-form ExpressionScrip
    valid = .ls_valid_character
  ),

  "cssclass" = list(
    description = list(
      de = "Benutzerdefinierte CSS-Klasse(n).",
      en = "Custom CSS class(es)."
    ),
    valid = .ls_valid_character
  ),

  "hidden" = list(
    description = list(
      de = "Frage ausblenden (1 = ausblenden, 0 = anzeigen).",
      en = "Hide question from participant (1 = hide, 0 = show)."
    ),
    valid = c(0, 1)
  ),

  "page_break" = list(
    description = list(
      de = "Seitenumbruch vor dieser Frage einfügen (nur Druckmodus; 1 = ja).",
      en = "Insert a page break before this question in print mode (1 = yes)."
    ),
    ls_type = c(
      "5", "!", "*", "L", "O", "D", "G", "Y", "K", "R", "X", "N",
      "1", "F", "B", "A", "C", ";", ":", "E", "H", "M", "P", "Q",
      "S", "T", "U", "|"
    ),
    valid = c(0, 1)
  ),

  "printable_help" = list(
    description = list(
      de = "Hilfetext für die Druckansicht.",
      en = "Help text for the print view."
    ),
    ls_type = c(
      "5",
      "!",
      "*",
      "L",
      "O",
      "D",
      "G",
      "Y",
      "K",
      "R",
      "N",
      "1",
      "F",
      "B",
      "A",
      "C",
      ":",
      "E",
      "H",
      "M",
      "P"
    ),
    language = TRUE
  ),

  "public_statistics" = list(
    description = list(
      de = "Frage in öffentlicher Statistik anzeigen (1 = ja, 0 = nein).",
      en = "Show question in public statistics (1 = yes, 0 = no)."
    ),
    ls_type = c(
      "1", "5", "A", "B", "C", "E", "F", "G", "H", "K", "L", "M",
      "N", "O", "P", "R", "Y", "!", ":", "*"
    ),
    valid = c(0, 1)
  ),

  "random_group" = list(
    description = list(
      de = "Randomisierungsgruppe: Fragen gleicher Gruppe werden zufällig sortiert.",
      en = "Randomisation group: questions in the same group are shown in random order."
    ),
    ls_type = c(
      "1", "5", "A", "B", "C", "D", "E", "F", "G", "H", "I", "K",
      "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "X", "Y",
      "!", ":", ";", "|"
    ),
    valid = .ls_valid_character
  ),

  "statistics_showgraph" = list(
    description = list(
      de = "Grafik in Statistik anzeigen (1 = ja, 0 = nein).",
      en = "Show graph in statistics (1 = yes, 0 = no)."
    ),
    valid = c(0, 1)
  ),

  "statistics_graphtype" = list(
    description = list(
      de = "Grafiktyp für die Statistikansicht (0 = Balkendiagramm, 1 = Tortendiagramm, 2 = Radar, 3 = Linie, 4 = Polar, 5 = Doughnut)",
      # CHECK - Diagrammtyp wird in Fragenübersicht als string angezeigt - muss noch in Tabelle geprüft werden
      en = "Graph type for the statistics view (0 = Bar chart, 1 = Pie chart, 2 = Radar, 3 = Line, 4 = PolarArea, 5 = Doughnut)"
    ),
    ls_type = c(
      "1", "5", "A", "B", "C", "D", "E", "F", "G", "H", "I", "K",
      "L", "M", "N", "O", "Q", "R", "S", "T", "U", "X", "Y", "!",
      ":", ";", "|", "*"
    ),
    valid = 0:5
  ),

  "scale_export" = list(
    description = list(
      de = "Für SPSS. Überschreibt die vom SPSS geratenen Standardskala mit der angegebenen Skala (0 = ausgeschaltet, 1 = Nominal, 2 = Ordinal, 3 = Skala).",
      en = "For SPSS. Overrides the default scale guessed by SPSS to the defined scale (0 = off, 1 = Nominal, 2 = Ordinal, 3 = Scale)."
    ),
    ls_type = c(
      "!",
      "L",
      "O",
      "G",
      "*",
      "1",
      "Y",
      "F",
      "C",
      ":",
      "E",
      "H",
      "M",
      "P"
    ),
    valid = 0:3
  ),

  # ══ hide_tip (all except equation *) ══════════════════════════════════════

  "hide_tip" = list(
    description = list(
      de = "Hilfe-Tooltip ausblenden (1 = ausblenden, 0 = anzeigen).",
      en = "Hide the help tooltip below the question (1 = hide, 0 = show)."
    ),
    ls_type = c(
      "!",
      "1",
      "5",
      ":",
      ";",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "K",
      "L",
      "M",
      "N",
      "O",
      "P",
      "Q",
      "R",
      "S",
      "T",
      "U",
      "X",
      "|",
      "Y"
    ),
    valid = c(0, 1)
  ),

  # ══ Validation expressions ════════════════════════════════════════════════

  "em_validation_q" = list(
    description = list(
      de = "Frage-Validierungsgleichung (Boolean ExpressionScript ohne {}).",
      en = "Question-level validation equation (Boolean ExpressionScript without {})."
    ),
    ls_type = c(
      "!",
      ":",
      ";",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "I",
      "K",
      "L",
      "M",
      "N",
      "O",
      "P",
      "Q",
      "R",
      "S",
      "T",
      "U",
      "X",
      "Y",
      "5",
      "*"
    )
  ),

  "em_validation_q_tip" = list(
    description = list(
      de = "Hinweistext zur Frage-Validierungsgleichung (wird dem Teilnehmer angezeigt).",
      en = "Tip shown to participants describing the question validation equation."
    ),
    ls_type = c(
      "!",
      ":",
      ";",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "I",
      "K",
      "L",
      "M",
      "N",
      "O",
      "P",
      "Q",
      "R",
      "S",
      "T",
      "U",
      "X",
      "Y",
      "5",
      "*"
    ),
    language = TRUE
  ),

  "em_validation_sq" = list(
    description = list(
      de = "Unterfragevalidierungsgleichung (Boolean ExpressionScript) ohne {}.",
      en = "Subquestion-level validation equation (Boolean ExpressionScript) without {}."
    ),
    ls_type = c(":", ";", "K", "N", "Q", "S", "T", "U")
  ),

  "em_validation_sq_tip" = list(
    description = list(
      de = "Hinweistext zur Unterfragevalidierungsgleichung.",
      en = "Tip shown to participants describing the subquestion validation equation."
    ),
    ls_type = c(":", ";", "K", "N", "Q", "S", "T", "U"),
    language = TRUE
  ),

  "validation" = list(
    description = list(
      de = "Regulärer Ausdruck zur Antwortvalidierung.",
      en = "Regular expression for validating the response."
    ),
    ls_type = c("N", "Q", "S", "T", "U", ";", ":", "K"),
    valid = .ls_valid_character
  ),

  # ══ Prefix / suffix ═══════════════════════════════════════════════════════

  "prefix" = list(
    description = list(
      de = "Text vor dem Eingabefeld (z.B. Währungszeichen).",
      en = "Text displayed before the input field (e.g. currency symbol)."
    ),
    ls_type = c("K", "N", "Q", "S"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "suffix" = list(
    description = list(
      de = "Text nach dem Eingabefeld (z.B. 'Jahre', 'kg').",
      en = "Text displayed after the input field (e.g. 'years', 'kg')."
    ),
    ls_type = c("K", "N", "Q", "S"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  # ══ Short / long / huge text (S, T, U) ═══════════════════════════════════

  "maximum_chars" = list(
    description = list(
      de = "Maximale Zeichenanzahl für die Texteingabe.",
      en = "Maximum number of characters allowed in the text input."
    ),
    ls_type = c(":", "K", "N", "Q", "S", "T", "U", ";"),
    valid = .ls_valid_positive_integer
  ),

  "text_input_width" = list(
    description = list(
      de = "Relative Breite des Texteingabefelds (Standard oder Stufe 1–12).",
      en = "Relative width of the text input field (default or level 1–12)."
    ),
    ls_type = c("K", "N", "S", "T", "U", ";"),
    valid = .ls_valid_input_width
  ),

  "display_rows" = list(
    description = list(
      de = "Anzahl der sichtbaren Zeilen des Textfelds.",
      en = "Number of visible rows in the text area."
    ),
    ls_type = c("Q", "S", "T", "U"),
    valid = .ls_valid_positive_integer
  ),

  "numbers_only" = list(
    description = list(
      de = "Nur Zahlen akzeptieren (1 = ja, 0 = nein).",
      en = "Accept numbers only (1 = yes, 0 = no)."
    ),
    ls_type = c("*", "Q", "S", ";"),
    valid = c(0, 1)
  ),

  "input_size" = list(
    description = list(
      de = "Breite des Eingabefelds (in Zeichen).",
      en = "Width of the input field (in characters)."
    ),
    ls_type = c(":", "K", "M", "N", "Q", "S", "T", "U", ";"),
    valid = .ls_valid_positive_integer
  ),

  "text_input_columns" = list(
    description = list(
      de = "Breite des Texteingabeboxes.",
      en = "Width of the text input box."
    ),
    ls_type = c("P", "Q"),
    valid = .ls_valid_input_width
  ),

  "label_input_columns" = list(
    description = list(
      de = "Relative Breite der Beschriftungsspalte (Standard, hidden oder Stufe 1–12).",
      en = "Relative label/subquestion width (default, hidden, or level 1–12)."
    ),
    ls_type = "Q",
    valid = .ls_valid_label_width
  ),

  "location_city" = list(
    description = list(
      de = "Stadtfeld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable city field in map picker (1 = yes)."
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  # ══ Location (only for S) ════════════════════════════════════════════════

  "location_state" = list(
    description = list(
      de = "Bundesland/Gliedstaat-Feld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable state field in map picker (1 = yes)."
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  "location_country" = list(
    description = list(
      de = "Länderfeld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable country field in map picker (1 = yes)."
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  "location_defaultcoordinates" = list(
    description = list(
      de = "Standard-Koordinaten für die Karte (lat,lng).",
      en = "Default coordinates for the map (lat,lng)."
    ),
    ls_type = "S"
  ),

  "location_mapservice" = list(
    description = list(
      de = "Kartendienst-ID. 0 = ausgeschaltet, 100 = Openstreet-Maps, 1 = GoogleMaps.",
      en = "Map service ID. 0 = none, 100 = Openstreet-Maps, 1 = GoogleMaps."
    ),
    ls_type = "S",
    valid = c(0, 1, 100)
  ),

  "location_mapwidth" = list(
    description = list(
      de = "Breite der Karte (in Pixeln, default = 500).",
      en = "Width of the map (in pixels, default = 500)."
    ),
    ls_type = "S",
    valid = .ls_valid_numeric
  ),

  "location_mapheight" = list(
    description = list(
      de = "Höhe der Karte (in Pixeln, , default = 300).",
      en = "Height of the map (in pixels, default = 300)."
    ),
    ls_type = "S",
    valid = .ls_valid_numeric
  ),

  "location_mapzoom" = list(
    description = list(
      de = "Standard-Zoom-Stufe der Karte (0–19).",
      en = "Default zoom level of the map (0–19)."
    ),
    ls_type = "S",
    valid = 0:19
  ),

  "location_nodefaultfromip" = list(
    description = list(
      de = "Keine Standard-Koordinaten aus IP-Adresse ableiten (1 = ja).",
      en = "Do not derive default coordinates from IP address (1 = yes)."
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  "location_postal" = list(
    description = list(
      de = "Postleitzahl in der Tabelle mit den Umfrageergebnissen speichern. Nur mit Google Map und einem gültigen Google-Map API Schlüssel nutzbar.",
      en = "Store postal code in the survey results table. Only usuable with google map and a valid google map API key. "
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  "statistics_showmap" = list(
    description = list(
      de = "Karte in Statistik anzeigen (0 = nein; nur für Typ S mit Kartenaktivierung).",
      en = "Show map in statistics (0 = no; only for type S with map enabled)."
    ),
    ls_type = "S",
    valid = c(0, 1)
  ),

  # ══ Numerical input (N, K) ════════════════════════════════════════════════

  "min_num_value_n" = list(
    description = list(
      de = "Minimalwert für numerische Eingabe.",
      en = "Minimum allowed value for numeric input."
    ),
    ls_type = c("K", "N"),
    valid = .ls_valid_number_or_expression
  ),

  "max_num_value_n" = list(
    description = list(
      de = "Maximalwert für numerische Eingabe.",
      en = "Maximum allowed value for numeric input."
    ),
    ls_type = c("K", "N"),
    valid = .ls_valid_number_or_expression
  ),

  "num_value_int_only" = list(
    description = list(
      de = "Nur ganze Zahlen akzeptieren (1 = ja, 0 = nein).",
      en = "Accept integers only (1 = yes, 0 = no)."
    ),
    ls_type = c("K", "N"),
    valid = c(0, 1)
  ),

  "equals_num_value" = list(
    description = list(
      de = "Pflicht-Gesamtsumme für multiple numerische Eingabe.",
      en = "Required total sum for multiple numerical input."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "value_range_allows_missing" = list(
    description = list(
      de = "Fehlende Werte im Bereich erlauben (0 = nein, 1 = ja).",
      en = "Allow missing values within the specified range (0 = no, 1 = yes)."
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "min_num_value" = list(
    description = list(
      de = "Minimalwert (ältere LS3-Syntax, bevorzuge min_num_value_n).",
      en = "Minimum value (legacy LS3 syntax; prefer min_num_value_n)."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "max_num_value" = list(
    description = list(
      de = "Maximalwert (ältere LS3-Syntax, bevorzuge max_num_value_n).",
      en = "Maximum value (legacy LS3 syntax; prefer max_num_value_n)."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  # ══ Slider (K) ════════════════════════════════════════════════════════════

  "slider_min" = list(
    description = list(
      de = "Minimalwert des Schiebereglers.",
      en = "Minimum slider value."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "slider_max" = list(
    description = list(
      de = "Maximalwert des Schiebereglers.",
      en = "Maximum slider value."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "slider_accuracy" = list(
    description = list(
      de = "Schrittweite des Schiebereglers.",
      en = "Step size of the slider."
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "slider_default_set" = list(
    description = list(
      de = "Stellt ein, dass der Anfangswert des Schiebereglers gespeichert wird, wenn der Schieberegler nicht bewegt wird (nein = 0, ja = 1 - Standard)",
      en = "Set the initial value that will be stored in the database. (no = 0, yes = 1 - Standard)"
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_default" = list(
    description = list(
      de = "Schieberegler Anfangswert definieren (Zahlen oder Expression ohne geschweifte Klammern)",
      en = "Choses a initial position of the slider as a value. (Numbers or expressions without curly brackets)"
    ),
    ls_type = "K",
    valid = .ls_valid_number_or_expression
  ),

  "slider_middlestart" = list(
    description = list(
      de = "Schieberegler startet in der Mitte (1 = ja, 0 = nein).",
      en = "Slider starts in the middle (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_layout" = list(
    description = list(
      de = "Schieberegler benutzen (1 = Standard, nicht benutzen).",
      en = "Use Slider layout (1 = default, not use). "
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_handle" = list(
    description = list(
      de = "Schieberegler-Handle-Typ (0 = Kreis - Standard, 1 = Quadrat, 2 = Dreieck, 3 = Angepasst).",
      en = "Slider handle type (0 = Circle - Standard, 1 - Square, 2 = Triangle, 3 - Custom)."
    ),
    ls_type = "K",
    valid = 0:3
  ),

  "slider_custom_handle" = list(
    description = list(
      de = "Benutzerdefinierter Schieberegler-Handle mit Font Awesome Icon-Code (Davor 'Angepasst' als Griff-Form auswählen).",
      en = "Custom slider handle with Font Awesome icon code (To use it, select 'custom' from the handle shape dropdown list)."
    ),
    ls_type = "K",
    valid = .ls_valid_character
  ),

  "slider_orientation" = list(
    description = list(
      de = "Schieberegler-Ausrichtung (0 = horizontal - Standard), 1 = vertikal",
      en = "Slider orientation (0 = horizontal - Standard). 1 = vertical"
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_reversed" = list(
    description = list(
      de = "Schieberegler-Richtung umkehren (1 = ja, 0 = nein).",
      en = "Reverse slider direction (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_reset" = list(
    description = list(
      de = "Schaltfläche zum Zurücksetzen des Schiebereglers anzeigen (1 = ja, 0 = nein).",
      en = "Show reset button for slider (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_showminmax" = list(
    description = list(
      de = "Min-/Max-Werte am Schieberegler anzeigen (1 = ja, 0 = nein).",
      en = "Show min/max values on slider (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    valid = c(0, 1)
  ),

  "slider_separator" = list(
    description = list(
      de = "Trennzeichen zwischen Schieberegler-Werten (Freitext).",
      en = "Separator character between slider values (free text)."
    ),
    ls_type = "K",
    valid = .ls_valid_character
  ),

  "slider_rating" = list(
    description = list(
      de = "Darstellung der 5-Punkte-Auswahl (0 = aus, 1 = Sterne, 2 = Schieberegler mit Emoticon).",
      en = "Five-point choice display (0 = off, 1 = stars, 2 = slider with emoticon)."
    ),
    ls_type = "5",
    valid = 0:2
  ),

  # ══ File Upload (|) ══════════════════════════════════════════════════════════

  "allowed_filetypes" = list(
    description = list(
      de = "Erlaubte Dateiformaten eingeben.",
      en = "Enter the file types allowed to be uploaded by the respondent."
    ),
    ls_type = "|",
    valid = .ls_valid_character
  ),

  "max_filesize" = list(
    description = list(
      de = "Maximale Dateigröße - bis zum 131072 KB.",
      en = "Maximum file size for upload - Limesurvey supports max. 131072 KB."
    ),
    ls_type = "|",
    valid = .ls_valid_positive_integer
  ),

  "max_num_of_files" = list(
    description = list(
      de = "Maximale Dateianzahl.",
      en = "Maximum number of files."
    ),
    ls_type = "|",
    valid = .ls_valid_positive_integer
  ),

  "min_num_of_files" = list(
    description = list(
      de = "Minimale Dateianzahl.",
      en = "Minimal number of files"
    ),
    ls_type = "|",
    valid = .ls_valid_nonnegative_integer
  ),

  "show_comment" = list(
    description = list(
      de = "Zeigt Kommentartextfeld für Kommentaren zur hochgeladener Datei an. (1 = ja - Standard)",
      en = "Displays a comment text box where the respondent can enter any comments on the uploaded file. (1 = yes - Standard)"
    ),
    ls_type = "|",
    valid = c(0, 1)
  ),

  "show_title" = list(
    description = list(
      de = "Zeigt eine Titelbox für die hochgeladene Datei an. (1 = ja - Standard)",
      en = "Displays a title textbox for the uploaded file. (1 = yes - Standard)"
    ),
    ls_type = "|",
    valid = c(0, 1)
  ),

  # ══ Date/time (D) ═════════════════════════════════════════════════════════

  "date_format" = list(
    description = list(
      de = "Benutzdefiniertes Datums- und Zeitformat setzen (z.B. 'Y-m-d', 'HH/MM').",
      en = "Specify a custom date or time format. (e.g. 'Y-m-d', 'HH/MM')."
    ),
    ls_type = "D",
    valid = .ls_valid_character
  ),

  "date_min" = list(
    description = list(
      de = "Frühestes erlaubtes Datum (im konfigurierten Datumsformat).",
      en = "Earliest allowed date (in the configured date format)."
    ),
    ls_type = "D",
    valid = .ls_valid_character
  ),

  "date_max" = list(
    description = list(
      de = "Spätestes erlaubtes Datum (im konfigurierten Datumsformat).",
      en = "Latest allowed date (in the configured date format)."
    ),
    ls_type = "D",
    valid = .ls_valid_character
  ),

  "dropdown_dates" = list(
    description = list(
      de = "Datum per Dropdown-Menüs auswählen (1 = ja, leer = nein).",
      en = "Use dropdown pickers for date input (1 = yes, empty = no)."
    ),
    ls_type = "D",
    valid = c(0, 1)
  ),

  "dropdown_dates_minute_step" = list(
    description = list(
      de = "Schrittweite für die Minuten-Dropdown (z.B. 5, 10, 15).",
      en = "Step size for the minutes dropdown (e.g. 5, 10, 15)."
    ),
    ls_type = "D"
  ),

  "dropdown_dates_month_style" = list(
    description = list(
      de = "Monatsdarstellung im Dropdown (0 = Kurzmonatsname, 1 = Vollmonatsname, 2 = Zahl).",
      en = "Month display style in dropdown (0 = Short names, 1 = Full names, 2 = Number)."
    ),
    ls_type = "D",
    valid = c(0, 1, 2)
  ),

  "reverse" = list(
    description = list(
      de = "Antwortskala umkehren (1 = ja, 0 = nein). Gilt für Datum und Matrix (Zahlen)",
      en = "Reverse the answer scale (1 = yes, 0 = no). Applies to Date and Array (Number)"
    ),
    ls_type = c("D", ":"),
    valid = c(0, 1)
  ),

  # ══ Yes/No (Y) ════════════════════════════════════════════════════════════

  "display_type" = list(
    description = list(
      de = "Anzeigetyp für Ja/Nein (0 = Standard Tastebutton; 1 = Radiolist).",
      en = "Display type for Yes/No (0 = Standard Buttongroup, 1 = Radiolist)."
    ),
    ls_type = c("G", "Y"),
    valid = c(0, 1)
  ),

  # ══ Equation (*) ══════════════════════════════════════════════════════════

  "equation" = list(
    description = list(
      de = "Ergebnis der eingegebenen Ausdruck in der Antworttabelle speichern. Es können alle Sprachen einschließlich ExpressionScript verwendet werden (ohne geschweifte Klammern).",
      en = "Store the result of the expression in the response table. All languages inlcuding ExpressionScript can be used (without curly braces)."
    ),
    ls_type = "*"
  ),

  # ══ Radio / dropdown / list (L, !, O) ════════════════════════════════════

  "other" = list(
    description = list(
      de = "Freie 'Andere'-Antwort aktivieren (Y = ja, N = nein).",
      en = "Enable a free-text 'Other' answer option (Y = yes, N = no)."
    ),
    ls_type = c("!", "L", "M", "P"),
    default = "N",
    valid = c("Y", "N")
  ),

  "other_replace_text" = list(
    description = list(
      de = "Beschriftung für die 'Andere'-Option (überschreibt 'Sonstiges').",
      en = "Label for the 'Other' input (overrides the default 'Other' label)."
    ),
    ls_type = c("!", "L", "M", "P"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "other_comment_mandatory" = list(
    description = list(
      de = "Kommentarfeld der 'Andere'-Option als Pflichtfeld (1 = ja).",
      en = "Make the 'Other' comment field mandatory (1 = yes)."
    ),
    ls_type = c("!", "L", "P"),
    valid = c(0, 1)
  ),

  "other_numbers_only" = list(
    description = list(
      de = "Im 'Andere'-Feld nur Zahlen akzeptieren (1 = ja).",
      en = "Accept numbers only in the 'Other' field (1 = yes)."
    ),
    ls_type = c("L", "M", "P"),
    valid = c(0, 1)
  ),

  "alphasort" = list(
    description = list(
      de = "Antwortoptionen alphabetisch sortieren (1 = ja, 0 = nein).",
      en = "Sort answer options alphabetically (1 = yes, 0 = no)."
    ),
    ls_type = c("!", "L", "O"),
    valid = c(0, 1)
  ),

  "display_columns" = list(
    description = list(
      de = "Anzahl der Spalten für die Antwortoptionen.",
      en = "Number of columns used to display answer options."
    ),
    ls_type = c("L", "M"),
    valid = .ls_valid_positive_integer
  ),

  "answer_width" = list(
    description = list(
      de = "Breite der Antwortoption-Beschriftung (in Prozent).",
      en = "Width of the answer option label (percentage)."
    ),
    ls_type = c("1", "A", "B", "C", "E", "F", ";", ":"),
    valid = 1:100
  ),

  "answer_width_bycolumn" = list(
    description = list(
      de = "Antwortbreite pro Spalte (in Prozent) für Matrix nach Spalte.",
      en = "Answer width per column (percentage) for array-by-column."
    ),
    ls_type = "H",
    valid = 1:100
  ),

  "category_separator" = list(
    description = list(
      de = "Trennzeichen für Kategorien in der Klappbox.",
      en = "Category separator string for the dropdown list."
    ),
    ls_type = "!"
  ),

  "dropdown_prefix" = list(
    description = list(
      de = "Präfix-Text für Klappbox-Einträge (1 = Antwortoptionen werden numeriert). ",
      en = "Prefix text for dropdown entries (1 = answer options are numbered)."
    ),
    ls_type = "!",
    valid = c(0, 1)
  ),

  "dropdown_size" = list(
    description = list(
      de = "Anzahl der sichtbaren Einträge in der Klappbox.",
      en = "Number of visible entries in the dropdown list."
    ),
    ls_type = "!",
    valid = .ls_valid_positive_integer
  ),

  "use_dropdown" = list(
    description = list(
      de = "Antwortliste als Klappbox darstellen (1 = ja, 0 = nein).",
      en = "Display answer list as dropdown (1 = yes, 0 = no)."
    ),
    ls_type = c("1", "F", "O"),
    valid = c(0, 1)
  ),

  # ══ Array / multiple choice shared ════════════════════════════════════════

  "random_order" = list(
    description = list(
      de = "Antwortoptionen/Teilfragen zufällig sortieren (1 = ja, 0 = nein).",
      en = "Display answer options/subquestion rows in random order (1 = yes, 0 = no)."
    ),
    ls_type = c(
      "1",
      ":",
      ";",
      "A",
      "B",
      "C",
      "E",
      "F",
      "H",
      "K",
      "M",
      "O",
      "P",
      "Q"
    ),
    valid = c(0, 1),
    version = "<5.5.0"
  ),

  "array_filter" = list(
    description = list(
      de = "Antworten einer Multiple-Choice-Frage als Teilfragen einer nachfolgenden Listen-, Array- oder Multiple-Choice-Frage verwenden. (Frage- und Antwortcodes eingeben)",
      en = "Use responses of multiple choice question as subquestions of a subsequent list, array or multiple choice question. (enter the question and the answer codes)"
    ),
    ls_type = c(
      "L",
      "Q",
      "K",
      "R",
      "1",
      "F",
      "B",
      "A",
      "C",
      ";",
      ":",
      "E",
      "M",
      "P"
    )
  ),

  "array_filter_exclude" = list(
    description = list(
      de = "Wie array_filter, aber mit umgekehrter Logik.",
      en = "Like array_filter but inverts the filter logic."
    ),
    ls_type = c(
      "1",
      ":",
      ";",
      "A",
      "B",
      "C",
      "E",
      "F",
      "K",
      "L",
      "M",
      "P",
      "Q",
      "R"
    )
  ),

  "array_filter_style" = list(
    description = list(
      de = "Stil der Array-Filterung (0 = default (nicht zeigen) 1 = grau einfärben).",
      en = "Array filter style (0 = default (not show) 1 = grey out)."
    ),
    ls_type = c(
      "1",
      ":",
      ";",
      "A",
      "B",
      "C",
      "E",
      "F",
      "K",
      "L",
      "M",
      "P",
      "Q",
      "R"
    ),
    valid = c(0, 1)
  ),

  "exclude_all_others" = list(
    description = list(
      de = "Deaktiviert die anderen Optionen bei Mehrfachauswahlfragen, wenn die Auswahl einer Antwort alle anderen unmöglich macht",
      en = "Disables the other options when selected (for multiple-choice questions where selecting one answer makes the other answers invalid.)."
    ),
    ls_type = c("A", "B", "C", "E", "F", "K", "M", "P", "Q")
  ),

  "exclude_all_others_auto" = list(
    description = list(
      de = "Exklusiv-Option automatisch abwählen wenn andere gewählt (1 = ja).",
      en = "Auto-deselect exclusive option when others are chosen (1 = yes)."
    ),
    ls_type = c("M", "P"),
    valid = c(0, 1)
  ),

  "min_answers" = list(
    description = list(
      de = "Mindestanzahl zu beantwortender Unterzeilen.",
      en = "Minimum number of subquestion rows that must be answered."
    ),
    ls_type = c(
      "1",
      ":",
      "A",
      "B",
      "C",
      "E",
      "F",
      "K",
      "M",
      "P",
      "Q",
      "R",
      ";"
    ),
    valid = .ls_valid_number_or_expression
  ),

  "max_answers" = list(
    description = list(
      de = "Maximalanzahl zu beantwortender Unterzeilen.",
      en = "Maximum number of subquestion rows that can be answered."
    ),
    ls_type = c(
      "1",
      ":",
      "A",
      "B",
      "C",
      "E",
      "F",
      "K",
      "M",
      "P",
      "Q",
      "R",
      ";"
    ),
    valid = .ls_valid_number_or_expression
  ),

  "repeat_headings" = list(
    description = list(
      de = "Spaltenüberschriften nach jeder N-ten Unterzeile wiederholen.",
      en = "Repeat column headings every N subquestion rows."
    ),
    ls_type = c("1", ":", ";", "F"),
    valid = .ls_valid_nonnegative_integer
  ),

  "input_boxes" = list(
    description = list(
      de = "Eingabefelder statt Checkboxen verwenden für Matrix Zahlen (1 = ja).",
      en = "Use input boxes instead of checkboxes for array numbers (1 = yes)."
    ),
    ls_type = ":",
    valid = c(0, 1)
  ),

  "show_grand_total" = list(
    description = list(
      de = "Gesamtsumme unten rechts in der Matrix anzeigen. (1 = ja, 0 = nein)",
      en = "Display the grand total in the bottom right part of the array. (1 = yes, 0 = no)"
    ),
    ls_type = ";",
    valid = c(1, 0)
  ),

  "show_totals" = list(
    description = list(
      de = "Summen für Zeilen, Spalten oder beide anzeigen. (X = aus, R = Zeilen, C = Spalten, B = beide)",
      en = "Display totals for rows, columns or both (X = off, R = rows, C = columns, B = both)"
    ),
    ls_type = ";",
    valid = c("X", "R", "C", "B")
  ),

  # ══ Array dual scale (1) ══════════════════════════════════════════════════

  "dualscale_headerA" = list(
    description = list(
      de = "Überschrift für Skala A der Doppelskalen-Matrix.",
      en = "Header label for scale A of the dual-scale array."
    ),
    ls_type = "1",
    valid = .ls_valid_character,
    language = TRUE
  ),

  "dualscale_headerB" = list(
    description = list(
      de = "Überschrift für Skala B der Doppelskalen-Matrix.",
      en = "Header label for scale B of the dual-scale array."
    ),
    ls_type = "1",
    valid = .ls_valid_character,
    language = TRUE
  ),

  "dropdown_prepostfix" = list(
    description = list(
      de = "Vor-/Nachsilbe für Dropdown-Einträge der Doppelskala.",
      en = "Pre/postfix text for dual-scale dropdown entries."
    ),
    ls_type = "1",
    valid = .ls_valid_character,
    language = TRUE
  ),

  "dropdown_separators" = list(
    description = list(
      de = "Trennzeichen zwischen Dropdown-Einträgen der Doppelskala.",
      en = "Separator between dual-scale dropdown entries."
    ),
    ls_type = "1",
    valid = .ls_valid_character
  ),

  "parent_order" = list(
    description = list(
      de = "Reihenfolge der Teilfragen einer bisherigen Frage (Array Zahlen) beibehalten.",
      en = "Keeps the order of subquestions from a previous question (array numbers)."
    ),
    ls_type = c(":")
    # check: valid is missing?
  ),

  # ══ Array numbers multiflexible (:) ═══════════════════════════════════════

  "multiflexible_checkbox" = list(
    description = list(
      de = "Antwortoptionen als Checkboxen darstellen (1 = ja).",
      en = "Display answers as checkboxes (1 = yes)."
    ),
    ls_type = ":",
    valid = c(0, 1)
  ),

  "multiflexible_min" = list(
    description = list(
      de = "Minimalwert für den Fragetyp Matrix (Zahlen) setzen.",
      en = "Set a minimum value for the question type Array (Numbers)."
    ),
    ls_type = ":",
    valid = .ls_valid_numeric
  ),

  "multiflexible_max" = list(
    description = list(
      de = "Maximalwert für den Fragetyp Matrix (Zahlen) setzen.",
      en = "Set a maximum value for the question type Array (Numbers)."
    ),
    ls_type = ":",
    valid = .ls_valid_numeric
  ),

  "multiflexible_step" = list(
    description = list(
      de = "Schrittweite für den Fragetyp Matrix (Zahlen) setzen.",
      en = "Step size for the question type Array (Numbers)."
    ),
    ls_type = ":",
    valid = .ls_valid_numeric
  ),

  # ══ Multiple choice comments (P) ══════════════════════════════════════════

  "commented_checkbox" = list(
    description = list(
      de = "Kommentiertes Checkbox-Verhalten (allways = Kommentar immer erlaubt, checked (Standard) = automatisch gecheckt beim kommentieren, unchecked = Kommentar nur bei nicht ausgewählten Itens.",
      en = "Commented checkbox behaviour (allways = user can alwas comment, checked (standard) = checkbox automatic checks when user comments, unchecked = user can only comment on unchecked boxes)"
    ),
    ls_type = "P",
    valid = c("allways", "checked", "unchecked") #CGP es geht auch mit den richtig geschriebene Variante "always", aber Limesurvey erkennt "allways" direk
  ),

  "commented_checkbox_auto" = list(
    description = list(
      de = "Steuern, ob die Textangabe beim Nicht-Ankreuzen gespeichert wird (1 = ja (default), 0 = nein).",
      en = "Define whether the text input remains saved or not once you uncheck its corresponding textbox.  (1 = yes (default), 0 = no)."
    ),
    ls_type = "P",
    valid = c(0, 1)
  ),

  "choice_input_columns" = list(
    description = list(
      de = "Breite des Texteingabeboxes (für den Fragentyp Mehrfachauswahl mit Kommentar).",
      en = "Width of the text input box (for the question type multiple choice with comments)."
    ),
    ls_type = "P"
  ),

  # ══ Ranking (R) ═══════════════════════════════════════════════════════════

  "max_subquestions" = list(
    description = list(
      de = "Maximale Anzahl der zuzuordnenden Rangplätzen (Ranking).",
      en = "Maximum number of ranking positions."
    ),
    ls_type = "R",
    valid = .ls_valid_positive_integer
  ),

  "rank_title" = list(
    description = list(
      de = "Beschriftung der Rang-Kopfzeile.",
      en = "Label for the rank column header."
    ),
    ls_type = "R",
    valid = .ls_valid_character,
    language = TRUE
  ),

  "choice_title" = list(
    description = list(
      de = "Beschriftung der Auswahlspalten-Überschrift.",
      en = "Label for the choice column header."
    ),
    ls_type = "R",
    language = TRUE
  ),

  "samechoiceheight" = list(
    description = list(
      de = "Gleiche Höhe für alle Auswahlfelder erzwingen (0 = nein, ja - Standard).",
      en = "Force equal height for all choice items (0 = no, 1 = yes)."
    ),
    ls_type = "R",
    valid = c(0, 1)
  ),

  "samelistheight" = list(
    description = list(
      de = "Gleiche Höhe für die Antwortliste erzwingen (0 = nein, ja).",
      en = "Force equal height for the answer list (0 = no, 1 = yes)."
    ),
    ls_type = "R",
    valid = c(0, 1)
  ),

  "showpopups" = list(
    description = list(
      de = "Javascript-Hinweise anzeigen, wenn die maximale Antwortenanzahl beim Ranking überschritten ist (0 = nein, 1 = ja - Standard).",
      en = "Show Javascript alert if the number of answers exceeds the maximum (0 = no, 1 = yes - Standard)."
    ),
    ls_type = "R",
    valid = c(0, 1)
  ),

  # ══ Multiple choice assessment (M, P) ════════════════════════════════════

  "assessment_value" = list(
    description = list(
      de = "Bewertungswert (Assessment) für jede angekreuzte Teilfrage.",
      en = "Assessment value for each checked subquestion."
    ),
    ls_type = c("M", "P"),
    valid = .ls_valid_integer
  ),

  # ══ Time limit ("!", "L", "S", "T", "U", "X")  ══════════════════════════════════

  "time_limit" = list(
    description = list(
      de = "Zeitlimit für die Frage (in Sekunden).",
      en = "Time limit for the question (in seconds)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_action" = list(
    description = list(
      de = "Aktion bei Zeitablauf: 1 = warne und geh weiter, 2 = Gehe weiter ohne Warnung, 3 = Nur Deaktivieren (Änderungen werden nicht übernommen).",
      en = "Action on time expiry: 1 = warn and move on (Standard), 2 = move on without warning, 3 = Disable only (Changes in the question are disabled)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = 1:3
  ),

  "time_limit_disable_next" = list(
    description = list(
      de = "'Nächster'-Knopf während Zeitlimit deaktivieren (1 = ja).",
      en = "Disable 'Next' button during time limit countdown (1 = yes)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = c(0, 1)
  ),

  "time_limit_disable_prev" = list(
    description = list(
      de = "'Zurück'-Knopf während Zeitlimit deaktivieren (1 = ja).",
      en = "Disable 'Back' button during time limit countdown (1 = yes)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = c(0, 1)
  ),

  "time_limit_countdown_message" = list(
    description = list(
      de = "Zeitlimitablauf-Nachricht",
      en = "Message displayed during the countdown."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "time_limit_timer_style" = list(
    description = list(
      de = "CSS-Stil für den Timer-Text.",
      en = "CSS style string for the timer text."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character
  ),

  "time_limit_message_delay" = list(
    description = list(
      de = "Verzögerung (in Sekunden) bevor die Ablaufnachricht angezeigt wird. Standard = 1 Sekunde. Einstellungsname: Zeitlimit Ablaufnachricht Anzeigezeit",
      en = "Delay (in seconds) before the expiry message is shown. Standard = 1 second. Configuration name: Time limit expiry message display time"
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_message" = list(
    description = list(
      de = "Nachricht, die nach Ablauf des Zeitlimits angezeigt wird.",
      en = "Message displayed after the time limit expires."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "time_limit_message_style" = list(
    description = list(
      de = "CSS-Stil für die Ablaufnachricht.",
      en = "CSS style string for the expiry message."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character
  ),

  "time_limit_warning" = list(
    description = list(
      de = "Zeit (in Sekunden) vor Ablauf des Zeitlimits, wenn die (erste) Vorwarnmeldung angezeigt wird.",
      en = "Set the time (in seconds) when the (first) warning message is shown before the time limit expires."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_warning_display_time" = list(
    description = list(
      de = "Anzeigedauer der ersten Vorwarnmeldung (in Sekunden).",
      en = "Display duration of the first warning (in seconds)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_warning_message" = list(
    description = list(
      de = "Text der ersten Vorwarnmeldung.",
      en = "Text of the first time limit warning."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "time_limit_warning_style" = list(
    description = list(
      de = "CSS-Stil für die erste Vorwarnmeldung.",
      en = "CSS style string for the first warning."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character
  ),

  "time_limit_warning_2" = list(
    description = list(
      de = "Zeit (in Sekunden) für zweite Vorwarnmeldung, bevor das Zeitlimit abläuft.",
      en = "Time (in seconds) before the time limit expires when the second warning is shown."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_warning_2_display_time" = list(
    description = list(
      de = "Anzeigedauer der zweiten Vorwarnung (in Sekunden).",
      en = "Display duration of the second warning (in seconds)."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_numeric
  ),

  "time_limit_warning_2_message" = list(
    description = list(
      de = "Text der zweiten Vorwarnmeldung.",
      en = "Text of the second time limit warning."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character,
    language = TRUE
  ),

  "time_limit_warning_2_style" = list(
    description = list(
      de = "CSS-Stil für die zweite Vorwarnmeldung.",
      en = "CSS style string for the second warning."
    ),
    ls_type = c("!", "L", "S", "T", "U", "X"),
    valid = .ls_valid_character
  )
)


.ls_q_character_options <- c(
  "relevance",
  "cssclass",
  "printable_help",
  "random_group",
  "question_theme_name",
  "em_validation_q",
  "em_validation_q_tip",
  "em_validation_sq",
  "em_validation_sq_tip",
  "validation",
  "prefix",
  "suffix",
  "location_defaultcoordinates",
  "location_mapservice",
  "slider_custom_handle",
  "slider_separator",
  "date_format",
  "date_min",
  "date_max",
  "equation",
  "other_replace_text",
  "category_separator",
  "array_filter",
  "array_filter_exclude",
  "exclude_all_others",
  "dualscale_headerA",
  "dualscale_headerB",
  "dropdown_prepostfix",
  "dropdown_separators",
  "parent_order",
  "rank_title",
  "choice_title",
  "time_limit_countdown_message",
  "time_limit_timer_style",
  "time_limit_message",
  "time_limit_message_style",
  "time_limit_warning_message",
  "time_limit_warning_style",
  "time_limit_warning_2_message",
  "time_limit_warning_2_style"
)

.ls_q_numeric_options <- c(
  "maximum_chars",
  "text_input_width",
  "display_rows",
  "input_size",
  "text_input_columns",
  "label_input_columns",
  "location_mapwidth",
  "location_mapheight",
  "min_num_value_n",
  "max_num_value_n",
  "equals_num_value",
  "min_num_value",
  "max_num_value",
  "slider_min",
  "slider_max",
  "slider_accuracy",
  "slider_default",
  "dropdown_dates_minute_step",
  "display_columns",
  "answer_width",
  "answer_width_bycolumn",
  "dropdown_size",
  "min_answers",
  "max_answers",
  "repeat_headings",
  "multiflexible_min",
  "multiflexible_max",
  "multiflexible_step",
  "choice_input_columns",
  "max_subquestions",
  "assessment_value",
  "time_limit",
  "time_limit_message_delay",
  "time_limit_warning",
  "time_limit_warning_display_time",
  "time_limit_warning_2",
  "time_limit_warning_2_display_time"
)

for (nm in .ls_q_character_options) {
  if (is.null(LS_Q_OPTIONS[[nm]]$valid)) {
    LS_Q_OPTIONS[[nm]]$valid <- .ls_valid_character
  }
}

for (nm in .ls_q_numeric_options) {
  if (is.null(LS_Q_OPTIONS[[nm]]$valid)) {
    LS_Q_OPTIONS[[nm]]$valid <- .ls_valid_numeric
  }
}

rm(.ls_q_character_options, .ls_q_numeric_options)


# ══ Derived objects ═══════════════════════════════════════════════════════════
# Built automatically from LS_QUESTION_TYPES — do not edit manually.

# All label synonyms (lowercase) mapped to their LS type code
LS_LABEL_TO_CODE <- local({
  result <- list()
  for (code in names(LS_QUESTION_TYPES)) {
    for (lbl in LS_QUESTION_TYPES[[code]]$labels) {
      result[[tolower(lbl)]] <- code
    }
  }
  result
})

# All valid LS type codes
LS_ALL_TYPE_CODES <- names(LS_QUESTION_TYPES)

# Unified lookup keyed by LS type code.
# Each entry: code, labels, quota, requires, options (filtered LS_Q_OPTIONS).
LS_TYPES <- local({
  result <- setNames(
    vector("list", length(LS_ALL_TYPE_CODES)),
    LS_ALL_TYPE_CODES
  )

  for (code in LS_ALL_TYPE_CODES) {
    defn <- LS_QUESTION_TYPES[[code]]

    applicable_opts <- Filter(
      function(opt) is.null(opt$ls_type) || code %in% opt$ls_type,
      LS_Q_OPTIONS
    )

    result[[code]] <- list(
      code = code,
      labels = defn$labels,
      quota = isTRUE(defn$quota),
      requires = defn$requires %||% character(0),
      options = applicable_opts
    )
  }

  result
})


# ══ Type resolution (shared by builder and validator) ═════════════════════════
#
# Resolution order:
#   1. `lsType` key in YAML  → validated against LS_ALL_TYPE_CODES, used directly
#   2. `type` key in YAML    → matched against all registered label synonyms
#                              (case-insensitive)
#   3. `type` key in YAML    → treated as a direct LS code (power-user path)
#   4. Nothing matches       → return NULL (caller emits the error)

#' @keywords internal
resolve_question_type <- function(qst_code, qst_data) {
  # 1. Explicit lsType
  if (!is.null(qst_data$lsType)) {
    code <- as.character(qst_data$lsType)
    if (code %in% LS_ALL_TYPE_CODES) {
      return(code)
    }
    return(NULL)
  }

  type_input <- trimws(tolower(as.character(qst_data$type %||% "")))

  # 2. Label synonym match (case-insensitive)
  if (type_input %in% names(LS_LABEL_TO_CODE)) {
    return(LS_LABEL_TO_CODE[[type_input]])
  }

  # 3. Direct LS code (power-user path)
  if (type_input %in% LS_ALL_TYPE_CODES) {
    return(type_input)
  }

  NULL
}


# ══ Survey-level settings ═════════════════════════════════════════════════════
#
# Unified registry for survey settings, matching the structure of LS_Q_OPTIONS.
# ?UNCERTAIN marks fields where exact valid codes might vary by LS version.

.is_num <- function(x) is.numeric(x) || grepl("^[0-9]+$", as.character(x))

LS_SETTINGS <- list(
  "sid" = list(
    description = list(de = "Umfrage-ID.", en = "Survey ID."),
    default = 1L,
    valid = .is_num
  ),
  "gsid" = list(
    description = list(de = "Basis-Sprach-ID.", en = "Base language ID."),
    default = 1L,
    valid = .is_num
  ),
  "admin" = list(
    description = list(
      de = "Name des Administrators.",
      en = "Administrator name."
    ),
    default = ""
  ),
  "adminemail" = list(
    description = list(
      de = "E-Mail des Administrators.",
      en = "Administrator email."
    ),
    default = ""
  ),
  "anonymized" = list(
    description = list(
      de = "Antworten anonymisieren (Y/N).",
      en = "Anonymize responses (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "faxto" = list(
    description = list(de = "Faxnummer.", en = "Fax number."),
    default = ""
  ),
  "format" = list(
    description = list(
      de = "Umfrageformat (G = Gruppe für Gruppe, Q = Frage für Frage, A = Alles auf einer Seite).",
      en = "Survey format (G = Group by group, Q = Question by question, A = All in one)."
    ),
    default = "G",
    valid = c("G", "Q", "A")
  ),
  "savetimings" = list(
    description = list(
      de = "Zeitnahme speichern (Y/N).",
      en = "Save timings (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "template" = list(
    description = list(
      de = "Name des Umfrage-Themes (z.B. vanilla).",
      en = "Survey theme name (e.g., vanilla)."
    ),
    default = "vanilla"
  ),
  "language" = list(
    description = list(
      de = "Primärsprache der Umfrage.",
      en = "Primary language of the survey."
    ),
    default = ""
  ),
  "additional_languages" = list(
    description = list(
      de = "Zusätzliche Sprachen (als String).",
      en = "Additional languages (as string)."
    ),
    default = ""
  ),
  "datestamp" = list(
    description = list(
      de = "Datum/Zeitstempel speichern (Y/N).",
      en = "Save datestamp (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "usecookie" = list(
    description = list(
      de = "Cookie nutzen um wiederholte Teilnahme zu verhindern (Y/N).",
      en = "Use cookie to prevent repeated participation (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "allowregister" = list(
    description = list(
      de = "Öffentliche Registrierung erlauben (Y/N).",
      en = "Allow public registration (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "allowsave" = list(
    description = list(
      de = "Zwischenspeichern erlauben (Y/N).",
      en = "Allow save and return later (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "autonumber_start" = list(
    description = list(
      de = "Startwert für automatische Nummerierung.",
      en = "Start value for auto-numbering."
    ),
    default = 0L,
    valid = .is_num
  ),
  "autoredirect" = list(
    description = list(
      de = "Nach Abschluss automatisch weiterleiten (Y/N).",
      en = "Auto-redirect after completion (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "allowprev" = list(
    description = list(
      de = "Rückwärtsnavigation erlauben (Y/N).",
      en = "Allow backward navigation (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "printanswers" = list(
    description = list(
      de = "Drucken der Antworten erlauben (Y/N).",
      en = "Allow printing answers (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "ipaddr" = list(
    description = list(
      de = "IP-Adresse speichern (Y/N).",
      en = "Save IP address (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "refurl" = list(
    description = list(
      de = "Referrer-URL speichern (Y/N).",
      en = "Save referrer URL (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "showsurveypolicynotice" = list(
    description = list(
      de = "Datenschutzrichtlinie anzeigen (0, 1). ?UNCERTAIN.",
      en = "Show survey policy notice (0, 1). ?UNCERTAIN."
    ),
    default = 0L,
    valid = c(0, 1)
  ),
  "publicstatistics" = list(
    description = list(
      de = "Öffentliche Statistiken aktivieren (Y/N).",
      en = "Enable public statistics (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "publicgraphs" = list(
    description = list(
      de = "Öffentliche Grafiken aktivieren (Y/N).",
      en = "Enable public graphs (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "listpublic" = list(
    description = list(
      de = "Umfrage öffentlich listen (Y/N).",
      en = "List survey publicly (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "htmlemail" = list(
    description = list(
      de = "HTML-E-Mails verwenden (Y/N).",
      en = "Use HTML emails (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "sendconfirmation" = list(
    description = list(
      de = "Bestätigungs-E-Mail senden (Y/N).",
      en = "Send confirmation email (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "tokenanswerspersistence" = list(
    description = list(
      de = "Token-basierte Antwortpersistenz (Y/N).",
      en = "Token-based response persistence (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "assessments" = list(
    description = list(
      de = "Bewertungsmodus (Assessments) aktivieren (Y/N).",
      en = "Enable assessments mode (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "usecaptcha" = list(
    description = list(
      de = "CAPTCHA verwenden. ?UNCERTAIN: Genaue Werte (A, B, C, N, Y).",
      en = "Use CAPTCHA. ?UNCERTAIN: Exact values (A, B, C, N, Y)."
    ),
    default = "N",
    valid = c("N", "Y", "A", "B", "C", "D", "X")
  ),
  "usetokens" = list(
    description = list(
      de = "Teilnehmer-Panel (Tokens) verwenden (Y/N).",
      en = "Use participant panel (tokens) (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "bounce_email" = list(
    description = list(
      de = "E-Mail-Adresse für unzustellbare Nachrichten.",
      en = "Bounce email address."
    ),
    default = ""
  ),
  "emailresponseto" = list(
    description = list(
      de = "E-Mail für Antwortbenachrichtigungen.",
      en = "Email for response notifications."
    ),
    default = ""
  ),
  "emailnotificationto" = list(
    description = list(
      de = "E-Mail für detaillierte Antwortbenachrichtigungen.",
      en = "Email for detailed response notifications."
    ),
    default = ""
  ),
  "tokenlength" = list(
    description = list(
      de = "Länge des Zugangsschlüssels (Token).",
      en = "Token length."
    ),
    default = 15L,
    valid = .is_num
  ),
  "showxquestions" = list(
    description = list(
      de = "Anzahl der Fragen anzeigen (Y/N).",
      en = "Show 'There are X questions' (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "showgroupinfo" = list(
    description = list(
      de = "Gruppeninfo anzeigen (B=Beides, D=Beschreibung, N=Name, X=Verbergen).",
      en = "Show group info (B=Both, D=Description, N=Name, X=Hide)."
    ),
    default = "X",
    valid = c("B", "D", "N", "X")
  ),
  "shownoanswer" = list(
    description = list(
      de = "Option 'Keine Antwort' anzeigen (Y/N/C). ?UNCERTAIN: C = Admin Choice?",
      en = "Show 'No answer' option (Y/N/C). ?UNCERTAIN: C = Admin Choice?"
    ),
    default = "N",
    valid = c("Y", "N", "C")
  ),
  "showqnumcode" = list(
    description = list(
      de = "Zeige Fragennummer und/oder Fragencode (N=Nummer, N=Beides verbergen, C=Code, B=Beides). ?UNCERTAIN.",
      en = "Show question number and/or code (N=Number, N=Hide both, C=Code, B=Both). ?UNCERTAIN."
    ),
    default = "X",
    valid = c("X", "N", "B", "C")
  ),
  "bounceprocessing" = list(
    description = list(
      de = "Bounce-Verarbeitung aktivieren (Y/N).",
      en = "Enable bounce processing (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "showwelcome" = list(
    description = list(
      de = "Willkommensbildschirm anzeigen (Y/N).",
      en = "Show welcome screen (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "showprogress" = list(
    description = list(
      de = "Fortschrittsbalken anzeigen (Y/N).",
      en = "Show progress bar (Y/N)."
    ),
    default = "Y",
    valid = c("Y", "N")
  ),
  "questionindex" = list(
    description = list(
      de = "Fragenindex anzeigen (0=Deaktiviert, 1=Inkrementell, 2=Voll).",
      en = "Show question index (0=Disabled, 1=Incremental, 2=Full)."
    ),
    default = 0L,
    valid = c(0, 1, 2)
  ),
  "navigationdelay" = list(
    description = list(
      de = "Navigationsverzögerung (Sekunden).",
      en = "Navigation delay (seconds)."
    ),
    default = 0L,
    valid = .is_num
  ),
  "nokeyboard" = list(
    description = list(
      de = "Tastaturnavigation deaktivieren (Y/N).",
      en = "Disable keyboard navigation (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "alloweditaftercompletion" = list(
    description = list(
      de = "Bearbeiten nach Abschluss erlauben (Y/N).",
      en = "Allow editing after completion (Y/N)."
    ),
    default = "N",
    valid = c("Y", "N")
  ),
  "googleanalyticsstyle" = list(
    description = list(
      de = "Google Analytics Stil (0, 1, 2). ?UNCERTAIN.",
      en = "Google Analytics style (0, 1, 2). ?UNCERTAIN."
    ),
    default = 0L,
    valid = c(0, 1, 2)
  ),
  "googleanalyticsapikey" = list(
    description = list(
      de = "Google Analytics Tracking-ID.",
      en = "Google Analytics Tracking ID."
    ),
    default = ""
  )
)


# ══ Survey-level language settings (SL rows) ═════════════════════════════════
#
# Unified registry for survey seed keys that map to LimeSurvey SL rows.
# `sl_name` is the TSV column name written by build_lang_rows().

LS_SL_SETTINGS <- list(
  "titles" = list(
    sl_name = "surveyls_title",
    description = list(de = "Umfragetitel.", en = "Survey title."),
    required = TRUE,
    language = TRUE
  ),
  "descriptions" = list(
    sl_name = "surveyls_description",
    description = list(
      de = "Umfragebeschreibung.",
      en = "Survey description."
    ),
    language = TRUE
  ),
  "welcomeTexts" = list(
    sl_name = "surveyls_welcometext",
    description = list(de = "Willkommenstext.", en = "Welcome text."),
    language = TRUE
  ),
  "policy_notice" = list(
    sl_name = "surveyls_policy_notice",
    description = list(
      de = "Datenschutzhinweis.",
      en = "Survey privacy notice."
    ),
    language = TRUE
  ),
  "policy_notice_label" = list(
    sl_name = "surveyls_policy_notice_label",
    description = list(
      de = "Label des Datenschutzhinweises.",
      en = "Survey privacy notice label."
    ),
    language = TRUE
  ),
  "endTexts" = list(
    sl_name = "surveyls_endtext",
    description = list(de = "Abschlusstext.", en = "End text."),
    language = TRUE
  ),
  "endURLs" = list(
    sl_name = "surveyls_url",
    description = list(de = "Abschluss-URL.", en = "End URL."),
    language = TRUE
  ),
  "endURLdescriptions" = list(
    sl_name = "surveyls_urldescription",
    description = list(
      de = "Beschreibung der Abschluss-URL.",
      en = "End URL description."
    ),
    language = TRUE
  ),
  "dateformats" = list(
    sl_name = "surveyls_dateformat",
    description = list(de = "Datumsformat.", en = "Date format."),
    default = 6L,
    valid = .is_num
  ),
  "numberformats" = list(
    sl_name = "surveyls_numberformat",
    description = list(de = "Zahlenformat.", en = "Number format."),
    default = 0L,
    valid = .is_num
  )
)


# ══ Q-row core YAML keys ══════════════════════════════════════════════════════
#
# Q_CORE_FIELDS lists every YAML key that build_question_rows() already handles
# explicitly — either written directly into a core TSV column or consumed to
# generate a separate row class (SQ, A).  Keys in this set are intentionally
# skipped in the attribute-options loop so they are never double-written.
#
# The set is minimal: removing any entry would cause that field to be processed
# twice (once explicitly, once through the options loop).

Q_CORE_FIELDS <- c(
  # Type resolution
  "type",
  "lsType",
  # Language-aware text fields (written via get_text_fb())
  "questionTexts",
  "helpTexts",
  "prefix",
  "suffix",
  "default",
  # Structural keys that generate their own row classes
  "answerOptions",
  "subquestions",
  # Core TSV columns written explicitly in ls_row()
  "relevance",
  "mandatory",
  "other",
  "validation",
  "same_default"
)


# ══ TSV column names ══════════════════════════════════════════════════════════

LS_CORE_COLUMNS <- c(
  "id",
  "related_id",
  "class",
  "type.scale",
  "name",
  "relevance",
  "text",
  "help",
  "language",
  "validation",
  "mandatory",
  "other",
  "default",
  "same_default",
  "random_group",
  "other_replace_text",
  "prefix",
  "suffix"
)


LS_COLUMNS <- c(LS_CORE_COLUMNS, setdiff(names(LS_Q_OPTIONS), LS_CORE_COLUMNS))


# ══ Quota field definitions ════════════════════════════════════════════════════

LS_QUOTA_OPTIONS <- list(
  "limit" = list(
    description = list(
      de = "Maximale Anzahl gültiger Antworten bevor die Quota greift.",
      en = "Maximum number of completes before the quota triggers."
    ),
    required = TRUE,
    valid = function(x) is.numeric(x) && x >= 0 && x %% 1 == 0
  ),
  "members" = list(
    description = list(
      de = "Benannte Liste: Fragencode → Antwortcode(s), die diese Quota zählen.",
      en = "Named list: question code → answer code(s) that count toward this quota."
    ),
    required = TRUE,
    # Must be a non-empty named list where every name is a non-empty string
    valid = function(x) {
      is.list(x) &&
        length(x) > 0 &&
        !is.null(names(x)) &&
        all(nzchar(names(x)))
    }
  ),
  "action" = list(
    description = list(
      de = "Aktion bei Quoten-Erfüllung (1 = Beenden, 2 = Warnung).",
      en = "Action on quota trigger (1 = Terminate, 2 = Warning)."
    ),
    default = 1L,
    valid = c(1, 2)
  ),
  "active" = list(
    description = list(
      de = "Quota aktiv (1 = ja, 0 = nein).",
      en = "Quota active (1 = yes, 0 = no)."
    ),
    default = 1L,
    valid = c(0, 1)
  ),
  "autoloadURL" = list(
    description = list(
      de = "URL automatisch laden wenn Quota greift (1 = ja, 0 = nein).",
      en = "Autoload the redirect URL when quota triggers (1 = yes, 0 = no)."
    ),
    default = 0L,
    valid = c(0, 1)
  ),
  "messageTexts" = list(
    description = list(
      de = "Nachricht bei Quoten-Erfüllung (mehrsprachig).",
      en = "Message displayed when quota is triggered (multilingual)."
    ),
    language = TRUE
  ),
  "urls" = list(
    description = list(
      de = "Weiterleitungs-URL (mehrsprachig).",
      en = "Redirect URL when quota triggers (multilingual)."
    ),
    language = TRUE
  ),
  "urlDescriptions" = list(
    description = list(
      de = "Beschreibung der Weiterleitungs-URL (mehrsprachig).",
      en = "Description of the redirect URL (multilingual)."
    ),
    language = TRUE
  )
)


# ══ SL-row email defaults ═════════════════════════════════════════════════════

EMAIL_DEFAULTS <- list(
  surveyls_email_invite_subj = "Invitation to participate in a survey",
  surveyls_email_invite = paste0(
    "Dear {FIRSTNAME},<br /><br />you have been invited to participate in a ",
    "survey.<br /><br />The survey is titled:<br />\"{SURVEYNAME}\"<br />",
    "<br />\"{SURVEYDESCRIPTION}\"<br /><br />To participate, please click on ",
    "the link below.<br /><br />Sincerely,<br /><br />{ADMINNAME} ({ADMINEMAIL})",
    "<br /><br />----------------------------------------------<br />Click here ",
    "to do the survey:<br />{SURVEYURL}<br /><br />If you do not want to ",
    "participate in this survey and don't want to receive any more invitations ",
    "please click the following link:<br />{OPTOUTURL}<br /><br />If you are ",
    "blacklisted but want to participate in this survey and want to receive ",
    "invitations please click the following link:<br />{OPTINURL}"
  ),
  surveyls_email_remind_subj = "Reminder to participate in a survey",
  surveyls_email_remind = paste0(
    "Dear {FIRSTNAME},<br /><br />Recently we invited you to participate in a ",
    "survey.<br /><br />We note that you have not yet completed the survey, ",
    "and wish to remind you that the survey is still available should you ",
    "wish to take part.<br /><br />The survey is titled:<br />\"{SURVEYNAME}\"",
    "<br /><br />\"{SURVEYDESCRIPTION}\"<br /><br />To participate, please click ",
    "on the link below.<br /><br />Sincerely,<br /><br />{ADMINNAME} ({ADMINEMAIL})",
    "<br /><br />----------------------------------------------<br />Click here ",
    "to do the survey:<br />{SURVEYURL}<br /><br />If you do not want to ",
    "participate in this survey and don't want to receive any more invitations ",
    "please click the following link:<br />{OPTOUTURL}"
  ),
  surveyls_email_register_subj = "Survey registration confirmation",
  surveyls_email_register = paste0(
    "Dear {FIRSTNAME},<br /><br />You, or someone using your email address, ",
    "have registered to participate in an online survey titled {SURVEYNAME}.",
    "<br /><br />To complete this survey, click on the following URL:<br />",
    "<br />{SURVEYURL}<br /><br />If you have any questions about this survey, ",
    "or if you did not register to participate and believe this email is in ",
    "error, please contact {ADMINNAME} at {ADMINEMAIL}."
  ),
  surveyls_email_confirm_subj = "Confirmation of your participation in our survey",
  surveyls_email_confirm = paste0(
    "Dear {FIRSTNAME},<br /><br />this email is to confirm that you have ",
    "completed the survey titled {SURVEYNAME} and your response has been saved.",
    " Thank you for participating.<br /><br />If you have any further questions ",
    "about this email, please contact {ADMINNAME} on {ADMINEMAIL}.<br /><br />",
    "Sincerely,<br /><br />{ADMINNAME}"
  ),
  email_admin_notification_subj = "Response submission for survey {SURVEYNAME}",
  email_admin_notification = paste0(
    "Hello,<br /><br />A new response was submitted for your survey ",
    "'{SURVEYNAME}'.<br /><br />Click the following link to see the individual ",
    "response:<br />{VIEWRESPONSEURL}<br /><br />Click the following link to ",
    "edit the individual response:<br />{EDITRESPONSEURL}<br /><br />View ",
    "statistics by clicking here:<br />{STATISTICSURL}"
  ),
  email_admin_responses_subj = "Response submission for survey {SURVEYNAME} with results",
  email_admin_responses = paste0(
    "Hello,<br /><br />A new response was submitted for your survey ",
    "'{SURVEYNAME}'.<br /><br />Click the following link to see the individual ",
    "response:<br />{VIEWRESPONSEURL}<br /><br />Click the following link to ",
    "edit the individual response:<br />{EDITRESPONSEURL}<br /><br />View ",
    "statistics by clicking here:<br />{STATISTICSURL}<br /><br /><br />The ",
    "following answers were given by the participant:<br />{ANSWERTABLE}"
  )
)
