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
#   resolve_question_type()

# ══ Question type definitions ═════════════════════════════════════════════════
#
# KEY : the single-character LimeSurvey type code
#   labels:      character vector of human-readable synonyms accepted in YAML `type:`
#   requires:    YAML structural keys that must be present ("answerOptions", "subquestions")
#   quota :      TRUE when this type officially supports quota membership
#   description: optional named list(de=, en=) for documentation

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
      "Liste (mit Kommentar)"
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
      "Text (riesig)"
    )
  ),
  "S" = list(
    labels = c(
      "short text",
      "short free text",
      "free text (short)",
      "text (short)",
      "Kurzer Freitext",
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
      "Freitext (lang)",
      "Text (lang)"
    )
  ),
  "Q" = list(
    labels = c(
      "multiple short text",
      "short text (multiple)",
      "Mehrere kurze Texte",
      "Mehrere kurze Freitexte",
      "Mehrere Texte",
      "Kurzer Text (mehrfach)"
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
    )
  ),
  "D" = list(labels = c("date", "Datum", "Datum/Zeit")),
  "G" = list(labels = c("gender", "Geschlecht"), quota = TRUE),
  "*" = list(labels = c("equation", "Gleichung"), quota = TRUE),
  "Y" = list(
    labels = c("yes/no", "yes no", "yes, no", "Ja/Nein", "Ja Nein", "Ja, Nein"),
    quota = TRUE
  ),
  "K" = list(
    labels = c(
      "multiple numerical",
      "multiple numerical input",
      "slider",
      "Mehrfache numerische Eingabe",
      "Schieberegler"
    ),
    requires = "subquestions"
  ),
  "R" = list(
    labels = c("ranking", "Reihenfolge", "Rangfolge"),
    requires = "answerOptions"
  ),
  "I" = list(
    labels = c("language switch", "language", "Sprachauswahl", "Sprache"),
    quota = TRUE
  ),
  "X" = list(
    labels = c("text display", "boilerplate", "Textanzeige", "Nur Text"),
    description = list(
      de = "Nur Text, keine Eingabe.",
      en = "Read-only text display."
    )
  ),
  "N" = list(
    labels = c("numerical input", "Zahleneingabe", "Numerische Eingabe"),
    description = list(de = "Numerische Eingabe.", en = "Numeric input.")
  ),

  # ── Array types ───────────────────────────────────────────────────────────
  "1" = list(
    labels = c(
      "array dual scale",
      "array (dual scale)",
      "Matrix (Zwei Skalen)",
      "Matrix duale Skala"
    ),
    requires = c("subquestions", "answerOptions")
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
    quota = TRUE
  ),
  "C" = list(
    labels = c(
      "array yes no uncertain",
      "array (yes/no/uncertain)",
      "yes/no/uncertain array",
      "Matrix (Ja/Nein/Unsicher)",
      "Ja/Nein/Unsicher-Matrix"
    ),
    requires = "subquestions"
  ),
  ";" = list(
    labels = c(
      "array text",
      "array (flexible labels) multiple texts",
      "multiple texts array",
      "Matrix (Texte)",
      "Matrix (Flexible Beschriftungen) Mehrere Texteingabefelder",
      "Text-Matrix"
    ),
    requires = c("subquestions", "answerOptions")
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
    requires = c("subquestions", "answerOptions")
  ),
  "E" = list(
    labels = c(
      "array increase same decrease",
      "array (increase/same/decrease)",
      "increase/same/decrease array",
      "Matrix (Zunahme/Gleich/Abnahme)"
    ),
    requires = "subquestions"
  ),
  "H" = list(
    labels = c(
      "array by column",
      "array (flexible labels) by column",
      "Matrix (nach Spalte)",
      "Spalten-Matrix"
    ),
    requires = c("subquestions", "answerOptions")
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

LS_Q_OPTIONS <- list(
  # ══ Options valid for ALL types ═══════════════════════════════════════════

  "mandatory" = list(
    description = list(de = "Pflichtfrage.", en = "Mandatory question."),
    default = "N",
    valid = c("Y", "N", "S") # S = soft mandatory
  ),

  "relevance" = list(
    description = list(
      de = "Anzeige-Bedingung (ExpressionScript, ohne {}). Standard: 1 = immer zeigen.",
      en = "Display condition (ExpressionScript, without {}). Default: 1 = always show."
    ),
    default = "1",
    valid = NULL # free-form ExpressionScript
  ),

  "cssclass" = list(
    description = list(
      de = "Benutzerdefinierte CSS-Klasse(n) für den Fragecontainer.",
      en = "Custom CSS class(es) for the question container."
    ),
    default = NULL,
    valid = NULL
  ),

  "hidden" = list(
    description = list(
      de = "Frage ausblenden (1 = ausblenden, 0 = anzeigen).",
      en = "Hide question from participant (1 = hide, 0 = show)."
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  "page_break" = list(
    description = list(
      de = "Seitenumbruch vor dieser Frage einfügen (nur Druckmodus; 1 = ja).",
      en = "Insert a page break before this question in print mode (1 = yes)."
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  "printable_help" = list(
    description = list(
      de = "Hilfetext für die Druckansicht.",
      en = "Help text for the print view."
    ),
    default = NULL,
    valid = NULL
  ),

  "public_statistics" = list(
    description = list(
      de = "Frage in öffentlicher Statistik anzeigen (1 = ja, 0 = nein).",
      en = "Show question in public statistics (1 = yes, 0 = no)."
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  "random_group" = list(
    description = list(
      de = "Randomisierungsgruppe: Fragen gleicher Gruppe werden zufällig sortiert.",
      en = "Randomisation group: questions in the same group are shown in random order."
    ),
    default = NULL,
    valid = NULL
  ),

  "statistics_showgraph" = list(
    description = list(
      de = "Grafik in Statistik anzeigen (1 = ja, 0 = nein).",
      en = "Show graph in statistics (1 = yes, 0 = no)."
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  "statistics_graphtype" = list(
    description = list(
      de = "Grafiktyp für die Statistikansicht (1–5). ?UNCERTAIN: weitere Werte möglich.",
      en = "Graph type for the statistics view (1–5). ?UNCERTAIN: further values possible."
    ),
    default = NULL,
    valid = 1:5
  ),

  "scale_export" = list(
    description = list(
      de = "Antwortskala exportieren (1, 2, 3). ?UNCERTAIN: genaue Bedeutung je Wert.",
      en = "Answer scale export mode (1, 2, 3). ?UNCERTAIN: exact meaning per value."
    ),
    ls_type = c(
      "!",
      "*",
      "1",
      "5",
      "A",
      "B",
      "C",
      "E",
      "F",
      "G",
      "H",
      "L",
      "M",
      "O",
      "P",
      "Y"
    ),
    default = NULL,
    valid = 1:3
  ),

  "question_theme_name" = list(
    description = list(
      de = "Name des Fragenthemas (LS 6+), z.B. 'bootstrap_buttons'.",
      en = "Question theme name (LS 6+ only), e.g. 'bootstrap_buttons'."
    ),
    default = NULL,
    valid = NULL
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
      "Y"
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Validation expressions ════════════════════════════════════════════════

  "em_validation_q" = list(
    description = list(
      de = "Frage-Validierungsgleichung (Boolean ExpressionScript).",
      en = "Question-level validation equation (Boolean ExpressionScript)."
    ),
    ls_type = c(
      "!",
      "1",
      ":",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "H",
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
      "U"
    ),
    default = NULL,
    valid = NULL
  ),

  "em_validation_q_tip" = list(
    description = list(
      de = "Hinweistext zur Frage-Validierungsgleichung (wird dem Teilnehmer angezeigt).",
      en = "Tip shown to participants describing the question validation equation."
    ),
    ls_type = c(
      "!",
      "1",
      ":",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "H",
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
      "U"
    ),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "em_validation_sq" = list(
    description = list(
      de = "Unterfragevalidierungsgleichung (Boolean ExpressionScript).",
      en = "Subquestion-level validation equation (Boolean ExpressionScript)."
    ),
    ls_type = c(":", "K", "N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "em_validation_sq_tip" = list(
    description = list(
      de = "Hinweistext zur Unterfragevalidierungsgleichung.",
      en = "Tip shown to participants describing the subquestion validation equation."
    ),
    ls_type = c(":", "K", "N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "regex_validation" = list(
    description = list(
      de = "Regulärer Ausdruck zur Antwortvalidierung.",
      en = "Regular expression for validating the response."
    ),
    ls_type = c("N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  # ══ Prefix / suffix ═══════════════════════════════════════════════════════

  "prefix" = list(
    description = list(
      de = "Text vor dem Eingabefeld (z.B. Währungszeichen).",
      en = "Text displayed before the input field (e.g. currency symbol)."
    ),
    ls_type = c("K", "N", "Q", "S"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "suffix" = list(
    description = list(
      de = "Text nach dem Eingabefeld (z.B. 'Jahre', 'kg').",
      en = "Text displayed after the input field (e.g. 'years', 'kg')."
    ),
    ls_type = c("K", "N", "Q", "S"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  # ══ Short / long / huge text (S, T, U) ═══════════════════════════════════

  "maximum_chars" = list(
    description = list(
      de = "Maximale Zeichenanzahl für die Texteingabe.",
      en = "Maximum number of characters allowed in the text input."
    ),
    ls_type = c(":", "K", "N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL # any positive integer
  ),

  "text_input_width" = list(
    description = list(
      de = "Breite des Texteingabefelds (in em).",
      en = "Width of the text input field (in em)."
    ),
    ls_type = c("K", "N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "display_rows" = list(
    description = list(
      de = "Anzahl der sichtbaren Zeilen des Textfelds.",
      en = "Number of visible rows in the text area."
    ),
    ls_type = c("Q", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "numbers_only" = list(
    description = list(
      de = "Nur Zahlen akzeptieren (1 = ja, 0 = nein).",
      en = "Accept numbers only (1 = yes, 0 = no)."
    ),
    ls_type = c("*", "Q", "S"),
    default = NULL,
    valid = c(0, 1)
  ),

  "input_size" = list(
    description = list(
      de = "Breite des Eingabefelds (in Zeichen). Für M: Breite des Sonstiges-Felds.",
      en = "Width of the input field (in characters). For M: width of the Other field."
    ),
    ls_type = c(":", "K", "M", "N", "Q", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "text_input_columns" = list(
    description = list(
      de = "Anzahl der Spalten des Textbereichs.",
      en = "Number of columns of the text area."
    ),
    ls_type = c("P", "Q"),
    default = NULL,
    valid = NULL
  ),

  "label_input_columns" = list(
    description = list(
      de = "Anzahl der Spalten für Beschriftungseingaben.",
      en = "Number of columns for label inputs."
    ),
    ls_type = c("K", "Q"),
    default = NULL,
    valid = NULL
  ),

  "location_city" = list(
    description = list(
      de = "Stadtfeld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable city field in map picker (1 = yes)."
    ),
    ls_type = "S",
    default = NULL,
    valid = c(0, 1)
  ),

  "location_state" = list(
    description = list(
      de = "Bundesland/Staat-Feld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable state field in map picker (1 = yes)."
    ),
    ls_type = "S",
    default = NULL,
    valid = c(0, 1)
  ),

  "location_country" = list(
    description = list(
      de = "Länderfeld in der Kartenauswahl aktivieren (1 = ja).",
      en = "Enable country field in map picker (1 = yes)."
    ),
    ls_type = "S",
    default = NULL,
    valid = c(0, 1)
  ),

  "location_defaultcoordinates" = list(
    description = list(
      de = "Standard-Koordinaten für die Karte (lat,lng).",
      en = "Default coordinates for the map (lat,lng)."
    ),
    ls_type = "S",
    default = NULL,
    valid = NULL
  ),

  "location_mapservice" = list(
    description = list(
      de = "Kartendienst-ID. ?UNCERTAIN: genaue Werte je Dienst.",
      en = "Map service ID. ?UNCERTAIN: exact values per service."
    ),
    ls_type = "S",
    default = NULL,
    valid = NULL
  ),

  "location_mapwidth" = list(
    description = list(
      de = "Breite der Karte (in Pixeln).",
      en = "Width of the map (in pixels)."
    ),
    ls_type = "S",
    default = NULL,
    valid = NULL
  ),

  "location_mapheight" = list(
    description = list(
      de = "Höhe der Karte (in Pixeln).",
      en = "Height of the map (in pixels)."
    ),
    ls_type = "S",
    default = NULL,
    valid = NULL
  ),

  "location_mapzoom" = list(
    description = list(
      de = "Standard-Zoom-Stufe der Karte (0–21).",
      en = "Default zoom level of the map (0–21)."
    ),
    ls_type = "S",
    default = NULL,
    valid = 0:21
  ),

  "location_nodefaultfromip" = list(
    description = list(
      de = "Keine Standard-Koordinaten aus IP-Adresse ableiten (1 = ja).",
      en = "Do not derive default coordinates from IP address (1 = yes)."
    ),
    ls_type = "S",
    default = NULL,
    valid = c(0, 1)
  ),

  "statistics_showmap" = list(
    description = list(
      de = "Karte in Statistik anzeigen (0 = nein; nur für Typ S mit Kartenaktivierung).",
      en = "Show map in statistics (0 = no; only for type S with map enabled)."
    ),
    ls_type = "S",
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Numerical input (N, K) ════════════════════════════════════════════════

  "min_num_value_n" = list(
    description = list(
      de = "Minimalwert für numerische Eingabe.",
      en = "Minimum allowed value for numeric input."
    ),
    ls_type = c("K", "N"),
    default = NULL,
    valid = NULL
  ),

  "max_num_value_n" = list(
    description = list(
      de = "Maximalwert für numerische Eingabe.",
      en = "Maximum allowed value for numeric input."
    ),
    ls_type = c("K", "N"),
    default = NULL,
    valid = NULL
  ),

  "num_value_int_only" = list(
    description = list(
      de = "Nur ganze Zahlen akzeptieren (1 = ja, 0 = nein).",
      en = "Accept integers only (1 = yes, 0 = no)."
    ),
    ls_type = c("K", "N"),
    default = NULL,
    valid = c(0, 1)
  ),

  "equals_num_value" = list(
    description = list(
      de = "Pflicht-Gesamtsumme für multiple numerische Eingabe.",
      en = "Required total sum for multiple numerical input."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "value_range_allows_missing" = list(
    description = list(
      de = "Fehlende Werte im Bereich erlauben (0 = nein, 1 = ja).",
      en = "Allow missing values within the specified range (0 = no, 1 = yes)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "min_num_value" = list(
    description = list(
      de = "Minimalwert (ältere LS3-Syntax, bevorzuge min_num_value_n).",
      en = "Minimum value (legacy LS3 syntax; prefer min_num_value_n)."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "max_num_value" = list(
    description = list(
      de = "Maximalwert (ältere LS3-Syntax, bevorzuge max_num_value_n).",
      en = "Maximum value (legacy LS3 syntax; prefer max_num_value_n)."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  # ══ Slider (K) ════════════════════════════════════════════════════════════

  "slider_min" = list(
    description = list(
      de = "Minimalwert des Sliders.",
      en = "Minimum slider value."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_max" = list(
    description = list(
      de = "Maximalwert des Sliders.",
      en = "Maximum slider value."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_accuracy" = list(
    description = list(
      de = "Schrittweite des Sliders.",
      en = "Step size of the slider."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_default" = list(
    description = list(
      de = "Standardposition des Sliders.",
      en = "Default position of the slider handle."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_default_set" = list(
    description = list(
      de = "Standard des Sliders aktivieren (1 = ja, 0 = nein).",
      en = "Activate slider default position (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "slider_middlestart" = list(
    description = list(
      de = "Slider startet in der Mitte (1 = ja, 0 = nein).",
      en = "Slider starts in the middle (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "slider_layout" = list(
    description = list(
      de = "Slider-Layout (1 = Standard). ?UNCERTAIN: weitere Werte möglich.",
      en = "Slider layout (1 = default). ?UNCERTAIN: further values possible."
    ),
    ls_type = "K",
    default = NULL,
    valid = 1:3 # ?UNCERTAIN
  ),

  "slider_handle" = list(
    description = list(
      de = "Slider-Handle-Typ (1 = Standard). ?UNCERTAIN: weitere Werte möglich.",
      en = "Slider handle type (1 = default). ?UNCERTAIN: further values possible."
    ),
    ls_type = "K",
    default = NULL,
    valid = 1:3 # ?UNCERTAIN
  ),

  "slider_custom_handle" = list(
    description = list(
      de = "Benutzerdefinierter Slider-Handle (Font Awesome Icon-Code).",
      en = "Custom slider handle (Font Awesome icon code)."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_orientation" = list(
    description = list(
      de = "Slider-Ausrichtung (1 = horizontal). ?UNCERTAIN: 2 = vertikal?",
      en = "Slider orientation (1 = horizontal). ?UNCERTAIN: 2 = vertical?"
    ),
    ls_type = "K",
    default = NULL,
    valid = c(1, 2) # ?UNCERTAIN
  ),

  "slider_reversed" = list(
    description = list(
      de = "Slider-Richtung umkehren (1 = ja, 0 = nein).",
      en = "Reverse slider direction (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "slider_reset" = list(
    description = list(
      de = "Schaltfläche zum Zurücksetzen des Sliders anzeigen (1 = ja, 0 = nein).",
      en = "Show reset button for slider (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "slider_showminmax" = list(
    description = list(
      de = "Min-/Max-Werte am Slider anzeigen (1 = ja, 0 = nein).",
      en = "Show min/max values on slider (1 = yes, 0 = no)."
    ),
    ls_type = "K",
    default = NULL,
    valid = c(0, 1)
  ),

  "slider_separator" = list(
    description = list(
      de = "Trennzeichen zwischen Slider-Werten (Freitext).",
      en = "Separator character between slider values (free text)."
    ),
    ls_type = "K",
    default = NULL,
    valid = NULL
  ),

  "slider_rating" = list(
    description = list(
      de = "Slider-Bewertungsmodus für 5-Punkte-Auswahl (1, 2). ?UNCERTAIN: genaue Werte.",
      en = "Slider rating mode for 5-point choice (1, 2). ?UNCERTAIN: exact values."
    ),
    ls_type = "5",
    default = NULL,
    valid = c(1, 2) # ?UNCERTAIN
  ),

  # ══ Date/time (D) ═════════════════════════════════════════════════════════

  "date_format" = list(
    description = list(
      de = "Datumsformat (PHP date()-Formatstring, z.B. 'Y-m-d').",
      en = "Date format (PHP date() format string, e.g. 'Y-m-d')."
    ),
    ls_type = "D",
    default = NULL,
    valid = NULL
  ),

  "date_min" = list(
    description = list(
      de = "Frühestes erlaubtes Datum (im konfigurierten Datumsformat).",
      en = "Earliest allowed date (in the configured date format)."
    ),
    ls_type = "D",
    default = NULL,
    valid = NULL
  ),

  "date_max" = list(
    description = list(
      de = "Spätestes erlaubtes Datum (im konfigurierten Datumsformat).",
      en = "Latest allowed date (in the configured date format)."
    ),
    ls_type = "D",
    default = NULL,
    valid = NULL
  ),

  "dropdown_dates" = list(
    description = list(
      de = "Datum per Dropdown-Menüs auswählen (1 = ja, 0 = nein).",
      en = "Use dropdown pickers for date input (1 = yes, 0 = no)."
    ),
    ls_type = "D",
    default = NULL,
    valid = c(0, 1)
  ),

  "dropdown_dates_minute_step" = list(
    description = list(
      de = "Schrittweite für die Minuten-Dropdown (z.B. 5, 10, 15).",
      en = "Step size for the minutes dropdown (e.g. 5, 10, 15)."
    ),
    ls_type = "D",
    default = NULL,
    valid = NULL
  ),

  "dropdown_dates_month_style" = list(
    description = list(
      de = "Monatsdarstellung im Dropdown (1 = Zahl). ?UNCERTAIN: 2 = Text?",
      en = "Month display style in dropdown (1 = number). ?UNCERTAIN: 2 = text?"
    ),
    ls_type = "D",
    default = NULL,
    valid = c(1, 2) # ?UNCERTAIN
  ),

  "reverse" = list(
    description = list(
      de = "Antwortskala umkehren (1 = ja, 0 = nein). Gilt für D und :",
      en = "Reverse the answer scale (1 = yes, 0 = no). Applies to D and :"
    ),
    ls_type = c("D", ":"),
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Yes/No (Y) ════════════════════════════════════════════════════════════

  "display_type" = list(
    description = list(
      de = "Anzeigetyp für Ja/Nein (1 = Standard). ?UNCERTAIN: weitere Werte möglich.",
      en = "Display type for Yes/No (1 = default). ?UNCERTAIN: further values possible."
    ),
    ls_type = "Y",
    default = NULL,
    valid = c(1, 2) # ?UNCERTAIN
  ),

  # ══ Equation (*) ══════════════════════════════════════════════════════════

  "equation" = list(
    description = list(
      de = "ExpressionScript-Gleichung (ohne geschweifte Klammern).",
      en = "ExpressionScript equation (without curly braces)."
    ),
    ls_type = "*",
    default = NULL,
    valid = NULL
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
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "other_comment_mandatory" = list(
    description = list(
      de = "Kommentarfeld der 'Andere'-Option als Pflichtfeld (1 = ja).",
      en = "Make the 'Other' comment field mandatory (1 = yes)."
    ),
    ls_type = c("!", "L", "P"),
    default = NULL,
    valid = c(0, 1)
  ),

  "other_numbers_only" = list(
    description = list(
      de = "Im 'Andere'-Feld nur Zahlen akzeptieren (1 = ja).",
      en = "Accept numbers only in the 'Other' field (1 = yes)."
    ),
    ls_type = c("L", "M", "P"),
    default = NULL,
    valid = c(0, 1)
  ),

  "alphasort" = list(
    description = list(
      de = "Antwortoptionen alphabetisch sortieren (1 = ja, 0 = nein).",
      en = "Sort answer options alphabetically (1 = yes, 0 = no)."
    ),
    ls_type = c("!", "L", "O"),
    default = NULL,
    valid = c(0, 1)
  ),

  "display_columns" = list(
    description = list(
      de = "Anzahl der Spalten für die Antwortoptionen.",
      en = "Number of columns used to display answer options."
    ),
    ls_type = c("L", "M"),
    default = NULL,
    valid = NULL
  ),

  "answer_width" = list(
    description = list(
      de = "Breite der Antwortoption-Beschriftung (in Prozent).",
      en = "Width of the answer option label (percentage)."
    ),
    ls_type = c("1", "A", "B", "C", "E", "F", "L", "M", "R"),
    default = NULL,
    valid = NULL
  ),

  "answer_width_bycolumn" = list(
    description = list(
      de = "Antwortbreite pro Spalte (in Prozent) für Matrix nach Spalte.",
      en = "Answer width per column (percentage) for array-by-column."
    ),
    ls_type = "H",
    default = NULL,
    valid = NULL
  ),

  "category_separator" = list(
    description = list(
      de = "Trennzeichen für Kategorien in der Klappbox.",
      en = "Category separator string for the dropdown list."
    ),
    ls_type = "!",
    default = NULL,
    valid = NULL
  ),

  "dropdown_prefix" = list(
    description = list(
      de = "Präfix-Text für Klappbox-Einträge (1 = ja). ?UNCERTAIN: Wertbedeutung.",
      en = "Prefix text for dropdown entries (1 = yes). ?UNCERTAIN: exact meaning."
    ),
    ls_type = "!",
    default = NULL,
    valid = c(0, 1) # ?UNCERTAIN
  ),

  "dropdown_size" = list(
    description = list(
      de = "Anzahl der sichtbaren Einträge in der Klappbox.",
      en = "Number of visible entries in the dropdown list."
    ),
    ls_type = "!",
    default = NULL,
    valid = NULL
  ),

  "use_dropdown" = list(
    description = list(
      de = "Antwortliste als Klappbox darstellen (1 = ja, 0 = nein).",
      en = "Display answer list as dropdown (1 = yes, 0 = no)."
    ),
    ls_type = c("1", "F", "O"),
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Array / multiple choice shared ════════════════════════════════════════

  "random_order" = list(
    description = list(
      de = "Antwortoptionen/Unterzeilen zufällig sortieren (1 = ja, 0 = nein).",
      en = "Display answer options/subquestion rows in random order (1 = yes, 0 = no)."
    ),
    ls_type = c(
      "!",
      "1",
      ":",
      "A",
      "B",
      "C",
      "E",
      "F",
      "H",
      "K",
      "L",
      "M",
      "O",
      "P",
      "Q",
      "R"
    ),
    default = NULL,
    valid = c(0, 1)
  ),

  "array_filter" = list(
    description = list(
      de = "Fragencode, dessen Antworten als Filter für Unterzeilen dienen.",
      en = "Question code whose answers filter which subquestions are shown."
    ),
    ls_type = c(
      "!",
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
    default = NULL,
    valid = NULL
  ),

  "array_filter_exclude" = list(
    description = list(
      de = "Wie array_filter, aber mit umgekehrter Logik.",
      en = "Like array_filter but inverts the filter logic."
    ),
    ls_type = c(
      "!",
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
    default = NULL,
    valid = NULL
  ),

  "array_filter_style" = list(
    description = list(
      de = "Stil der Array-Filterung (1 = ausblenden). ?UNCERTAIN: weitere Werte.",
      en = "Array filter style (1 = hide). ?UNCERTAIN: further values possible."
    ),
    ls_type = c(
      "!",
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
    default = NULL,
    valid = c(1, 2) # ?UNCERTAIN
  ),

  "exclude_all_others" = list(
    description = list(
      de = "Fragencode/Antwortcode für 'Keine der anderen' Option.",
      en = "Question/answer code for the 'None of the above' exclusive option."
    ),
    ls_type = c("1", "A", "B", "C", "E", "F", "K", "M", "P", "Q"),
    default = NULL,
    valid = NULL
  ),

  "exclude_all_others_auto" = list(
    description = list(
      de = "Exklusiv-Option automatisch abwählen wenn andere gewählt (1 = ja).",
      en = "Auto-deselect exclusive option when others are chosen (1 = yes)."
    ),
    ls_type = c("M", "P"),
    default = NULL,
    valid = c(0, 1)
  ),

  "min_answers" = list(
    description = list(
      de = "Mindestanzahl zu beantwortender Unterzeilen.",
      en = "Minimum number of subquestion rows that must be answered."
    ),
    ls_type = c("1", ":", "A", "B", "C", "E", "F", "K", "M", "P", "Q", "R"),
    default = NULL,
    valid = NULL
  ),

  "max_answers" = list(
    description = list(
      de = "Maximalanzahl zu beantwortender Unterzeilen.",
      en = "Maximum number of subquestion rows that can be answered."
    ),
    ls_type = c("1", ":", "A", "B", "C", "E", "F", "K", "M", "P", "Q", "R"),
    default = NULL,
    valid = NULL
  ),

  "repeat_headings" = list(
    description = list(
      de = "Spaltenüberschriften nach jeder N-ten Unterzeile wiederholen.",
      en = "Repeat column headings every N subquestion rows."
    ),
    ls_type = c("1", ":", "F"),
    default = NULL,
    valid = NULL
  ),

  "input_boxes" = list(
    description = list(
      de = "Eingabefelder statt Checkboxen verwenden, Array Zahlen (1 = ja).",
      en = "Use input boxes instead of checkboxes for array numbers (1 = yes)."
    ),
    ls_type = ":",
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Array dual scale (1) ══════════════════════════════════════════════════

  "dualscale_headerA" = list(
    description = list(
      de = "Überschrift für Skala A der Doppelskalen-Matrix.",
      en = "Header label for scale A of the dual-scale array."
    ),
    ls_type = "1",
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "dualscale_headerB" = list(
    description = list(
      de = "Überschrift für Skala B der Doppelskalen-Matrix.",
      en = "Header label for scale B of the dual-scale array."
    ),
    ls_type = "1",
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "dropdown_prepostfix" = list(
    description = list(
      de = "Vor-/Nachsilbe für Dropdown-Einträge der Doppelskala.",
      en = "Pre/postfix text for dual-scale dropdown entries."
    ),
    ls_type = "1",
    default = NULL,
    valid = NULL
  ),

  "dropdown_separators" = list(
    description = list(
      de = "Trennzeichen zwischen Dropdown-Einträgen der Doppelskala.",
      en = "Separator between dual-scale dropdown entries."
    ),
    ls_type = "1",
    default = NULL,
    valid = NULL
  ),

  "parent_order" = list(
    description = list(
      de = "Reihenfolge der übergeordneten Elemente (Array Zahlen). ?UNCERTAIN.",
      en = "Order of parent elements (array numbers). ?UNCERTAIN: meaning unclear."
    ),
    ls_type = ":",
    default = NULL,
    valid = NULL # ?UNCERTAIN
  ),

  # ══ Array numbers multiflexible (:) ═══════════════════════════════════════

  "multiflexible_checkbox" = list(
    description = list(
      de = "Multiflexiblen Zahlen-Array als Checkboxen darstellen (1 = ja).",
      en = "Display multi-flexible number array as checkboxes (1 = yes)."
    ),
    ls_type = ":",
    default = NULL,
    valid = c(0, 1)
  ),

  "multiflexible_min" = list(
    description = list(
      de = "Minimalwert für multiflexiblen Zahlen-Array.",
      en = "Minimum value for multi-flexible number array."
    ),
    ls_type = ":",
    default = NULL,
    valid = NULL
  ),

  "multiflexible_max" = list(
    description = list(
      de = "Maximalwert für multiflexiblen Zahlen-Array.",
      en = "Maximum value for multi-flexible number array."
    ),
    ls_type = ":",
    default = NULL,
    valid = NULL
  ),

  "multiflexible_step" = list(
    description = list(
      de = "Schrittweite für multiflexiblen Zahlen-Array.",
      en = "Step size for multi-flexible number array."
    ),
    ls_type = ":",
    default = NULL,
    valid = NULL
  ),

  # ══ Multiple choice comments (P) ══════════════════════════════════════════

  "commented_checkbox" = list(
    description = list(
      de = "Kommentiertes Checkbox-Verhalten ('always', 'check'). ?UNCERTAIN: alle Werte.",
      en = "Commented checkbox behaviour ('always', 'check'). ?UNCERTAIN: all values."
    ),
    ls_type = "P",
    default = NULL,
    valid = c("always", "check") # ?UNCERTAIN
  ),

  "commented_checkbox_auto" = list(
    description = list(
      de = "Kommentarfeld automatisch beim Ankreuzen anzeigen (1 = ja, 0 = nein).",
      en = "Auto-show comment field when checkbox is checked (1 = yes, 0 = no)."
    ),
    ls_type = "P",
    default = NULL,
    valid = c(0, 1)
  ),

  "choice_input_columns" = list(
    description = list(
      de = "Anzahl der Spalten für Checkboxen (Mehrfachauswahl mit Kommentar).",
      en = "Number of columns for checkboxes (multiple choice with comments)."
    ),
    ls_type = "P",
    default = NULL,
    valid = NULL
  ),

  # ══ Ranking (R) ═══════════════════════════════════════════════════════════

  "max_subquestions" = list(
    description = list(
      de = "Maximale Anzahl an Rangplätzen (Ranking).",
      en = "Maximum number of ranking positions to show."
    ),
    ls_type = "R",
    default = NULL,
    valid = NULL
  ),

  "rank_title" = list(
    description = list(
      de = "Beschriftung der Rang-Spaltenüberschrift.",
      en = "Label for the rank column header."
    ),
    ls_type = "R",
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "choice_title" = list(
    description = list(
      de = "Beschriftung der Auswahlspalten-Überschrift.",
      en = "Label for the choice column header."
    ),
    ls_type = "R",
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "samechoiceheight" = list(
    description = list(
      de = "Gleiche Höhe für alle Auswahlfelder erzwingen (0 = nein, 1 = ja).",
      en = "Force equal height for all choice items (0 = no, 1 = yes)."
    ),
    ls_type = "R",
    default = NULL,
    valid = c(0, 1)
  ),

  "samelistheight" = list(
    description = list(
      de = "Gleiche Höhe für die Antwortliste erzwingen (0 = nein, 1 = ja).",
      en = "Force equal height for the answer list (0 = no, 1 = yes)."
    ),
    ls_type = "R",
    default = NULL,
    valid = c(0, 1)
  ),

  "showpopups" = list(
    description = list(
      de = "Popup-Hinweise beim Ranking anzeigen (0 = nein, 1 = ja). ?UNCERTAIN.",
      en = "Show popup hints during ranking (0 = no, 1 = yes). ?UNCERTAIN."
    ),
    ls_type = "R",
    default = NULL,
    valid = c(0, 1)
  ),

  # ══ Multiple choice assessment (M, P) ════════════════════════════════════

  "assessment_value" = list(
    description = list(
      de = "Bewertungswert (Assessment) für die gesamte Frage.",
      en = "Assessment value for the entire question."
    ),
    ls_type = c("M", "P"),
    default = NULL,
    valid = NULL
  ),

  # ══ Time limit (!, L, O, Q, R, S, T, U) ══════════════════════════════════

  "time_limit" = list(
    description = list(
      de = "Zeitlimit für die Frage (in Sekunden).",
      en = "Time limit for the question (in seconds)."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_action" = list(
    description = list(
      de = "Aktion bei Zeitablauf: 1 = nichts, 2 = Warnung und weiter, 3 = automatisch weiter.",
      en = "Action on time expiry: 1 = nothing, 2 = warn and move, 3 = auto-move."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = 1:3
  ),

  "time_limit_disable_next" = list(
    description = list(
      de = "'Weiter'-Schaltfläche während Zeitlimit deaktivieren (1 = ja).",
      en = "Disable 'Next' button during time limit countdown (1 = yes)."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = c(0, 1)
  ),

  "time_limit_disable_prev" = list(
    description = list(
      de = "'Zurück'-Schaltfläche während Zeitlimit deaktivieren (1 = ja).",
      en = "Disable 'Back' button during time limit countdown (1 = yes)."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = c(0, 1)
  ),

  "time_limit_countdown_message" = list(
    description = list(
      de = "Nachricht während des Countdowns.",
      en = "Message displayed during the countdown."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "time_limit_timer_style" = list(
    description = list(
      de = "CSS-Stil für den Timer-Text.",
      en = "CSS style string for the timer text."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_message_delay" = list(
    description = list(
      de = "Verzögerung (in Sekunden) bevor die Ablaufnachricht angezeigt wird.",
      en = "Delay (in seconds) before the expiry message is shown."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_message" = list(
    description = list(
      de = "Nachricht, die nach Ablauf des Zeitlimits angezeigt wird.",
      en = "Message displayed after the time limit expires."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "time_limit_message_style" = list(
    description = list(
      de = "CSS-Stil für die Ablaufnachricht.",
      en = "CSS style string for the expiry message."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning" = list(
    description = list(
      de = "Zeitpunkt (Sekunden vor Ablauf) für erste Vorwarnmeldung.",
      en = "Time (seconds before expiry) at which the first warning is shown."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning_display_time" = list(
    description = list(
      de = "Anzeigedauer der ersten Vorwarnung (in Sekunden).",
      en = "Display duration of the first warning (in seconds)."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning_message" = list(
    description = list(
      de = "Text der ersten Vorwarnmeldung.",
      en = "Text of the first time limit warning."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "time_limit_warning_style" = list(
    description = list(
      de = "CSS-Stil für die erste Vorwarnmeldung.",
      en = "CSS style string for the first warning."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning_2" = list(
    description = list(
      de = "Zeitpunkt (Sekunden vor Ablauf) für zweite Vorwarnmeldung.",
      en = "Time (seconds before expiry) at which the second warning is shown."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning_2_display_time" = list(
    description = list(
      de = "Anzeigedauer der zweiten Vorwarnung (in Sekunden).",
      en = "Display duration of the second warning (in seconds)."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  ),

  "time_limit_warning_2_message" = list(
    description = list(
      de = "Text der zweiten Vorwarnmeldung.",
      en = "Text of the second time limit warning."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL,
    language = TRUE
  ),

  "time_limit_warning_2_style" = list(
    description = list(
      de = "CSS-Stil für die zweite Vorwarnmeldung.",
      en = "CSS style string for the second warning."
    ),
    ls_type = c("!", "L", "O", "Q", "R", "S", "T", "U"),
    default = NULL,
    valid = NULL
  )
)


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

SETTINGS_DEFAULTS <- list(
  sid = 1,
  gsid = 1,
  admin = "",
  adminemail = "",
  anonymized = "Y",
  faxto = "",
  format = "G",
  savetimings = "Y",
  template = "vanilla",
  language = "",
  additional_languages = "",
  datestamp = "Y",
  usecookie = "N",
  allowregister = "N",
  allowsave = "N",
  autonumber_start = 0,
  autoredirect = "Y",
  allowprev = "N",
  printanswers = "N",
  ipaddr = "N",
  refurl = "N",
  showsurveypolicynotice = 0,
  publicstatistics = "N",
  publicgraphs = "N",
  listpublic = "N",
  htmlemail = "Y",
  sendconfirmation = "N",
  tokenanswerspersistence = "N",
  assessments = "N",
  usecaptcha = "N",
  usetokens = "N",
  bounce_email = "",
  emailresponseto = "",
  emailnotificationto = "",
  tokenlength = 15,
  showxquestions = "N",
  showgroupinfo = "X",
  shownoanswer = "N",
  showqnumcode = "X",
  bounceprocessing = "N",
  showwelcome = "Y",
  showprogress = "Y",
  questionindex = 0,
  navigationdelay = 0,
  nokeyboard = "N",
  alloweditaftercompletion = "N",
  googleanalyticsstyle = 0,
  googleanalyticsapikey = ""
)

SETTINGS_VALID <- list(
  format = c("G", "Q", "A"),
  showgroupinfo = c("B", "D", "N", "X"),
  showqnumcode = c("Y", "N", "X"),
  questionindex = c(0, 1, 2)
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
    default = 1,
    valid = c(1, 2)
  ),
  "active" = list(
    description = list(
      de = "Quota aktiv (1 = ja, 0 = nein).",
      en = "Quota active (1 = yes, 0 = no)."
    ),
    default = 1,
    valid = c(0, 1)
  ),
  "autoloadURL" = list(
    description = list(
      de = "URL automatisch laden wenn Quota greift (1 = ja, 0 = nein).",
      en = "Autoload the redirect URL when quota triggers (1 = yes, 0 = no)."
    ),
    default = 0,
    valid = c(0, 1)
  ),
  "messageTexts" = list(
    description = list(
      de = "Nachricht bei Quoten-Erfüllung (mehrsprachig).",
      en = "Message displayed when quota is triggered (multilingual)."
    ),
    valid = NULL,
    language = TRUE
  ),
  "urls" = list(
    description = list(
      de = "Weiterleitungs-URL (mehrsprachig).",
      en = "Redirect URL when quota triggers (multilingual)."
    ),
    valid = NULL,
    language = TRUE
  ),
  "urlDescriptions" = list(
    description = list(
      de = "Beschreibung der Weiterleitungs-URL (mehrsprachig).",
      en = "Description of the redirect URL (multilingual)."
    ),
    valid = NULL,
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
