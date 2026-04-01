# LimeSeed

**Author your [LimeSurvey](https://www.limesurvey.org) questionnaires in YAML — export to TSV in one call.**

LimeSeed is an R package that allows you to define surveys as human-readable YAML files and compile them into TSV format for import into [LimeSurvey](https://www.limesurvey.org).

Avoid tedious and error-prone manual clicking through the web interface. Edit your survey in your preferred coding environment, collaborate with colleagues, and share and publish it seamlessly.

--- 

## Pipeline

LimeSeed follows a simple, three-stage pipeline for turning survey definitions into LimeSurvey import files:

1. **Load & Validate** – validate_seed() checks and normalizes the input (internally using load_seed() to support multiple input formats such as YAML files, folders, or R lists).
2. **Build** – build_lsdf() compiles the validated seed into a structured LimeSurvey data frame.
3. **Write** – write_lsdf() formats and exports the result as a .tsv file ready for import.

The convenience wrapper seed_to_tsv() runs this full pipeline in a single call.
For more control, each stage can also be used independently to inspect, modify, or debug the survey before export.

```mermaid
flowchart LR

    %% ── Inputs ─────────────────────────────
    A(["YAML\nfile(s)/path"])
    B(["R List"])

    A --> C
    B --> C

    %% ── Main wrapper ───────────────────────
    subgraph C["seed_to_tsv()"]
        direction TB

        D["validate_seed()\n ↪ load_seed()"]
        E["build_lsdf()"]
        F["write_lsdf()"]

        D --> E --> F
    end

    %% ── Output ─────────────────────────────
    G[[survey.tsv]]
    H[(LimeSurvey Import)]

    C --> G --> H

    %% ── Styling ────────────────────────────
    classDef input fill:#e8f5e9,stroke:#2e7d32,stroke-width:2px;
    classDef wrapper fill:#e3f2fd,stroke:#1565c0,stroke-width:2px;
    classDef validate fill:#fff3e0,stroke:#ef6c00,stroke-width:2px;
    classDef build fill:#f3e5f5,stroke:#6a1b9a,stroke-width:2px;
    classDef write fill:#e0f2f1,stroke:#00695c,stroke-width:2px;
    classDef output fill:#fffde7,stroke:#f9a825,stroke-width:2px;
    classDef external fill:#eeeeee,stroke:#616161,stroke-width:2px;

    class A,B input;
    class C wrapper;
    class D validate;
    class E build;
    class F write;
    class G output;
    class H external;
```

---

## Quick Start


### Installation

```r
# Install from GitHub
devtools::install_github("https://github.com/StefanMunnes/LimeSeed")

library(LimeSeed)
```

### One-liner — seed file directly to TSV

```r
seed_to_tsv("my_survey.yaml", "output/my_survey.tsv")
```

### Step-by-step — modify before exporting

```r
# Stage 1 — load and normalise
seed <- load_seed("my_survey.yaml")

# Stage 2 — manipulate
seed$settings$anonymized <- "N"
seed$structure$Demographics$Age$mandatory <- "Y"

# Stage 3 — compile and export
seed_to_tsv(seed, "output/my_survey.tsv")
```

### Programmatic seed — no YAML file needed

```r
seed <- list(
  settings = list(
    language = "en",
    titles   = "My Programmatic Survey"
  ),
  structure = list(
    G1 = list(
      Q1 = list(
        type          = "short text",
        questionTexts = "What is your name?"
      )
    )
  )
)

seed_to_tsv(seed, "output/my_survey.tsv")
```

---

## Survey YAML Format

A LimeSeed YAML file has up to three top-level keys:

```yaml
settings:   # Survey-level settings (required)
structure:  # Groups → questions (required)
quota:      # Quota definitions (optional)
```

*Tip*: When working with larger surveys create distinct `settings.yml`, `structure.yml`, and `quota.yml` files and point a folder path at them as seed input.

### Minimal example

```yaml
settings:
  language: 'en'
  titles: 'Customer Satisfaction Survey'

structure:
  Demographics:
    Age:
      type: 'numerical input'
      questionTexts: 'How old are you?'
      mandatory: 'Y'
      min_num_value_n: 18
      max_num_value_n: 99

    Gender:
      type: 'radio'
      questionTexts: 'How do you identify?'
      answerOptions:
        M: 'Male'
        F: 'Female'
        D: 'Diverse'
        N: 'Prefer not to say'

  Satisfaction:
    OverallRating:
      type: 'array'
      questionTexts: 'Rate the following aspects:'
      subquestions:
        SQ1: 'Product quality'
        SQ2: 'Customer service'
        SQ3: 'Value for money'
      answerOptions:
        A1: 'Very poor'
        A2: 'Poor'
        A3: 'Neutral'
        A4: 'Good'
        A5: 'Excellent'

    Comments:
      type: 'long text'
      questionTexts: 'Any further comments?'
      relevance: "OverallRating_SQ1 <= 2 or OverallRating_SQ2 <= 2"
```

### Multi-language example

```yaml
settings:
  language: 'en'
  additional_languages: 'de'
  titles:
    en: 'Customer Survey'
    de: 'Kundenbefragung'
  welcomeTexts:
    en: 'Thank you for your participation!'
    de: 'Vielen Dank für Ihre Teilnahme!'

structure:
  G1:
    Q1:
      type: 'radio'
      questionTexts:
        en: 'What is your age group?'
        de: 'Welcher Altersgruppe gehören Sie an?'
      answerOptions:
        A1:
          optionTexts:
            en: 'Under 25'
            de: 'Unter 25'
        A2:
          optionTexts:
            en: '25–44'
            de: '25–44'
        A3:
          optionTexts:
            en: '45 and over'
            de: '45 und älter'
```

### Quota Definitions

Quotas are defined as a top-level `quota:` key in your YAML or seed.

```yaml
quota:
 quota_female:
   limit: 150
   members:
     Gender:
       - 'F'
   action: 1
   active: 1
   messageTexts:
     en: 'Thank you — we have reached our quota for this group.'
     de: 'Vielen Dank — wir haben unser Kontingent für diese Gruppe erreicht.'
   urls:
     en: 'https://example.com/closed'
     de: 'https://example.com/geschlossen'
```

