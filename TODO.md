# TODO

- exclude `TODO.md` from `pkgdown`
    - see <https://github.com/r-lib/pkgdown/issues/2959>

## Vignettes

- update arXiv paper for softmax
    [after JOSS paper]

## Add `inst/CITATION`
- Given the arXiv preprint reference being added in item 6, also add `inst/CITATION` so `citation("tmfast")` returns the methods paper.
    [We'll use the JOSS paper once it's accepted]



## JOSS Paper

Submission target: Journal of Open Source Software (JOSS)
Paper format spec: <https://joss.readthedocs.io/en/latest/paper.html>
Submission checklist: <https://joss.readthedocs.io/en/latest/submitting.html>
Word limit: 750–1,750 words. Keep API/implementation details out of the paper (they belong in package docs).
Key sources: `vignettes/` (authoritative for current softmax behavior), `arxiv/paper.qmd` (background, comparisons — but outdated on tidying), `arxiv/tmfast.yaml` (bibliography source)

To build: `cd "/Users/danhicks/Google Drive/Coding/inara" && make ARTICLE="../tmfast/joss/paper.md"`

- [x] Set up paper infrastructure: create `joss/paper.md` with YAML frontmatter (title, authors with ORCID 0000-0001-7945-4416, affiliation, date, bibliography field pointing to `paper.bib`) and `joss/paper.bib` (port from `arxiv/tmfast.yaml`). See paper.html for required frontmatter fields.
- [x] Write **Summary** section (~150 words): high-level description of tmfast for a non-specialist audience — what it does (fast topic modeling via varimax-rotated PCA), key features (speed, determinism, vocabulary selection, tidy tidiers). Draw from `DESCRIPTION` and `README.md`.
- [x] Write **Statement of Need** section: describe the research purpose and target audience (computational social scientists, digital humanists, text-mining researchers); explain problems solved (speed vs. stm/LDA, determinism, no Bayesian degrees of freedom). Draw from `arxiv/paper.qmd` Introduction.
- [x] Write **State of the Field** section: compare tmfast to `stm`, `topicmodels`, and other LDA-based packages; cite Rohe & Zheng (2023, arXiv:2004.05387) as the mathematical basis; justify the varimax/PCA approach. Draw from `arxiv/paper.qmd` Introduction and Mathematical Background.
- [x] Write **Software Design** section: describe the pipeline (irlba truncated PCA → varimax rotation → softmax tidiers), information-theoretic vocabulary selection (ndH/ndR), broom-compatible tidiers, simulation samplers, and key design trade-offs. Draw from vignettes (NOT `arxiv/paper.qmd` — preprint predates the softmax change).
- [x] Write **Research Impact Statement** section: note arXiv preprint (arXiv:2305.01535), CRAN availability, and any downstream citations or external use. Check Google Scholar for citations of the arXiv preprint.
- [x] Write **AI Usage Disclosure** section (required by JOSS): declare whether generative AI was used in authoring this paper or the package documentation; if so, describe tools and extent of use.
- [x] Review and trim: check total word count is within 750–1,750 words; remove any content that duplicates package documentation (function signatures, argument lists, etc.)
- [x] Validate rendering: compile `joss/paper.md` with pandoc or the JOSS Docker image to confirm bibliography and formatting render correctly
