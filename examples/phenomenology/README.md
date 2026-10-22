## Example: compassion study (42 interviews)

This is the dataset used as the reference example in the article. Each interview was analyzed independently by two of three researchers (Coder 1, Coder 2, Coder 3).

| File | Content |
|---|---|
| `codebook.xlsx` | Working structure (`Código \| Comentario`). The comment gives the role of each code in the hierarchy. |
| `assignment.xlsx` | Interviews and dyads (`Entrevista \| Dupla`). |
| `Coder1.xlsx`, `Coder2.xlsx`, `Coder3.xlsx` | ATLAS.ti exports of each researcher: one row per quotation and one 0/1 column per code. |
| `phenomenology_protocol.json` | Protocol ready to use. |

### Test the app

* **Tab 1 (Protocol).** Upload `codebook.xlsx` and `assignment.xlsx`, or load `phenomenology_protocol.json`. Select the temporal phases 1.1, 1.2, 1.3.1, 1.3.2 and 1.3.3 (1.0 Preparation and 1.4 Post-Interaction are not analyzed), and set family *0* as the experiential structures.
* **Tab 2 (Report).** Upload the protocol and the three `Coder*.xlsx` files, then click *Generate report*.

Expected results, with all interviews pooled: temporal phases, 210 units and Fleiss' κ = .91; synchronic categories, 6,370 units and κ = .88; experiential structures, 42 interviews and κ = 1.00.

### Notes

* The code labels of the three exports were harmonized to the English labels of the codebook. Some researchers had labeled the codes in Spanish, and two codes had numbering slips (2.1.1/2.1.2). The coding values are unchanged.
* The dyads are defined by export file. The codings of interviews S30–S43 are stored in the files of Coder 1 and Coder 2.
