# schema-protocol

CLOS **interchange schema** protocol for [cl-stack](https://github.com/egao1980/cl-stack) — models, validation, nested schemas, computed fields.

JSON Schema parse/emit lives in [`schema-protocol-json`](https://github.com/egao1980/schema-protocol-json) (`stack-schema-json`). Same pattern later: `schema-protocol-xsd`, …

| System | Role | OCI |
|--------|------|-----|
| `schema-protocol` (`stack-schema`) | Metaclass + `defschema` + parse/validate/dump | **0.1.0** |

Wire codecs stay in [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) / [`json-protocol`](https://github.com/egao1980/json-protocol). This package owns **shape + constraints**, not bytes.

**Not** [`sql-orm`](https://github.com/egao1980/sql-orm) `defmodel` (persistence). Use `defschema` for API / config / RPC payloads.

```lisp
(asdf:load-system "schema-protocol")

(stack-schema:defschema address ()
  (city string)
  (country string :default "GB"))

(stack-schema:defschema user ()
  "API user."
  (name string :min-length 1)
  (age (integer 0) :optional t)
  (email string :format :email :optional t)
  (address address)
  (tags (vector string))
  (:compute display-name (self)
    (format nil "~A" (slot-value self 'name)))
  (:extra :forbid))

(let ((u (stack-schema:parse 'user '(:name "Ada"
                                     :address (:city "London")
                                     :tags #("lisp" "clos")))))
  (stack-schema:dump u))
```

## Prior art — take / leave

Pydantic is the *feature checklist* (nested models, validators, computed fields, JSON Schema). It is **not** the API.

| Source | Take | Leave |
|--------|------|-------|
| **CLOS / MOP** | `schema-class` metaclass; slot options; GF hooks; `validate-object :after` | God `BaseModel` |
| **CL type specifiers** | `string`, `(integer 0 120)`, `(or :null string)`, `(member :a :b)`, `(vector user)` | Python `Annotated[]` / `Field()` |
| **sql-orm `defmodel`** | defclass-shaped `defschema`, `:compute` as methods | Tables / PK / relations |
| **sanity-clause** | Metaclass + Lisp `:type`; load from alist | Parallel `integer-field` zoo; JSON-library coupling |
| **json-mop / json-clos** | Slot metadata; extras bag | Hard yason; `:json-key` as the model |
| **fisxoj/json-schema** | — | Validator for foreign docs. See `schema-protocol-json` |
| **Pydantic** | Nested / computed / extra policy / `loc` paths | Implicit coercion default; `model_validate`; decorators; `model_config` |
| **cl-stack-config** | Env/TOML settings | Typed interchange models (this package) |

**JSON null** is `:null` (same as `json-protocol`). Lisp `nil` is JSON **false** (`boolean`). Do not write CL `null` and expect “missing key”.

**Coercion is opt-in** (`:coerce t`) plus `coerce-field` methods. Restarts: `use-value`, `skip-field`, `use-default` around each field.

**Pattern** is a function designator, not a regex string.

## Protocol

```lisp
(defgeneric schema-of (designator))
(defgeneric validate (schema value &key coerce extra format))
(defgeneric parse (schema source &key coerce extra format))
(defgeneric dump (object &key as include-computed format))
(defgeneric json-schema (schema &key draft))   ; method in schema-protocol-json
(defgeneric validate-object (object))          ; :after methods
(defgeneric validate-field (schema-name slot-name value))
(defgeneric coerce-field (schema-name slot-name value))
```

`:format` decodes/encodes via `serdes-protocol` when that system is loaded. Default `dump` / `parse` speak hash-tables, plists, alists.

## Non-goals (0.1.0)

- JSON Schema parse / generate (→ `schema-protocol-json`; later `schema-protocol-xsd`, …)
- Discriminated unions
- Settings/env overlay (stay on `cl-stack-config`)
- ORM / persistence
- Aggressive implicit coercion

## License

MIT — see [LICENSE](LICENSE).
