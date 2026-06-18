#!/usr/bin/env python3
"""
Scrape state vaccine requirements by setting from Immunize.org and write tidy CSVs.

Source index : https://www.immunize.org/official-guidance/state-policies/requirements/
Each vaccine has a "table and map" page whose HTML table carries the full
state-by-state requirement data (no PDF parsing needed). This script fetches
every vaccine x setting page for the given year and emits:

  data-raw/csv/vaccine_requirements_by_setting.csv   (wide: one row per jurisdiction x vaccine x setting)
  data-raw/csv/vaccine_requirements_long.csv         (long: one row per field)
  data/csv/vaccine_requirements_by_setting.csv        (curated copy for the app)

Refresh cadence: Immunize.org updates these pages annually (look for the year in
the slug, e.g. "-2025"). To refresh, bump YEAR below and re-run, then eyeball the
printed validation summary before committing.

Usage:
    pip install requests beautifulsoup4
    python data-raw/scrape_immunize_requirements.py            # uses repo-relative paths
    python data-raw/scrape_immunize_requirements.py --year 2026

NOTE: parsing relies on the live HTML table's colspan/rowspan headers, which is
more robust than the rendered-markdown alignment. If Immunize.org restructures
the tables, re-check the FIELD_CANON / setting mapping below.
"""
from __future__ import annotations
import argparse, csv, os, re, sys
from pathlib import Path

import requests
from bs4 import BeautifulSoup

YEAR = 2025
BASE = "https://www.immunize.org/official-guidance/state-policies/vaccine-requirements"

# slug stem (without -YEAR) -> display vaccine name
PAGES = {
    "covid-child-school": "COVID-19",
    "dtap-child-school": "DTaP",
    "hepa-child-school": "Hepatitis A",
    "hepb-child-school": "Hepatitis B",
    "hepb-college": "Hepatitis B",
    "hib-childcare": "Hib",
    "hpv-secondary": "HPV",
    "influenza-childcare": "Influenza",
    "menacwy-school": "MenACWY",
    "menacwy-college": "MenACWY",
    "mmr-child-school": "MMR",
    "pcv-childcare": "PCV",
    "polio-child-school": "Polio",
    "rotavirus-childcare": "Rotavirus",
    "tdap-school": "Tdap",
    "varicella-child-school": "Varicella",
}
# pages with no CHILDCARE/SCHOOL/COLLEGE group-header row -> single implied setting
SINGLE_SETTING = {
    "menacwy-school": "School",
    "menacwy-college": "College",
    "hepb-college": "College",
}
SET_NORM = {"CHILDCARE": "Childcare", "SCHOOL": "School", "COLLEGE": "College"}

FIELD_CANON = {
    "requirement?": "required", "required?": "required",
    "number of doses required": "doses_required",
    "grade(s) included": "grades_included",
    "requirement by age or grade(s)": "age_or_grade",
    "children subject to requirement": "population_subject",
    "students subject to requirement": "population_subject",
    "type of institution": "type_of_institution",
    "year first implemented": "year_first_implemented",
    "year implemented": "year_first_implemented",
    "historical note": "historical_note",
    "comment": "comment",
    "requirement for 1 dose": "note_1dose",
    "requirement for dose at age 16 years or older": "note_dose_age16",
}
WIDE_COLS = ["jurisdiction", "vaccine", "setting", "required", "required_bool",
             "doses_required", "grades_included", "age_or_grade", "population_subject",
             "type_of_institution", "year_first_implemented", "historical_note",
             "comment", "note_1dose", "note_dose_age16", "source_year", "source_url"]

JURIS = {"Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
"District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
"Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
"Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
"North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
"South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
"West Virginia","Wisconsin","Wyoming"}
EXTRA = {"New York City", "Puerto Rico", "Guam", "U.S. Virgin Islands"}
ALLJ = JURIS | EXTRA
REQ = {"requirement?", "required?"}


def clean(x: str) -> str:
    return re.sub(r"\s+", " ", (x or "").replace("\xa0", " ")).strip()


def expand_row(tr):
    """Return a flat list of cell texts, expanding colspan (rowspan ignored: these
    tables only span the group-header row horizontally)."""
    out = []
    for cell in tr.find_all(["th", "td"]):
        txt = clean(cell.get_text(" ", strip=True))
        span = int(cell.get("colspan", 1))
        out.extend([txt] * span if span > 1 else [txt])
    return out


def parse_table(table, slug, vaccine, url):
    rows = [tr for tr in table.find_all("tr")]
    grid = [expand_row(tr) for tr in rows]
    grid = [g for g in grid if any(c for c in g)]
    # locate header row (contains 'Jurisdiction')
    h = next(i for i, g in enumerate(grid) if any("jurisdiction" in c.lower() for c in g))
    group = grid[h]
    # is the next row a sub-header (no jurisdiction in col0, has a Requirement field)?
    sub = grid[h + 1] if h + 1 < len(grid) else []
    has_sub = sub and clean(sub[0]) not in ALLJ and any(c.lower() in REQ for c in sub)

    colmap = []  # (setting, canonical_field) per data column index >=1
    if has_sub:
        settings = [c for c in group[1:] if c]
        fields = sub[1:] if len(sub) == len(group) else sub  # align to data cols
        si = -1
        for f in fields:
            if f.lower() in REQ:
                si += 1
            setting = settings[si] if 0 <= si < len(settings) else (settings[-1] if settings else "")
            colmap.append((SET_NORM.get(setting, setting), FIELD_CANON.get(f.lower())))
        data_start = h + 2
    else:
        default = SINGLE_SETTING.get(slug, "")
        for f in group[1:]:
            m = re.match(r"(CHILDCARE|SCHOOL|COLLEGE)\s+(.+)", f, re.I)
            if m:
                colmap.append((SET_NORM.get(m.group(1).upper(), m.group(1)), FIELD_CANON.get(m.group(2).lower())))
            else:
                colmap.append((default, FIELD_CANON.get(f.lower())))
        data_start = h + 1

    records = []
    for g in grid[data_start:]:
        juris = clean(g[0])
        if not juris or juris.lower() in REQ or juris == "Jurisdiction":
            continue
        vals = g[1:]
        off = len(colmap) - len(vals)
        per = {}
        for k, (setting, canon) in enumerate(colmap):
            vi = k - off
            val = clean(vals[vi]) if 0 <= vi < len(vals) else ""
            per.setdefault(setting, {})
            if canon:
                per[setting][canon] = val
        for setting, fields in per.items():
            r = {"jurisdiction": juris, "vaccine": vaccine, "setting": setting,
                 "source_year": YEAR, "source_url": url, **fields}
            rb = str(fields.get("required", "")).strip().lower()
            r["required_bool"] = True if rb == "yes" else (False if rb in ("", "no") else "")
            records.append(r)
    return records


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--year", type=int, default=YEAR)
    ap.add_argument("--repo", default=str(Path(__file__).resolve().parents[1]))
    args = ap.parse_args()
    global YEAR
    YEAR = args.year

    repo = Path(args.repo)
    session = requests.Session()
    session.headers["User-Agent"] = "Mozilla/5.0 (vax-impact-map data refresh)"

    allrows, summary = [], []
    for stem, vaccine in PAGES.items():
        slug = f"{stem}-{YEAR}"
        url = f"{BASE}/{slug}/"
        resp = session.get(url, timeout=30)
        resp.raise_for_status()
        soup = BeautifulSoup(resp.text, "html.parser")
        table = next((t for t in soup.find_all("table")
                      if "jurisdiction" in t.get_text(" ", strip=True).lower()
                      and "alabama" in t.get_text(" ", strip=True).lower()), None)
        if table is None:
            print(f"!! {slug}: no requirement table found", file=sys.stderr)
            continue
        recs = parse_table(table, stem, vaccine, url)
        allrows.extend(recs)
        nj = len({r["jurisdiction"] for r in recs})
        nreq = sum(1 for r in recs if r["required_bool"] is True)
        summary.append((slug, nj, len(recs), nreq))

    (repo / "data-raw/csv").mkdir(parents=True, exist_ok=True)
    (repo / "data/csv").mkdir(parents=True, exist_ok=True)

    wide_paths = [repo / "data-raw/csv/vaccine_requirements_by_setting.csv",
                  repo / "data/csv/vaccine_requirements_by_setting.csv"]
    for p in wide_paths:
        with open(p, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=WIDE_COLS)
            w.writeheader()
            for r in allrows:
                w.writerow({k: r.get(k, "") for k in WIDE_COLS})

    with open(repo / "data-raw/csv/vaccine_requirements_long.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["jurisdiction", "vaccine", "setting", "field", "value", "source_year", "source_url"])
        skip = {"jurisdiction", "vaccine", "setting", "source_year", "source_url", "required_bool"}
        for r in allrows:
            for k in WIDE_COLS:
                if k in skip:
                    continue
                v = r.get(k, "")
                if v != "":
                    w.writerow([r["jurisdiction"], r["vaccine"], r["setting"], k, v, r["source_year"], r["source_url"]])

    print(f"\nValidation summary ({YEAR}):")
    print(f"{'slug':30} {'#juris':>6} {'#rows':>6} {'#req=yes':>8}")
    for s in summary:
        print(f"{s[0]:30} {s[1]:>6} {s[2]:>6} {s[3]:>8}")
    print(f"\nWrote {len(allrows)} rows across {len({r['vaccine'] for r in allrows})} vaccines.")


if __name__ == "__main__":
    main()
