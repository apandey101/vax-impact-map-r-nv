#!/usr/bin/env python3
"""
Build per-state deep links into the National Network of Immunization Coalitions
member directory, for the VaxImpactMap state-profile "Find a local coalition" button.

How the deep link works (reverse-engineered from immunizationcoalitions.org):
    https://www.immunizationcoalitions.org/network-members/?listing=1
        &country=usa
        &states=<token>      # base64( JSON array of lowercase state abbrs ), e.g. ["ca"] -> WyJjYSJd
        &focus=W10=          # base64( [] ) = no focus-area filter

So the only per-state piece is `states`: base64.b64encode('["ca"]') == 'WyJjYSJd'.

has_coalition:
    The directory's own state filter only lists states that have >=1 member coalition.
    As verified on 2026-06-18, every US state/DC is present EXCEPT the seven below, which
    return "No Results Found". Re-verify this set on refresh (it changes as coalitions
    join/leave) by checking the "Filter by state(s)" list on the directory page.

Usage:
    python data-raw/build_coalition_links.py
Outputs:
    data-raw/csv/immunization_coalition_links.csv
    data/csv/immunization_coalition_links.csv
"""
import base64
import csv
import json
from pathlib import Path

STATES = {
    "Alabama": "AL", "Alaska": "AK", "Arizona": "AZ", "Arkansas": "AR", "California": "CA",
    "Colorado": "CO", "Connecticut": "CT", "Delaware": "DE", "District of Columbia": "DC",
    "Florida": "FL", "Georgia": "GA", "Hawaii": "HI", "Idaho": "ID", "Illinois": "IL",
    "Indiana": "IN", "Iowa": "IA", "Kansas": "KS", "Kentucky": "KY", "Louisiana": "LA",
    "Maine": "ME", "Maryland": "MD", "Massachusetts": "MA", "Michigan": "MI", "Minnesota": "MN",
    "Mississippi": "MS", "Missouri": "MO", "Montana": "MT", "Nebraska": "NE", "Nevada": "NV",
    "New Hampshire": "NH", "New Jersey": "NJ", "New Mexico": "NM", "New York": "NY",
    "North Carolina": "NC", "North Dakota": "ND", "Ohio": "OH", "Oklahoma": "OK", "Oregon": "OR",
    "Pennsylvania": "PA", "Rhode Island": "RI", "South Carolina": "SC", "South Dakota": "SD",
    "Tennessee": "TN", "Texas": "TX", "Utah": "UT", "Vermont": "VT", "Virginia": "VA",
    "Washington": "WA", "West Virginia": "WV", "Wisconsin": "WI", "Wyoming": "WY",
}

# US states/DC with NO coalition in the directory (verified 2026-06-18). Re-check on refresh.
NO_COALITION = {"AK", "GA", "MS", "ND", "RI", "VT", "WY"}


def coalition_url(abbr: str) -> str:
    token = base64.b64encode(json.dumps([abbr.lower()], separators=(",", ":")).encode()).decode()
    return ("https://www.immunizationcoalitions.org/network-members/"
            f"?listing=1&country=usa&states={token}&focus=W10=")


def main():
    repo = Path(__file__).resolve().parents[1]
    rows = [{
        "state_name": name,
        "state_abbr": abbr,
        "has_coalition": str(abbr not in NO_COALITION).upper(),
        "coalition_url": coalition_url(abbr),
    } for name, abbr in STATES.items()]

    for d in (repo / "data-raw/csv", repo / "data/csv"):
        d.mkdir(parents=True, exist_ok=True)
        with open(d / "immunization_coalition_links.csv", "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=["state_name", "state_abbr", "has_coalition", "coalition_url"])
            w.writeheader()
            w.writerows(rows)

    n_has = sum(r["has_coalition"] == "TRUE" for r in rows)
    print(f"Wrote {len(rows)} rows ({n_has} with a coalition, {len(rows) - n_has} without).")


if __name__ == "__main__":
    main()
