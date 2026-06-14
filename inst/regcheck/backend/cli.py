"""Command-line entrypoint to run RegCheck comparisons without the frontend.

Usage examples:
  python -m backend.cli general --preregistration /path/prereg.pdf --paper /path/paper.pdf --dimensions-csv dims.csv --client openai
  python -m backend.cli clinical --registration-id NCT0000 --paper /path/paper.pdf --dimensions-csv dims.csv --client openai
"""

from __future__ import annotations

import argparse
import asyncio
import csv
import json
import logging
import sys
from pathlib import Path
from typing import Iterable

from backend.services.comparisons import (
    animals_trial_comparison,
    clinical_trial_comparison,
    general_preregistration_comparison,
)

logger = logging.getLogger("backend.cli")


def _load_dimensions_from_csv(csv_path: Path) -> list[dict[str, str]]:
    """Read dimensions from a CSV with 'dimension' and optional 'definition' columns."""
    if not csv_path.exists():
        raise FileNotFoundError(f"Dimensions CSV not found: {csv_path}")
    with csv_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if not reader.fieldnames:
            raise ValueError(
                "Dimensions CSV is missing headers. Expected at least a 'dimension' column."
            )
        dimensions: list[dict[str, str]] = []
        for row in reader:
            name = (
                (row.get("dimension") or row.get("name") or row.get("Dimension") or row.get("Name") or "")
                .strip()
            )
            definition = (row.get("definition") or row.get("Definition") or "").strip()
            if name:
                dimensions.append({"dimension": name, "definition": definition})
        if not dimensions:
            raise ValueError(
                "No dimensions found in CSV. Ensure there is a 'dimension' column with values."
            )
        return dimensions


def _normalized_suffix(path: str) -> str:
    suffix = Path(path).suffix
    if not suffix:
        raise ValueError(f"Cannot determine file extension for {path}.")
    return suffix.lower()


async def _run_general(args) -> dict:
    logger.info("Loading dimensions from CSV: %s", args.dimensions_csv)
    dimensions = _load_dimensions_from_csv(Path(args.dimensions_csv))
    logger.info("Running general preregistration comparison (dimensions=%d)", len(dimensions))
    result = await general_preregistration_comparison(
        args.preregistration,
        _normalized_suffix(args.preregistration),
        args.paper,
        _normalized_suffix(args.paper),
        args.client,
        args.parser_choice,
        selected_dimensions=dimensions,
        append_previous_output=args.append_previous_output,
        reasoning_effort=args.reasoning_effort,
        multiple_experiments=args.multiple_experiments,
        experiment_number=args.experiment_number,
        experiment_text=args.experiment_text,
    )
    logger.info("Completed general comparison.")
    return result.model_dump()


async def _run_clinical(args) -> dict:
    dimensions = None
    if args.dimensions_csv:
        logger.info("Loading dimensions from CSV: %s", args.dimensions_csv)
        dimensions = _load_dimensions_from_csv(Path(args.dimensions_csv))
    logger.info(
        "Running clinical trial comparison for %s (dimensions=%s)",
        args.registration_id,
        "custom" if dimensions else "default",
    )
    result = await clinical_trial_comparison(
        args.registration_id,
        args.paper,
        _normalized_suffix(args.paper),
        args.client,
        parser_choice=args.parser_choice,
        selected_dimensions=dimensions,
        append_previous_output=args.append_previous_output,
        reasoning_effort=args.reasoning_effort,
    )
    logger.info("Completed clinical comparison.")
    return result.model_dump()


async def _run_animals(args) -> dict:
    if not args.registration_csv:
        raise ValueError(
            "Animal trial comparisons currently require --registration-csv until API support is available."
        )
    dimensions = None
    if args.dimensions_csv:
        logger.info("Loading dimensions from CSV: %s", args.dimensions_csv)
        dimensions = _load_dimensions_from_csv(Path(args.dimensions_csv))
    logger.info(
        "Running animals (PCT) comparison for %s using CSV %s (dimensions=%s)",
        args.registration_id,
        args.registration_csv,
        "custom" if dimensions else "default",
    )
    result = await animals_trial_comparison(
        args.registration_id,
        args.paper,
        _normalized_suffix(args.paper),
        args.client,
        registration_csv_path=args.registration_csv,
        parser_choice=args.parser_choice,
        selected_dimensions=dimensions,
        append_previous_output=args.append_previous_output,
        reasoning_effort=args.reasoning_effort,
    )
    logger.info("Completed animals comparison.")
    return result.model_dump()


def _write_output(payload: dict, output_path: str | None, output_format: str) -> None:
    """Write results to stdout or file in the requested format."""
    if output_format == "json":
        text = json.dumps(payload, indent=2)
        if output_path:
            Path(output_path).write_text(text, encoding="utf-8")
        else:
            print(text)
        return

    # CSV output (default)
    items = payload.get("items", []) if isinstance(payload, dict) else []
    fieldnames = [
        "dimension",
        "paper_content_quotes",
        "paper_content_summary",
        "registration_content_quotes",
        "registration_content_summary",
        "deviation_judgement",
        "deviation_information",
    ]
    rows: list[dict[str, str]] = []
    for item in items:
        if not isinstance(item, dict):
            continue
        row: dict[str, str] = {}
        for key in fieldnames:
            value = item.get(key, "")
            row[key] = "" if value is None else str(value)
        rows.append(row)

    if output_path:
        path = Path(output_path)
        with path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)
    else:
        writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run RegCheck backend comparisons from the command line."
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    general = subparsers.add_parser(
        "general", help="Compare a preregistration document to a paper."
    )
    general.add_argument(
        "--preregistration",
        required=True,
        help="Path to the preregistration file (.pdf or .docx).",
    )
    general.add_argument(
        "--paper",
        required=True,
        help="Path to the published paper file (.pdf or .docx).",
    )
    general.add_argument(
        "--dimensions-csv",
        required=True,
        help="CSV file with columns 'dimension' and optional 'definition'.",
    )
    general.add_argument(
        "--client",
        default="openai",
        choices=["openai", "deepseek", "groq", "ollama"],
        help="LLM provider to use.",
    )
    general.add_argument(
        "--parser-choice",
        default="grobid",
        choices=["grobid", "dpt2"],
        help="PDF parser to extract paper text.",
    )
    general.add_argument(
        "--append-previous-output",
        action="store_true",
        help="Append previous dimension responses into later prompts.",
    )
    general.add_argument(
        "--reasoning-effort",
        default="medium",
        choices=["low", "medium", "high"],
        help="Reasoning setting for OpenAI models (ignored by other providers).",
    )
    general.add_argument(
        "--multiple-experiments",
        action="store_true",
        help="Indicate the paper has multiple experiments and you want to isolate one experiment's text.",
    )
    general.add_argument(
        "--experiment-number",
        help="Identifier for the relevant experiment (e.g., 2, 2B).",
    )
    general.add_argument(
        "--experiment-text",
        help="Optional freeform note about the relevant experiment to include in the extraction prompt.",
    )
    general.add_argument(
        "--output",
        help="Optional path to write JSON results. Defaults to stdout.",
    )
    general.add_argument(
        "--output-format",
        default="csv",
        choices=["csv", "json"],
        help="Output format (csv or json). Defaults to csv.",
    )

    clinical = subparsers.add_parser(
        "clinical", help="Compare a clinical trial registration (by ID) to a paper."
    )
    clinical.add_argument(
        "--registration-id",
        required=True,
        help="Clinical trial registration identifier (e.g., NCT number).",
    )
    clinical.add_argument(
        "--paper",
        required=True,
        help="Path to the published paper file (.pdf or .docx).",
    )
    clinical.add_argument(
        "--dimensions-csv",
        help="Optional CSV with 'dimension' and 'definition' columns to override defaults.",
    )
    clinical.add_argument(
        "--client",
        default="openai",
        choices=["openai", "deepseek", "groq", "ollama"],
        help="LLM provider to use.",
    )
    clinical.add_argument(
        "--parser-choice",
        default="grobid",
        choices=["grobid", "dpt2"],
        help="PDF parser to extract paper text.",
    )
    clinical.add_argument(
        "--append-previous-output",
        action="store_true",
        help="Append previous dimension responses into later prompts.",
    )
    clinical.add_argument(
        "--reasoning-effort",
        default="medium",
        choices=["low", "medium", "high"],
        help="Reasoning setting for OpenAI models (ignored by other providers).",
    )
    clinical.add_argument(
        "--output",
        help="Optional path to write JSON results. Defaults to stdout.",
    )
    clinical.add_argument(
        "--output-format",
        default="csv",
        choices=["csv", "json"],
        help="Output format (csv or json). Defaults to csv.",
    )

    animals = subparsers.add_parser(
        "animals", help="Compare a preclinical (PCT) registration to a paper."
    )
    animals.add_argument(
        "--registration-id",
        required=True,
        help="PreclinicalTrials.eu identifier (e.g., PCTE0000405).",
    )
    animals.add_argument(
        "--registration-csv",
        help="CSV export containing a pct_id column. Required until API integration is available.",
    )
    animals.add_argument(
        "--paper",
        required=True,
        help="Path to the published paper file (.pdf or .docx).",
    )
    animals.add_argument(
        "--dimensions-csv",
        help="Optional CSV with 'dimension' and 'definition' columns to override defaults.",
    )
    animals.add_argument(
        "--client",
        default="openai",
        choices=["openai", "deepseek", "groq", "ollama"],
        help="LLM provider to use.",
    )
    animals.add_argument(
        "--parser-choice",
        default="grobid",
        choices=["grobid", "dpt2"],
        help="PDF parser to extract paper text.",
    )
    animals.add_argument(
        "--append-previous-output",
        action="store_true",
        help="Append previous dimension responses into later prompts.",
    )
    animals.add_argument(
        "--reasoning-effort",
        default="medium",
        choices=["low", "medium", "high"],
        help="Reasoning setting for OpenAI models (ignored by other providers).",
    )
    animals.add_argument(
        "--output",
        help="Optional path to write JSON results. Defaults to stdout.",
    )
    animals.add_argument(
        "--output-format",
        default="csv",
        choices=["csv", "json"],
        help="Output format (csv or json). Defaults to csv.",
    )

    return parser


def main(argv: Iterable[str] | None = None) -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s | %(levelname)s | %(message)s",
    )
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        if args.command == "general":
            payload = asyncio.run(_run_general(args))
        elif args.command == "clinical":
            payload = asyncio.run(_run_clinical(args))
        elif args.command == "animals":
            payload = asyncio.run(_run_animals(args))
        else:
            parser.error("Unknown command")
            return
    except Exception as exc:
        print(f"Error: {exc}", file=sys.stderr)
        sys.exit(1)
    _write_output(payload, getattr(args, "output", None), getattr(args, "output_format", "csv"))


if __name__ == "__main__":
    main()
