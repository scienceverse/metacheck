from __future__ import annotations

import logging
import re
from datetime import date, datetime
from typing import Any, Dict, Tuple

import requests

logger = logging.getLogger(__name__)

V2_STUDY_URL = "https://clinicaltrials.gov/api/v2/studies/{nct_id}"
HISTORY_LIST_URL = "https://clinicaltrials.gov/api/int/studies/{nct_id}/history"
HISTORY_VERSION_URL = "https://clinicaltrials.gov/api/int/studies/{nct_id}/history/{version}"


def extract_nct_id(text: str) -> str:
    match = re.search(r"(NCT\d{8})", text, re.IGNORECASE)
    if not match:
        raise ValueError("Unable to parse NCT ID from input")
    return match.group(1).upper()

def parse_date(date_str: str | None) -> date | None:
    if not date_str:
        return None
    for fmt in ("%Y-%m-%d", "%Y-%m", "%Y"):
        try:
            parsed = datetime.strptime(date_str, fmt).date()
        except ValueError:
            continue
        if fmt == "%Y-%m":
            return parsed.replace(day=1)
        if fmt == "%Y":
            return parsed.replace(month=1, day=1)
        return parsed
    return None


def fetch_latest_study(nct_id: str) -> Dict[str, Any]:
    response = requests.get(V2_STUDY_URL.format(nct_id=nct_id), timeout=30)
    response.raise_for_status()
    return response.json()


def fetch_history(nct_id: str) -> list[dict[str, Any]]:
    response = requests.get(HISTORY_LIST_URL.format(nct_id=nct_id), timeout=30)
    response.raise_for_status()
    payload = response.json()
    changes = payload.get("changes", [])
    return changes if isinstance(changes, list) else []


def fetch_version(nct_id: str, version: int) -> Dict[str, Any]:
    response = requests.get(
        HISTORY_VERSION_URL.format(nct_id=nct_id, version=version), timeout=30
    )
    response.raise_for_status()
    payload = response.json()
    if isinstance(payload, dict) and isinstance(payload.get("study"), dict):
        return payload["study"]
    return payload if isinstance(payload, dict) else {}


def select_version_before_start(
    history: list[dict[str, Any]], start_date: date | None
) -> Tuple[int | None, date | None, date | None]:
    if not start_date:
        return None, None, None

    parsed: list[tuple[int, date | None]] = []
    for entry in history:
        version = entry.get("version")
        if version is None:
            continue
        try:
            version_int = int(version)
        except (TypeError, ValueError):
            continue
        parsed.append((version_int, parse_date(entry.get("date"))))

    if not parsed:
        return None, None, None

    with_dates = [(ver, dt) for ver, dt in parsed if dt is not None]
    earliest_date = min((dt for _, dt in with_dates), default=None)
    latest_date = max((dt for _, dt in with_dates), default=None)

    if with_dates:
        eligible = [(ver, dt) for ver, dt in with_dates if dt <= start_date]
        if eligible:
            eligible.sort(key=lambda item: (item[1], item[0]))
            return eligible[-1][0], earliest_date, latest_date
        with_dates.sort(key=lambda item: (item[1], item[0]))
        return with_dates[0][0], earliest_date, latest_date

    parsed.sort(key=lambda item: item[0])
    return parsed[0][0], earliest_date, latest_date


def fetch_trial_json_with_metadata(nct_id: str) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    latest = fetch_latest_study(nct_id)
    status = latest.get("protocolSection", {}).get("statusModule", {})
    start_date = parse_date((status.get("startDateStruct") or {}).get("date"))

    metadata: dict[str, Any] = {
        "nct_id": nct_id,
        "start_date": start_date.isoformat() if start_date else None,
        "source": "latest",
        "selected_version": None,
        "selected_version_date": None,
        "history_length": 0,
        "target_version": None,
        "earliest_version_date": None,
        "latest_version_date": None,
        "latest_history_version": -1,
    }

    if not start_date:
        return latest, metadata

    history = fetch_history(nct_id)
    metadata["history_length"] = len(history)

    target_version, earliest_date, latest_date = select_version_before_start(history, start_date)
    versions: list[int] = []
    for entry in history:
        try:
            versions.append(int(entry.get("version")))
        except (TypeError, ValueError):
            continue

    metadata.update(
        {
            "target_version": target_version,
            "earliest_version_date": earliest_date.isoformat() if earliest_date else None,
            "latest_version_date": latest_date.isoformat() if latest_date else None,
            "latest_history_version": max(versions, default=-1),
        }
    )

    if target_version is None:
        return latest, metadata

    if target_version == metadata["latest_history_version"]:
        metadata["selected_version"] = target_version
        metadata["selected_version_date"] = metadata.get("latest_version_date")
        return latest, metadata

    historical = fetch_version(nct_id, target_version)
    if not historical:
        return latest, metadata

    metadata["source"] = "history"
    metadata["selected_version"] = target_version
    selected_date = None
    for entry in history:
        try:
            version = int(entry.get("version"))
        except (TypeError, ValueError):
            continue
        if version == target_version:
            selected_date = parse_date(entry.get("date"))
            break
    if selected_date:
        metadata["selected_version_date"] = selected_date.isoformat()

    logger.info(
        "Using historical ClinicalTrials.gov version for %s",
        nct_id,
        extra={"metadata": metadata},
    )
    return historical, metadata


def fetch_trial_json(nct_id: str) -> Dict[str, Any]:
    return fetch_latest_study(nct_id)


def flatten_json(data: Dict[str, Any], parent_key: str = "", sep: str = " → ") -> Dict[str, str]:
    items: Dict[str, str] = {}
    for key, value in data.items():
        cleaned_key = key.replace("Module", "").replace("module", "")
        new_key = f"{parent_key}{sep}{cleaned_key}" if parent_key else cleaned_key

        if isinstance(value, dict):
            items.update(flatten_json(value, new_key, sep=sep))
        elif isinstance(value, list):
            if all(isinstance(item, dict) for item in value):
                for index, item in enumerate(value):
                    items.update(flatten_json(item, f"{new_key}[{index}]", sep=sep))
            else:
                items[new_key] = "\n".join(map(str, value))
        elif value not in [None, ""]:
            items[new_key] = str(value)
    return items


def nested_flatten_json(
    data: Dict[str, Any], parent_key: str = "", sep: str = " → "
) -> Dict[str, Dict[str, str]]:
    nested: Dict[str, Dict[str, str]] = {}

    def recurse(obj: Any, current_path: list[str]) -> None:
        if isinstance(obj, dict):
            for key, value in obj.items():
                cleaned_key = key.replace("Module", "").replace("module", "")
                new_path = current_path + [cleaned_key]
                recurse(value, new_path)
        elif isinstance(obj, list):
            for index, item in enumerate(obj):
                recurse(item, current_path + [f"[{index}]"])
        elif obj not in [None, ""]:
            if len(current_path) >= 2:
                dimension = current_path[0]
                subcomponent = sep.join(current_path[1:])
                nested.setdefault(dimension, {})[subcomponent] = str(obj)
            elif len(current_path) == 1:
                dimension = current_path[0]
                nested.setdefault(dimension, {})[""] = str(obj)

    recurse(data, [])
    return nested


def extract_flattened_trial(nct_id: str) -> Dict[str, str]:
    trial_json, _metadata = fetch_trial_json_with_metadata(nct_id)
    protocol_section = trial_json.get("protocolSection", {})
    return flatten_json(protocol_section)


def extract_nested_trial(nct_id: str) -> Dict[str, Dict[str, str]]:
    trial_json, _metadata = fetch_trial_json_with_metadata(nct_id)
    protocol_section = trial_json.get("protocolSection", {})
    return nested_flatten_json(protocol_section)


def extract_nested_trial_with_metadata(
    nct_id: str,
) -> Tuple[Dict[str, Dict[str, str]], Dict[str, Any]]:
    trial_json, metadata = fetch_trial_json_with_metadata(nct_id)
    protocol_section = trial_json.get("protocolSection", {})
    return nested_flatten_json(protocol_section), metadata
