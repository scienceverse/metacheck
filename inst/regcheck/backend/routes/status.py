from __future__ import annotations

import json
import logging

from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse

router = APIRouter()
logger = logging.getLogger(__name__)


def _decode(value):
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return value


def _coerce_int(value):
    value = _decode(value)
    if value is None:
        return None
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def task_status_payload_from_data(data: dict) -> dict | None:
    if not data:
        return None
    result_json = _decode(data.get("result_json"))
    parsed_result = None
    if result_json:
        try:
            parsed_result = json.loads(result_json)
        except json.JSONDecodeError:
            parsed_result = None
    total_dimensions = _coerce_int(data.get("total_dimensions"))
    if total_dimensions is None:
        dimensions_raw = _decode(data.get("dimensions"))
        if dimensions_raw:
            try:
                total_dimensions = len(json.loads(dimensions_raw))
            except (json.JSONDecodeError, TypeError):
                total_dimensions = None
    return {
        "state": _decode(data.get("state")),
        "status": _decode(data.get("status")) or "Pending...",
        "result": parsed_result,
        "total_dimensions": total_dimensions,
        "processed_dimensions": _coerce_int(data.get("processed_dimensions")),
    }


async def get_task_status_payload(store, task_id: str) -> dict | None:
    data = await store.hgetall(task_id)
    return task_status_payload_from_data(data)
