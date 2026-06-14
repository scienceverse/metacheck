"""
Minimal API routes for metacheck-local.

Only implements:
  POST /api/v1/comparisons/text  — JSON text submission
  GET  /api/v1/comparisons/{task_id} — poll for result

Jobs run as asyncio background tasks; no Redis or file uploads required.
"""
from __future__ import annotations

import asyncio
import json
import secrets
import uuid
from typing import Any

from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse
from pydantic import BaseModel

from ..services.dimensions import default_dimensions_for
from ..worker import _dispatch_job
from .status import get_task_status_payload

router = APIRouter(prefix="/api/v1")


# ── auth ──────────────────────────────────────────────────────────────────────

def _api_error(code: int, key: str, message: str) -> JSONResponse:
    return JSONResponse(status_code=code, content={"error": {"code": key, "message": message}})


def _auth_error(request: Request) -> JSONResponse | None:
    configured = getattr(request.app.state.settings, "api_token", None)
    if not configured:
        return _api_error(503, "API_AUTH_NOT_CONFIGURED", "REGCHECK_API_TOKEN is not configured.")
    auth = request.headers.get("authorization", "")
    scheme, _, token = auth.partition(" ")
    supplied = token.strip() if scheme.lower() == "bearer" else ""
    if not supplied:
        supplied = (request.headers.get("x-api-key") or "").strip()
    if not supplied:
        return _api_error(401, "MISSING_API_AUTH", "Provide an API token.")
    if not secrets.compare_digest(supplied, configured):
        return _api_error(401, "INVALID_API_AUTH", "Invalid API token.")
    return None


# ── text comparison request model ─────────────────────────────────────────────

class TextComparisonRequest(BaseModel):
    paper_text: str
    registration_text: str | None = None
    registration_id: str | None = None
    client: str = "ollama"
    reasoning_effort: str = "medium"
    append_previous_output: bool = True
    multiple_experiments: bool = False
    experiment_number: str | None = None
    dimensions: list[dict[str, str]] | None = None


# ── endpoints ─────────────────────────────────────────────────────────────────

@router.post("/comparisons/text")
async def create_text_comparison(body: TextComparisonRequest, request: Request) -> JSONResponse:
    auth_err = _auth_error(request)
    if auth_err is not None:
        return auth_err

    if not body.paper_text or not body.paper_text.strip():
        return _api_error(400, "MISSING_PAPER_TEXT", "paper_text is required.")
    has_text = bool(body.registration_text and body.registration_text.strip())
    has_id   = bool(body.registration_id and body.registration_id.strip())
    if has_text == has_id:
        return _api_error(400, "AMBIGUOUS_REGISTRATION_INPUT",
                          "Provide exactly one of registration_text or registration_id.")

    dimensions = body.dimensions or default_dimensions_for("general_preregistration")
    task_id = str(uuid.uuid4())
    store = request.app.state.redis

    await store.hset(task_id, mapping={
        "state": "PENDING",
        "status": "Task queued",
        "result_json": json.dumps({"items": []}),
        "total_dimensions": len(dimensions),
        "processed_dimensions": 0,
        "dimensions": json.dumps([d["dimension"] for d in dimensions]),
    })

    job = {
        "comparison_type": "general_preregistration",
        "task_id": task_id,
        "client": body.client,
        "reasoning_effort": body.reasoning_effort if body.client == "openai" else None,
        "append_previous_output": body.append_previous_output,
        "selected_dimensions": dimensions,
        "paper_text": body.paper_text,
        "prereg_text": body.registration_text,
        "registration_id": body.registration_id,
        "multiple_experiments": body.multiple_experiments,
        "experiment_number": body.experiment_number,
    }

    asyncio.create_task(_dispatch_job(job))

    return JSONResponse(status_code=202, content={
        "task_id": task_id,
        "state": "queued",
        "status": "Task queued",
        "status_url": f"/api/v1/comparisons/{task_id}",
    })


@router.get("/comparisons/{task_id}")
async def get_comparison(task_id: str, request: Request) -> JSONResponse:
    auth_err = _auth_error(request)
    if auth_err is not None:
        return auth_err

    store = request.app.state.redis
    payload = await get_task_status_payload(store, task_id)
    if payload is None:
        return _api_error(404, "TASK_NOT_FOUND", "Task not found.")

    state_map = {"PENDING": "queued", "IN_PROGRESS": "in_progress",
                 "SUCCESS": "success", "FAILURE": "failure"}
    raw = (payload.get("state") or "").strip()
    state = state_map.get(raw.upper(), raw.lower() or "unknown")

    result = payload.get("result")
    if isinstance(result, list):
        result = {"items": result}
    elif not isinstance(result, dict):
        result = {"items": []}

    return JSONResponse({
        "task_id": task_id,
        "state": state,
        "status": payload.get("status") or "Pending...",
        "processed_dimensions": payload.get("processed_dimensions") or 0,
        "total_dimensions": payload.get("total_dimensions") or 0,
        "result": result,
    })
