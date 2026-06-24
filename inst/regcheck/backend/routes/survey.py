from __future__ import annotations

import logging
from datetime import datetime, timezone
import json

from fastapi import APIRouter, Form, Request
from fastapi.responses import HTMLResponse, RedirectResponse

router = APIRouter()
logger = logging.getLogger(__name__)


SURVEY_QUESTIONS = [
    {
        "name": "use_case",
        "label": "Why are you using RegCheck?",
        "type": "select",
        "options": [
            "Author of paper",
            "Reviewer of paper",
            "Journal editor of paper",
            "Reader of paper",
            "Other",
        ],
    },
    {
        "name": "academic_position",
        "label": "What is your academic position?",
        "type": "select",
        "options": ["Undergrad", "Masters", "PhD", "Postdoc", "Professor", "Nonacademic"],
    },
    {
        "name": "research_field",
        "label": "What is your scientific field?",
        "type": "select",
        "options": ["Psychology", "Economics", "Medicine", "Animal research", "Other"],
    },
]


def _safe_int(value):
    try:
        return int(value) if value is not None else None
    except (TypeError, ValueError):
        return None


def _string_or_empty(value: str | None) -> str:
    if value is None:
        return ""
    return value.strip()


@router.get("/survey/{task_id}", response_class=HTMLResponse, name="survey")
async def survey(request: Request, task_id: str):
    redis_client = request.app.state.redis
    task_meta = {}
    try:
        task_meta = await redis_client.hgetall(task_id)
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.warning("Unable to load task metadata for survey page", exc_info=exc)

    state = (task_meta.get("state") or "PENDING") if task_meta else "PENDING"
    processed_dimensions = _safe_int(task_meta.get("processed_dimensions")) if task_meta else None
    total_dimensions = _safe_int(task_meta.get("total_dimensions")) if task_meta else None
    try:
        result_url = request.url_for("result", task_id=task_id)
    except Exception:
        result_url = f"/result/{task_id}"

    return request.app.state.templates.TemplateResponse(
        "survey.html",
        {
            "request": request,
            "task_id": task_id,
            "result_url": result_url,
            "state": state,
            "processed_dimensions": processed_dimensions,
            "total_dimensions": total_dimensions,
            "questions": SURVEY_QUESTIONS,
        },
    )


@router.post("/survey/{task_id}")
async def submit_survey(
    request: Request,
    task_id: str,
    academic_position: str | None = Form(None),
    research_field: str | None = Form(None),
    use_case: str | None = Form(None),
    skip: str | None = Form(None),
):
    redis_client = request.app.state.redis
    submission = {
        "submitted_at": datetime.now(timezone.utc).isoformat(),
        "academic_position": _string_or_empty(academic_position),
        "research_field": _string_or_empty(research_field),
        "use_case": _string_or_empty(use_case),
        "skipped": "1" if skip else "0",
    }

    try:
        task_meta = await redis_client.hgetall(task_id)
        if task_meta:
            comparison_type = task_meta.get("comparison_type")
            if comparison_type:
                submission["comparison_type"] = comparison_type
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.warning("Unable to load task metadata while saving survey", exc_info=exc)

    try:
        await redis_client.hset(f"survey:{task_id}", mapping=submission)
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.error("Failed to persist survey response", exc_info=exc)
    else:
        record = {"task_id": task_id, **submission}
        try:
            await redis_client.lpush("survey:responses", json.dumps(record))
            await redis_client.sadd("survey:task_ids", task_id)
        except Exception as exc:  # pragma: no cover - defensive logging
            logger.warning("Failed to append survey response to index", exc_info=exc)

    try:
        result_url = request.url_for("result", task_id=task_id)
    except Exception:
        result_url = f"/result/{task_id}"

    return RedirectResponse(url=result_url, status_code=303)
