from __future__ import annotations

import asyncio
import json
import logging
import os
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

from fastapi import APIRouter, File, Form, HTTPException, Request, UploadFile
from fastapi.responses import RedirectResponse

from ..core.storage import get_s3_config, guess_content_type, s3_upload_fileobj

router = APIRouter()
logger = logging.getLogger(__name__)
DEFAULT_MAX_UPLOAD_BYTES = 20 * 1024 * 1024  # 20 MB


def _upload_limit() -> int:
    raw = os.getenv("MAX_UPLOAD_BYTES")
    if raw is None:
        return DEFAULT_MAX_UPLOAD_BYTES
    try:
        parsed = int(raw)
    except (TypeError, ValueError):
        return DEFAULT_MAX_UPLOAD_BYTES
    return max(1, parsed)


MAX_UPLOAD_BYTES = _upload_limit()

ComparisonType = Literal[
    "clinical_trials",
    "general_preregistration",
    "animals_trials",
]


@dataclass(slots=True)
class QueuedComparison:
    task_id: str
    state: str
    status: str


async def _store_upload(
    destination: Path,
    upload: UploadFile,
    *,
    max_bytes: int | None = None,
) -> str:
    destination.parent.mkdir(parents=True, exist_ok=True)
    size_limit = max_bytes or MAX_UPLOAD_BYTES
    total_read = 0
    try:
        with open(destination, "wb") as handle:
            while True:
                chunk = await upload.read(1024 * 1024)
                if not chunk:
                    break
                total_read += len(chunk)
                if size_limit and total_read > size_limit:
                    raise HTTPException(
                        status_code=413,
                        detail="Uploaded file exceeds the permitted size limit.",
                    )
                handle.write(chunk)
    except Exception:
        destination.unlink(missing_ok=True)
        raise
    finally:
        try:
            await upload.seek(0)
        except Exception:
            pass
    return str(destination)


def _safe_filename(filename: str | None) -> str:
    name = Path(filename or "").name.strip()
    return name or "upload"


def _file_ext(filename: str | None) -> str:
    return Path(_safe_filename(filename)).suffix.lower()


async def _save_upload(
    upload_dir: Path,
    upload: UploadFile,
    *,
    prefix: str,
    max_bytes: int | None = None,
) -> tuple[str, str]:
    filename = _safe_filename(upload.filename)
    destination = upload_dir / f"{prefix}_{filename}"
    stored = await _store_upload(destination, upload, max_bytes=max_bytes)
    return stored, _file_ext(filename)


async def _store_upload_to_redis(redis_client, redis_key: str, file_path: str, ttl_seconds: int = 86400) -> None:
    """Store an uploaded file's contents in Redis (compressed + base64) so workers can reconstruct it."""
    try:
        raw = Path(file_path).read_bytes()
    except Exception as exc:
        raise HTTPException(status_code=500, detail=f"Failed to read uploaded file: {exc}") from exc
    import base64
    import gzip

    compressed = gzip.compress(raw)
    encoded = base64.b64encode(compressed).decode("ascii")
    try:
        await redis_client.set(redis_key, encoded, ex=ttl_seconds)
    except Exception as exc:
        raise HTTPException(status_code=500, detail="Failed to persist upload to queue store") from exc


async def _store_upload_to_s3(task_id: str, file_path: str, *, label: str) -> str:
    cfg = get_s3_config()
    if cfg is None:
        raise RuntimeError("S3_BUCKET not configured")
    ext = Path(file_path).suffix.lower()
    key = f"regcheck/uploads/{task_id}/{label}{ext}"

    def _upload() -> None:
        with open(file_path, "rb") as handle:
            s3_upload_fileobj(
                cfg,
                key=key,
                fileobj=handle,
                content_type=guess_content_type(file_path),
            )

    await asyncio.to_thread(_upload)
    return key


def _bool_from_yes(value: str | None) -> bool:
    return (value or "").strip().lower() == "yes"


def _parse_dimensions(dimensions_data: str) -> list[dict[str, str]]:
    try:
        payload = json.loads(dimensions_data)
    except json.JSONDecodeError as exc:
        raise HTTPException(status_code=400, detail="Invalid dimension payload") from exc

    if not isinstance(payload, list):
        raise HTTPException(status_code=400, detail="Invalid dimension payload")

    selected_dimensions: list[dict[str, str]] = []
    for item in payload:
        if not isinstance(item, dict):
            continue
        name = (item.get("dimension") or item.get("name") or "").strip()
        definition = (item.get("definition") or "").strip()
        if name:
            selected_dimensions.append({"dimension": name, "definition": definition})

    if not selected_dimensions:
        raise HTTPException(status_code=400, detail="At least one dimension must be selected")

    return selected_dimensions


def _normalize_parser_choice(parser_choice: str) -> str:
    normalized = (parser_choice or "").strip().lower()
    if normalized not in {"grobid", "dpt2"}:
        raise HTTPException(status_code=400, detail="Unsupported parser choice")
    return normalized


def _normalize_reasoning_effort(client: str, reasoning_effort: str | None) -> str | None:
    effort_normalized = (reasoning_effort or "").strip().lower()
    if client == "openai":
        if effort_normalized not in {"low", "medium", "high"}:
            effort_normalized = "medium"
        return effort_normalized
    return None


async def _safe_hset(redis_client, task_id: str, mapping: dict, retries: int = 2) -> None:
    last_error: Exception | None = None
    for attempt in range(retries + 1):
        try:
            await redis_client.hset(task_id, mapping=mapping)
            return
        except Exception as exc:  # pragma: no cover - defensive logging
            last_error = exc
            logger.error("Redis hset failed (attempt %s/%s)", attempt + 1, retries + 1, exc_info=exc)
            await asyncio.sleep(0.2 * (attempt + 1))
    if last_error:
        raise last_error


async def _enqueue_comparison(
    request: Request,
    *,
    comparison_type: ComparisonType,
    parser_choice: str,
    client: str,
    reasoning_effort: str | None,
    append_previous_output: str | None,
    selected_dimensions: list[dict[str, str]],
    registration_id: str | None = None,
    preregistration: UploadFile | None = None,
    paper: UploadFile | None = None,
    registration_csv: UploadFile | None = None,
    multiple_experiments: str | None = None,
    experiment_number: str | None = None,
    experiment_text: str | None = None,
) -> QueuedComparison:
    settings = request.app.state.settings
    upload_dir = Path(settings.upload_dir)
    redis_client = request.app.state.redis
    upload_ttl = max(86400, getattr(settings, "task_ttl_seconds", 86400))

    # Basic backpressure: refuse new jobs if queue + in-flight exceeds configured limit.
    try:
        queued = await redis_client.llen("comparison:queue")
        in_flight = await redis_client.llen("comparison:processing")
        if queued + in_flight >= settings.max_queue_length:
            raise HTTPException(status_code=503, detail="System is busy; please retry shortly.")
    except HTTPException:
        raise
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.warning("Failed to compute queue depth; proceeding without backpressure", exc_info=exc)

    dimension_names = [item["dimension"] for item in selected_dimensions]

    append_previous = _bool_from_yes(append_previous_output)
    parser_choice_normalized = _normalize_parser_choice(parser_choice)
    effort_normalized = _normalize_reasoning_effort(client, reasoning_effort)
    logger.info(
        "queue_comparison normalized reasoning effort",
        extra={"client": client, "reasoning_effort": effort_normalized, "comparison_type": comparison_type},
    )

    if paper is None:
        raise HTTPException(status_code=400, detail="Paper upload is required")
    task_id = str(uuid.uuid4())
    paper_path, paper_ext = await _save_upload(
        upload_dir, paper, prefix=f"{task_id}_paper", max_bytes=MAX_UPLOAD_BYTES
    )
    paper_redis_key = f"upload:{task_id}:paper"
    prereg_redis_key: str | None = None
    csv_redis_key: str | None = None

    # Prefer durable object storage (S3) so worker dynos can always access uploads.
    # Fall back to storing compressed blobs in Redis when S3 isn't configured.
    s3_keys: dict[str, str | None] = {"paper": None, "prereg": None, "csv": None}
    if get_s3_config() is not None:
        s3_keys["paper"] = await _store_upload_to_s3(task_id, paper_path, label="paper")
        try:
            Path(paper_path).unlink(missing_ok=True)
        except Exception:
            pass
    else:
        await _store_upload_to_redis(redis_client, paper_redis_key, paper_path, ttl_seconds=upload_ttl)
        try:
            Path(paper_path).unlink(missing_ok=True)
        except Exception:
            pass

    stored_prereg_path: str | None = None
    prereg_ext: str | None = None
    stored_csv_path: str | None = None

    if comparison_type == "clinical_trials":
        if not registration_id or not registration_id.strip():
            raise HTTPException(
                status_code=400, detail="ClinicalTrials.gov link or ID is required for this option"
            )
    elif comparison_type == "general_preregistration":
        if preregistration is None:
            raise HTTPException(
                status_code=400, detail="Preregistration upload is required for this option"
            )
        stored_prereg_path, prereg_ext = await _save_upload(
            upload_dir, preregistration, prefix=f"{task_id}_prereg", max_bytes=MAX_UPLOAD_BYTES
        )
        prereg_redis_key = f"upload:{task_id}:prereg"
        if get_s3_config() is not None:
            s3_keys["prereg"] = await _store_upload_to_s3(task_id, stored_prereg_path, label="prereg")
            try:
                Path(stored_prereg_path).unlink(missing_ok=True)
            except Exception:
                pass
        else:
            await _store_upload_to_redis(redis_client, prereg_redis_key, stored_prereg_path, ttl_seconds=upload_ttl)
            try:
                Path(stored_prereg_path).unlink(missing_ok=True)
            except Exception:
                pass
    elif comparison_type == "animals_trials":
        if not registration_id or not registration_id.strip():
            raise HTTPException(status_code=400, detail="Registration ID is required for this option")
        if registration_csv is None:
            raise HTTPException(
                status_code=400,
                detail="CSV required for animals trials until API retrieval is implemented.",
            )
        stored_csv_path, _ = await _save_upload(
            upload_dir, registration_csv, prefix=f"{task_id}_registration", max_bytes=MAX_UPLOAD_BYTES
        )
        csv_redis_key = f"upload:{task_id}:csv"
        if get_s3_config() is not None:
            s3_keys["csv"] = await _store_upload_to_s3(task_id, stored_csv_path, label="registration")
            try:
                Path(stored_csv_path).unlink(missing_ok=True)
            except Exception:
                pass
        else:
            await _store_upload_to_redis(redis_client, csv_redis_key, stored_csv_path, ttl_seconds=upload_ttl)
            try:
                Path(stored_csv_path).unlink(missing_ok=True)
            except Exception:
                pass
    else:
        raise HTTPException(status_code=400, detail="Unsupported comparison type")

    initial_payload = {
        "state": "PENDING",
        "status": "Task queued",
        "result_json": json.dumps({"items": []}),
        "total_dimensions": len(dimension_names),
        "processed_dimensions": 0,
        "dimensions": json.dumps(dimension_names),
        "comparison_type": comparison_type,
    }
    try:
        await _safe_hset(redis_client, task_id, initial_payload)
        await redis_client.expire(task_id, settings.task_ttl_seconds)
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.error("Redis failed to set initial state", exc_info=exc)
        raise HTTPException(status_code=503, detail="Failed to queue task; please retry.") from exc

    job_payload = {
        "comparison_type": comparison_type,
        "task_id": task_id,
        "client": client,
        "parser_choice": parser_choice_normalized,
        "reasoning_effort": effort_normalized,
        "append_previous_output": append_previous,
        "selected_dimensions": selected_dimensions,
        "upload_keys": {"paper": paper_redis_key, "prereg": prereg_redis_key, "csv": csv_redis_key},
        "s3_keys": s3_keys,
    }

    if comparison_type == "clinical_trials":
        job_payload.update(
            {
                "registration_id": registration_id,
                "paper_path": paper_path,
                "paper_ext": paper_ext,
            }
        )
    elif comparison_type == "general_preregistration":
        multiple_experiments_flag = _bool_from_yes(multiple_experiments)
        job_payload.update(
            {
                "prereg_path": stored_prereg_path,
                "prereg_ext": prereg_ext or "",
                "paper_path": paper_path,
                "paper_ext": paper_ext,
                "multiple_experiments": multiple_experiments_flag,
                "experiment_number": experiment_number,
                "experiment_text": experiment_text,
            }
        )
    else:
        job_payload.update(
            {
                "registration_id": registration_id,
                "paper_path": paper_path,
                "paper_ext": paper_ext,
                "registration_csv_path": stored_csv_path,
            }
        )

    try:
        await redis_client.rpush("comparison:queue", json.dumps(job_payload))
    except Exception as exc:  # pragma: no cover - defensive logging
        logger.error("Failed to enqueue comparison job", exc_info=exc, extra={"task_id": task_id})
        await redis_client.hset(
            task_id,
            mapping={
                "state": "FAILURE",
                "status": "Failed to enqueue job; please retry.",
            },
        )
        raise HTTPException(status_code=503, detail="Failed to queue comparison. Please retry.") from exc

    return QueuedComparison(task_id=task_id, state="queued", status="Task queued")


async def _queue_comparison(
    request: Request,
    *,
    comparison_type: ComparisonType,
    parser_choice: str,
    client: str,
    reasoning_effort: str | None,
    append_previous_output: str | None,
    dimensions_data: str,
    registration_id: str | None = None,
    preregistration: UploadFile | None = None,
    paper: UploadFile | None = None,
    registration_csv: UploadFile | None = None,
    multiple_experiments: str | None = None,
    experiment_number: str | None = None,
    experiment_text: str | None = None,
) -> RedirectResponse:
    selected_dimensions = _parse_dimensions(dimensions_data)
    queued = await _enqueue_comparison(
        request,
        comparison_type=comparison_type,
        parser_choice=parser_choice,
        client=client,
        reasoning_effort=reasoning_effort,
        append_previous_output=append_previous_output,
        selected_dimensions=selected_dimensions,
        registration_id=registration_id,
        preregistration=preregistration,
        paper=paper,
        registration_csv=registration_csv,
        multiple_experiments=multiple_experiments,
        experiment_number=experiment_number,
        experiment_text=experiment_text,
    )
    return RedirectResponse(url=f"/survey/{queued.task_id}", status_code=302)


@router.post("/compare", name="compare_post")
async def compare_post(
    request: Request,
    parser_choice: str = Form(...),
    client: str = Form(...),
    reasoning_effort: str | None = Form(None),
    append_previous_output: str = Form("no"),
    multiple_experiments: str = Form("no"),
    experiment_number: str | None = Form(None),
    experiment_text: str | None = Form(None),
    clinical_registration: str = Form("no"),
    registration_id: str | None = Form(None),
    preregistration: UploadFile | None = File(None),
    paper: UploadFile | None = File(None),
    dimensions_data: str = Form(...),
):
    comparison_type: ComparisonType = (
        "clinical_trials" if _bool_from_yes(clinical_registration) else "general_preregistration"
    )
    return await _queue_comparison(
        request,
        comparison_type=comparison_type,
        parser_choice=parser_choice,
        client=client,
        reasoning_effort=reasoning_effort,
        append_previous_output=append_previous_output,
        multiple_experiments=multiple_experiments,
        experiment_number=experiment_number,
        experiment_text=experiment_text,
        registration_id=registration_id,
        preregistration=preregistration,
        paper=paper,
        dimensions_data=dimensions_data,
    )


@router.post("/clinical_trials")
async def clinical_trials_post(
    request: Request,
    parser_choice: str = Form(...),
    client: str = Form(...),
    reasoning_effort: str | None = Form(None),
    append_previous_output: str = Form("no"),
    registration_id: str = Form(...),
    paper: UploadFile = File(...),
    dimensions_data: str = Form(...),
):
    return await _queue_comparison(
        request,
        comparison_type="clinical_trials",
        parser_choice=parser_choice,
        client=client,
        reasoning_effort=reasoning_effort,
        append_previous_output=append_previous_output,
        registration_id=registration_id,
        paper=paper,
        dimensions_data=dimensions_data,
    )


@router.post("/general_preregistration")
async def general_preregistration_post(
    request: Request,
    parser_choice: str = Form(...),
    client: str = Form(...),
    reasoning_effort: str | None = Form(None),
    append_previous_output: str = Form("no"),
    multiple_experiments: str = Form("no"),
    experiment_number: str | None = Form(None),
    experiment_text: str | None = Form(None),
    preregistration: UploadFile = File(...),
    paper: UploadFile = File(...),
    dimensions_data: str = Form(...),
):
    return await _queue_comparison(
        request,
        comparison_type="general_preregistration",
        parser_choice=parser_choice,
        client=client,
        reasoning_effort=reasoning_effort,
        append_previous_output=append_previous_output,
        multiple_experiments=multiple_experiments,
        experiment_number=experiment_number,
        experiment_text=experiment_text,
        preregistration=preregistration,
        paper=paper,
        dimensions_data=dimensions_data,
    )


@router.post("/animals_trials")
async def animals_trials_post(
    request: Request,
    parser_choice: str = Form(...),
    client: str = Form(...),
    reasoning_effort: str | None = Form(None),
    append_previous_output: str = Form("no"),
    registration_id: str = Form(...),
    paper: UploadFile = File(...),
    registration_csv: UploadFile | None = File(None),
    dimensions_data: str = Form(...),
):
    return await _queue_comparison(
        request,
        comparison_type="animals_trials",
        parser_choice=parser_choice,
        client=client,
        reasoning_effort=reasoning_effort,
        append_previous_output=append_previous_output,
        registration_id=registration_id,
        paper=paper,
        registration_csv=registration_csv,
        dimensions_data=dimensions_data,
    )
