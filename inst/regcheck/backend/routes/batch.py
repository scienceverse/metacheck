from __future__ import annotations

import asyncio
import csv
import io
import json
import logging
import re
import uuid
import zipfile
from pathlib import Path

import fitz
from fastapi import APIRouter, File, Form, HTTPException, Request, UploadFile
from fastapi.responses import JSONResponse, RedirectResponse, Response

from ..services.comparisons import clinical_trial_comparison
from ..services.dimensions import default_dimension_sets
from .comparisons import (
    _bool_from_yes,
    _normalize_parser_choice,
    _normalize_reasoning_effort,
    _parse_dimensions,
    _safe_filename,
    _store_upload,
)

router = APIRouter()
logger = logging.getLogger(__name__)

_NCT_PATTERN = re.compile(r"\bNCT\d{8}\b", re.IGNORECASE)

_CSV_FIELDS = [
    "dimension",
    "paper_content_quotes",
    "paper_content_summary",
    "registration_content_quotes",
    "registration_content_summary",
    "deviation_judgement",
    "deviation_information",
]


def _extract_nct_id(pdf_path: str) -> str | None:
    try:
        with fitz.open(pdf_path) as doc:
            for page in doc:
                m = _NCT_PATTERN.search(page.get_text())
                if m:
                    return m.group(0).upper()
    except Exception as exc:
        logger.warning("Could not extract NCT ID from %s: %s", pdf_path, exc)
    return None


def _result_to_csv_bytes(result_json: str) -> bytes:
    payload = json.loads(result_json)
    items = payload.get("items", []) if isinstance(payload, dict) else []
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=_CSV_FIELDS)
    writer.writeheader()
    for item in items:
        if not isinstance(item, dict):
            continue
        writer.writerow({k: "" if item.get(k) is None else str(item.get(k, "")) for k in _CSV_FIELDS})
    return buf.getvalue().encode("utf-8")


async def _run_batch(
    batch_id: str,
    papers: list[tuple[str, str, str]],
    client: str,
    parser_choice: str,
    reasoning_effort: str | None,
    append_previous: bool,
    selected_dimensions: list[dict[str, str]],
    redis_client,
) -> None:
    """Process all papers sequentially and update batch state in Redis."""
    await redis_client.hset(f"batch:{batch_id}", mapping={"state": "RUNNING"})

    for i, (paper_path, _filename, file_ext) in enumerate(papers):
        paper_key = f"batch:{batch_id}:p:{i}"
        await redis_client.hset(paper_key, mapping={"state": "RUNNING"})

        nct_id = _extract_nct_id(paper_path)
        if not nct_id:
            await redis_client.hset(paper_key, mapping={
                "nct_id": "",
                "state": "SKIPPED",
                "error": "No NCT ID found in PDF",
            })
            await redis_client.hincrby(f"batch:{batch_id}", "failed", 1)
            continue

        await redis_client.hset(paper_key, mapping={"nct_id": nct_id})
        try:
            result = await clinical_trial_comparison(
                nct_id,
                paper_path,
                file_ext,
                client,
                parser_choice=parser_choice,
                reasoning_effort=reasoning_effort,
                selected_dimensions=selected_dimensions,
                append_previous_output=append_previous,
            )
            await redis_client.hset(paper_key, mapping={
                "state": "SUCCESS",
                "result_json": json.dumps(result.model_dump()),
            })
            await redis_client.hincrby(f"batch:{batch_id}", "completed", 1)
        except Exception as exc:
            logger.error("Batch %s paper %d failed: %s", batch_id, i, exc, exc_info=True)
            await redis_client.hset(paper_key, mapping={
                "state": "FAILED",
                "error": str(exc)[:500],
            })
            await redis_client.hincrby(f"batch:{batch_id}", "failed", 1)

    await redis_client.hset(f"batch:{batch_id}", mapping={"state": "COMPLETE"})
    logger.info("Batch %s complete", batch_id)


@router.get("/batch", name="batch")
async def batch_get(request: Request):
    """Render the batch upload form."""
    templates = request.app.state.templates
    return templates.TemplateResponse(
        "batch.html",
        {
            "request": request,
            "default_dimension_sets": default_dimension_sets(),
        },
    )


@router.post("/batch")
async def batch_post(
    request: Request,
    client: str = Form(...),
    parser_choice: str = Form(...),
    reasoning_effort: str | None = Form(None),
    append_previous_output: str = Form("no"),
    dimensions_data: str = Form(...),
    papers: list[UploadFile] = File(...),
):
    """Accept multiple PDFs for batch clinical trial comparison.

    Saves all uploads, initialises per-paper state in Redis, and launches an
    async background task that processes papers sequentially. Redirects to
    the batch progress page once the task is queued.
    """
    if not papers or all(not p.filename for p in papers):
        raise HTTPException(status_code=400, detail="At least one paper PDF is required")

    settings = request.app.state.settings
    upload_dir = Path(settings.upload_dir)
    upload_dir.mkdir(parents=True, exist_ok=True)

    selected_dimensions = _parse_dimensions(dimensions_data)
    append_prev = _bool_from_yes(append_previous_output)
    parser_norm = _normalize_parser_choice(parser_choice)
    effort_norm = _normalize_reasoning_effort(client, reasoning_effort)

    batch_id = str(uuid.uuid4())

    saved: list[tuple[str, str, str]] = []
    for i, paper in enumerate(papers):
        filename = _safe_filename(paper.filename)
        destination = upload_dir / f"{batch_id}_paper_{i}_{filename}"
        await _store_upload(destination, paper)
        saved.append((str(destination), filename, Path(filename).suffix.lower()))

    redis_client = request.app.state.redis
    await redis_client.hset(f"batch:{batch_id}", mapping={
        "state": "PENDING",
        "total": len(saved),
        "completed": 0,
        "failed": 0,
    })
    for i, (_, filename, _ext) in enumerate(saved):
        await redis_client.hset(f"batch:{batch_id}:p:{i}", mapping={
            "filename": filename,
            "nct_id": "",
            "state": "PENDING",
            "error": "",
        })

    background_tasks: set = request.app.state.background_tasks
    task = asyncio.create_task(
        _run_batch(
            batch_id=batch_id,
            papers=saved,
            client=client,
            parser_choice=parser_norm,
            reasoning_effort=effort_norm,
            append_previous=append_prev,
            selected_dimensions=selected_dimensions,
            redis_client=redis_client,
        )
    )
    background_tasks.add(task)
    task.add_done_callback(background_tasks.discard)

    return RedirectResponse(url=f"/batch/{batch_id}", status_code=302)


@router.get("/batch/{batch_id}", name="batch_progress")
async def batch_progress_get(request: Request, batch_id: str):
    """Render the per-batch progress page."""
    templates = request.app.state.templates
    return templates.TemplateResponse(
        "batch_progress.html",
        {
            "request": request,
            "batch_id": batch_id,
        },
    )


@router.get("/batch_json/{batch_id}")
async def batch_json(request: Request, batch_id: str):
    """Return JSON status for a batch (polled by the progress page)."""
    redis_client = request.app.state.redis
    meta = await redis_client.hgetall(f"batch:{batch_id}")
    if not meta:
        return JSONResponse({"error": "Batch not found"}, status_code=404)

    def _d(v: object) -> str:
        return v.decode() if isinstance(v, bytes) else str(v or "")

    total = int(_d(meta.get("total")) or 0)
    papers = []
    for i in range(total):
        p = await redis_client.hgetall(f"batch:{batch_id}:p:{i}")
        if p:
            papers.append({
                "filename": _d(p.get("filename")),
                "nct_id": _d(p.get("nct_id")),
                "state": _d(p.get("state")),
                "error": _d(p.get("error")),
            })

    return JSONResponse({
        "state": _d(meta.get("state")),
        "total": total,
        "completed": int(_d(meta.get("completed")) or 0),
        "failed": int(_d(meta.get("failed")) or 0),
        "papers": papers,
    })


@router.get("/batch_download/{batch_id}")
async def batch_download(request: Request, batch_id: str):
    """Generate and stream a ZIP of all successful result CSVs in a batch."""
    redis_client = request.app.state.redis
    meta = await redis_client.hgetall(f"batch:{batch_id}")
    if not meta:
        raise HTTPException(status_code=404, detail="Batch not found")

    def _d(v: object) -> str:
        return v.decode() if isinstance(v, bytes) else str(v or "")

    if _d(meta.get("state")) != "COMPLETE":
        raise HTTPException(status_code=400, detail="Batch is not yet complete")

    total = int(_d(meta.get("total")) or 0)
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as zf:
        for i in range(total):
            p = await redis_client.hgetall(f"batch:{batch_id}:p:{i}")
            if not p or _d(p.get("state")) != "SUCCESS":
                continue
            filename = _d(p.get("filename"))
            nct_id = _d(p.get("nct_id"))
            result_json = _d(p.get("result_json"))
            try:
                csv_bytes = _result_to_csv_bytes(result_json)
            except Exception as exc:
                logger.warning(
                    "Failed to convert result to CSV for batch %s paper %d (%s): %s",
                    batch_id, i, filename, exc
                )
                continue
            stem = re.sub(r"[^\w\-]", "_", Path(filename).stem)
            zf.writestr(f"{nct_id}_{stem}.csv", csv_bytes)

    zip_name = f"batch_{batch_id[:8]}_results.zip"
    return Response(
        content=buf.getvalue(),
        media_type="application/zip",
        headers={"Content-Disposition": f"attachment; filename={zip_name}"},
    )
