"""
Minimal worker for metacheck-local — uses MemStore instead of Redis.

Jobs are dispatched as asyncio background tasks directly from the API route.
When a job carries paper_text/prereg_text directly (the text endpoint path),
they are written to temp files so the existing comparison function can read them.
"""
from __future__ import annotations

import asyncio
import logging
import tempfile
from pathlib import Path
from typing import Any

from backend.core.config import get_settings
from backend.core.memstore import get_memstore
from backend.services.comparisons import (
    general_preregistration_comparison,
    run_with_concurrency_limit,
)

logger = logging.getLogger(__name__)


async def _dispatch_job(job: dict[str, Any]) -> None:
    store = get_memstore()
    settings = get_settings()
    task_id = job.get("task_id")

    tmp_paper = tmp_prereg = None
    try:
        # text endpoint: write texts to temp files so comparisons.py can read them
        paper_text = job.get("paper_text")
        prereg_text = job.get("prereg_text")

        if paper_text:
            tmp_paper = tempfile.NamedTemporaryFile(
                suffix=".txt", delete=False, mode="w", encoding="utf-8"
            )
            tmp_paper.write(paper_text)
            tmp_paper.flush()
            tmp_paper.close()
            paper_path = tmp_paper.name
            paper_ext = ".txt"
        else:
            paper_path = job.get("paper_path", "")
            paper_ext = job.get("paper_ext", "")

        if prereg_text:
            tmp_prereg = tempfile.NamedTemporaryFile(
                suffix=".txt", delete=False, mode="w", encoding="utf-8"
            )
            tmp_prereg.write(prereg_text)
            tmp_prereg.flush()
            tmp_prereg.close()
            prereg_path = tmp_prereg.name
            prereg_ext = ".txt"
        else:
            prereg_path = job.get("prereg_path", "")
            prereg_ext = job.get("prereg_ext", "")

        async def _runner() -> None:
            await general_preregistration_comparison(
                prereg_path,
                prereg_ext,
                paper_path,
                paper_ext,
                job.get("client", "ollama"),
                "grobid",  # parser_choice: unused for .txt files
                task_id,
                store,
                job.get("selected_dimensions"),
                append_previous_output=job.get("append_previous_output", False),
                reasoning_effort=job.get("reasoning_effort"),
                multiple_experiments=job.get("multiple_experiments"),
                experiment_number=job.get("experiment_number"),
                experiment_text=job.get("experiment_text"),
            )

        await run_with_concurrency_limit(_runner)

    except Exception as exc:
        logger.error("Job failed", exc_info=exc)
        if task_id:
            await store.hset(task_id, mapping={
                "state": "FAILURE",
                "status": f"Worker error: {exc}",
            })
            await store.expire(task_id, settings.task_ttl_seconds)
    finally:
        for tmp in (tmp_paper, tmp_prereg):
            if tmp is not None:
                try:
                    Path(tmp.name).unlink(missing_ok=True)
                except Exception:
                    pass
