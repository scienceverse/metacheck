"""
Minimal metacheck-local entry point.

Differences from upstream:
- Redis replaced with in-memory MemStore (no Redis installation required)
- Web UI routes (pages, batch, survey, comparisons HTML) omitted
- Static file serving omitted
- SessionMiddleware omitted (no web UI sessions needed)
"""
from __future__ import annotations

import logging

from fastapi import FastAPI

from .core.config import get_settings
from .core.logging import configure_logging
from .core.memstore import get_memstore
from .routes import api, status


def create_app() -> FastAPI:
    settings = get_settings()
    configure_logging()

    app = FastAPI(title="RegCheck (local)")

    app.state.settings = settings
    app.state.redis = get_memstore()
    app.state.background_tasks: set = set()

    app.include_router(status.router)
    app.include_router(api.router)

    return app
