from __future__ import annotations

import logging
import os
import sys
from typing import Any


def configure_logging(level: int | str | None = None, **kwargs: Any) -> None:
    """Configure application-wide logging."""
    if level is None:
        level = os.environ.get("LOG_LEVEL", "INFO")
    if isinstance(level, str):
        level_name = level.strip().upper()
        level = getattr(logging, level_name, logging.INFO)
    logging.basicConfig(
        level=level,
        format="%(asctime)s [%(levelname)s] %(message)s",
        handlers=[logging.StreamHandler(sys.stdout)],
        **kwargs,
    )
