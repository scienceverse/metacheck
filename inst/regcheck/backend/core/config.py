from __future__ import annotations

import os
import secrets
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path

from dotenv import load_dotenv


@dataclass(slots=True)
class Settings:
    """Application configuration values loaded from the environment."""

    redis_url: str
    session_secret: str
    api_token: str | None
    task_ttl_seconds: int
    max_queue_length: int
    static_dir: str
    templates_dir: str
    upload_dir: str

    def ensure_directories(self) -> None:
        """Ensure that directories required by the application exist."""
        Path(self.upload_dir).mkdir(parents=True, exist_ok=True)


@lru_cache()
def get_settings() -> Settings:
    """Return cached application settings."""
    load_dotenv()

    base_dir = Path(__file__).resolve().parents[2]
    require_session_secret = (os.environ.get("REQUIRE_SESSION_SECRET") or "").strip().lower() in {
        "1",
        "true",
        "yes",
    }
    redis_url = (
        os.environ.get("REDIS_TLS_URL")
        or os.environ.get("REDIS_URL")
        or os.environ.get("HEROKU_REDIS_OLIVE_TLS_URL")
        or os.environ.get("HEROKU_REDIS_OLIVE_URL")
        or os.environ.get("REDISCLOUD_URL")
        or os.environ.get("REDISGREEN_URL")
        or "redis://localhost:6379/0"
    )
    if os.environ.get("DYNO") and redis_url.startswith("redis://localhost"):
        raise RuntimeError("REDIS_URL/REDIS_TLS_URL must be set for production deployments.")
    session_secret_env = (os.environ.get("SESSION_SECRET") or "").strip()
    if (os.environ.get("DYNO") or require_session_secret) and not session_secret_env:
        raise RuntimeError("SESSION_SECRET must be set for production deployments.")
    session_secret = session_secret_env or secrets.token_urlsafe(32)
    api_token = (os.environ.get("REGCHECK_API_TOKEN") or "").strip() or None

    def _int_env(name: str, default: int, minimum: int = 1) -> int:
        raw = os.environ.get(name)
        if raw is None:
            return default
        try:
            parsed = int(str(raw).strip())
            return max(minimum, parsed)
        except (TypeError, ValueError):
            return default

    task_ttl_seconds = _int_env("TASK_TTL_SECONDS", 3 * 24 * 60 * 60, minimum=60)
    max_queue_length = _int_env("MAX_QUEUE_LENGTH", 200, minimum=1)
    static_dir = os.environ.get("STATIC_DIR", str(base_dir / "static"))
    templates_dir = os.environ.get("TEMPLATES_DIR", str(base_dir / "templates"))
    upload_dir = os.environ.get("UPLOAD_DIR", str(base_dir / "uploads"))

    settings = Settings(
        redis_url=redis_url,
        session_secret=session_secret,
        api_token=api_token,
        task_ttl_seconds=task_ttl_seconds,
        max_queue_length=max_queue_length,
        static_dir=static_dir,
        templates_dir=templates_dir,
        upload_dir=upload_dir,
    )
    settings.ensure_directories()
    return settings
