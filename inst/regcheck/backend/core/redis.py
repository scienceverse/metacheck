from __future__ import annotations

from redis import asyncio as aioredis


def create_redis_client(
    redis_url: str,
    *,
    socket_connect_timeout: float | None = 5,
    socket_timeout: float | None = 5,
):
    """Create an asyncio Redis client from a configured URL."""
    kwargs = {
        "decode_responses": True,
        # Make connection issues fail fast and allow redis-py to retry on timeouts.
        "socket_connect_timeout": socket_connect_timeout,
        # NOTE: For blocking commands (e.g. BRPOPLPUSH), socket_timeout must be
        # greater than the command's server-side timeout to avoid false timeouts.
        "socket_timeout": socket_timeout,
        "retry_on_timeout": True,
        # Keep connections warm to reduce first-request failures after idling.
        "health_check_interval": 30,
    }
    if redis_url.startswith("rediss://"):
        return aioredis.from_url(redis_url, ssl_cert_reqs=None, **kwargs)
    return aioredis.from_url(redis_url, **kwargs)
