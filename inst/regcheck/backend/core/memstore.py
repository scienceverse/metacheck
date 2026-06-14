"""
In-memory replacement for Redis — sufficient for single-process local use.

Implements the async subset of the redis-py interface used by regcheck:
hset, hgetall, expire, llen, ping.  Task state is lost when the server
stops, which is fine for interactive local use.
"""
from __future__ import annotations

import asyncio
import time
from typing import Any


class MemStore:
    def __init__(self) -> None:
        self._hashes: dict[str, dict[str, Any]] = {}
        self._expiry: dict[str, float] = {}
        self._lists: dict[str, list] = {}

    def _expired(self, key: str) -> bool:
        exp = self._expiry.get(key)
        return exp is not None and time.monotonic() > exp

    def _clean(self, key: str) -> None:
        if self._expired(key):
            self._hashes.pop(key, None)
            self._lists.pop(key, None)
            self._expiry.pop(key, None)

    async def ping(self) -> bool:
        return True

    async def hset(self, key: str, mapping: dict | None = None, **kwargs) -> int:
        self._clean(key)
        if key not in self._hashes:
            self._hashes[key] = {}
        if mapping:
            self._hashes[key].update(mapping)
        if kwargs:
            self._hashes[key].update(kwargs)
        return 1

    async def hgetall(self, key: str) -> dict:
        self._clean(key)
        return dict(self._hashes.get(key, {}))

    async def expire(self, key: str, seconds: int) -> int:
        self._expiry[key] = time.monotonic() + seconds
        return 1

    async def llen(self, key: str) -> int:
        self._clean(key)
        return len(self._lists.get(key, []))

    async def rpush(self, key: str, *values) -> int:
        self._clean(key)
        if key not in self._lists:
            self._lists[key] = []
        self._lists[key].extend(values)
        return len(self._lists[key])

    async def get(self, key: str) -> Any:
        return None

    async def set(self, key: str, value: Any, ex: int | None = None) -> bool:
        return True

    async def delete(self, *keys: str) -> int:
        return 0

    async def lrange(self, key: str, start: int, end: int) -> list:
        return []

    async def lrem(self, key: str, count: int, value: Any) -> int:
        return 0


_store: MemStore | None = None


def get_memstore() -> MemStore:
    global _store
    if _store is None:
        _store = MemStore()
    return _store
