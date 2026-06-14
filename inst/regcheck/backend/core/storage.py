from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import IO, Optional


@dataclass(frozen=True, slots=True)
class S3Config:
    bucket: str
    region: str | None


def get_s3_config() -> S3Config | None:
    bucket = (os.environ.get("S3_BUCKET") or "").strip()
    if not bucket:
        return None
    region = (os.environ.get("AWS_REGION") or os.environ.get("AWS_DEFAULT_REGION") or "").strip() or None
    return S3Config(bucket=bucket, region=region)


def _s3_client(region: str | None):
    import boto3

    kwargs = {}
    if region:
        kwargs["region_name"] = region
    return boto3.client("s3", **kwargs)


def s3_upload_fileobj(
    cfg: S3Config,
    *,
    key: str,
    fileobj: IO[bytes],
    content_type: str | None = None,
) -> None:
    client = _s3_client(cfg.region)
    extra_args = {}
    if content_type:
        extra_args["ContentType"] = content_type
    if extra_args:
        client.upload_fileobj(fileobj, cfg.bucket, key, ExtraArgs=extra_args)
    else:
        client.upload_fileobj(fileobj, cfg.bucket, key)


def s3_download_to_path(cfg: S3Config, *, key: str, path: str) -> None:
    client = _s3_client(cfg.region)
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    client.download_file(cfg.bucket, key, str(target))


def s3_delete_key(cfg: S3Config, *, key: str) -> None:
    client = _s3_client(cfg.region)
    client.delete_object(Bucket=cfg.bucket, Key=key)


def guess_content_type(path: str) -> Optional[str]:
    suffix = Path(path).suffix.lower()
    if suffix == ".pdf":
        return "application/pdf"
    if suffix == ".docx":
        return "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    if suffix == ".txt":
        return "text/plain"
    if suffix == ".csv":
        return "text/csv"
    return None

