from __future__ import annotations

import os
import pickle
import logging
import re
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Sequence, Any

import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

logger = logging.getLogger(__name__)

try:  # pragma: no cover - optional dependency
    import nltk
except ModuleNotFoundError:  # pragma: no cover - graceful fallback
    nltk = None
else:  # pragma: no cover
    nltk.data.path.append("./nltk_data")

try:  # pragma: no cover - optional dependency
    import tiktoken
except ModuleNotFoundError:  # pragma: no cover - graceful fallback
    tiktoken = None

_SENTENCE_SPLIT_PATTERN = re.compile(r"(?:\n{2,}|(?<=[.!?])\s+)")


def _fallback_sentence_split(text: str) -> list[str]:
    cleaned = (text or "").strip()
    if not cleaned:
        return []
    return [part.strip() for part in _SENTENCE_SPLIT_PATTERN.split(cleaned) if part.strip()]


@lru_cache(maxsize=1)
def _ensure_nltk_sentence_tokenizer() -> None:
    if nltk is None:  # pragma: no cover - optional dependency
        return

    data_dir = Path("./nltk_data")
    try:
        data_dir.mkdir(parents=True, exist_ok=True)
    except OSError:
        return

    data_path = str(data_dir)
    if data_path not in nltk.data.path:
        nltk.data.path.append(data_path)

    for resource in ("punkt_tab", "punkt"):
        try:
            nltk.download(resource, download_dir=data_path, quiet=True)
        except Exception:
            continue


def _get_tokenizer(model_name: str = "text-embedding-3-large"):
    if tiktoken is None:
        raise RuntimeError("tiktoken is required for token-based chunking")
    return tiktoken.encoding_for_model(model_name)


def extract_chunks_tokens(
    text: str,
    max_chunk_tokens: int = 300,
    encoding_name: str = "text-embedding-3-large",
) -> list[str]:
    """Token-based chunking that keeps chunk boundaries on sentence edges when possible."""
    tokenizer = _get_tokenizer(encoding_name)
    sentences: list[str]
    if nltk is None:
        sentences = _fallback_sentence_split(text)
    else:
        from nltk.tokenize import sent_tokenize

        try:
            sentences = sent_tokenize(text)
        except LookupError:
            _ensure_nltk_sentence_tokenizer()
            try:
                sentences = sent_tokenize(text)
            except Exception:
                sentences = _fallback_sentence_split(text)
    chunks: list[str] = []
    current: list[str] = []
    current_tokens = 0

    for sentence in sentences:
        sent_tokens = tokenizer.encode(sentence)
        sent_len = len(sent_tokens)

        # If a single sentence exceeds the limit, break it into token-sized slices so no chunk
        # sent to the embeddings API is over the max context.
        if sent_len > max_chunk_tokens:
            if current:
                chunks.append(" ".join(current).strip())
                current = []
                current_tokens = 0
            for start in range(0, sent_len, max_chunk_tokens):
                slice_tokens = sent_tokens[start : start + max_chunk_tokens]
                try:
                    piece = tokenizer.decode(slice_tokens)
                except Exception:
                    piece = " ".join(sentence.split())  # fallback without token decode
                if piece.strip():
                    chunks.append(piece.strip())
            continue

        if current_tokens + sent_len <= max_chunk_tokens:
            current.append(sentence)
            current_tokens += sent_len
        else:
            if current:
                chunks.append(" ".join(current).strip())
            current = [sentence]
            current_tokens = sent_len

    if current:
        chunks.append(" ".join(current).strip())

    # Remove empties
    return [c for c in chunks if c]

def tfidf_embed_text(text: str) -> tuple[list[str], np.ndarray, TfidfVectorizer]:
    """Segment text and produce TF-IDF embeddings — no API key required.

    Uses the same token-based chunker as the OpenAI path so segment boundaries
    are consistent regardless of which embedding backend is active.
    """
    segments = extract_chunks_tokens(text)
    if not segments:
        segments = [text] if text.strip() else [""]
    vectorizer = TfidfVectorizer(ngram_range=(1, 2), sublinear_tf=True)
    try:
        matrix = vectorizer.fit_transform(segments)
        return segments, matrix.toarray().astype(np.float32), vectorizer
    except ValueError:
        # Empty vocabulary (empty or stopword-only text) — return zero embeddings
        return segments, np.zeros((len(segments), 1), dtype=np.float32), vectorizer


def tfidf_embed_query(query: str, vectorizer: TfidfVectorizer) -> np.ndarray:
    """Transform a single query string using an already-fitted TF-IDF vectorizer."""
    if not getattr(vectorizer, "vocabulary_", None):
        return np.zeros(1, dtype=np.float32)
    return vectorizer.transform([query]).toarray()[0].astype(np.float32)


def openai_embed_segments(segments: Sequence[str], model: str = "text-embedding-3-large") -> np.ndarray:
    from openai import OpenAI

    client = OpenAI()
    max_batch = 2048
    embeddings: list[list[float]] = []
    for start in range(0, len(segments), max_batch):
        batch = segments[start : start + max_batch]
        response = client.embeddings.create(input=list(batch), model=model)
        embeddings.extend(d.embedding for d in response.data)
    return np.asarray(embeddings, dtype=np.float32)


def openai_embed_text(
    text: str,
    model: str = "text-embedding-3-large",
    *,
    max_chunk_tokens: int = 300,
) -> tuple[list[str], np.ndarray]:
    segments = extract_chunks_tokens(text, max_chunk_tokens=max_chunk_tokens, encoding_name=model)
    embeddings = openai_embed_segments(segments, model=model)
    return list(segments), embeddings


def ollama_embed_segments(
    segments: Sequence[str],
    model: str = "nomic-embed-text-v2-moe",
    base_url: str | None = None,
) -> np.ndarray:
    from openai import OpenAI

    client = OpenAI(api_key="ollama", base_url=base_url or _ollama_base_url())
    max_batch = 2048
    embeddings: list[list[float]] = []
    for start in range(0, len(segments), max_batch):
        batch = segments[start : start + max_batch]
        response = client.embeddings.create(input=list(batch), model=model)
        embeddings.extend(d.embedding for d in response.data)
    return np.asarray(embeddings, dtype=np.float32)


def ollama_embed_text(
    text: str,
    model: str = "nomic-embed-text-v2-moe",
    base_url: str | None = None,
    *,
    max_chunk_tokens: int = 300,
) -> tuple[list[str], np.ndarray]:
    # Use the standard tiktoken tokenizer for chunk sizing
    segments = extract_chunks_tokens(text, max_chunk_tokens=max_chunk_tokens, encoding_name="text-embedding-3-large")
    embeddings = ollama_embed_segments(segments, model=model, base_url=base_url)
    return list(segments), embeddings


def save_embeddings(segments: Sequence[str], embeddings: np.ndarray, path: str) -> None:
    with open(path, "wb") as handle:
        pickle.dump(
            {
                "segments": list(segments),
                "embeddings": np.asarray(embeddings, dtype=np.float32),
            },
            handle,
        )


def load_embeddings(path: str) -> tuple[list[str], np.ndarray]:
    with open(path, "rb") as handle:
        data = pickle.load(handle)
    return list(data["segments"]), np.asarray(data["embeddings"], dtype=np.float32)


def _coerce_embeddings_matrix(embeddings: np.ndarray, segment_count: int) -> np.ndarray:
    """Ensure embeddings are a 2D float32 matrix with shape (n_segments, dim)."""
    arr = np.asarray(embeddings, dtype=np.float32)
    if arr.ndim == 0:
        return arr.reshape(0, 0)
    if arr.ndim == 1:
        if arr.size == 0:
            return arr.reshape(0, 0)
        if segment_count <= 1:
            return arr.reshape(1, -1)
        if arr.size % segment_count == 0:
            return arr.reshape(segment_count, -1)
        return arr.reshape(1, -1)
    if arr.ndim == 2:
        return arr
    return arr.reshape(arr.shape[0], -1)


def retrieve_relevant_chunks(
    query_embedding: np.ndarray,
    corpus: "EmbeddingCorpus",
    top_k: int | None = None,
    threshold: float | None = None,
) -> list[tuple[str, str, float]]:
    if top_k is None and threshold is None:
        raise ValueError("At least one of 'top_k' or 'threshold' must be specified.")

    if not isinstance(query_embedding, np.ndarray):
        query_embedding = np.asarray(query_embedding, dtype=np.float32)
    qvec = query_embedding.astype(np.float32, copy=False).reshape(-1)
    qnorm = float(np.linalg.norm(qvec)) or 1.0

    embeddings = corpus.embeddings
    if embeddings.size == 0:
        return []
    # cosine similarity: (E·q) / (||E|| * ||q||)
    sims = (embeddings @ qvec) / (corpus.norms * qnorm)

    if threshold is not None:
        idx = np.flatnonzero(sims >= threshold)
    else:
        idx = np.arange(sims.shape[0])

    if idx.size == 0:
        return []

    if top_k is not None and idx.size > top_k:
        # argpartition yields top_k indices in arbitrary order; then sort by score desc.
        local = np.argpartition(sims[idx], -top_k)[-top_k:]
        idx = idx[local]
    # Sort by similarity descending
    idx = idx[np.argsort(sims[idx])[::-1]]

    return [
        (corpus.chunk_ids[int(i)], corpus.segments[int(i)], float(sims[int(i)]))
        for i in idx
    ]


def get_top_k_segments_openai(
    segments: Sequence[str],
    embeddings: np.ndarray,
    query: str,
    k: int,
    model: str = "text-embedding-3-large",
) -> list[str]:
    from numpy.linalg import norm

    query_embedding = openai_embed_segments([query], model=model)[0]
    scores = np.array(
        [np.dot(embedding, query_embedding) / (norm(embedding) * norm(query_embedding)) for embedding in embeddings]
    )
    top_indices = np.argsort(-scores)[:k]
    return [segments[index] for index in top_indices]


def get_embedding(text: str, model: str = "text-embedding-3-large"):
    return openai_embed_segments([text], model=model)[0]


def count_tokens(text: str, model_name: str = "gpt-4o") -> int:
    if tiktoken is None:
        raise RuntimeError("tiktoken is required for token counting")
    encoding = tiktoken.encoding_for_model(model_name)
    return len(encoding.encode(text))


@dataclass(slots=True)
class EmbeddingCorpus:
    segments: list[str]
    embeddings: np.ndarray
    chunk_ids: list[str]
    norms: np.ndarray
    vectorizer: Any = None # populated only in TF-IDF mode, none when using OpenAI embeddings

def _openai_key_available() -> bool:
    return bool(os.environ.get("OPENAI_API_KEY", "").strip())


def _ollama_embedding_available() -> bool:
    return bool(os.environ.get("OLLAMA_EMBEDDING_MODEL", "").strip())


def _ollama_embedding_model() -> str:
    return os.environ.get("OLLAMA_EMBEDDING_MODEL", "nomic-embed-text").strip()


def _ollama_base_url() -> str:
    return os.environ.get("OLLAMA_BASE_URL", "http://localhost:11434/v1").strip()


def build_corpus(
    text: str,
    model: str = "text-embedding-3-large",
    embeddings_path: str | None = None,
    chunk_prefix: str | None = None,
    max_segments: int | None = None,
    max_chunk_tokens: int = 300,
) -> EmbeddingCorpus:
    segments: list[str]
    embeddings: np.ndarray
    vectorizer: Any = None

    if _openai_key_available():
        # OpenAI path (supports cached embeddings on disk)
        if embeddings_path and os.path.exists(embeddings_path):
            segments, embeddings = load_embeddings(embeddings_path)
        else:
            segments, embeddings = openai_embed_text(text, model=model, max_chunk_tokens=max_chunk_tokens)
            if embeddings_path:
                save_embeddings(segments, embeddings, embeddings_path)
    elif _ollama_embedding_available():
        # Ollama path — dense embeddings via local model, no API key required
        if embeddings_path and os.path.exists(embeddings_path):
            segments, embeddings = load_embeddings(embeddings_path)
        else:
            segments, embeddings = ollama_embed_text(
                text,
                model=_ollama_embedding_model(),
                max_chunk_tokens=max_chunk_tokens,
            )
            if embeddings_path:
                save_embeddings(segments, embeddings, embeddings_path)
    else:
        # TF-IDF fallback — no external service required
        segments, embeddings, vectorizer = tfidf_embed_text(text)

    embeddings = _coerce_embeddings_matrix(embeddings, len(segments))
    if embeddings.shape[0] != len(segments):
        min_rows = min(len(segments), int(embeddings.shape[0]))
        if min_rows == 0:
            segments = []
            embeddings = embeddings[:0]
        else:
            logger.warning(
                "Embedding corpus row mismatch; truncating",
                extra={"segment_count": len(segments), "embedding_rows": int(embeddings.shape[0]), "keep": min_rows},
            )
            segments = list(segments[:min_rows])
            embeddings = embeddings[:min_rows]

    if max_segments is not None and max_segments > 0 and len(segments) > max_segments:
        segments = segments[:max_segments]
        embeddings = embeddings[:max_segments]

    chunk_ids = [
        f"{(chunk_prefix or 'CHUNK').upper()}_{i+1:04d}" for i in range(len(segments))
    ]
    if embeddings.size == 0:
        norms = np.empty((0,), dtype=np.float32)
    else:
        # Final defensive reshape to avoid AxisError if earlier coercion missed anything.
        if embeddings.ndim == 1:
            embeddings = embeddings.reshape(1, -1)
        elif embeddings.ndim > 2:
            embeddings = embeddings.reshape(embeddings.shape[0], -1)
        try:
            norms = np.linalg.norm(embeddings, axis=1).astype(np.float32, copy=False)
        except np.AxisError:
            embeddings = np.atleast_2d(embeddings)
            norms = np.linalg.norm(embeddings, axis=1).astype(np.float32, copy=False)
        norms[norms == 0] = 1.0
    return EmbeddingCorpus(
        segments=list(segments),
        embeddings=np.asarray(embeddings, dtype=np.float32),
        chunk_ids=list(chunk_ids),
        norms=norms,
        vectorizer=vectorizer,
    )
