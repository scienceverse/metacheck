from __future__ import annotations

import re
import uuid
from pathlib import Path

import docx
import fitz
from fpdf import FPDF

__all__ = [
    "extract_text_from_docx",
    "extract_text_from_pdf",
    "convert_txt_to_pdf",
    "clean_document_text",
    "read_file",
    "read_file_as_pdf",
]


UPLOADS_DIR = Path("uploads")


def extract_text_from_docx(file_path: str) -> str:
    """Extract plain text from a DOCX document."""
    document = docx.Document(file_path)
    return "\n".join(paragraph.text for paragraph in document.paragraphs)


def extract_text_from_pdf(file_path: str) -> str:
    """Extract text from all pages of a PDF document."""
    document = fitz.open(file_path)
    text = ""
    for page in document:
        text += page.get_text()
    return text


def convert_txt_to_pdf(txt_path: str) -> str:
    """Convert a TXT file into a PDF stored near the source file with a unique name."""
    pdf = FPDF()
    pdf.add_page()
    pdf.set_auto_page_break(auto=True, margin=15)
    pdf.set_font("Arial", size=12)

    with open(txt_path, "r", encoding="utf-8", errors="ignore") as txt_file:
        for line in txt_file:
            pdf.multi_cell(0, 10, line)

    stem = Path(txt_path).stem or "converted_txt"
    target_dir = Path(txt_path).parent if Path(txt_path).parent else UPLOADS_DIR
    output_path = target_dir / f"{stem}_{uuid.uuid4().hex}.pdf"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    pdf.output(str(output_path))
    return str(output_path)


REFERENCE_PATTERN = re.compile(
    r"(?:^|\n)([A-Z\s]*\bReferences\b|Bibliography|Cited Works)[\s]*\n",
    re.IGNORECASE,
)
INTRODUCTION_PATTERN = re.compile(
    r"(?:^|\n)([A-Z\s]*\bIntroduction\b)[\s]*\n", re.IGNORECASE
)


def remove_references(document_text: str) -> str:
    """Remove trailing references sections from a document."""
    match = REFERENCE_PATTERN.search(document_text)
    if match:
        return document_text[: match.start()]
    return document_text


def clean_document_text(document_text: str) -> str:
    """Trim boilerplate sections from parsed documents."""
    document_text = _normalize_whitespace(document_text)
    match = INTRODUCTION_PATTERN.search(document_text)
    if match:
        document_text = document_text[match.start() :]
    return remove_references(document_text)


def _read_txt(path: str) -> str:
    with open(path, "r", encoding="utf-8", errors="ignore") as handle:
        return handle.read()


def read_file(file_path: str, file_extension: str) -> str:
    """Read a document and normalise its text content."""
    file_extension = file_extension.lower()
    if file_extension == ".txt":
        text = _read_txt(file_path)
    elif file_extension == ".docx":
        text = extract_text_from_docx(file_path)
    elif file_extension == ".pdf":
        text = extract_text_from_pdf(file_path)
    else:
        raise ValueError("Unsupported file type")
    return clean_document_text(text)


def read_file_as_pdf(filename: str, file_extension: str) -> str:
    """Return a path to a PDF representation of the provided document."""
    file_extension = file_extension.lower()
    if file_extension == ".txt":
        return convert_txt_to_pdf(filename)
    if file_extension == ".docx":
        return filename
    if file_extension == ".pdf":
        return filename
    raise ValueError("Unsupported file type")


def _normalize_whitespace(text: str) -> str:
    """Normalize whitespace to reduce mid-quote line breaks without losing content."""
    if not isinstance(text, str):
        return text
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    # Drop standalone page numbers (lines that are only digits)
    text = re.sub(r"^\s*\d+\s*$", "", text, flags=re.MULTILINE)
    # Collapse consecutive blank lines to a single blank line
    text = re.sub(r"\n{3,}", "\n\n", text)
    text = re.sub(r"[ \t]+", " ", text)
    # Keep at most double newlines to preserve some paragraph structure
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()
