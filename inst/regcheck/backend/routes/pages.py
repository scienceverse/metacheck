from __future__ import annotations

from fastapi import APIRouter, Request
from fastapi.responses import HTMLResponse, RedirectResponse

from ..services.dimensions import default_dimension_sets

router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def index(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("index.html", {"request": request})


@router.get("/compare", response_class=HTMLResponse, name="compare")
async def compare(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse(
        "general_preregistration.html",
        {
            "request": request,
            "default_dimension_sets": default_dimension_sets(),
        },
    )


@router.get("/clinical_trials", response_class=HTMLResponse, name="clinical_trials")
async def clinical_trials_get(request: Request):
    return RedirectResponse(url=request.url_for("compare"), status_code=302)


@router.get(
    "/general_preregistration",
    response_class=HTMLResponse,
    name="general_preregistration",
)
async def general_preregistration_get(request: Request):
    return RedirectResponse(url=request.url_for("compare"), status_code=302)


@router.get("/contact", response_class=HTMLResponse)
async def contact(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("contact.html", {"request": request})


@router.get("/demo", name="demo")
async def demo(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("demo.html", {"request": request})


@router.get("/team", name="team")
async def team(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("team.html", {"request": request})


@router.get("/jobs", name="jobs")
async def jobs(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("jobs.html", {"request": request})


@router.get("/privacy", response_class=HTMLResponse, name="privacy")
async def privacy(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("privacy.html", {"request": request})


@router.get("/faq", response_class=HTMLResponse, name="faq")
async def faq(request: Request):
    templates = request.app.state.templates
    return templates.TemplateResponse("faq.html", {"request": request})
