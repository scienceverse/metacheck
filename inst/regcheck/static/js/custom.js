// reveals the experiment number question if yes is selected
// also updates the value of the preceding text
document.addEventListener('DOMContentLoaded', function () {
    const multipleExperimentsSelect = document.getElementById('multiple_experiments');
    const experimentNumberGroup = document.getElementById('experiment_number_group');
    const experimentNumberInput = document.getElementById('experiment_number');
    const experimentTextInput = document.getElementById('experiment_text'); // Hidden input
    let experimentText = "";

    multipleExperimentsSelect.addEventListener('change', function () {
        if (this.value === 'yes') { // Use lowercase 'yes' to match the HTML value
            experimentNumberGroup.style.display = 'block';
            updateExperimentText();
        } else {
            experimentNumberGroup.style.display = 'none';
            experimentText = "";
            experimentTextInput.value = ""; // Ensure hidden input is cleared
        }

        // Store the experimentText in a hidden input or somewhere accessible if needed in form submission
    });

    experimentNumberInput.addEventListener('input', function () {
        updateExperimentText();
    });

    function updateExperimentText() {
        if (multipleExperimentsSelect.value === 'yes') {
            const experimentNumber = experimentNumberInput.value;
            experimentText = experimentNumber ? 
                `This was a multistudy paper. The preregistration here pertains to Experiment ${experimentNumber} only.`  : 
                "";
        } else {
            experimentText = "";
        }

        experimentTextInput.value = experimentText; // Update the hidden input field
    }
});


// updates the span text for the names of the upload files
document.addEventListener('DOMContentLoaded', function () {
    function updateFileInputLabels() {
        document.querySelectorAll('.custom-file-input').forEach(input => {
            input.removeEventListener('change', handleFileInputChange);
            input.addEventListener('change', handleFileInputChange);
        });

        // Also fit current labels on load and on resize
        fitAllFileNameSpans();
        window.addEventListener('resize', fitAllFileNameSpans);
    }

    function handleFileInputChange(event) {
        const input = event.target;
        const fullName = input.files[0] ? input.files[0].name : 'No file chosen';
        const label = input.nextElementSibling;
        const fileNameSpan = label?.nextElementSibling;

        if (fileNameSpan && fileNameSpan.classList.contains('file-name')) {
            fileNameSpan.dataset.fullName = fullName; // keep original
            fileNameSpan.title = fullName; // show full name on hover
            applyFileNameFormatting(fileNameSpan, fullName);
        }
    }

    function fitAllFileNameSpans() {
        document.querySelectorAll('.custom-file-input').forEach(input => {
            const label = input.nextElementSibling;
            const fileNameSpan = label?.nextElementSibling;
            if (!fileNameSpan || !fileNameSpan.classList.contains('file-name')) return;
            // Skip if the span is not laid out yet
            if (fileNameSpan.clientWidth === 0) return;
            const fullName = input.files && input.files[0] ? input.files[0].name : (fileNameSpan.dataset.fullName || fileNameSpan.textContent || 'No file chosen');
            applyFileNameFormatting(fileNameSpan, fullName);
        });
    }

    function applyFileNameFormatting(span, fullName) {
        const computed = window.getComputedStyle(span);
        const storedBase = span.dataset.baseFontSize ? parseFloat(span.dataset.baseFontSize) : null;
        const basePx = storedBase || parseFloat(computed.fontSize) || 16;
        const minPx = span.dataset.minFontSize ? parseFloat(span.dataset.minFontSize) : 10;
        const shrinkStart = span.dataset.shrinkStart ? parseInt(span.dataset.shrinkStart, 10) : 22;
        const shrinkRate = span.dataset.shrinkRate ? parseFloat(span.dataset.shrinkRate) : 0.35;

        span.dataset.baseFontSize = basePx;

        let targetPx = basePx;
        if (fullName && fullName !== 'No file chosen') {
            const length = fullName.length;
            if (length > shrinkStart) {
                const shrinkAmount = (length - shrinkStart) * shrinkRate;
                targetPx = Math.max(minPx, basePx - shrinkAmount);
            }
        }

        span.style.fontSize = targetPx + 'px';
        span.textContent = fullName;

        if (span.clientWidth === 0) {
            return;
        }

        shrinkToFit(span, minPx, targetPx);

        if (span.scrollWidth > span.clientWidth) {
            const abbreviated = abbreviateMiddle(fullName);
            span.textContent = abbreviated;
            span.style.fontSize = targetPx + 'px';
            shrinkToFit(span, minPx, targetPx);
        }
    }

    function shrinkToFit(span, minPx = 10, startPx) {
        let current = typeof startPx === 'number' ? startPx : parseFloat(window.getComputedStyle(span).fontSize) || 16;
        span.style.fontSize = current + 'px';

        while (span.scrollWidth > span.clientWidth && current > minPx) {
            const next = Math.max(minPx, current - 0.5);
            if (next === current) {
                break;
            }
            current = next;
            span.style.fontSize = current + 'px';
        }

        return current;
    }

    function abbreviateMiddle(name) {
        if (!name || name === 'No file chosen') return name;

        const dot = name.lastIndexOf('.');
        let ext = '';
        let base = name;
        if (dot > 0) {
            ext = name.slice(dot); // includes dot
            base = name.slice(0, dot);
        }

        // Start with a reasonable budget; will be further shrink-to-fit
        const isMobile = window.matchMedia('(max-width: 600px)').matches;
        const budget = isMobile ? 24 : 36; // total visible chars not counting ext and ellipsis

        if (base.length <= budget) return name; // no need to abbreviate

        const keepEnd = Math.max(6, Math.min(10, Math.floor(budget / 3)));
        const keepStart = Math.max(6, budget - keepEnd);
        const head = base.slice(0, keepStart);
        const tail = base.slice(-keepEnd);
        return head + '...' + tail + ext;
    }

    // Make fitter available for pages that want to trigger it on step change
    window.fitAllFileNameSpans = fitAllFileNameSpans;

    // Initial call to set up event listeners and fit
    updateFileInputLabels();
});
