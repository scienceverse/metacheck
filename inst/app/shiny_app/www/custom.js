$(function() {
  // Surface Shiny's built-in "Maximum upload size exceeded" error prominently.
  // Shiny renders it as small red text under the file input (#upload_pdf_progress
  // gets the 'shiny-file-input-progress' bar with an 'active'/error state). We
  // watch the progress element's text for the size error and show a clear,
  // styled banner instead of the easy-to-miss default.
  $(document).on('shiny:inputchanged shiny:value', function() {});
  var lastShown = 0;
  function checkUploadError() {
    var $bar = $('#upload_pdf_progress');
    var txt = ($bar.text() || '').toLowerCase();
    if (txt.indexOf('maximum upload size') !== -1 ||
        txt.indexOf('exceeded') !== -1) {
      var now = Date.now();
      if (now - lastShown > 2000) {           // debounce
        lastShown = now;
        var msg = 'Your file is too large to upload. Please choose a smaller ' +
                  'PDF (the limit is shown above the upload button).';
        $('#upload_pdf_clientmsg').remove();
        $('<div id="upload_pdf_clientmsg" class="upload-size-error"></div>')
          .text(msg)
          .insertAfter($('.pdf-upload'));
      }
    }
  }
  // The progress bar text updates via DOM mutations; poll briefly after a change.
  $(document).on('change', '#upload_pdf', function() {
    var n = 0, iv = setInterval(function() {
      checkUploadError();
      if (++n > 20) clearInterval(iv);        // ~4s of polling
    }, 200);
  });

  closeBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (!box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  openBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  $('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box')
           .find('[data-widget=collapse]')
           .click();
  });

});
