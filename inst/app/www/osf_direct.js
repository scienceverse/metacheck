// Fetch OSF archives straight from files.osf.io into the visitor's browser.
//
// The Shiny server never holds the files: it sends this page a list of OSF
// zip addresses, and the browser requests each one itself. The OSF allows
// this from another site (its preflight answers 204 with
// access-control-allow-origin set to the requesting page and
// access-control-allow-headers including Authorization), so a private
// project works too by sending the token from here.
//
// The token is only ever sent to files.osf.io, and only when one was entered.
$(function() {

  function saveBlob(blob, filename) {
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    // Give the browser a moment to start the save before releasing the data
    setTimeout(function() { URL.revokeObjectURL(url); }, 60000);
  }

  function report(state, id, message, received, total) {
    Shiny.setInputValue('direct_progress', {
      state: state,            // "start" | "done" | "error"
      id: id,
      message: message || '',
      received: received || 0,
      total: total || 0,
      at: Date.now()           // makes each report a new value
    });
  }

  // Read the response a chunk at a time so progress can be shown for an
  // archive the OSF builds on the fly and sends without a length.
  function fetchWithProgress(item, token) {
    // An Authorization header must ALWAYS be sent, even for a public project
    // with no token. The OSF returns Access-Control-Allow-Origin only on
    // requests that carry one, and a browser discards a cross-origin response
    // without that header, so omitting it makes public downloads fail
    // silently. A non-empty placeholder is accepted and still returns the
    // archive; an empty value is rejected with status 400.
    var headers = {
      'Authorization': 'Bearer ' + (token && token.length ? token : 'none')
    };

    report('start', item.id, item.name);

    return fetch(item.url, { headers: headers, mode: 'cors' })
      .then(function(resp) {
        if (!resp.ok) {
          throw new Error('the OSF answered ' + resp.status +
            (resp.status === 401 || resp.status === 403
              ? ' (not authorised - a private project needs a valid token)'
              : ''));
        }
        var total = parseInt(resp.headers.get('content-length') || '0', 10);
        if (!resp.body || !resp.body.getReader) {
          return resp.blob();   // older browser: no progress, still works
        }
        var reader = resp.body.getReader();
        var chunks = [];
        var received = 0;
        return (function read() {
          return reader.read().then(function(res) {
            if (res.done) return new Blob(chunks, { type: 'application/zip' });
            chunks.push(res.value);
            received += res.value.length;
            report('start', item.id, item.name, received, total);
            return read();
          });
        })();
      })
      .then(function(blob) {
        saveBlob(blob, item.filename);
        // Report the finished size so the server can add it to the anonymous
        // usage statistics. It is a byte count and nothing else.
        report('done', item.id, item.name, blob.size, blob.size);
      })
      .catch(function(err) {
        report('error', item.id, String(err.message || err));
      });
  }

  Shiny.addCustomMessageHandler('osfDirectDownload', function(msg) {
    var items = msg.items || [];
    var token = msg.token || '';
    // One at a time: several large archives at once would compete for
    // bandwidth and the OSF builds each one on demand anyway.
    items.reduce(function(chain, item) {
      return chain.then(function() { return fetchWithProgress(item, token); });
    }, Promise.resolve());
  });

});
