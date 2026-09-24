
# =============================================================================
#  UI
# =============================================================================
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Fraunces:wght@500;600&",
                            "family=Inter:wght@400;500&display=swap"))
  ),
  tags$head(tags$style(HTML("
    .field-block { margin-bottom: 18px; }
    .field-block .control-label { display: block; margin-bottom: 2px; font-weight: 600; }
    .field-block .field-help { color: #737373; font-size: 12px; margin: 2px 0 6px; line-height: 1.35; }

    details > summary { cursor: pointer; list-style: none; }
    details > summary::-webkit-details-marker { display: none; }
    details > summary::before {
      content: '\\25B6';
      display: inline-block;
      margin-right: 6px;
      font-size: 11px;
      transition: transform 0.15s ease;
    }
    details[open] > summary::before { transform: rotate(90deg); }

    body, .nav-item, label, input, select, .selectize-input, .help-block {
      font-family: 'Inter', sans-serif;
    }
    .app-title, h2, h3, h4, details > summary b {
      font-family: 'Fraunces', serif;
      font-weight: 600;
    }
    /* headings were rendering visually smaller than the field labels and
       input text beneath them (Fraunces reads smaller than Inter at the
       same size) -- bump them up so the hierarchy is clear at a glance */
    details > summary b { font-size: 16px; }

    :root {
      --brand-primary: #111827;
      --brand-accent:  #0EA5E9;
      --brand-warm:    #F59E0B;
    }

    .btn, button.btn, .btn-default {
      background: var(--brand-warm) !important;
      border-color: var(--brand-warm) !important;
      color: #111827 !important;
      font-family: 'Inter', sans-serif;
    }
    .btn:hover, button.btn:hover { background: #D97706 !important; }

    .content-card {
      background: #FFFFFF;
      border: 1px solid #E5E7EB;
      border-radius: 12px;
      padding: 24px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      margin-bottom: 20px;
    }
    .content-card p {
      font-size: 16px;
      line-height: 1.7;
      color: #1F2430;
    }
    .content-card ul {
      font-size: 16px;
      line-height: 1.7;
      color: #1F2430;
      padding-left: 22px;
      margin-bottom: 16px;
    }
    .content-card li { margin-bottom: 6px; }
    .content-card h2 {
      font-size: 30px;
      margin-bottom: 6px;
    }
    .content-card .subheadline {
      font-family: 'Fraunces', serif;
      font-style: italic;
      font-size: 18px;
      color: #6B7280;
      margin-bottom: 20px;
    }
    .content-card h3 {
      font-size: 19px;
      margin-top: 28px;
      margin-bottom: 10px;
    }
    .section-summary {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 24px;
      margin-top: 28px;
      margin-bottom: 14px;
      padding-bottom: 8px;
      border-bottom: 3px solid var(--brand-accent);
      display: inline-block;
    }
    .numbered-card {
      background: #E0F2FE;
      border-radius: 12px;
      padding: 20px 24px;
      margin: 16px 0 24px;
    }
    .numbered-card-intro {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 16px;
      color: #111827;
      margin-bottom: 14px;
    }
    .numbered-item {
      display: flex;
      align-items: flex-start;
      gap: 14px;
      margin-bottom: 14px;
    }
    .numbered-item:last-child { margin-bottom: 0; }
    .numbered-badge {
      flex: 0 0 auto;
      width: 26px;
      height: 26px;
      border-radius: 50%;
      background: var(--brand-accent);
      color: #FFFFFF;
      font-weight: 700;
      font-size: 13px;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .numbered-content {
      font-size: 16px;
      line-height: 1.6;
      color: #1F2430;
      padding-top: 2px;
    }
    .brand-name {
      font-family: 'Fraunces', serif;
    }
    .brand-name em {
      font-style: italic;
      color: var(--brand-accent);
    }
    .app-header {
      display: flex;
      align-items: center;
      gap: 28px;
      padding: 18px 24px;
      background: #F8FAFC;
      border-bottom: 1px solid #E5E7EB;
      margin-bottom: 18px;
    }
    .app-title {
      font-size: 26px;
      font-weight: 600;
      letter-spacing: -0.2px;
      white-space: nowrap;
    }
    .word1 { color: var(--brand-primary); font-weight: 700; }
    .app-title em {
      font-style: italic;
      color: var(--brand-accent);
    }
    .app-nav { display: flex; gap: 8px; align-items: center; }
    .nav-item {
      color: #4B5563 !important;
      text-decoration: none !important;
      font-size: 15px;
      padding: 6px 14px;
      border-radius: 999px;
    }
    .nav-item.active {
      color: var(--brand-primary) !important;
      font-weight: 500;
      background: #F3F4F6;
    }
    .nav-item.disabled {
      opacity: 0.4;
      pointer-events: none;
      cursor: default;
    }
    .nav-tabs { display: none !important; }

    /* scoped to the actual sidebarLayout columns only (direct children of the
       top-level row) so nested fluidRow/column() grids elsewhere, e.g. the
       Data-specifics selectors, are never caught by this rule */
    body.hide-sidebar > .container-fluid > .row > .col-sm-3 { display: none !important; }
    body.hide-sidebar > .container-fluid > .row > .col-sm-9 { width: 100% !important; }

    /* the sidebar scrolls independently within its own capped height instead
       of growing the whole page -- so working through a long settings list
       never scrolls the plot out of view on the right */
    body:not(.hide-sidebar) > .container-fluid > .row > .col-sm-3 {
      position: sticky;
      top: 12px;
      max-height: calc(100vh - 24px);
      overflow-y: auto;
    }

    .status-strip {
      font-size: 14px;
      color: #4B5563;
      padding: 8px 0 4px;
    }
    #curationLogDetails { margin-top: 6px; }
    #curationLogDetails > summary b { font-size: 18px; }
    .sample-data-row {
      margin-top: 8px;
      display: flex;
      align-items: center;
      gap: 10px;
      flex-wrap: wrap;
    }
    .btn-sample {
      background: #FFFFFF;
      border: 1px solid #D1D5DB;
      color: #374151;
      font-size: 13px;
      padding: 4px 10px;
    }
    .btn-sample:hover { background: #F3F4F6; }
    .sample-data-note {
      font-size: 12.5px;
      color: #6B7280;
    }
    .token-id-sep-row {
      display: flex;
      align-items: center;
      gap: 8px;
      margin: -4px 0 8px;
    }
    .token-id-sep-row span {
      font-size: 13px;
      color: #4B5563;
    }
    .token-id-sep-row .form-group { margin-bottom: 0; }
    .token-id-sep-row input { width: 60px; }
    /* live confirmation of what the Apply buttons will actually do -- typing
       in the variable/value boxes has no Enter-to-submit step, so this is
       the only feedback that the typed text was picked up */
    .record-preview {
      font-size: 13.5px;
      padding: 8px 10px;
      border-radius: 5px;
      margin: 2px 0 10px;
    }
    .record-preview-ready {
      background: #FEF3C7;
      color: #78350F;
      border: 1px solid #FCD34D;
    }
    .record-preview-partial {
      background: #EFF6FF;
      color: #1E3A8A;
      border: 1px solid #BFDBFE;
    }
    .record-preview-empty {
      background: #F3F4F6;
      color: #6B7280;
      font-style: italic;
    }
    .panel-banner {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 17px;
      letter-spacing: 0.2px;
      padding: 9px 14px;
      border-radius: 6px;
      margin-bottom: 18px;
    }
    .panel-banner-curate { background: #FFF7ED; border-left: 4px solid var(--brand-warm); color: #92400E; }
    .panel-banner-inspect { background: #F0F9FF; border-left: 4px solid var(--brand-accent); color: #075985; }
    .panel-banner-data { background: #ECFDF5; border-left: 4px solid #10B981; color: #065F46; }
    .panel-banner-plotset { background: #F5F3FF; border-left: 4px solid #8B5CF6; color: #5B21B6; }
    /* left-panel banners that fold their section away on click */
    details.panel-fold > summary.panel-banner { display: block; user-select: none; }
    details.panel-fold > summary.panel-banner::before { font-size: 12px; margin-right: 8px; }
    details.panel-fold > summary.panel-banner:hover { filter: brightness(0.97); }
    details.panel-fold:not([open]) > summary.panel-banner { margin-bottom: 10px; }
    /* the four sections of the Inspect page */
    .inspect-section-head {
      font-family: 'Fraunces', serif;
      font-weight: 600;
      font-size: 18px;
      color: #111827;
      margin: 6px 0 12px;
    }
    /* Page-section headings. A pale filled bar has area but little contrast,
       and loses to a small saturated button every time; the weight belongs in
       the type, with the rule as an accent. Matches the Home page. */
    .section-head {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      font-size: 22px;
      color: #111827;
      display: inline-block;
      margin: 30px 0 18px;
      padding-bottom: 7px;
      border-bottom: 3px solid var(--brand-accent);
    }
    .section-head:first-of-type { margin-top: 4px; }

    /* Colour means: this does something. The global .btn rule above uses
       !important, so .btn-sample never applied and every Browse button came
       out in the accent colour, outshouting the headings and the one real
       action on the page. */
    .btn-sample, button.btn.btn-sample,
    .btn-file, .btn.btn-file, span.btn.btn-file {
      background: #FFFFFF !important;
      border-color: #D1D5DB !important;
      color: #374151 !important;
      font-weight: 400 !important;
    }
    .btn-sample:hover, button.btn.btn-sample:hover,
    .btn-file:hover, .btn.btn-file:hover, span.btn.btn-file:hover {
      background: #F3F4F6 !important;
    }

    .subtle-hr { border: 0; border-top: 1px solid #F0F1F3; margin: 26px 0; }
    /* Sits between the 22px section heading and 13px body text. At 15px
       Fraunces read as smaller than the help text above it, which made the
       numbered steps look like a footnote to the paragraph they follow. */
    .subsection-label {
      font-family: 'Fraunces', serif;
      font-weight: 700;
      color: #1F2430;
      font-size: 17px;
      letter-spacing: -0.01em;
      margin: 4px 0 10px;
    }
    /* draw the eye straight to the download-as-png button in the plotly
       toolbar, so the camera-icon instruction has something to point at */
    .modebar-btn[data-title='Download plot as a png'] {
      outline: 2px solid var(--brand-warm);
      outline-offset: 1px;
      border-radius: 4px;
      background: rgba(245, 158, 11, 0.18) !important;
    }
    /* NOTE: intentionally NOT position:sticky. The sidebar already scrolls
       independently (see .col-sm-3 rule above), so pinning this too was
       redundant -- and it actively broke layout: once the plot rendered in
       (a size Plotly sets asynchronously, after this box's height was
       already reserved in the page flow), the sticky box's reserved space
       fell out of sync with its real height, and the curation log below it
       ended up overlapped/hidden no matter how far you scrolled. Plain,
       normal-flow positioning avoids that class of bug entirely. */
    .sticky-inspect-top {
      background: #FFFFFF;
      padding-bottom: 4px;
    }

    /* ---- audio setup ------------------------------------------------- */
    .audio-step {
      border-left: 3px solid #DBE2EA;
      padding: 2px 0 2px 16px;
      margin: 0 0 24px;
    }
    .audio-step .subsection-label { margin-top: 0; }
    .audio-step.done { border-left-color: #10B981; }
    .clip-row {
      display: flex;
      align-items: center;
      gap: 12px;
      margin-bottom: 6px;
      flex-wrap: wrap;
    }
    .clip-name {
      min-width: 150px;
      font-size: 13px;
      font-weight: 600;
      color: #1F2430;
    }
    .clip-meta {
      font-size: 12px;
      color: #6B7280;
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
    }
    /* a token produced more than once gets one row per production, and the
       badge is the only thing distinguishing them at a glance */
    .clip-take {
      display: inline-block;
      background: #EDE9FE;
      color: #5B21B6;
      border-radius: 999px;
      font-size: 11px;
      font-weight: 700;
      padding: 1px 8px;
    }
    /* ---- folder picker ------------------------------------------------
       A browser file input reports file CONTENTS and deliberately hides
       their location, so no Browse button can fill in a folder path.
       Inspectour runs locally, so R lists the disk itself instead. */
    .dir-row { display: flex; gap: 8px; align-items: flex-start; }
    .dir-row .form-group { margin-bottom: 0; flex: 1 1 auto; }
    .dir-row input { width: 100%; }
    .dir-quick { display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 10px; }
    .dir-crumb {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 12.5px;
      color: #4B5563;
      word-break: break-all;
      margin: 8px 0;
    }
    .dir-list {
      max-height: 320px;
      overflow-y: auto;
      border: 1px solid #E5E7EB;
      border-radius: 8px;
      padding: 6px;
      margin-top: 8px;
    }
    .dir-list a {
      display: block;
      padding: 5px 8px;
      border-radius: 4px;
      color: #1F2430 !important;
      text-decoration: none !important;
      font-size: 14px;
    }
    .dir-list a:hover { background: #F3F4F6; }

    .diag-grid { display: flex; gap: 8px; flex-wrap: wrap; margin: 10px 0; }
    .diag-pill {
      font-size: 12.5px;
      padding: 4px 10px;
      border-radius: 999px;
      background: #F3F4F6;
      color: #374151;
    }
    .diag-pill.good { background: #DCFCE7; color: #14532D; }
    .diag-pill.warn { background: #FEF3C7; color: #78350F; }
    .diag-pill.mute { background: #F3F4F6; color: #6B7280; }
    .near-miss-box {
      background: #FEF3C7;
      border: 1px solid #FCD34D;
      border-radius: 8px;
      padding: 12px 14px;
      margin: 10px 0;
      font-size: 13.5px;
      color: #78350F;
    }
    .near-miss-box code {
      background: rgba(255,255,255,0.7);
      color: #78350F;
      padding: 1px 5px;
      border-radius: 3px;
    }
    .map-row {
      display: flex;
      align-items: center;
      gap: 10px;
      margin-bottom: 8px;
      flex-wrap: wrap;
    }
    .map-row .map-col {
      min-width: 150px;
      font-size: 13.5px;
      font-weight: 600;
      color: #1F2430;
    }
    .map-row .form-group { margin-bottom: 0; }
    .map-row select { min-width: 210px; }
    .map-arrow { color: #9CA3AF; }
    .map-preview {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 12px;
      color: #6B7280;
      margin: 2px 0 4px 0;
    }
    .map-preview.bad { color: #B45309; font-weight: 600; }

    /* ---- column roles: one per row, controls on a common left edge ---- */
    .role-table { border-top: 1px solid #F0F1F3; margin: 4px 0 8px; }
    .role-row {
      display: flex;
      gap: 24px;
      align-items: flex-start;
      padding: 12px 0;
      border-bottom: 1px solid #F0F1F3;
    }
    .role-key { flex: 0 0 200px; padding-top: 5px; }
    .role-name { font-weight: 600; font-size: 14.5px; color: #1F2430; }
    .role-hint {
      font-size: 12px;
      color: #6B7280;
      line-height: 1.4;
      margin-top: 2px;
    }
    /* the sentence that actually defines the role, lifted out of the
       qualifying text around it. Bold read as the same weight as the
       Unique token ID label above it, so this uses shading instead: a
       tinted strip, same trick as .record-preview-partial elsewhere */
    .hint-key {
      display: block;
      font-size: 12.5px;
      color: #1E3A8A;
      background: #EFF6FF;
      border-radius: 4px;
      padding: 3px 7px;
      margin: 2px 0 5px;
    }
    .role-val { flex: 1 1 auto; max-width: 440px; }
    /* used to link back to a control set earlier on the page, e.g. from the
       audio section to the Unique token ID row -- a real link rather than a
       styled span, so a jump target is recognisable as clickable */
    .jump-link, .jump-link:visited {
      color: var(--brand-accent);
      text-decoration: underline;
    }
    .role-val .form-group { margin-bottom: 0; }
    .role-val .selectize-control { margin-bottom: 0; }
    /* the summary sat flush under the sample-data button, reading as that
       button's own output rather than as the result of loading either way */
    #data_overview { margin-top: 26px; }

    .audio-more > summary {
      display: inline-block;
      font-size: 14px;
      font-weight: 600;
      color: #075985;
      background: #E0F2FE;
      border-radius: 999px;
      padding: 6px 14px;
      margin: 2px 0 12px;
    }
    .audio-more > summary::before { content: none; }
    .audio-more > summary:hover { background: #BAE6FD; }
    /* .content-card ul sets 16px and is more specific than a bare
       .audio-points, so these bullets were rendering at full body size and
       the numbered step heading under them looked like a shrink. */
    .content-card ul.audio-points {
      font-size: 14px;
      color: #4B5563;
      line-height: 1.6;
      padding-left: 20px;
      margin: 0 0 18px;
    }
    .audio-points li { margin-bottom: 5px; }
    /* prose form of the same disclosure -- used where the content is one
       continuous explanation rather than a list of separate facts */
    .content-card .audio-more p {
      font-size: 14px;
      color: #4B5563;
      line-height: 1.6;
      margin: 0 0 10px;
    }
    /* an opened disclosure needs the same breathing room below it as the
       bullet version (18px), or the next field label sits flush against it */
    .content-card .audio-more p:last-child { margin-bottom: 18px; }

    /* The token id is inherited from Data specifics, not asked for again:
       stated as a fact, with the override folded away underneath. */
    .map-inherit {
      font-size: 13px;
      color: #374151;
      background: #F3F6F9;
      border-radius: 5px;
      padding: 7px 10px;
      margin: 0 0 10px;
    }
    .map-inherit b { color: #1F2430; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
    .map-inherit-q { font-weight: 600; color: #1F2430; margin-bottom: 4px; }
    .map-inherit .radio { margin: 2px 0; }
    .map-inherit .radio label { font-size: 13px; color: #374151; }
    /* Shiny gives every input container a fixed 300px, which wrapped the
       longer of the two ids onto a second line mid-name */
    .map-inherit .shiny-input-container { width: auto; margin-bottom: 0; }
    /* a real control, not a footnote: the override changes what every row
       below is matched against, so it is drawn like a button */
    .map-cols-more > summary {
      display: inline-block;
      font-size: 13.5px;
      font-weight: 600;
      color: #075985;
      background: #F0F9FF;
      border: 1px solid #7DD3FC;
      border-radius: 6px;
      padding: 5px 12px;
      margin: 2px 0 12px;
    }
    .map-cols-more > summary:hover { background: #E0F2FE; color: #0C4A6E; }
    .map-cols-more[open] > summary { margin-bottom: 6px; }
    .map-cols-more[open] { margin-bottom: 12px; }

    .filter-controls { display: flex; gap: 14px; flex-wrap: wrap; }
    .filter-controls .form-group { margin-bottom: 0; min-width: 170px; }
    .token-chips {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
      max-height: 120px;
      overflow-y: auto;
      margin-top: 6px;
    }
    .token-chip {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
      font-size: 11.5px;
      background: #F3F4F6;
      color: #374151;
      border-radius: 4px;
      padding: 2px 6px;
      white-space: nowrap;
    }

    .role-alt {
      font-size: 12.5px;
      color: #6B7280;
      margin: 12px 0 5px;
    }
  "))),
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('setActiveNav', function(tab) {
      document.querySelectorAll('.nav-item').forEach(function(el) {
        el.classList.toggle('active', el.getAttribute('data-tab') === tab);
      });
    });
    Shiny.addCustomMessageHandler('toggleSidebar', function(hide) {
      document.body.classList.toggle('hide-sidebar', hide);
    });
    Shiny.addCustomMessageHandler('setNavEnabled', function(msg) {
      var el = document.querySelector('.nav-item[data-tab=\"' + msg.tab + '\"]');
      if (el) el.classList.toggle('disabled', !msg.enabled);
    });
    // ---- after Try it with sample audio: scroll to the Result ------
    // The result box redraws a few times while the audio is read and
    // matched, so wait until it has been quiet for a moment (or 15s at
    // most) and then scroll to its final position.
    (function() {
      var pending = false, timer = null, deadline = 0;
      function go() {
        pending = false;
        var el = document.getElementById('audio_status');
        if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
      }
      Shiny.addCustomMessageHandler('scrollToResult', function(x) {
        pending = true; deadline = Date.now() + 15000;
        clearTimeout(timer); timer = setTimeout(go, 3000);
      });
      $(document).on('shiny:value', function(e) {
        if (!pending || e.name !== 'audio_status') return;
        clearTimeout(timer);
        timer = setTimeout(go, Date.now() > deadline ? 0 : 500);
      });
    })();
    // ---- remember whether a <details> the user opened stays open ------
    // renderUI rebuilds the whole block on nearly every input change in it,
    // which would otherwise snap an opened fold shut the moment something
    // inside it changes. The 'toggle' event does not bubble in every
    // browser, so this listens in the capture phase on document instead.
    document.addEventListener('toggle', function(e) {
      var d = e.target;
      if (d.tagName === 'DETAILS' && d.classList.contains('map-cols-more')) {
        Shiny.setInputValue('map_cols_open', d.open);
      }
    }, true);
    // ---- play a list of clips back to back, with a short gap ----------
    Shiny.addCustomMessageHandler('playSequence', function(urls) {
      if (!urls) return;
      if (!Array.isArray(urls)) urls = [urls];
      if (!urls.length) return;
      if (window.__inspectourSeq) { window.__inspectourSeq.pause(); }
      var i = 0;
      var a = new Audio();
      window.__inspectourSeq = a;
      a.onended = function() {
        i += 1;
        if (i < urls.length) {
          setTimeout(function() { a.src = urls[i]; a.play(); }, 300);
        }
      };
      a.src = urls[0];
      a.play();
    });
  "))),

  tags$div(class = "app-header",
           HTML("
      <svg width='52' height='44' viewBox='0 0 88 74'>
        <path d='M4,36 Q24,4 44,36 T84,36' fill='none' stroke='#111827' stroke-width='2.5'/>
        <path d='M4,46 Q24,22 44,46 T84,46' fill='none' stroke='#0EA5E9' stroke-width='2.5'/>
        <circle cx='24' cy='8' r='4' fill='#111827'/>
        <circle cx='64' cy='66' r='4' fill='#0EA5E9'/>
      </svg>
      <span class='app-title'><span class='word1'>Inspec</span><em>tour</em></span>
    "),
           tags$div(class = "app-nav",
                    actionLink("nav_home", label = "Home",
                               class = "nav-item", `data-tab` = "Home"),
                    actionLink("nav_data", label = "Data",
                               class = "nav-item", `data-tab` = "Data"),
                    actionLink("nav_inspect", label = "Inspect",
                               class = "nav-item", `data-tab` = "Inspect")
           )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ---- Inspect-only controls: everything you adjust while working ----
      conditionalPanel(
        "output.dataLoaded == true",

        # ---- plot settings: both halves collapsible now, so either can be
        # ---- tucked away -- interactive starts open since it's touched more
        # ---- often, export starts closed since it's only needed pre-download --
        tags$details(class = "panel-fold", id = "plotSettingsFold", open = NA,
        tags$summary(class = "panel-banner panel-banner-curate", "Plot settings"),
        tags$div(id = "plotDetails",
                 tags$details(
                   id = "interactiveDetails",
                   open = NA,
                   tags$summary(tags$b("Interactive plot")),
                   tags$br(),

                   field_block("Mark a break (optional)",
                               "A line where this column's value changes within a token.",
                               selectInput("col_break", label = NULL, choices = c("(none)"))),

                   field_block("Break style", NULL,
                               radioButtons("break_style", label = NULL,
                                            choices = c("Insert boundary line" = "boundary",
                                                        "Disconnect the contour" = "disconnect"),
                                            selected = "boundary")),

                   field_block("Show on hover", widget =
                                 selectizeInput("col_hover", label = NULL,
                                                choices = NULL, multiple = TRUE)),

                   field_block("f0 range", widget =
                                 sliderInput("ylim", label = NULL, min = -6, max = 6,
                                             value = c(-4, 4), step = 0.5)),

                   field_block("Plot height (px)", widget =
                                 numericInput("plot_height", label = NULL, 620,
                                              min = 300, max = 2000, step = 20)),

                   sliderInput("lw_token", "Token line width",
                               min = 0.1, max = 2, value = 0.4, step = 0.05),
                   sliderInput("alpha_token", "Token opacity",
                               min = 0.05, max = 1, value = 0.75, step = 0.05),

                   # only meaningful once clips exist, so it is hidden until then
                   conditionalPanel(
                     "output.hasAudio == true",
                     checkboxInput("mark_audio", "Mark tokens that have audio", value = TRUE)
                   )
                 ),
                 tags$hr(),
                 tags$details(
                   id = "exportDetails",
                   tags$summary(tags$b("Export settings (for download)")),
                   tags$br(),
                   helpText("Use the camera icon in the plot toolbar."),

                   sliderInput("x_breaks", "x-axis labels",
                               min = 2, max = 10, value = 4, step = 1),
                   sliderInput("font_size", "Text size",
                               min = 7, max = 20, value = 13, step = 1),
                   sliderInput("axis_size", "Axis number size",
                               min = 5, max = 16, value = 9, step = 1),
                   sliderInput("lw_mean", "Mean line width",
                               min = 0.4, max = 3, value = 1.1, step = 0.1),
                   checkboxInput("show_legend", "Show legend", value = TRUE)
                 )
        )
        ),
        tags$hr(),

        # ---- select & curate: what you touch most while looking at the plot --
        tags$details(class = "panel-fold", id = "curateFold", open = NA,
        tags$summary(class = "panel-banner panel-banner-curate", "Select & curate"),
        tags$div(id = "curateSection",
                 field_block("Highlight token(s)",
                             "Click a line in the plot, or search here.",
                             selectizeInput("highlight", label = NULL,
                                            choices = NULL, multiple = TRUE,
                                            options = list(placeholder = "click a line, or type to search..."))),
                 tags$hr(),

                 tags$b("Record a decision"),
                 helpText("Correct one of your own columns for the selected tokens."),
                 selectInput("edit_var_existing", "Existing variable", choices = NULL),
                 textInput("edit_var_new", "...or create a new variable",
                           placeholder = "type a new column name"),
                 helpText("A typed name wins, and becomes a new column on export."),
                 uiOutput("var_preview"),
                 tags$hr(),
                 selectInput("edit_value_existing", "Existing value", choices = NULL),
                 textInput("edit_value_new", "...or type a new value",
                           placeholder = "type a value not seen before"),
                 helpText("A typed value wins."),
                 uiOutput("record_preview"),
                 tags$br(),
                 uiOutput("label_add_selected_ui"),
                 br(),
                 uiOutput("label_add_filtered_ui"),
                 br(),
                 actionButton("label_undo", "Undo last")
        )
        )
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Home",
                 br(),
                 tags$div(class = "content-card",
                          tags$h2(brand_name(), ": inspecting contours through grouping and comparison"),

                          tags$details(
                            open = NA,
                            tags$summary(class = "section-summary", "What it does"),
                            tags$br(),
                            tags$p(brand_name(), " puts every contour in your dataset on screen at",
                                   "once, and lets you work in two directions:"),
                            tags$ul(
                              tags$li(tags$b("Explore."), " Start with no categories at all. Look at",
                                      "everything together, select specific tokens to compare directly",
                                      "against each other, and see which shapes recur."),
                              tags$li(tags$b("Verify."), " Start from categories you already have, such",
                                      "as transcribed tones, clustering output, speaker, or syntactic",
                                      "structure. Group and colour contours by them to check whether each",
                                      "category holds together, which tokens don't fit, and where",
                                      "categories overlap.")
                            ),
                            tags$p("If you have the original recordings and Praat TextGrids, you can",
                                   "also ", tags$b("listen to any contour you click"), ", which makes it",
                                   "much easier to tell a genuine category difference from a",
                                   "segmentation or tracking artefact.")
                          ),

                          tags$details(
                            open = NA,
                            tags$summary(class = "section-summary", "Why it matters"),
                            tags$br(),
                            tags$p(tags$b("Compared to traditional transcription,"), " ", brand_name(),
                                   " keeps the raw acoustic detail intact and lets you check transcribed",
                                   "categories against the contours themselves. Looking directly at",
                                   "contours makes it easy to see which ones really resemble each other",
                                   "and which stand apart. Relying on transcribed values alone can obscure",
                                   "this: is a 51 tone really different from a 52, or is that just",
                                   "transcription noise? This is especially useful for fieldworkers",
                                   "working with a system they don't yet know well."),
                            tags$p(tags$b("Compared to machine clustering,"), " ", brand_name(),
                                   " is complementary rather than competing. Use it before clustering to",
                                   "get a clear sense of how many contour shapes are actually present and",
                                   "what they look like. Use it after clustering to inspect what each",
                                   "cluster contains, spot tokens that may have been misassigned, and",
                                   "decide whether two clusters are really distinct. This matters most",
                                   "with small or unevenly distributed datasets, where some categories",
                                   "are represented by only one or two tokens and clustering has little",
                                   "to go on, which is often exactly the situation in the early stages of",
                                   "fieldwork on an under-studied prosodic system."),

                            numbered_card(
                              intro = "This flexibility is especially useful in fieldwork:",
                              items = list(
                                tagList(tags$b("Nothing gets lost."), " Whatever you choose to show on",
                                        "hover, such as speaker or tone category, stays attached to every",
                                        "contour throughout."),
                                tagList(tags$b("Zoom in without losing the big picture."), " Switch between",
                                        "individual tokens and group means at any point, so you can hold",
                                        "the overall pattern in mind while still checking specific tokens",
                                        "closely."),
                                tagList(tags$b("Test any grouping, instantly."), " Changing which",
                                        "variable you facet or colour by takes one click, whether it is a",
                                        "speaker, a syntactic structure, a transcribed tone, or a cluster",
                                        "label, so you can check whether a pattern holds across it."),
                                tagList(tags$b("Single out what doesn't fit."), " Borderline or unusual",
                                        "contours, including tokens that sit oddly in their assigned",
                                        "category, can be isolated and inspected closely rather than",
                                        "getting lost in a crowd of overlapping lines."),
                                tagList(tags$b("Hear what you are looking at."), " With TextGrids in hand,",
                                        "a click plays the token itself, straight out of a long",
                                        "session recording.")
                              )
                            )
                          ),
                          HTML("
                     <svg width='100%' viewBox='0 0 680 240' role='img'>
                       <title>Three-stage figure: all tokens, group by variable, curate</title>
                       <defs>
                         <marker id='arrow' viewBox='0 0 10 10' refX='8' refY='5' markerWidth='6' markerHeight='6' orient='auto-start-reverse'>
                           <path d='M2 1L8 5L2 9' fill='none' stroke='#9CA3AF' stroke-width='1.5' stroke-linecap='round' stroke-linejoin='round'/>
                         </marker>
                       </defs>
                       <text x='90' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>All tokens</text>
                       <path d='M30,110 Q60,82 90,110 Q120,138 150,110' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,120 Q60,145 90,120 Q120,95 150,120' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,115 Q60,88 90,115 Q120,142 150,115' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,105 Q60,130 90,105 Q120,80 150,105' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <path d='M30,118 Q60,92 90,118 Q120,144 150,118' fill='none' stroke='#0EA5E9' stroke-width='1.5' opacity='0.3'/>
                       <line x1='165' y1='115' x2='195' y2='115' stroke='#9CA3AF' stroke-width='1.5' marker-end='url(#arrow)'/>
                       <text x='335' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>Group by variable</text>
                       <rect x='205' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='205' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='250' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S1</text>
                       <path d='M215,105 Q235,88 255,105 Q275,122 285,110' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <path d='M215,115 Q235,98 255,115 Q275,132 285,120' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M215,110 Q235,93 255,110 Q275,127 285,115' fill='none' stroke='#111827' stroke-width='1.2' opacity='0.6'/>
                       <rect x='300' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='300' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='345' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S2</text>
                       <path d='M310,100 Q330,118 350,100 Q370,82 380,95' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M310,110 Q330,128 350,110 Q370,92 380,105' fill='none' stroke='#0EA5E9' stroke-width='1.2' opacity='0.6'/>
                       <path d='M310,120 Q330,103 350,120 Q370,137 380,125' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <rect x='395' y='45' width='90' height='120' fill='#FFFFFF' stroke='#D1D5DB'/>
                       <rect x='395' y='45' width='90' height='16' fill='#E5E7EB'/>
                       <text x='440' y='57' text-anchor='middle' font-family='Inter, sans-serif' font-size='9' fill='#4B5563'>speaker: S3</text>
                       <path d='M405,108 Q425,90 445,108 Q465,126 475,113' fill='none' stroke='#111827' stroke-width='1.5'/>
                       <path d='M405,118 Q425,136 445,118 Q465,100 475,112' fill='none' stroke='#0EA5E9' stroke-width='1.5'/>
                       <path d='M405,113 Q425,95 445,113 Q465,131 475,118' fill='none' stroke='#111827' stroke-width='1.2' opacity='0.6'/>
                       <line x1='215' y1='182' x2='235' y2='182' stroke='#111827' stroke-width='2'/>
                       <text x='240' y='186' font-family='Inter, sans-serif' font-size='10' fill='#4B5563'>NP</text>
                       <line x1='285' y1='182' x2='305' y2='182' stroke='#0EA5E9' stroke-width='2'/>
                       <text x='310' y='186' font-family='Inter, sans-serif' font-size='10' fill='#4B5563'>VP</text>
                       <text x='440' y='186' font-family='Inter, sans-serif' font-size='9' font-style='italic' fill='#9CA3AF'>(colour = syntactic structure)</text>
                       <line x1='500' y1='115' x2='530' y2='115' stroke='#9CA3AF' stroke-width='1.5' marker-end='url(#arrow)'/>
                       <text x='605' y='25' text-anchor='middle' font-family='Inter, sans-serif' font-size='14' font-weight='600' fill='#111827'>Curate</text>
                       <rect x='545' y='35' width='80' height='18' rx='9' fill='#F3F4F6'/>
                       <text x='585' y='47' text-anchor='middle' font-family='Inter, sans-serif' font-size='10' fill='#111827'>pattern A</text>
                       <path d='M545,95 Q565,75 585,95 Q605,115 625,95' fill='none' stroke='#111827' stroke-width='2'/>
                       <path d='M545,107 Q565,125 585,107 Q605,89 625,107' fill='none' stroke='#0EA5E9' stroke-width='2'/>
                       <rect x='545' y='150' width='80' height='18' rx='9' fill='#F3F4F6'/>
                       <text x='585' y='162' text-anchor='middle' font-family='Inter, sans-serif' font-size='10' fill='#0EA5E9'>pattern B</text>
                     </svg>
                   ")
                 )),

        # =====================================================================
        #  DATA PAGE - one-time setup: load, map columns, add audio, overview
        # =====================================================================
        tabPanel("Data",
                 br(),
                 tags$div(class = "content-card",
                          tags$div(class = "section-head", "Load data"),
                          fileInput("file", label = NULL, accept = c(".csv"),
                                    placeholder = "select a .csv file"),
                          conditionalPanel(
                            "output.dataLoaded != true",
                            verbatimTextOutput("format_help")
                          ),
                          tags$div(class = "sample-data-row",
                                   actionButton("load_sample", "Try it with sample data",
                                                class = "btn-sample"),
                                   tags$span(class = "sample-data-note",
                                             "Xiangshan Wu tone contours.")),

                          conditionalPanel(
                            "output.dataLoaded == true",
                            verbatimTextOutput("data_overview"),
                            DT::dataTableOutput("full_preview"),

                            # ---- answered once, so they belong here rather
                            # ---- than in the working sidebar ----------------
                            tags$div(class = "section-head", "Data specifics"),
                            tags$div(class = "role-table",
                              role_row("Time", NULL,
                                       selectInput("col_time", label = NULL, choices = NULL)),
                              role_row("f0", "Normalised f0 recommended.",
                                       selectInput("col_f0", label = NULL, choices = NULL)),
                              role_row("Speaker", NULL,
                                       selectInput("col_speaker", label = NULL, choices = NULL)),
                              # The dropdown and the build-from-columns box are
                              # two ways to say ONE thing. Giving them separate
                              # rows put a divider between them, which read as
                              # two different variables.
                              # id'd so the audio section below can link back
                              # to it, rather than just saying "above"
                              tags$div(id = "token_id_row",
                                role_row("Unique token ID",
                                       # the first sentence is the definition;
                                       # the rest only rules out the common
                                       # misreading, so it stays quiet
                                       tagList(
                                         tags$span(class = "hint-key",
                                                   "Unique to a single production by a single speaker."),
                                         " It is not an item number, which represents unique items",
                                         " produced across different conditions, e.g., speaker,",
                                         " repetition, etc."),
                                       tagList(
                                         selectInput("col_token", label = NULL, choices = NULL),
                                         tags$div(class = "role-alt",
                                                  "or combine existing columns:"),
                                         selectizeInput("token_id_parts", label = NULL,
                                                        choices = NULL, multiple = TRUE, width = "100%",
                                                        options = list(placeholder = "pick 2 or more columns")),
                                         conditionalPanel(
                                           "input.token_id_parts && input.token_id_parts.length >= 2",
                                           tags$div(class = "token-id-sep-row",
                                                    tags$span("Separator:"),
                                                    textInput("token_id_sep", label = NULL, value = "_")))
                                       ))
                              )
                            ),
                            # NOT wrapped in the conditionalPanel above -- an
                            # output nested inside a conditionalPanel that
                            # starts hidden gets suspended by Shiny and can
                            # stop refreshing reliably once shown; the render
                            # function itself decides whether to show
                            # anything, so it is never hidden client-side
                            uiOutput("token_id_preview"),
                            # Audio below is entirely optional, and someone
                            # who has none doesn't want to scroll past it. A
                            # second exit here does exactly what the one at
                            # the bottom does; it's just reachable earlier.
                            tags$div(style = "text-align: right; margin-top: 6px;",
                                     actionButton("go_inspect_early",
                                                  "No audio? Start inspecting →",
                                                  class = "btn-lg"))
                          ),

                          tags$hr(class = "subtle-hr"),

                          # =============================================================
                          #  AUDIO SETUP
                          #  Three steps, in the order they have to happen: where the
                          #  sound is, where the segmentation is, and how a label in
                          #  that segmentation names a token in the dataset.
                          # =============================================================
                          tags$div(class = "section-head", "Audio (optional)"),
                          # Only the file shape has to be said before you act on it:
                          # you cannot discover it by trying. Everything else is
                          # reported by the Result section with your real numbers,
                          # so it is folded away rather than promised in advance.
                          tags$p(class = "field-help", style = "margin: -8px 0 6px;",
                                 paste("Add recordings to hear your tokens: one file per token named",
                                       "by its token id, or long recordings with a Praat TextGrid.")),
                          tags$div(class = "sample-data-row", style = "margin: 0 0 12px;",
                                   actionButton("load_sample_audio", "Try it with sample audio",
                                                class = "btn-sample"),
                                   tags$span(class = "sample-data-note",
                                             "Recordings for the sample dataset (loads it too).")),
                          tags$div(class = "audio-step",
                            sub_label("1. Recordings"),
                            tags$details(id = "rec_supply_more", class = "audio-more",
                              tags$summary("\U0001F4A1 What can I supply?"),
                              tags$ul(class = "audio-points",
                                tags$li("You can add more than one recording, either long ones",
                                        "with segmentation or one file per token."),
                                tags$li("Recordings need not cover every token, and may hold",
                                        "tokens your dataset does not. Whatever overlaps is",
                                        "what you can listen to."),
                                # the server case is an edge case, and the control
                                # itself already says "Local runs only", so the only
                                # difference worth stating here is size
                                tags$li("A folder and an upload supply the same files. The folder",
                                        "has no size limit; uploads are capped at 200 MB."))),
                            field_block("Folder on this computer",
                                        paste("Only works if you are running this app yourself",
                                              "(e.g. from RStudio) on the same computer your files",
                                              "are on, since it then reads them straight from disk.",
                                              "If you're using a copy someone else is hosting for you",
                                              "(e.g. on a shared server), it can't see your computer",
                                              "at all. Upload your files below instead."),
                                        tags$div(class = "dir-row",
                                                 textInput("audio_dir", label = NULL,
                                                           placeholder = "/path/to/audio"),
                                                 actionButton("browse_audio_dir", "Browse...",
                                                              class = "btn-sample"))),
                            field_block("...or upload files", "Max 200 MB.",
                                        fileInput("audio_files", label = NULL, multiple = TRUE,
                                                  placeholder = "select audio files",
                                                  accept = c(".wav", ".mp3", ".ogg", ".flac", ".m4a"))),
                            uiOutput("audio_files_note")
                          ),

                          tags$div(class = "audio-step",
                            sub_label("2. TextGrids"),
                            # Static, not reactive -- it explains what a TextGrid needs to
                            # supply before any are even uploaded, right under the heading
                            # rather than buried after the folder/upload controls.
                            tags$details(id = "tg_supply_more", class = "audio-more",
                              tags$summary("\U0001F4A1 What can I supply?"),
                              tags$p("A TextGrid needs an interval tier whose labels identify",
                                     " your tokens, e.g., an item number, speaker id, etc."),
                              tags$p("A label doesn't have to spell out the token ID in full:",
                                     " it can hold just one piece, with the rest, like the",
                                     " speaker, coming from the file name instead. Nothing",
                                     " needs to be relabeled to use this tool.")),
                            field_block("Folder on this computer",
                                        paste("Optional, to locate tokens inside longer recordings.",
                                              "Same as above: only works when you are running this",
                                              "app yourself."),
                                        tags$div(class = "dir-row",
                                                 textInput("tg_dir", label = NULL,
                                                           placeholder = "/path/to/textgrids"),
                                                 actionButton("browse_tg_dir", "Browse...",
                                                              class = "btn-sample"))),
                            field_block("...or upload files", NULL,
                                        fileInput("tg_files", label = NULL, multiple = TRUE,
                                                  placeholder = "select .TextGrid files",
                                                  accept = c(".TextGrid", ".textgrid"))),
                            uiOutput("tg_tier_ui")
                          ),

                          tags$div(class = "audio-step",
                            sub_label("3. How a label names a token"),
                            # four siblings, not one block, so that no control is
                            # redrawn by its own effect -- see server.R
                            tags$div(id = "audio_map_ui",
                                     uiOutput("audio_map_head"),
                                     uiOutput("audio_map_matching"),
                                     uiOutput("audio_map_parts"),
                                     uiOutput("audio_map_rows"),
                                     uiOutput("audio_map_preview"))
                          ),

                          uiOutput("audio_status"),

                          # last thing on the page, because setup now reads
                          # top to bottom: load, name the columns, add audio
                          conditionalPanel(
                            "output.dataLoaded == true",
                            tags$hr(class = "subtle-hr"),
                            tags$div(style = "text-align: right;",
                                     actionButton("go_inspect", "Start inspecting →",
                                                  class = "btn-lg"))
                          )
                 )),

        # =====================================================================
        #  INSPECT PAGE - filter, plot, curate: the working loop
        # =====================================================================
        tabPanel("Inspect",
                 tags$div(class = "content-card",
                          conditionalPanel("output.dataLoaded != true",
                                           h4("Load a dataset to begin."),
                                           actionLink("goto_data_from_inspect", "Go to the Data page →")
                          ),
                          conditionalPanel("output.dataLoaded == true",

                                           # ---- pinned while the (longer) sidebar scrolls, so the plot
                                           # ---- never scrolls out of view while you adjust settings ----
                                           tags$div(class = "sticky-inspect-top",
                                                    tags$div(class = "panel-banner panel-banner-inspect", "Inspection"),

                                                    # ---- 1. view & grouping: filter first, then how to draw it ----
                                                    tags$div(id = "viewGroupingSection",
                                                             tags$h4(class = "inspect-section-head", "View & grouping"),
                                                             fluidRow(
                                                               column(3, selectizeInput(
                                                                 "filter_vars", "Filter by", choices = NULL,
                                                                 multiple = TRUE, width = "100%",
                                                                 options = list(placeholder = "add a variable..."))),
                                                               column(9, uiOutput("filter_controls"))
                                                             ),
                                                             fluidRow(
                                                               column(3, radioButtons("view", "View",
                                                                                      choices = c("Individual tokens" = "tokens",
                                                                                                  "Group means"       = "means",
                                                                                                  "Means over tokens" = "both"),
                                                                                      selected = "tokens")),
                                                               column(3, selectInput("col_colour", "Colour by", choices = NULL)),
                                                               column(3, selectInput("facet_x", "Facet (columns)", choices = NULL)),
                                                               column(3, selectInput("facet_y", "Facet (rows)", choices = NULL))
                                                             ),
                                                             fluidRow(
                                                               column(3, offset = 6,
                                                                      numericInput("facet_ncol", "Facet columns (single facet only)",
                                                                                   2, min = 1, max = 10, step = 1))
                                                             )
                                                    ),
                                                    tags$hr(),

                                                    # ---- 2. figure ----
                                                    tags$div(id = "inspectSection",
                                                             tags$h4(class = "inspect-section-head", "Figure"),
                                                             uiOutput("plot_ui")
                                                    ),
                                                    tags$hr(),

                                                    # ---- 3. audio: audio-only filter, status line, players ----
                                                    tags$div(id = "audioSection",
                                                             tags$h4(class = "inspect-section-head", "Audio"),
                                                             conditionalPanel(
                                                               "output.hasAudio == true",
                                                               checkboxInput("only_audio",
                                                                             "Only tokens with audio",
                                                                             value = FALSE)),
                                                             uiOutput("inspect_status"),
                                                             uiOutput("audio_panel")
                                                    )
                                           ),
                                           tags$hr(),

                                           # ---- curation log, open by default so it's clearly visible
                                           # ---- under the plot rather than needing to be discovered ----
                                           tags$details(
                                             id = "curationLogDetails",
                                             open = NA,
                                             tags$summary(tags$b("Curation log "), textOutput("curation_log_count", inline = TRUE)),
                                             tags$br(),
                                             DT::dataTableOutput("curation_log_table"),
                                             br(),
                                             # two distinct exports, named so neither can be mistaken for
                                             # the small log table above: one is everything, one is just
                                             # the tokens that were actually touched
                                             downloadButton("label_dl", "Download full dataset (.csv)"),
                                             downloadButton("label_dl_curated", "Download curated tokens only (.csv)")
                                           )
                          )
                 ))
      )
    )
  )
)
