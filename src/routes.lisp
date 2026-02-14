(in-package :blog)



;;; ------------------------------
;;; HTTP response helpers
;;; ------------------------------

(defun respond-html (html &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/html; charset=utf-8")
  html)

(defun respond-text (text &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/plain; charset=utf-8")
  text)

;;; ------------------------------
;;; Pages (Level 2)
;;; ------------------------------



(defun sort-posts-by-updated-desc (posts)
  (sort posts #'> :key (lambda (p) (or (getf p :updated-at) 0))))

(hunchentoot:define-easy-handler (index-handler :uri "/") ()
  (let* ((posts (sort-posts-by-updated-desc (all-posts-list))))
    (respond-html
     (page "Blog"
           (with-output-to-string (out)
             (format out "<h2>Posts</h2>")
             (if (null posts)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (progn
                   (write-string "<ol class=\"postlist\">" out)
                   (dolist (p posts)
                     (let* ((slug (getf p :slug))
                            (title (or (getf p :title) slug))
                            (updated (getf p :updated-at))
                            (cat (getf p :category))
                            (tags (getf p :tags)))
                       (format out
                               "<li><a href=\"~a\">~a</a><div class=\"meta\">Updated: ~a · Slug: ~a</div></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape (ut->ymdhm updated))
                               (html-escape slug)

                               ;; category segment
                               (if cat
                                   (format nil " · Cagetory: <a href=\"~a\">~a</a>"
                                           (html-escape (url-for-category cat))
                                           (html-escape cat))
                                   "")

                               ;; tags segment
                               (if (and (listp tags) tags)
                                   (with-output-to-string (o)
                                     (write-string " · Tags: " o)
                                     (loop for ta in tags for i from 0 do
                                           (when (> i 0) (write-string ", " o))
                                           (format o "<a href=\"~a\">~a</a>"
                                                   (html-escape (url-for-tag ta))
                                                   (html-escape ta))))
                                   ""))))
                   (write-string "</ol>" out))))
           :subtitle "A long-term technical notebook"))))

(defun group-posts-by-ym (posts)
  "Return alist (\"YYYY-MM\" . (posts...)) in reverse chronological order."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (p posts)
      (push p (gethash (ut->ym (or (getf p :created-at) 0)) ht)))
    (let ((alist '()))
      (maphash (lambda (k v)
                 (push (cons k (sort-posts-by-updated-desc v)) alist))
               ht)
      (sort alist #'string> :key #'car))))

(hunchentoot:define-easy-handler (archive-handler :uri "/archive") ()
  (let* ((posts (all-posts-list))
         (groups (group-posts-by-ym posts)))
    (respond-html
     (page "Archive"
           (with-output-to-string (out)
             (if (null groups)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (dolist (g groups)
                   (format out "<h2>~a</h2>~%" (html-escape (car g)))
                   (write-string "<ul class=\"postlist\">" out)
                   (dolist (p (cdr g))
                     (let* ((slug (getf p :slug))
                            (title (or (getf p :title) slug))
                            (created (getf p :created-at)))
                       (format out
                               "<li><a href=\"~a\">~a</a> <span class=\"meta\">(Created: ~a)</span></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape (ut->ymdhm created)))))
                   (write-string "</ul>" out))))))))



(hunchentoot:define-easy-handler (about-handler :uri "/about") ()
  (respond-html
   (page "About"
         (with-output-to-string (out)
           ;; Minimal, calm, English About page
           (write-string "<p>This site — <strong>mingmingli.site</strong> — is a long-term technical notebook.</p>" out)
           (write-string "<p>It exists for one reason: to turn understanding into something durable.</p>" out)

           (write-string "<h2>Focus</h2>" out)
           (write-string "<ul>" out)
           (write-string "<li><strong>AI</strong>: behavior, limits, verification, practical workflows.</li>" out)
           (write-string "<li><strong>Linux</strong>: systems and tools designed to remain usable for years.</li>" out)
           (write-string "<li><strong>Programming</strong>: structure, abstraction, and maintainability.</li>" out)
           (write-string "</ul>" out)

           (write-string "<h2>Principles</h2>" out)
           (write-string "<ul>" out)
           (write-string "<li>Simple system, inspectable content.</li>" out)
           (write-string "<li>Stable URLs; posts are versionable files.</li>" out)
           (write-string "<li>Clear assumptions and boundaries.</li>" out)
           (write-string "</ul>" out)

           (write-string "<p class=\"muted\">Not a tutorial site. A record of thinking.</p>" out)

           ;; Keep your existing note about posts directory (now not ./posts)
           ;; (format out "<p class=\"muted\">Posts live in: <code>~a</code></p>"
           ;;         (html-escape (namestring *posts-dir*)))
           ))))




(hunchentoot:define-easy-handler (post-query-handler :uri "/post") (slug)
  "Fallback: /post?slug=..."
  (if (and slug (safe-slug-p slug))
      (let ((p (find-post slug)))
        (if p
            (respond-html (post-page p))
            (respond-html (post-not-found-page slug) 404)))
      (respond-text "Bad slug" 400)))

(defun post-dispatcher (request)
  "Dispatcher for /post/<slug>."
  (let* ((path (hunchentoot:script-name request))
         (prefix "/post/"))
    (when (and (>= (length path) (length prefix))
               (string= prefix (subseq path 0 (length prefix))))
      (let ((slug (subseq path (length prefix))))
        (lambda ()
          (if (safe-slug-p slug)
              (let ((p (find-post slug)))
                (if p
                    (respond-html (post-page p))
                    (respond-html (post-not-found-page slug) 404)))
              (respond-text "Bad slug" 400)))))))


(defun posts-by-category (cat)
  (let ((cat (normalize-category cat)))
    (remove-if-not (lambda (p)
                     (equal (normalize-category (getf p :category)) cat))
                   (all-posts-list))))


(defun posts-by-category-token (cat-token)
  (remove-if-not
   (lambda (p)
     (let ((c (getf p :category)))
       (and c (string= (sanitize-token c) cat-token))))
   (all-posts-list)))

(defun posts-by-tag (tag)
  (let ((tag (sanitize-token tag)))
    (remove-if-not (lambda (p)
                     (member tag (or (getf p :tags) '()) :test #'equal))
                   (all-posts-list))))



(defun taxonomy-page (title subtitle posts)
  (respond-html
   (page title
         (with-output-to-string (out)
           (if (null posts)
               (write-string "<p class=\"muted\">No posts.</p>" out)
               (progn
                 (write-string "<ol class=\"postlist\">" out)
                 (dolist (p (sort-posts-by-updated-desc posts))
                   (let* ((slug (getf p :slug))
                          (ptitle (or (getf p :title) slug))
                          (updated (getf p :updated-at)))
                     (format out
                             "<li><a href=\"~a\">~a</a><div class=\"meta\">Updated: ~a</div></li>~%"
                             (url-for-post slug)
                             (html-escape ptitle)
                             (html-escape (ut->ymdhm updated)))))
                 (write-string "</ol>" out))))
         :subtitle subtitle)))


(defun category-dispatcher (request)
  "Dispatcher for /category/<name>. (token based)."
  (let* ((path (hunchentoot:script-name request))
         (prefix "/category/"))
    (when (and (>= (length path) (length prefix))
               (string= prefix (subseq path 0 (length prefix))))
      (let* ((name (subseq path (length prefix)))
             (tok (sanitize-token name)))
        (lambda ()
          (if (> (length tok) 0)
              (let* ((posts (posts-by-category-token tok))
                     (label (or (and posts (getf (first posts) :category)) tok)))
                (taxonomy-page (format nil "Category: ~a" label)
                             (format nil "/category/~a" tok)
                             posts))
              (respond-text "Bad category" 400)))))))


(defun tag-dispatcher (request)
  "Dispatcher for /tag/<name>."
  (let* ((path (hunchentoot:script-name request))
         (prefix "/tag/"))
    (when (and (>= (length path) (length prefix))
               (string= prefix (subseq path 0 (length prefix))))
      (let* ((name (subseq path (length prefix)))
             (tag (sanitize-token name)))
        (lambda ()
          (if (> (length tag) 0)
              (taxonomy-page (format nil "Tag: ~a" tag)
                             (format nil "/tag/~a" tag)
                             (posts-by-tag tag))
              (respond-text "Bad tag" 400)))))))




;;; ------------------------------
;;; Graph (Plan A MVP): nodes = posts, links = internal post links
;;; ------------------------------

(defun string-prefix-p (prefix s)
  (and (stringp prefix) (stringp s)
       (<= (length prefix) (length s))
       (string= prefix (subseq s 0 (length prefix)))))


(defun json-escape (s)
  "Escape a string for safe inclusion inside JSON double quotes."
  (with-output-to-string (out)
    (loop for ch across (princ-to-string (or s "")) do
      (case ch
        (#\\ (write-string "\\\\" out))
        (#\" (write-string "\\\"" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char ch out))))))



(defun parse-slug-from-href (href)
  "Extract post slug from href. Supports:
  - /post/<slug>
  - /post/<slug>/
  - /post/<slug>?...
  - /post/<slug>#...
  - https://domain/.../post/<slug>...
Return slug string or NIL."
  (when (and href (stringp href))
    (let* ((needle "/post/")
           (pos (search needle href :test #'char=)))
      (when pos
        (let* ((start (+ pos (length needle)))
               (end start)
               (n (length href)))
          ;; move end until delimiter ? # / or end
          (loop while (< end n)
                for ch = (char href end)
                while (and (char/= ch #\?)
                           (char/= ch #\#)
                           (char/= ch #\/))
                do (incf end))
          (when (> end start)
            (subseq href start end)))))))




(defun node-a-href (node)
  "If node is an <a> sexp, return href string or NIL.
Supports:
  (a (:href \"...\") ...)
  (a :href \"...\" ...)"
  (when (and (consp node) (symbolp (car node)))
    (let* ((tag (string-downcase (symbol-name (car node))))
           (args (cdr node)))
      (when (string= tag "a")
        (cond
          ;; (a (:href "...") ...)
          ((and args (consp (car args)) (keywordp (caar args)))
           (getf (car args) :href))
          ;; (a :href "..." ...)
          ((and args (keywordp (car args)) (eq (car args) :href)
                (consp (cdr args)) (stringp (cadr node)))
           (cadr node))
          (t nil))))))



(defun collect-post-link-slugs (content)
  "Traverse SEXP content and collect target slugs from <a href=...>."
  (let ((acc '()))
    (labels ((walk (x)
               (cond
                 ((null x) nil)
                 ((stringp x) nil)
                 ((consp x)
                  (let ((href (node-a-href x)))
                    (when href
                      (let ((slug (parse-slug-from-href href)))
                        (when slug (push slug acc)))))
                  ;; continue walking children
                  (dolist (e x) (walk e)))
                 (t nil))))
      (walk content))
    (nreverse acc)))


(defun posts->graph-json (posts)
  "Return JSON {nodes:[...],links:[...]}.
links are deduped and only include targets that exist in *posts*."
  (let ((seen (make-hash-table :test 'equal)))
    (with-output-to-string (out)
      ;; nodes
      (write-string "{" out)
      (write-string "\"nodes\":[" out)
      (loop for p in posts
            for i from 0 do
              (when (> i 0) (write-string "," out))
              (let* ((slug (getf p :slug))
                     (title (or (getf p :title) slug))
                     (url (url-for-post slug)))
                (format out
                        "{\"id\":\"~a\",\"label\":\"~a\",\"url\":\"~a\"}"
                        (json-escape slug)
                        (json-escape title)
                        (json-escape url))))
      (write-string "],\"links\":[" out)

      ;; links
      (let ((first-link t))
        (dolist (p posts)
          (let* ((source (getf p :slug))
                 (targets (collect-post-link-slugs (getf p :content))))
            (dolist (tgt targets)
              (when (and (safe-slug-p tgt) (find-post tgt))
                (let ((k (format nil "~a->~a" source tgt)))
                  (unless (gethash k seen)
                    (setf (gethash k seen) t)
                    (unless first-link (write-string "," out))
                    (setf first-link nil)
                    (format out
                            "{\"source\":\"~a\",\"target\":\"~a\"}"
                            (json-escape source)
                            (json-escape tgt)))))))))

      (write-string "]}" out))))





(define-easy-handler (graph-handler :uri "/graph") ()
  (let* ((posts (sort-posts-by-updated-desc (all-posts-list)))
         (graph-json (posts->graph-json posts)))
    (respond-html
     (page "Graph"
           (with-output-to-string (out)
             (write-string "<p class=\"muted\">Read-only graph. Double-click a node to open the post.</p>" out)
             (write-string "<div id=\"graph\" style=\"height:600px;border:1px solid #eee;border-radius:12px;\"></div>" out)

             ;; vis-network
             (write-string "<link rel=\"stylesheet\" href=\"https://unpkg.com/vis-network/styles/vis-network.min.css\">" out)
             (write-string "<script src=\"https://unpkg.com/vis-network/standalone/umd/vis-network.min.js\"></script>" out)

             ;; script
             (write-string "<script>" out)
             (format out "const GRAPH = ~a;" graph-json)

             ;; IMPORTANT: map source/target -> from/to for vis-network
             (write-string "
/* =========================
   Graph view (read-only)
   - node color/size by degree
   - labels appear when zoomed in
   ========================= */

/* ===== data ===== */
const rawNodes = GRAPH.nodes || [];
const rawEdges = (GRAPH.links || []).map(e => ({ from: e.source, to: e.target }));

/* ===== degree (in + out) ===== */
const degree = Object.create(null);
rawNodes.forEach(n => { degree[n.id] = 0; });
rawEdges.forEach(e => {
  if (degree[e.from] !== undefined) degree[e.from]++;
  if (degree[e.to]   !== undefined) degree[e.to]++;
});

const degrees = rawNodes.map(n => degree[n.id] || 0);
const maxDeg = Math.max(0, ...degrees);

/* ===== thresholds (tune these) ===== */
// Hubs = higher-degree nodes; this is used for color emphasis + earlier label reveal.
const hubDeg = Math.max(3, Math.ceil(maxDeg * 0.30));

/* ===== helpers ===== */
function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
function lerp(a, b, t) { return a + (b - a) * t; }

function sizeByDeg(d) {
  // Make small degrees visibly different.
  if (d <= 0) return 6;   // isolated
  if (d === 1) return 9;  // 1 link
  if (d === 2) return 12; // 2 links
  // 3+ grow slowly + cap
  const base = 11;
  const k = 1.6;
  const cap = 15;
  return Math.min(cap, base + Math.log1p(d - 2) * k);
}


function roleColor(d) {
  // isolated red, hub orange, mid green, leaf dark gray
  if (d === 0) return { bg: '#e74c3c', border: '#e74c3c' };
  if (d >= hubDeg) return { bg: '#d97706', border: '#92400e' };
  if (d >= 2) return { bg: '#10b981', border: '#0f766e' };
  return { bg: '#374151', border: '#111827' };
}

/* ===== build datasets ===== */
const nodes = new vis.DataSet(
  rawNodes.map(n => {
    const d = degree[n.id] || 0;
    const c = roleColor(d);
    return {
      id: n.id,
      label: n.label,
      url: n.url,
//      value: d,
      size: sizeByDeg(d),
      color: {
        background: c.bg,
        border: c.border,
        highlight: { background: c.bg, border: '#000000' }
      },
      // Start with labels hidden; zoom handler will set real sizes.
      font: { size: 0, face: 'system-ui' }
    };
  })
);

const edges = new vis.DataSet(
  rawEdges.map(e => ({
    from: e.from,
    to: e.to
  }))
);

/* ===== render ===== */
const container = document.getElementById('graph');
const network = new vis.Network(container, { nodes, edges }, {
  nodes: {
    shape: 'dot'
  },
  edges: {
    color: { color: '#94a3b8', highlight: '#64748b' },
    width: 1,
    smooth: { type: 'continuous' }
  },
  interaction: {
    hover: true,
    tooltipDelay: 200,
    multiselect: false
  },
  physics: {
    stabilization: { iterations: 200 },
    barnesHut: {
      gravitationalConstant: -1200,
      centralGravity: 0.10,
      springLength: 90,
      springConstant: 0.04,
      damping: 0.12,
      avoidOverlap: 0.60
    }
  }
});

/* ===== zoom-dependent labels (degree-aware) =====
   - zoom out: low-degree labels disappear first
   - zoom in : high-degree labels appear first
*/

// Global base scales (tune)
const LABEL_SHOW_BASE = 0.65;  // baseline 'start showing' for mid nodes
const LABEL_FULL_BASE = 1.45;  // baseline 'fully visible' for mid nodes

// Font sizes at FULL visibility
const FONT_LEAF = 12;
const FONT_MID  = 13;
const FONT_HUB  = 16;

// How strongly degree affects label timing (tune)
const DEG_ALPHA = 0.22;  // bigger => high-degree appears much earlier
const DEG_BETA  = 0.14;  // bigger => full visibility comes earlier too

function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
function lerp(a, b, t) { return a + (b - a) * t; }

function baseFontByDegree(d) {
  if (d >= hubDeg) return FONT_HUB;
  if (d >= 2) return FONT_MID;
  return FONT_LEAF;
}

function showScaleByDegree(d) {
  // Higher degree => smaller threshold => shows earlier when zooming in
  // log1p compresses extremes; maxDeg normalizes
  const norm = maxDeg > 0 ? (Math.log1p(d) / Math.log1p(maxDeg)) : 0;
  return LABEL_SHOW_BASE - DEG_ALPHA * norm;
}

function fullScaleByDegree(d) {
  const norm = maxDeg > 0 ? (Math.log1p(d) / Math.log1p(maxDeg)) : 0;
  // ensure full >= show + some gap
  return (LABEL_FULL_BASE - DEG_BETA * norm);
}

function desiredFontSize(d, scale) {
  const showAt = showScaleByDegree(d);
  const fullAt = Math.max(showAt + 0.18, fullScaleByDegree(d)); // keep gap

  if (scale <= showAt) return 0;
  const t = clamp((scale - showAt) / (fullAt - showAt), 0, 1);

  const base = baseFontByDegree(d);
  return Math.round(lerp(0, base, t));
}

function applyZoomLabels() {
  const scale = network.getScale();
  const updates = rawNodes.map(n => {
    const d = degree[n.id] || 0;
    const fs = desiredFontSize(d, scale);
    return { id: n.id, font: { size: fs, face: 'system-ui' } };
  });
  nodes.update(updates);
}




applyZoomLabels();
network.on('zoom', applyZoomLabels);

/* ===== navigation (read-only) ===== */
network.on('doubleClick', params => {
  if (!params.nodes || params.nodes.length === 0) return;
  const id = params.nodes[0];
  const n = nodes.get(id);
  if (n && n.url) window.location.href = n.url;
});
" out)

             (write-string "</script>" out))))))


(define-easy-handler (graph-debug-handler :uri "/graph/debug") ()
  (let ((posts (sort-posts-by-updated-desc (all-posts-list))))
    (respond-text
     (with-output-to-string (out)
       (dolist (p posts)
         (let* ((slug (getf p :slug))
                (targets (collect-post-link-slugs (getf p :content))))
           (format out "~&~a -> ~s~%" slug targets)))))))



;;; ------------------------------
;;; Search
;;; ------------------------------
(defun %snippet (text &key (center "") (radius 140))
  "Return a small snippet from TEXT. If CENTER is found, center around it."
  (let* ((text (or text ""))
         (center (or center "")))
    (cond
      ((= (length text) 0) "")
      ((or (null center) (= (length center) 0))
       (if (> (length text) (* 2 radius))
           (concatenate 'string (subseq text 0 (* 2 radius)) "...")
           text))
      (t
       (let* ((pos (search center text :test #'char-equal))
              (pos (or pos 0))
              (start (max 0 (- pos radius)))
              (end (min (length text) (+ pos radius (length center)))))
         (concatenate 'string
                      (if (> start 0) "..." "")
                      (subseq text start end)
                      (if (< end (length text)) "..." "")))))))





(hunchentoot:define-easy-handler (search-handler :uri "/search") (q)
  (let* ((q (trim-string (or q "")))
         (results (if (> (length q) 0) (search-posts q :limit 60) '())))
    (respond-html
     (page "Search"
           (with-output-to-string (out)
             (cond
               ((= (length q) 0)
                (write-string "<p class=\"muted\">Use the search box in the top navigation.</p>" out)
                (write-string "<p class=\"muted\">Search covers title, slug, category, tags, and body text.</p>" out))

               ((null results)
                (format out "<p class=\"muted\">No results for <code>~a</code>.</p>" (html-escape q)))

               (t
                (format out "<p class=\"muted\">~a result(s) for <code>~a</code>.</p>"
                        (length results) (html-escape q))
                (write-string "<ol class=\"postlist\">" out)
                (dolist (pair results)
                  (let* ((p (car pair))
                         (score (cdr pair))
                         (slug (getf p :slug))
                         (title (or (getf p :title) slug))
                         (updated (getf p :updated-at)))
                    (format out
                            "<li><a href=\"~a\">~a</a><div class=\"meta\">Score: ~a · Updated: ~a · Slug: ~a</div></li>~%"
                            (url-for-post slug)
                            (html-escape title)
                            score
                            (html-escape (ut->ymdhm updated))
                            (html-escape slug))))
                (write-string "</ol>" out)))))
           ;; 注意：这里不要再传 :subtitle "/search"
           )))



;;; ------------------------------
;;; RSS (text + full HTML)
;;; ------------------------------
(defun respond-xml (xml &optional (code 200))
  (setf (return-code*) code
        (content-type*) "application/rss+xml; charset=utf-8")
  xml)

(defun xml-escape (s)
  "Escape text for XML element content."
  (let ((s (or s "")))
    (with-output-to-string (out)
      (loop for ch across s do
        (case ch
          (#\< (write-string "&lt;" out))
          (#\> (write-string "&gt;" out))
          (#\& (write-string "&amp;" out))
          (#\" (write-string "&quot;" out))
          (#\' (write-string "&apos;" out))
          (t (write-char ch out)))))))

(defun absolute-url (path)
  "Make an absolute URL from a site-relative PATH (string)."
  (let ((base (or *site-url* "")))
    (cond
      ((or (null path) (= (length path) 0)) base)
      ((and (>= (length path) 4)
            (or (string= "http" (subseq path 0 4))
                (string= "HTTP" (subseq path 0 4))))
       path)
      ((char= (char path 0) #\/)
       (format nil "~a~a" base path))
      (t
       (format nil "~a/~a" base path)))))

(defun %squeeze-ws (s)
  (let ((s (or s "")))
    (with-output-to-string (out)
      (let ((prev-ws nil))
        (loop for ch across s do
          (if (member ch '(#\Space #\Tab #\Newline #\Return))
              (unless prev-ws
                (write-char #\Space out)
                (setf prev-ws t))
              (progn
                (write-char ch out)
                (setf prev-ws nil))))))))

(defun %trim (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or s "")))

(defun %truncate (s n)
  (let ((s (or s "")))
    (if (<= (length s) n)
        s
        (concatenate 'string (subseq s 0 n) "…"))))


(defun node->plain-text (node)
  "Best-effort convert a rendered SEXP node into human readable plain text."
  (cond
    ((null node) "")
    ((stringp node) node)
    ;; Some nodes look like: (BLOG::P "text") or (BLOG::H2 "Title") etc.
    ((and (consp node) (symbolp (car node)))
     (let* ((tag (car node))
            (args (cdr node)))
       (cond
         ;; headings / paragraph / list items: join children
         ((member tag '(BLOG::H1 BLOG::H2 BLOG::H3 BLOG::H4 BLOG::H5 BLOG::H6
                        BLOG::P BLOG::LI BLOG::BLOCKQUOTE)
                  :test #'eq)
          (with-output-to-string (out)
            (dolist (a args)
              (let ((text (node->plain-text a)))
                (when (> (length text) 0)
                  (write-string text out)
                  (write-char #\Space out))))))
         ;; unordered/ordered lists: join items
         ((member tag '(BLOG::UL BLOG::OL) :test #'eq)
          (with-output-to-string (out)
            (dolist (a args)
              (let ((text (node->plain-text a)))
                (when (> (length text) 0)
                  (write-string text out)
                  (write-char #\Newline out))))))
         ;; code blocks: include content
         ((eq tag 'BLOG::SRC)
          ;; (BLOG::SRC (:LANG "lisp") "code...")
          (let ((code (car (last args))))
            (format nil "~%~a~%~%" (or code ""))))
         ;; math: include tex
         ((eq tag 'BLOG::MATH)
          (let ((tex (car (last args))))
            (format nil "~%[math] ~a~%~%" (or tex ""))))
         ;; image: keep alt if any
         ((eq tag 'BLOG::IMG)
          ;; (BLOG::IMG (:SRC "..." :ALT "Alt"))
          (let* ((plist (car args))
                 (alt (when (and (consp plist))
                        (getf plist :ALT))))
            (if (and alt (> (length alt) 0))
                (format nil "[image] ~a" alt)
                "[image]")))
         ;; links: keep text content if present
         ((eq tag 'BLOG::A)
          (with-output-to-string (out)
            (dolist (a args)
              (let ((text (node->plain-text a)))
                (when (> (length text) 0)
                  (write-string text out)
                  (write-char #\Space out))))))
         ;; default: flatten children
         (t
          (with-output-to-string (out)
            (dolist (a args)
              (let ((text (node->plain-text a)))
                (when (> (length text) 0)
                  (write-string text out)
                  (write-char #\Space out)))))))))
    ;; other lists: flatten
    ((consp node)
     (with-output-to-string (out)
       (dolist (a node)
         (let ((text (node->plain-text a)))
           (when (> (length text) 0)
             (write-string text out)
             (write-char #\Space out))))))
    (t "")))

(defun post->plain-summary (post &key (limit 600))
  (let* ((content (getf post :content))
         (txt (%trim (%squeeze-ws (node->plain-text content)))))
    (%truncate txt limit)))

(defun %cdata-safe (s)
  "Ensure CDATA does not contain the terminator ']]>'."
  (let ((s (or s "")))
    ;; split occurrences of ]]>
    (with-output-to-string (out)
      (loop with i = 0
            for pos = (search "]]>" s :start2 i)
            do (if pos
                   (progn
                     (write-string (subseq s i pos) out)
                     (write-string "]]]]><![CDATA[>" out)
                     (setf i (+ pos 3)))
                   (progn
                     (write-string (subseq s i) out)
                     (return)))))))

(defun post->rss-item-text (post)
  "RSS item for the plain-text feed."
  (let* ((slug (getf post :slug))
         (title (or (getf post :title) slug))
         (ut (or (getf post :updated-at) (getf post :created-at) 0))
         (link (absolute-url (url-for-post slug)))
         (guid link)
         (summary (post->plain-summary post :limit 600)))
    (with-output-to-string (out)
      (write-string "<item>\n" out)
      (format out "<title>~a</title>\n" (xml-escape title))
      (format out "<link>~a</link>\n" (xml-escape link))
      (format out "<guid isPermaLink=\"true\">~a</guid>\n" (xml-escape guid))
      (format out "<pubDate>~a</pubDate>\n" (xml-escape (ut->rfc822 ut)))
      (format out "<description>~a</description>\n" (xml-escape summary))
      (write-string "</item>\n" out))))

(defun post->rss-item-html (post)
  "RSS item for the full/HTML feed. Uses content:encoded with CDATA."
  (let* ((slug (getf post :slug))
         (title (or (getf post :title) slug))
         (ut (or (getf post :updated-at) (getf post :created-at) 0))
         (link (absolute-url (url-for-post slug)))
         (guid link)
         (summary (post->plain-summary post :limit 300))
         ;; IMPORTANT: reuse your existing renderer (same as post page body)
         (html (or (render-post-body post) "")))
    (with-output-to-string (out)
      (write-string "<item>\n" out)
      (format out "<title>~a</title>\n" (xml-escape title))
      (format out "<link>~a</link>\n" (xml-escape link))
      (format out "<guid isPermaLink=\"true\">~a</guid>\n" (xml-escape guid))
      (format out "<pubDate>~a</pubDate>\n" (xml-escape (ut->rfc822 ut)))
      ;; keep a short plain-text description for compatibility
      (format out "<description>~a</description>\n" (xml-escape summary))
      ;; full HTML payload
      (format out "<content:encoded><![CDATA[~a]]></content:encoded>\n"
              (%cdata-safe html))
      (write-string "</item>\n" out))))


(defun rss-feed (posts &key (limit 30) (full-html nil))
  (let* ((posts (sort-posts-by-updated-desc posts))
         (posts (subseq posts 0 (min limit (length posts))))
         (feed-link (absolute-url (if full-html *rss-full-path* *rss-path*)))
         (home-link (absolute-url "/"))
         (title (or *site-title* "Blog"))
         (desc (or *site-description* "")))
    (with-output-to-string (out)
      (if full-html
          (write-string "<rss version=\"2.0\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\">\n" out)
          (write-string "<rss version=\"2.0\">\n" out))
      (write-string "<channel>\n" out)
      (format out "<title>~a</title>\n" (xml-escape title))
      (format out "<link>~a</link>\n" (xml-escape home-link))
      (format out "<description>~a</description>\n" (xml-escape desc))
      (format out "<language>en</language>\n")
      (format out "<generator>Common Lisp mini blog</generator>\n")
      (format out "<docs>https://www.rssboard.org/rss-specification</docs>\n")
      (format out "<atom:link href=\"~a\" rel=\"self\" type=\"application/rss+xml\" xmlns:atom=\"http://www.w3.org/2005/Atom\" />\n"
              (xml-escape feed-link))
      (dolist (p posts)
        (write-string
         (if full-html
             (post->rss-item-html p)
             (post->rss-item-text p))
         out))
      (write-string "</channel>\n</rss>\n" out))))

(define-easy-handler (rss-handler :uri "/rss.xml") ()
  (respond-xml (rss-feed (all-posts-list) :full-html nil)))

(define-easy-handler (rss-full-handler :uri "/rss-full.xml") ()
  (respond-xml (rss-feed (all-posts-list) :full-html t)))
