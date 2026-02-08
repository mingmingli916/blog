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

(defun all-posts-list ()
  (loop for p being the hash-values of *posts* collect p))

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

;; (defun parse-slug-from-href (href)
;;   "Support:
;; - /post/<slug>
;; - /post?slug=<slug>
;; Return slug string or NIL."
;;   (when (and href (stringp href))
;;     (cond
;;       ((string-prefix-p "/post/" href)
;;        (let ((slug (subseq href (length "/post/"))))
;;          (and (plusp (length slug)) slug)))
;;       ((string-prefix-p "/post?slug=" href)
;;        (let ((slug (subseq href (length "/post?slug="))))
;;          (and (plusp (length slug)) slug)))
;;       (t nil))))

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


;; (defun node-a-href (node)
;;   "If node is (a (:href ...) ...), return href string or NIL."
;;   (when (and (consp node) (symbolp (car node)))
;;     (let* ((tag (string-downcase (symbol-name (car node))))
;;            (rest (cdr node))
;;            (has-attrs (and rest (consp (car rest)) (keywordp (caar rest))))
;;            (attrs (and has-attrs (car rest))))
;;       (when (and (string= tag "a") attrs)
;;         (getf attrs :href)))))

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

/* ===== zoom-dependent labels =====
   Behavior:
   - when zoomed out: labels hidden
   - zoom in: labels gradually appear
   - hubs appear earlier + larger
*/
const LABEL_SHOW_SCALE = 0.75;  // below: hide most labels
const LABEL_FULL_SCALE = 0.9;  // above: normal label sizes

const FONT_LEAF = 12;
const FONT_MID  = 13;
const FONT_HUB  = 18;

function desiredFontSize(d, isHub, scale) {
  // hubs show earlier
  const showAt = isHub ? (LABEL_SHOW_SCALE * 0.75) : LABEL_SHOW_SCALE;
  const fullAt = isHub ? (LABEL_FULL_SCALE * 0.85) : LABEL_FULL_SCALE;

  if (scale <= showAt) return 0;
  const t = clamp((scale - showAt) / (fullAt - showAt), 0, 1);

  let base;
  if (isHub) base = FONT_HUB;
  else if (d >= 2) base = FONT_MID;
  else base = FONT_LEAF;

  return Math.round(lerp(0, base, t));
}

function applyZoomLabels() {
  const scale = network.getScale();
  const updates = rawNodes.map(n => {
    const d = degree[n.id] || 0;
    const isHub = d >= hubDeg;
    const fs = desiredFontSize(d, isHub, scale);
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

             (write-string "</script>" out))
           :subtitle "/graph"))))


(define-easy-handler (graph-debug-handler :uri "/graph/debug") ()
  (let ((posts (sort-posts-by-updated-desc (all-posts-list))))
    (respond-text
     (with-output-to-string (out)
       (dolist (p posts)
         (let* ((slug (getf p :slug))
                (targets (collect-post-link-slugs (getf p :content))))
           (format out "~&~a -> ~s~%" slug targets)))))))
