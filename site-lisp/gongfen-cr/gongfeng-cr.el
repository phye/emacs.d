;;; gongfeng-cr.el --- Code Review overlay for Gongfeng (Tencent GitLab) MRs -*- lexical-binding: t; -*-

;; Author: phye
;; Version: 0.1.0
;; Keywords: tools, vc, review
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; gongfeng-cr is a minor mode for performing code review directly inside Emacs
;; against Gongfeng (git.woa.com) Merge Requests.
;;
;; Quick start:
;;   1. Set your Gongfeng private token once:
;;        M-x gongfeng-cr-set-token
;;      or add to your init file:
;;        (setq gongfeng-cr-private-token "YOUR_TOKEN")
;;
;;   2. Open any file that belongs to a Gongfeng project, then:
;;        M-x gongfeng-cr-mode
;;      You will be prompted for the MR IID (the per-project integer id shown
;;      in the MR URL, e.g. 42 in  .../merge_requests/42).
;;      Existing comments for the current file are fetched and shown as
;;      overlays inline in the buffer.
;;
;;   3. To add a new comment, select a region, then:
;;        M-x gongfeng-cr-add-comment
;;      An overlay input area opens beneath the selection.  Type your comment
;;      and press C-c C-c to submit, or C-c C-k to cancel.
;;
;; Troubleshooting:
;;   If you get HTTP 404 errors, run M-x gongfeng-cr-diagnose to see exactly
;;   which URL is being requested and what the server returns.  Common causes:
;;     - Wrong project path (check gongfeng-cr--project-id in *Messages*)
;;     - API base URL needs adjustment (default: https://git.woa.com/api/v3)
;;
;; API used: Gongfeng REST API v3 (gitlab-compatible)
;;   Authentication : PRIVATE-TOKEN header
;;   Base URL       : https://git.woa.com/api/v3
;;   Resolve MR id  : GET /projects/:encoded_path/merge_request/iid/:iid  → .id
;;   Comments       : GET/POST /projects/:encoded_path/merge_request/:mr_id/comments
;;
;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'subr-x)

;;;; ─── Customisation ────────────────────────────────────────────────────────

(defgroup gongfeng-cr nil
  "Code-review overlays for Gongfeng Merge Requests."
  :group 'tools
  :prefix "gongfeng-cr-")

(defcustom gongfeng-cr-private-token nil
  "Your Gongfeng private token.
Set this variable (e.g. via `gongfeng-cr-set-token') before using the mode.
The token is sent in the PRIVATE-TOKEN HTTP header on every API request."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Token"))
  :group 'gongfeng-cr)

(defcustom gongfeng-cr-base-url "https://git.woa.com/api/v3"
  "Base URL for the Gongfeng REST API (without trailing slash)."
  :type 'string
  :group 'gongfeng-cr)

(defcustom gongfeng-cr-comment-face 'gongfeng-cr-comment-face
  "Face used for comment overlay text."
  :type 'face
  :group 'gongfeng-cr)

;;;; ─── Faces ─────────────────────────────────────────────────────────────────

(defface gongfeng-cr-comment-face
  '((((background dark))
     :background "#2a2a4a" :foreground "#a0c8ff"
     :box (:line-width 1 :color "#5080c0")
     :extend t)
    (t
     :background "#eef4ff" :foreground "#2040a0"
     :box (:line-width 1 :color "#8090c0")
     :extend t))
  "Face for displaying fetched CR comments."
  :group 'gongfeng-cr)

(defface gongfeng-cr-input-face
  '((((background dark))
     :background "#2a3a2a" :foreground "#a0ffa0"
     :box (:line-width 1 :color "#50a050")
     :extend t)
    (t
     :background "#f0fff0" :foreground "#205020"
     :box (:line-width 1 :color "#70a070")
     :extend t))
  "Face for the comment-input overlay."
  :group 'gongfeng-cr)

(defface gongfeng-cr-header-face
  '((((background dark))
     :foreground "#80c0ff" :weight bold)
    (t
     :foreground "#1040c0" :weight bold))
  "Face for the header line inside a comment overlay."
  :group 'gongfeng-cr)

;;;; ─── Buffer-local state ────────────────────────────────────────────────────

(defvar-local gongfeng-cr--mr-iid nil
  "MR IID (per-project integer id) currently being reviewed.")

(defvar-local gongfeng-cr--mr-id nil
  "MR global integer id resolved from `gongfeng-cr--mr-iid'.
Cached after the first successful IID→ID lookup so subsequent operations
\(fetch comments, post comment) do not repeat the resolve request.")

(defvar-local gongfeng-cr--project-id nil
  "URL-encoded full_path of the current project, derived from the git remote.")

(defvar-local gongfeng-cr--overlays nil
  "List of comment overlays created by `gongfeng-cr-mode'.")

(defvar-local gongfeng-cr--input-overlay nil
  "The currently active comment-input overlay, if any.")

(defvar-local gongfeng-cr--input-prompt-end nil
  "Marker pointing to the end of the prompt in the input buffer.
Text after this position is the user's comment.")

;;;; ─── Token management ──────────────────────────────────────────────────────

;;;###autoload
(defun gongfeng-cr-set-token (token)
  "Interactively set `gongfeng-cr-private-token' to TOKEN.
This is the only configuration you normally need to perform."
  (interactive
   (list (read-string "Gongfeng private token: "
                      gongfeng-cr-private-token)))
  (setq gongfeng-cr-private-token token)
  (message "gongfeng-cr: token set."))

(defun gongfeng-cr--assert-token ()
  "Signal an error if `gongfeng-cr-private-token' is not configured."
  (unless (and gongfeng-cr-private-token
               (not (string-empty-p gongfeng-cr-private-token)))
    (user-error "gongfeng-cr: Please set your token first via M-x gongfeng-cr-set-token")))

;;;; ─── Project / remote helpers ─────────────────────────────────────────────

(defun gongfeng-cr--git-remote-url ()
  "Return the URL of the `origin' remote for the current buffer's repo."
  (let ((default-directory (or (locate-dominating-file
                                (or buffer-file-name default-directory)
                                ".git")
                               default-directory)))
    (string-trim
     (shell-command-to-string "git remote get-url origin 2>/dev/null"))))

(defun gongfeng-cr--parse-project-path (remote-url)
  "Extract <namespace>/<project> from REMOTE-URL (ssh or https)."
  ;; ssh:  git@git.woa.com:namespace/project.git
  ;; https: https://git.woa.com/namespace/project.git
  (when remote-url
    (cond
     ;; SSH form
     ((string-match "git@[^:]+:\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS form
     ((string-match "https?://[^/]+/\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS without .git suffix
     ((string-match "https?://[^/]+/\\(.*[^/]\\)/*$" remote-url)
      (match-string 1 remote-url)))))

(defun gongfeng-cr--url-encode-path (path)
  "URL-encode a project PATH (replace / with %2F, etc.)."
  (url-hexify-string path))

(defun gongfeng-cr--ensure-project-id ()
  "Set `gongfeng-cr--project-id' from the git remote, or ask the user."
  (unless gongfeng-cr--project-id
    (let* ((remote (gongfeng-cr--git-remote-url))
           (path   (gongfeng-cr--parse-project-path remote)))
      (if path
          (progn
            (message "gongfeng-cr: detected project %s" path)
            (setq gongfeng-cr--project-id (gongfeng-cr--url-encode-path path)))
        ;; Fallback: ask the user
        (let ((manual (read-string "Gongfeng project full_path (e.g. team/myproject): ")))
          (setq gongfeng-cr--project-id (gongfeng-cr--url-encode-path manual))))))
  gongfeng-cr--project-id)

;;;; ─── HTTP helpers ──────────────────────────────────────────────────────────

(defun gongfeng-cr--api-url (&rest path-segments)
  "Build a full API URL by joining PATH-SEGMENTS onto `gongfeng-cr-base-url'."
  (concat gongfeng-cr-base-url "/" (mapconcat #'identity path-segments "/")))

(defun gongfeng-cr--http-request (method url &optional payload callback)
  "Perform an async HTTP METHOD request to URL.
PAYLOAD is an alist that will be JSON-encoded and sent as the request body.
CALLBACK is called with the parsed JSON response (or nil on error)."
  (gongfeng-cr--assert-token)
  (message "gongfeng-cr: %s %s" method url)
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gongfeng-cr-private-token)
            ("Content-Type"  . "application/json")))
         (url-request-data
          (when payload
            ;; Bind json-encoding-coding-system so that non-ASCII characters
            ;; (e.g. Chinese) are emitted as raw UTF-8 code points rather than
            ;; \uXXXX escape sequences, then encode the resulting string to a
            ;; proper UTF-8 unibyte sequence for the HTTP body.
            (let ((json-encoding-coding-system 'utf-8))
              (encode-coding-string
               (json-encode payload) 'utf-8)))))
    (url-retrieve
     url
     (lambda (status)
       (let* ((http-status (gongfeng-cr--http-status-code))
              (err         (plist-get status :error)))
         (cond
          (err
           (let ((body (gongfeng-cr--response-body)))
             (message "gongfeng-cr: HTTP error %S (URL: %s)\n  response: %s"
                      err url
                      (substring body 0 (min 400 (length body))))))
          ((and http-status (>= http-status 400))
           (let ((body (gongfeng-cr--response-body)))
             (message "gongfeng-cr: HTTP %d for %s\n  response: %s"
                      http-status url
                      (substring body 0 (min 400 (length body))))))
          (t
           (let ((result (gongfeng-cr--parse-response)))
             (when callback
               (funcall callback result)))))))
     nil t)))

(defun gongfeng-cr--http-status-code ()
  "Return the integer HTTP status code from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun gongfeng-cr--response-body ()
  "Return the response body string from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*$" nil t)
        (decode-coding-string
         (buffer-substring (point) (point-max)) 'utf-8)
      "")))

(defun gongfeng-cr--parse-response ()
  "Parse JSON from the current url-retrieve response buffer.
Moves past the HTTP headers and decodes the body."
  (let ((body (gongfeng-cr--response-body)))
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-key-type    'symbol))
          (json-read-from-string body))
      (error
       (message "gongfeng-cr: JSON parse error: %S\nbody: %s" err body)
       nil))))

;;;; ─── Relative file path ────────────────────────────────────────────────────

(defun gongfeng-cr--relative-file-path ()
  "Return the path of the current buffer's file relative to the git root."
  (when buffer-file-name
    (let* ((root (locate-dominating-file buffer-file-name ".git")))
      (if root
          (file-relative-name buffer-file-name (expand-file-name root))
        (file-name-nondirectory buffer-file-name)))))

;;;; ─── Overlay utilities ─────────────────────────────────────────────────────

(defun gongfeng-cr--clear-overlays ()
  "Remove all comment overlays from the current buffer."
  (mapc #'delete-overlay gongfeng-cr--overlays)
  (setq gongfeng-cr--overlays nil))

(defun gongfeng-cr--line-end-pos (line)
  "Return the buffer position at the end of LINE (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-end-position)))

(defun gongfeng-cr--insert-comment-overlay (line author body &optional created-at)
  "Insert a read-only overlay after LINE showing AUTHOR, BODY, and CREATED-AT."
  (let* ((pos     (gongfeng-cr--line-end-pos line))
         (ov      (make-overlay pos pos nil t nil))
         (header  (propertize
                   (format "  💬 %s%s"
                           author
                           (if created-at (format "  [%s]" created-at) ""))
                   'face 'gongfeng-cr-header-face))
         ;; Indent body lines for readability
         (body-lines (mapconcat
                      (lambda (l) (concat "  │ " l))
                      (split-string body "\n")
                      "\n"))
         (text    (propertize
                   (concat "\n" header "\n"
                           (propertize body-lines 'face 'gongfeng-cr-comment-face)
                           "\n")
                   'cursor 0)))
    (overlay-put ov 'after-string text)
    (overlay-put ov 'gongfeng-cr t)
    (push ov gongfeng-cr--overlays)))

;;;; ─── MR id resolver ────────────────────────────────────────────────────────

(defun gongfeng-cr--resolve-mr-id (callback)
  "Resolve the global MR id for `gongfeng-cr--mr-iid' and call CALLBACK with it.

If `gongfeng-cr--mr-id' is already cached, CALLBACK is invoked immediately.
Otherwise calls:
  GET /projects/:project_id/merge_request/iid/:iid
and caches the returned .id field in `gongfeng-cr--mr-id'."
  (if gongfeng-cr--mr-id
      (funcall callback gongfeng-cr--mr-id)
    (let* ((project-id (gongfeng-cr--ensure-project-id))
           (url        (gongfeng-cr--api-url
                        "projects" project-id
                        "merge_request" "iid" (number-to-string gongfeng-cr--mr-iid)))
           (buf        (current-buffer)))
      (message "gongfeng-cr: resolving MR id for IID %d …" gongfeng-cr--mr-iid)
      (gongfeng-cr--http-request
       "GET" url nil
       (lambda (mr)
         (let ((mr-id (and mr (alist-get 'id mr))))
           (if (not (numberp mr-id))
               (message "gongfeng-cr: failed to resolve MR id (response: %S)" mr)
             (with-current-buffer buf
               (setq gongfeng-cr--mr-id mr-id))
             (funcall callback mr-id))))))))

;;;; ─── Fetching comments ─────────────────────────────────────────────────────

(defun gongfeng-cr--fetch-comments ()
  "Fetch MR comments from Gongfeng and render them as overlays.

Two-step process:
  1. GET /projects/:id/merge_request/iid/:iid  → resolves the global MR id
  2. GET /projects/:id/merge_requests/:mr_id/notes

Each comment object has the shape:
  { body, author{name,username}, created_at,
    file_path,
    note_position.latest_position.{right_line_num, left_line_num} }"
  (let* ((project-id (gongfeng-cr--ensure-project-id))
         (mr-iid     gongfeng-cr--mr-iid)
         (rel-path   (gongfeng-cr--relative-file-path))
         (buf        (current-buffer)))
    (message "gongfeng-cr: fetching comments for MR !%d …" mr-iid)
    (gongfeng-cr--resolve-mr-id
     (lambda (mr-id)
       (let ((url (concat (gongfeng-cr--api-url
                           "projects" project-id
                           "merge_requests" (number-to-string mr-id)
                           "notes")
                          "?per_page=100")))
         (gongfeng-cr--http-request
          "GET" url nil
          (lambda (comments)
            (with-current-buffer buf
              (gongfeng-cr--clear-overlays)
              (if (null comments)
                  (message "gongfeng-cr: no comments found (or request failed).")
                (let ((total (length comments))
                      (count 0))
                  (message "gongfeng-cr: fetched %d comment(s) total; current file rel-path=%S"
                           total rel-path)
                  (dolist (c comments)
                    ;; Gongfeng /comments response shape (actual field names):
                    ;;   .body              — the comment text
                    ;;   .file_path         — file path (relative to repo root)
                    ;;   .author.{name,username}
                    ;;   .created_at
                    ;;   .note_position.latest_position.{right_line_num, left_line_num}
                    ;;     right_line_num   — line on the new/right side of the diff
                    ;;     left_line_num    — line on the old/left side
                    (let* ((note-body  (alist-get 'body c))
                           (file-path  (alist-get 'file_path c))
                           (note-pos   (alist-get 'note_position c))
                           (latest-pos (and note-pos (alist-get 'latest_position note-pos)))
                           ;; prefer right (new) side; fall back to left (old) side
                           (line-num   (and latest-pos
                                            (or (alist-get 'right_line_num latest-pos)
                                                (alist-get 'left_line_num  latest-pos))))
                           (author-obj (alist-get 'author c))
                           (author     (if author-obj
                                           (or (alist-get 'name author-obj)
                                               (alist-get 'username author-obj)
                                               "unknown")
                                         "unknown"))
                           (created-at (alist-get 'created_at c)))
                      (when (and note-body
                                 (integerp line-num)
                                 file-path
                                 rel-path
                                 (string= file-path rel-path))
                        (gongfeng-cr--insert-comment-overlay
                         line-num author note-body created-at)
                        (cl-incf count))))
                  (message "gongfeng-cr: %d comment(s) in this file, %d total in MR."
                           count total)))))))))))

;;;; ─── Input overlay (adding a comment) ─────────────────────────────────────

(defvar gongfeng-cr--input-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'gongfeng-cr--submit-comment)
    (define-key m (kbd "C-c C-k") #'gongfeng-cr--cancel-comment)
    m)
  "Keymap active while a comment-input overlay is open.")

(defun gongfeng-cr--open-input-overlay (beg end)
  "Open an inline input overlay below the region from BEG to END."
  (when gongfeng-cr--input-overlay
    (gongfeng-cr--cancel-comment))
  (let* ((end-pos   (save-excursion (goto-char end) (line-end-position)))
         (ov        (make-overlay end-pos end-pos nil t nil))
         ;; A temporary indirect buffer lets the user type freely
         (ibuf      (generate-new-buffer "*gongfeng-cr-input*"))
         (prompt    (propertize
                     (concat "\n  ┌─ New CR comment "
                             (propertize "(C-c C-c submit, C-c C-k cancel)"
                                         'face '(:weight normal :slant italic))
                             "\n  │ ")
                     'face 'gongfeng-cr-input-face
                     'read-only t
                     'rear-nonsticky t)))
    ;; Store metadata on the overlay
    (overlay-put ov 'gongfeng-cr-input t)
    (overlay-put ov 'gongfeng-cr-region-beg beg)
    (overlay-put ov 'gongfeng-cr-region-end end)
    (overlay-put ov 'gongfeng-cr-input-buffer ibuf)
    (setq gongfeng-cr--input-overlay ov)
    ;; Switch to the input buffer in a side window
    (with-current-buffer ibuf
      (gongfeng-cr-input-mode)
      (insert prompt)
      (setq-local gongfeng-cr--input-overlay ov)
      (setq-local gongfeng-cr--input-prompt-end (point-marker)))
    (let ((win (display-buffer
                ibuf
                '(display-buffer-below-selected
                  (window-height . 6)))))
      (when win (select-window win)))
    (message "Type your comment, then C-c C-c to submit or C-c C-k to cancel.")))

(define-derived-mode gongfeng-cr-input-mode text-mode "CR-Input"
  "Transient mode for entering a Gongfeng CR comment.
The buffer coding system is set to UTF-8 so Chinese and other
non-ASCII input methods work correctly."
  (set-buffer-file-coding-system 'utf-8)
  (use-local-map gongfeng-cr--input-map))

(defun gongfeng-cr--get-input-text ()
  "Extract user-typed text from the input buffer (everything after the prompt)."
  (when gongfeng-cr--input-overlay
    (let ((ibuf (overlay-get gongfeng-cr--input-overlay 'gongfeng-cr-input-buffer)))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (string-trim
           (buffer-substring-no-properties
            gongfeng-cr--input-prompt-end
            (point-max))))))))

(defun gongfeng-cr--cancel-comment ()
  "Cancel the in-progress comment input without submitting."
  (interactive)
  (when gongfeng-cr--input-overlay
    (let ((ibuf (overlay-get gongfeng-cr--input-overlay 'gongfeng-cr-input-buffer)))
      (delete-overlay gongfeng-cr--input-overlay)
      (setq gongfeng-cr--input-overlay nil)
      (when (buffer-live-p ibuf)
        (let ((win (get-buffer-window ibuf)))
          (when win (delete-window win)))
        (kill-buffer ibuf))))
  (message "gongfeng-cr: comment cancelled."))

(defun gongfeng-cr--submit-comment ()
  "Submit the typed comment to the Gongfeng API and close the input overlay."
  (interactive)
  (let ((body (gongfeng-cr--get-input-text)))
    (if (or (null body) (string-empty-p body))
        (message "gongfeng-cr: empty comment, not submitting.")
      (let* ((ov        gongfeng-cr--input-overlay)
             (src-buf   (overlay-buffer ov))
             (beg       (overlay-get ov 'gongfeng-cr-region-beg))
             (end       (overlay-get ov 'gongfeng-cr-region-end)))
        (with-current-buffer src-buf
          (gongfeng-cr--post-comment beg end body)))))
  ;; Close overlay regardless
  (gongfeng-cr--cancel-comment))

;;;; ─── Posting a comment ─────────────────────────────────────────────────────

(defun gongfeng-cr--line-number-at (pos)
  "Return the 1-based line number for buffer position POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun gongfeng-cr--post-comment (_beg end body)
  "Post BODY as a line-comment ending at END of the current file.

Two-step process:
  1. GET /projects/:id/merge_request/iid/:iid  → resolves the global MR id
  2. POST /projects/:id/merge_requests/:mr_id/notes

Payload: { body, path, line, line_type }"
  (let* ((project-id (gongfeng-cr--ensure-project-id))
         (mr-iid     gongfeng-cr--mr-iid)
         (rel-path   (gongfeng-cr--relative-file-path))
         (end-line   (gongfeng-cr--line-number-at end))
         (src-buf    (current-buffer)))
    (message "gongfeng-cr: posting comment to MR !%d (line %d) …" mr-iid end-line)
    (gongfeng-cr--resolve-mr-id
     (lambda (mr-id)
       (let* ((url     (gongfeng-cr--api-url
                        "projects" project-id
                        "merge_requests" (number-to-string mr-id)
                        "notes"))
              ;; POST /merge_requests/:id/notes payload (Gongfeng API v3):
              ;; body      — comment text (required)
              ;; path      — file path relative to repo root (for line comments)
              ;; line      — line number as string (for line comments)
              ;; line_type — "new" (right side) or "old" (left side)
              (payload `((body      . ,body)
                         (path      . ,rel-path)
                         (line      . ,(number-to-string end-line))
                         (line_type . "new"))))
         (gongfeng-cr--http-request
          "POST" url payload
          (lambda (resp)
            (if (and resp (alist-get 'id resp))
                (progn
                  (message "gongfeng-cr: comment posted (id=%s)." (alist-get 'id resp))
                  (with-current-buffer src-buf
                    (gongfeng-cr--fetch-comments)))
              (message "gongfeng-cr: failed to post comment — see *Messages* for details.")))))))))

;;;; ─── Diagnostics ───────────────────────────────────────────────────────────

;;;###autoload
(defun gongfeng-cr-diagnose ()
  "Show diagnostic information and test the API connection.

Three HTTP requests are made:
  1. GET /projects/:id/merge_requests?state=opened&per_page=10
     Lists the most recent open MRs so you can verify the correct IID.
  2. GET /projects/:id/merge_request/iid/:iid
     Resolves the global MR id from the IID.
  3. GET /projects/:id/merge_request/:mr_id/comments?per_page=5
     Tests fetching comments using the resolved MR id.

Results are shown in the *gongfeng-cr-diagnose* buffer."
  (interactive)
  (gongfeng-cr--assert-token)
  (let* ((remote  (gongfeng-cr--git-remote-url))
         (path    (gongfeng-cr--parse-project-path remote))
         (encoded (when path (gongfeng-cr--url-encode-path path)))
         (iid     (or gongfeng-cr--mr-iid
                      (read-number "MR IID for test request: ")))
         (base    (when encoded
                    (gongfeng-cr--api-url "projects" encoded)))
         (mr-list-url  (when base
                         (concat base "/merge_requests?state=opened&per_page=10")))
         (mr-resolve-url (when base
                           (concat base "/merge_request/iid/" (number-to-string iid)))))
    (with-current-buffer (get-buffer-create "*gongfeng-cr-diagnose*")
      (erase-buffer)
      (insert "=== gongfeng-cr diagnostics ===\n\n")
      (insert (format "git remote url : %s\n" (or remote "(none)")))
      (insert (format "parsed path    : %s\n" (or path "(failed — will prompt)")))
      (insert (format "encoded path   : %s\n" (or encoded "(n/a)")))
      (insert (format "base URL       : %s\n" gongfeng-cr-base-url))
      (insert (format "MR IID tested  : %s\n\n" iid))
      (insert "── Step 1: list open MRs ──────────────────────────\n")
      (insert (format "URL: %s\n\n" (or mr-list-url "(cannot build)")))
      (display-buffer (current-buffer)))
    ;; Request 1 — list open MRs to show available IIDs
    (when mr-list-url
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              `(("PRIVATE-TOKEN" . ,gongfeng-cr-private-token))))
        (url-retrieve
         mr-list-url
         (lambda (_status)
           (let* ((body  (gongfeng-cr--response-body))
                  (code  (gongfeng-cr--http-status-code))
                  (mrs   (condition-case nil
                             (let ((json-object-type 'alist)
                                   (json-array-type  'list)
                                   (json-key-type    'symbol))
                               (json-read-from-string body))
                           (error nil))))
             (with-current-buffer (get-buffer-create "*gongfeng-cr-diagnose*")
               (goto-char (point-max))
               (insert (format "HTTP status: %s\n" (or code "?")))
               (if (and (listp mrs) mrs)
                   (progn
                     (insert "Open MRs (iid  title):\n")
                     (dolist (mr mrs)
                       (insert (format "  !%-6s  %s\n"
                                       (alist-get 'iid mr "?")
                                       (alist-get 'title mr "(no title)")))))
                 (insert (format "Raw response:\n%s\n"
                                 (substring body 0 (min 600 (length body))))))
               (insert (format "\n── Step 2: resolve MR id for IID !%s ───────────────\n" iid))
               (insert (format "URL: %s\n\n" mr-resolve-url)))))
         nil t)))
    ;; Request 2 — resolve IID → global MR id, then fetch comments
    (when mr-resolve-url
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              `(("PRIVATE-TOKEN" . ,gongfeng-cr-private-token))))
        (url-retrieve
         mr-resolve-url
         (lambda (_status)
           (let* ((body    (gongfeng-cr--response-body))
                  (code    (gongfeng-cr--http-status-code))
                  (mr-obj  (condition-case nil
                               (let ((json-object-type 'alist)
                                     (json-array-type  'list)
                                     (json-key-type    'symbol))
                                 (json-read-from-string body))
                             (error nil)))
                  (mr-id   (and mr-obj (alist-get 'id mr-obj))))
             (with-current-buffer (get-buffer-create "*gongfeng-cr-diagnose*")
               (goto-char (point-max))
               (insert (format "HTTP status: %s\n" (or code "?")))
               (if (numberp mr-id)
                   (progn
                     (insert (format "Resolved MR id: %d\n" mr-id))
                     (let ((comments-url (concat base "/merge_request/"
                                                 (number-to-string mr-id)
                                                 "/comments?per_page=5")))
                       (insert (format "\n── Step 3: comments for MR id %d ───────────────────\n" mr-id))
                       (insert (format "URL: %s\n\n" comments-url))
                       ;; Request 3 — fetch comments
                       (let* ((url-request-method "GET")
                              (url-request-extra-headers
                               `(("PRIVATE-TOKEN" . ,gongfeng-cr-private-token))))
                         (url-retrieve
                          comments-url
                          (lambda (_status2)
                            (let ((decoded (gongfeng-cr--response-body)))
                              (with-current-buffer (get-buffer-create "*gongfeng-cr-diagnose*")
                                (goto-char (point-max))
                                (insert decoded)
                                (insert "\n=== done ===\n")
                                (message "gongfeng-cr-diagnose: done — see *gongfeng-cr-diagnose*"))))
                          nil t))))
                 (insert (format "Failed to resolve MR id.\nRaw: %s\n=== done ===\n"
                                 (substring body 0 (min 400 (length body)))))
                 (message "gongfeng-cr-diagnose: done — see *gongfeng-cr-diagnose*")))))
         nil t)))))

;;;; ─── Public commands ────────────────────────────────────────────────────────

;;;###autoload
(defun gongfeng-cr-add-comment (beg end)
  "Add a CR comment for the selected region (BEG to END).
Opens an inline input overlay; press C-c C-c to submit, C-c C-k to cancel."
  (interactive "r")
  (gongfeng-cr--assert-token)
  (unless (bound-and-true-p gongfeng-cr-mode)
    (user-error "gongfeng-cr: please enable `gongfeng-cr-mode' first"))
  (unless gongfeng-cr--mr-iid
    (user-error "gongfeng-cr: no MR IID set — toggle the mode off/on to set one"))
  (gongfeng-cr--open-input-overlay beg end))

;;;###autoload
(defun gongfeng-cr-refresh ()
  "Re-fetch and redisplay all comments for the current MR / file."
  (interactive)
  (gongfeng-cr--assert-token)
  (unless gongfeng-cr--mr-iid
    (user-error "gongfeng-cr: no MR IID — toggle the mode off/on"))
  (gongfeng-cr--fetch-comments))

;;;; ─── Minor mode ─────────────────────────────────────────────────────────────

(defvar gongfeng-cr-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c r c") #'gongfeng-cr-add-comment)
    (define-key m (kbd "C-c r r") #'gongfeng-cr-refresh)
    m)
  "Keymap for `gongfeng-cr-mode'.")

;;;###autoload
(define-minor-mode gongfeng-cr-mode
  "Minor mode to review a Gongfeng MR inline using overlays.

When enabled you will be asked for the MR IID; comments targeting the
current file are then fetched and shown as inline overlays.

Key bindings:
  C-c r c   `gongfeng-cr-add-comment'   (requires an active region)
  C-c r r   `gongfeng-cr-refresh'       re-fetch comments"
  :lighter " GF-CR"
  :keymap gongfeng-cr-mode-map
  (if gongfeng-cr-mode
      (progn
        (gongfeng-cr--assert-token)
        (unless gongfeng-cr--mr-iid
          (let ((iid (read-number "MR IID to review (the integer in the MR URL): ")))
            (setq gongfeng-cr--mr-iid iid)))
        (gongfeng-cr--ensure-project-id)
        (gongfeng-cr--fetch-comments))
    ;; Disable: clean up
    (gongfeng-cr--clear-overlays)
    (when gongfeng-cr--input-overlay
      (gongfeng-cr--cancel-comment))
    (setq gongfeng-cr--mr-iid     nil
          gongfeng-cr--mr-id      nil
          gongfeng-cr--project-id nil)))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'gongfeng-cr)

;;; gongfeng-cr.el ends here
