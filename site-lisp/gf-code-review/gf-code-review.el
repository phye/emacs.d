;;; gf-code-review.el --- Code Review overlay for Gongfeng (Tencent GitLab) MRs -*- lexical-binding: t; -*-

;; Author: phye
;; Version: 0.1.0
;; Keywords: tools, vc, review
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; gf-code-review is a minor mode for performing code review directly inside Emacs
;; against Gongfeng (git.woa.com) Merge Requests.
;;
;; Quick start:
;;   1. Set your Gongfeng private token once:
;;        M-x gf-code-review-set-token
;;      or add to your init file:
;;        (setq gf-code-review-private-token "YOUR_TOKEN")
;;
;;   2. Open any file that belongs to a Gongfeng project, then:
;;        M-x gf-code-review-mode
;;      You will be prompted for the MR IID (the per-project integer id shown
;;      in the MR URL, e.g. 42 in  .../merge_requests/42).
;;      Existing comments for the current file are fetched and shown as
;;      overlays inline in the buffer.
;;
;;   3. To add a new comment, select a region, then:
;;        M-x gf-code-review-add-comment
;;      An overlay input area opens beneath the selection.  Type your comment
;;      and press C-c C-c to submit, or C-c C-k to cancel.
;;
;; Troubleshooting:
;;   If you get HTTP 404 errors, run M-x gf-code-review-diagnose to see exactly
;;   which URL is being requested and what the server returns.  Common causes:
;;     - Wrong project path (check gf-code-review--project-id in *Messages*)
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

(defgroup gf-code-review nil
  "Code-review overlays for Gongfeng Merge Requests."
  :group 'tools
  :prefix "gf-code-review-")

(defcustom gf-code-review-private-token nil
  "Your Gongfeng private token.
Set this variable (e.g. via `gf-code-review-set-token') before using the mode.
The token is sent in the PRIVATE-TOKEN HTTP header on every API request."
  :type '(choice (const :tag "Not set" nil) (string :tag "Token"))
  :group 'gf-code-review)

(defcustom gf-code-review-base-url "https://git.woa.com/api/v3"
  "Base URL for the Gongfeng REST API (without trailing slash)."
  :type 'string
  :group 'gf-code-review)

(defcustom gf-code-review-comment-face 'gf-code-review-comment-face
  "Face used for comment overlay text."
  :type 'face
  :group 'gf-code-review)

;;;; ─── Faces ─────────────────────────────────────────────────────────────────

(defface gf-code-review-comment-face
  '((((background dark))
     :background "#2a2a4a"
     :foreground "#a0c8ff"
     :box (:line-width 1 :color "#5080c0")
     :extend t)
    (t :background "#eef4ff" :foreground "#2040a0" :box (:line-width 1 :color "#8090c0") :extend t))
  "Face for displaying fetched CR comments."
  :group 'gf-code-review)

(defface gf-code-review-input-face
  '((((background dark))
     :background "#2a3a2a"
     :foreground "#a0ffa0"
     :box (:line-width 1 :color "#50a050")
     :extend t)
    (t :background "#f0fff0" :foreground "#205020" :box (:line-width 1 :color "#70a070") :extend t))
  "Face for the comment-input overlay."
  :group 'gf-code-review)

(defface gf-code-review-header-face
  '((((background dark)) :foreground "#80c0ff" :weight bold) (t :foreground "#1040c0" :weight bold))
  "Face for the header line inside a comment overlay."
  :group 'gf-code-review)

;;;; ─── Buffer-local state ────────────────────────────────────────────────────

(defvar-local gf-code-review--mr-iid nil
  "MR IID (per-project integer id) currently being reviewed.")

(defvar-local gf-code-review--mr-id nil
  "MR global integer id resolved from `gf-code-review--mr-iid'.
Cached after the first successful IID→ID lookup so subsequent operations
\(fetch comments, post comment) do not repeat the resolve request.")

(defvar-local gf-code-review--project-id nil
  "URL-encoded full_path of the current project, derived from the git remote.")

(defvar-local gf-code-review--overlays nil
  "List of comment overlays created by `gf-code-review-mode'.")

(defvar-local gf-code-review--input-overlay nil
  "The currently active comment-input overlay, if any.")

(defvar-local gf-code-review--input-prompt-end nil
  "Marker pointing to the end of the prompt in the input buffer.
Text after this position is the user's comment.")

;;;; ─── Token management ──────────────────────────────────────────────────────

;;;###autoload
(defun gf-code-review-set-token (token)
  "Interactively set `gf-code-review-private-token' to TOKEN.
This is the only configuration you normally need to perform."
  (interactive (list (read-string "Gongfeng private token: " gf-code-review-private-token)))
  (setq gf-code-review-private-token token)
  (message "gf-code-review: token set."))

(defun gf-code-review--assert-token ()
  "Signal an error if `gf-code-review-private-token' is not configured."
  (unless (and gf-code-review-private-token (not (string-empty-p gf-code-review-private-token)))
    (user-error "gf-code-review: Please set your token first via M-x gf-code-review-set-token")))

;;;; ─── Project / remote helpers ─────────────────────────────────────────────

(defun gf-code-review--git-remote-url ()
  "Return the URL of the `origin' remote for the current buffer's repo."
  (let ((default-directory
         (or (locate-dominating-file (or buffer-file-name default-directory) ".git")
             default-directory)))
    (string-trim (shell-command-to-string "git remote get-url origin 2>/dev/null"))))

(defun gf-code-review--parse-project-path (remote-url)
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

(defun gf-code-review--url-encode-path (path)
  "URL-encode a project PATH (replace / with %2F, etc.)."
  (url-hexify-string path))

(defun gf-code-review--ensure-project-id ()
  "Set `gf-code-review--project-id' from the git remote, or ask the user."
  (unless gf-code-review--project-id
    (let* ((remote (gf-code-review--git-remote-url))
           (path (gf-code-review--parse-project-path remote)))
      (if path
          (progn
            (message "gf-code-review: detected project %s" path)
            (setq gf-code-review--project-id (gf-code-review--url-encode-path path)))
        ;; Fallback: ask the user
        (let ((manual (read-string "Gongfeng project full_path (e.g. team/myproject): ")))
          (setq gf-code-review--project-id (gf-code-review--url-encode-path manual))))))
  gf-code-review--project-id)

;;;; ─── HTTP helpers ──────────────────────────────────────────────────────────

(defun gf-code-review--api-url (&rest path-segments)
  "Build a full API URL by joining PATH-SEGMENTS onto `gf-code-review-base-url'."
  (concat gf-code-review-base-url "/" (mapconcat #'identity path-segments "/")))

(defun gf-code-review--http-request (method url &optional payload callback)
  "Perform an async HTTP METHOD request to URL.
PAYLOAD is an alist that will be JSON-encoded and sent as the request body.
CALLBACK is called with the parsed JSON response (or nil on error)."
  (gf-code-review--assert-token)
  (message "gf-code-review: %s %s" method url)
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gf-code-review-private-token)
            ("Content-Type" . "application/json; charset=utf-8")))
         (url-request-data
          (when payload
            (let* ((body (json-encode payload))
                   ;; encode-coding-string produces a unibyte UTF-8 string,
                   ;; which url-http-create-request requires (Emacs Bug#23750).
                   (encoded (encode-coding-string body 'utf-8)))
              encoded))))
    (url-retrieve
     url
     (lambda (status)
       (let* ((http-status (gf-code-review--http-status-code))
              (err (plist-get status :error))
              (resp-body (gf-code-review--response-body)))
         (message "gf-code-review: response status=%S http=%S body=%s"
                  err
                  http-status
                  (substring resp-body 0 (min 400 (length resp-body))))
         (cond
          (err
           (message "gf-code-review: HTTP error %S (URL: %s)\n  response: %s"
                    err
                    url
                    (substring resp-body 0 (min 400 (length resp-body)))))
          ((and http-status (>= http-status 400))
           (message "gf-code-review: HTTP %d for %s\n  response: %s"
                    http-status
                    url
                    (substring resp-body 0 (min 400 (length resp-body)))))
          (t
           (let ((result (gf-code-review--parse-response)))
             (when callback
               (funcall callback result)))))))
     nil t)))

(defun gf-code-review--http-status-code ()
  "Return the integer HTTP status code from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun gf-code-review--response-body ()
  "Return the response body string from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*$" nil t)
        (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)
      "")))

(defun gf-code-review--parse-response ()
  "Parse JSON from the current url-retrieve response buffer.
Moves past the HTTP headers and decodes the body."
  (let ((body (gf-code-review--response-body)))
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string body))
      (error
       (message "gf-code-review: JSON parse error: %S\nbody: %s" err body)
       nil))))

;;;; ─── Relative file path ────────────────────────────────────────────────────

(defun gf-code-review--relative-file-path ()
  "Return the path of the current buffer's file relative to the git root."
  (when buffer-file-name
    (let* ((root (locate-dominating-file buffer-file-name ".git")))
      (if root
          (file-relative-name buffer-file-name (expand-file-name root))
        (file-name-nondirectory buffer-file-name)))))

;;;; ─── Overlay utilities ─────────────────────────────────────────────────────

(defun gf-code-review--clear-overlays ()
  "Remove all comment overlays from the current buffer."
  (mapc #'delete-overlay gf-code-review--overlays)
  (setq gf-code-review--overlays nil))

(defun gf-code-review--line-end-pos (line)
  "Return the buffer position at the end of LINE (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-end-position)))

(defun gf-code-review--insert-comment-overlay (line author body &optional created-at)
  "Insert a read-only overlay after LINE showing AUTHOR, BODY, and CREATED-AT."
  (let* ((pos (gf-code-review--line-end-pos line))
         (ov (make-overlay pos pos nil t nil))
         (header
          (propertize (format "  💬 %s%s"
                              author
                              (if created-at
                                  (format "  [%s]" created-at)
                                ""))
                      'face 'gf-code-review-header-face))
         ;; Indent body lines for readability
         (body-lines (mapconcat (lambda (l) (concat "  │ " l)) (split-string body "\n") "\n"))
         (text
          (propertize (concat
                       "\n"
                       header
                       "\n"
                       (propertize body-lines 'face 'gf-code-review-comment-face)
                       "\n")
                      'cursor 0)))
    (overlay-put ov 'after-string text)
    (overlay-put ov 'gf-code-review t)
    (push ov gf-code-review--overlays)))

;;;; ─── MR id resolver ────────────────────────────────────────────────────────

(defun gf-code-review--resolve-mr-id (callback)
  "Resolve the global MR id for `gf-code-review--mr-iid' and call CALLBACK with it.

If `gf-code-review--mr-id' is already cached, CALLBACK is invoked immediately.
Otherwise calls:
  GET /projects/:project_id/merge_request/iid/:iid
and caches the returned .id field in `gf-code-review--mr-id'."
  (if gf-code-review--mr-id
      (funcall callback gf-code-review--mr-id)
    (let* ((project-id (gf-code-review--ensure-project-id))
           (url
            (gf-code-review--api-url
             "projects" project-id "merge_request" "iid" (number-to-string gf-code-review--mr-iid)))
           (buf (current-buffer)))
      (message "gf-code-review: resolving MR id for IID %d …" gf-code-review--mr-iid)
      (gf-code-review--http-request
       "GET" url
       nil
       (lambda (mr)
         (let ((mr-id (and mr (alist-get 'id mr))))
           (if (not (numberp mr-id))
               (message "gf-code-review: failed to resolve MR id (response: %S)" mr)
             (with-current-buffer buf
               (setq gf-code-review--mr-id mr-id))
             (funcall callback mr-id))))))))

;;;; ─── Fetching comments ─────────────────────────────────────────────────────

(defun gf-code-review--fetch-comments ()
  "Fetch MR comments from Gongfeng and render them as overlays.

Two-step process:
  1. GET /projects/:id/merge_request/iid/:iid  → resolves the global MR id
  2. GET /projects/:id/merge_requests/:mr_id/notes

Each comment object has the shape:
  { body, author{name,username}, created_at,
    file_path,
    note_position.latest_position.{right_line_num, left_line_num} }"
  (let* ((project-id (gf-code-review--ensure-project-id))
         (mr-iid gf-code-review--mr-iid)
         (rel-path (gf-code-review--relative-file-path))
         (buf (current-buffer)))
    (message "gf-code-review: fetching comments for MR !%d …" mr-iid)
    (gf-code-review--resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (concat
               (gf-code-review--api-url
                "projects" project-id "merge_requests" (number-to-string mr-id) "notes")
               "?per_page=100")))
         (gf-code-review--http-request
          "GET" url
          nil
          (lambda (comments)
            (with-current-buffer buf
              (gf-code-review--clear-overlays)
              (if (null comments)
                  (message "gf-code-review: no comments found (or request failed).")
                (let ((total (length comments))
                      (count 0))
                  (message "gf-code-review: fetched %d comment(s) total; current file rel-path=%S"
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
                    (let* ((note-body (alist-get 'body c))
                           (file-path (alist-get 'file_path c))
                           (note-pos (alist-get 'note_position c))
                           (latest-pos (and note-pos (alist-get 'latest_position note-pos)))
                           ;; prefer right (new) side; fall back to left (old) side
                           (line-num
                            (and latest-pos
                                 (or (alist-get 'right_line_num latest-pos)
                                     (alist-get 'left_line_num latest-pos))))
                           (author-obj (alist-get 'author c))
                           (author
                            (if author-obj
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
                        (gf-code-review--insert-comment-overlay line-num author note-body
                                                                created-at)
                        (cl-incf count))))
                  (message "gf-code-review: %d comment(s) in this file, %d total in MR."
                           count total)))))))))))

;;;; ─── Input overlay (adding a comment) ─────────────────────────────────────

(defvar gf-code-review--input-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'gf-code-review--submit-comment)
    (define-key m (kbd "C-c C-k") #'gf-code-review--cancel-comment)
    m)
  "Keymap active while a comment-input overlay is open.")

(defun gf-code-review--open-input-overlay (beg end)
  "Open an inline input overlay below the region from BEG to END."
  (when gf-code-review--input-overlay
    (gf-code-review--cancel-comment))
  (let* ((end-pos
          (save-excursion
            (goto-char end)
            (line-end-position)))
         (ov (make-overlay end-pos end-pos nil t nil))
         ;; A temporary indirect buffer lets the user type freely
         (ibuf (generate-new-buffer "*gf-code-review-input*"))
         (prompt
          (propertize (concat
                       "\n  ┌─ New CR comment "
                       (propertize "(C-c C-c submit, C-c C-k cancel)"
                                   'face
                                   '(:weight normal :slant italic))
                       "\n  │ ")
                      'face 'gf-code-review-input-face 'read-only t 'rear-nonsticky t)))
    ;; Store metadata on the overlay
    (overlay-put ov 'gf-code-review-input t)
    (overlay-put ov 'gf-code-review-region-beg beg)
    (overlay-put ov 'gf-code-review-region-end end)
    (overlay-put ov 'gf-code-review-input-buffer ibuf)
    (setq gf-code-review--input-overlay ov)
    ;; Switch to the input buffer in a side window
    (with-current-buffer ibuf
      (gf-code-review-input-mode)
      (insert prompt)
      (setq-local gf-code-review--input-overlay ov)
      (setq-local gf-code-review--input-prompt-end (point-marker)))
    (let ((win (display-buffer ibuf '(display-buffer-below-selected (window-height . 6)))))
      (when win
        (select-window win)))
    (message "Type your comment, then C-c C-c to submit or C-c C-k to cancel.")))

(define-derived-mode gf-code-review-input-mode text-mode "CR-Input"
  "Transient mode for entering a Gongfeng CR comment.
The buffer coding system is set to UTF-8 so Chinese and other
non-ASCII input methods work correctly."
  (set-buffer-file-coding-system 'utf-8)
  (use-local-map gf-code-review--input-map))

(defun gf-code-review--get-input-text ()
  "Extract user-typed text from the input buffer (everything after the prompt)."
  (when gf-code-review--input-overlay
    (let ((ibuf (overlay-get gf-code-review--input-overlay 'gf-code-review-input-buffer)))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (string-trim
           (buffer-substring-no-properties gf-code-review--input-prompt-end (point-max))))))))

(defun gf-code-review--cancel-comment ()
  "Cancel the in-progress comment input without submitting."
  (interactive)
  (when gf-code-review--input-overlay
    (let ((ibuf (overlay-get gf-code-review--input-overlay 'gf-code-review-input-buffer)))
      (delete-overlay gf-code-review--input-overlay)
      (setq gf-code-review--input-overlay nil)
      (when (buffer-live-p ibuf)
        (let ((win (get-buffer-window ibuf)))
          (when win
            (delete-window win)))
        (kill-buffer ibuf))))
  (message "gf-code-review: comment cancelled."))

(defun gf-code-review--submit-comment ()
  "Submit the typed comment to the Gongfeng API and close the input overlay."
  (interactive)
  (let ((body (gf-code-review--get-input-text)))
    (if (or (null body) (string-empty-p body))
        (message "gf-code-review: empty comment, not submitting.")
      (let* ((ov gf-code-review--input-overlay)
             (src-buf (overlay-buffer ov))
             (beg (overlay-get ov 'gf-code-review-region-beg))
             (end (overlay-get ov 'gf-code-review-region-end)))
        (with-current-buffer src-buf
          (gf-code-review--post-comment beg end body)))))
  ;; Close overlay regardless
  (gf-code-review--cancel-comment))

;;;; ─── Posting a comment ─────────────────────────────────────────────────────

(defun gf-code-review--line-number-at (pos)
  "Return the 1-based line number for buffer position POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun gf-code-review--post-comment (_beg end body)
  "Post BODY as a line-comment ending at END of the current file.

Two-step process:
  1. GET /projects/:id/merge_request/iid/:iid  → resolves the global MR id
  2. POST /projects/:id/merge_requests/:mr_id/notes

Payload: { body, path, line, line_type }"
  (let* ((project-id (gf-code-review--ensure-project-id))
         (mr-iid gf-code-review--mr-iid)
         (rel-path (gf-code-review--relative-file-path))
         (end-line (gf-code-review--line-number-at end))
         (src-buf (current-buffer)))
    (message "gf-code-review: posting comment %s to MR !%d (line %d) …" body mr-iid end-line)
    (gf-code-review--resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (gf-code-review--api-url
                "projects" project-id "merge_requests" (number-to-string mr-id) "notes"))
              ;; POST /merge_requests/:id/notes payload (Gongfeng API v3):
              ;; body      — comment text (required)
              ;; path      — file path relative to repo root (for line comments)
              ;; line      — line number as string (for line comments)
              ;; line_type — "new" (right side) or "old" (left side)
              (payload
               `((body . ,body)
                 (path . ,rel-path)
                 (line . ,(number-to-string end-line))
                 (line_type . "new"))))
         (gf-code-review--http-request
          "POST" url
          payload
          (lambda (resp)
            (if (and resp (alist-get 'id resp))
                (progn
                  (message "gf-code-review: comment posted (id=%s)." (alist-get 'id resp))
                  (with-current-buffer src-buf
                    (gf-code-review--fetch-comments)))
              (message
               "gf-code-review: failed to post comment — see *Messages* for details.")))))))))

;;;; ─── Diagnostics ───────────────────────────────────────────────────────────

;;;###autoload
(defun gf-code-review-diagnose ()
  "Show diagnostic information and test the API connection.

Three HTTP requests are made:
  1. GET /projects/:id/merge_requests?state=opened&per_page=10
     Lists the most recent open MRs so you can verify the correct IID.
  2. GET /projects/:id/merge_request/iid/:iid
     Resolves the global MR id from the IID.
  3. GET /projects/:id/merge_request/:mr_id/comments?per_page=5
     Tests fetching comments using the resolved MR id.

Results are shown in the *gf-code-review-diagnose* buffer."
  (interactive)
  (gf-code-review--assert-token)
  (let* ((remote (gf-code-review--git-remote-url))
         (path (gf-code-review--parse-project-path remote))
         (encoded
          (when path
            (gf-code-review--url-encode-path path)))
         (iid (or gf-code-review--mr-iid (read-number "MR IID for test request: ")))
         (base
          (when encoded
            (gf-code-review--api-url "projects" encoded)))
         (mr-list-url
          (when base
            (concat base "/merge_requests?state=opened&per_page=10")))
         (mr-resolve-url
          (when base
            (concat base "/merge_request/iid/" (number-to-string iid)))))
    (with-current-buffer (get-buffer-create "*gf-code-review-diagnose*")
      (erase-buffer)
      (insert "=== gf-code-review diagnostics ===\n\n")
      (insert (format "git remote url : %s\n" (or remote "(none)")))
      (insert (format "parsed path    : %s\n" (or path "(failed — will prompt)")))
      (insert (format "encoded path   : %s\n" (or encoded "(n/a)")))
      (insert (format "base URL       : %s\n" gf-code-review-base-url))
      (insert (format "MR IID tested  : %s\n\n" iid))
      (insert "── Step 1: list open MRs ──────────────────────────\n")
      (insert (format "URL: %s\n\n" (or mr-list-url "(cannot build)")))
      (display-buffer (current-buffer)))
    ;; Request 1 — list open MRs to show available IIDs
    (when mr-list-url
      (let* ((url-request-method "GET")
             (url-request-extra-headers `(("PRIVATE-TOKEN" . ,gf-code-review-private-token))))
        (url-retrieve
         mr-list-url
         (lambda (_status)
           (let* ((body (gf-code-review--response-body))
                  (code (gf-code-review--http-status-code))
                  (mrs
                   (condition-case nil
                       (let ((json-object-type 'alist)
                             (json-array-type 'list)
                             (json-key-type 'symbol))
                         (json-read-from-string body))
                     (error
                      nil))))
             (with-current-buffer (get-buffer-create "*gf-code-review-diagnose*")
               (goto-char (point-max))
               (insert (format "HTTP status: %s\n" (or code "?")))
               (if (and (listp mrs) mrs)
                   (progn
                     (insert "Open MRs (iid  title):\n")
                     (dolist (mr mrs)
                       (insert
                        (format "  !%-6s  %s\n"
                                (alist-get 'iid mr "?")
                                (alist-get 'title mr "(no title)")))))
                 (insert (format "Raw response:\n%s\n" (substring body 0 (min 600 (length body))))))
               (insert (format "\n── Step 2: resolve MR id for IID !%s ───────────────\n" iid))
               (insert (format "URL: %s\n\n" mr-resolve-url)))))
         nil t)))
    ;; Request 2 — resolve IID → global MR id, then fetch comments
    (when mr-resolve-url
      (let* ((url-request-method "GET")
             (url-request-extra-headers `(("PRIVATE-TOKEN" . ,gf-code-review-private-token))))
        (url-retrieve
         mr-resolve-url
         (lambda (_status)
           (let* ((body (gf-code-review--response-body))
                  (code (gf-code-review--http-status-code))
                  (mr-obj
                   (condition-case nil
                       (let ((json-object-type 'alist)
                             (json-array-type 'list)
                             (json-key-type 'symbol))
                         (json-read-from-string body))
                     (error
                      nil)))
                  (mr-id (and mr-obj (alist-get 'id mr-obj))))
             (with-current-buffer (get-buffer-create "*gf-code-review-diagnose*")
               (goto-char (point-max))
               (insert (format "HTTP status: %s\n" (or code "?")))
               (if (numberp mr-id)
                   (progn
                     (insert (format "Resolved MR id: %d\n" mr-id))
                     (let ((comments-url
                            (concat
                             base
                             "/merge_request/"
                             (number-to-string mr-id)
                             "/comments?per_page=5")))
                       (insert
                        (format "\n── Step 3: comments for MR id %d ───────────────────\n" mr-id))
                       (insert (format "URL: %s\n\n" comments-url))
                       ;; Request 3 — fetch comments
                       (let* ((url-request-method "GET")
                              (url-request-extra-headers
                               `(("PRIVATE-TOKEN" . ,gf-code-review-private-token))))
                         (url-retrieve
                          comments-url
                          (lambda (_status2)
                            (let ((decoded (gf-code-review--response-body)))
                              (with-current-buffer (get-buffer-create "*gf-code-review-diagnose*")
                                (goto-char (point-max))
                                (insert decoded)
                                (insert "\n=== done ===\n")
                                (message
                                 "gf-code-review-diagnose: done — see *gf-code-review-diagnose*"))))
                          nil t))))
                 (insert
                  (format "Failed to resolve MR id.\nRaw: %s\n=== done ===\n"
                          (substring body 0 (min 400 (length body)))))
                 (message "gf-code-review-diagnose: done — see *gf-code-review-diagnose*")))))
         nil t)))))

;;;; ─── Public commands ────────────────────────────────────────────────────────

;;;###autoload
(defun gf-code-review-add-comment (beg end)
  "Add a CR comment for the selected region (BEG to END).
Opens an inline input overlay; press C-c C-c to submit, C-c C-k to cancel."
  (interactive "r")
  (gf-code-review--assert-token)
  (unless (bound-and-true-p gf-code-review-mode)
    (user-error "gf-code-review: please enable `gf-code-review-mode' first"))
  (unless gf-code-review--mr-iid
    (user-error "gf-code-review: no MR IID set — toggle the mode off/on to set one"))
  (gf-code-review--open-input-overlay beg end))

;;;###autoload
(defun gf-code-review-refresh ()
  "Re-fetch and redisplay all comments for the current MR / file."
  (interactive)
  (gf-code-review--assert-token)
  (unless gf-code-review--mr-iid
    (user-error "gf-code-review: no MR IID — toggle the mode off/on"))
  (gf-code-review--fetch-comments))

;;;; ─── Minor mode ─────────────────────────────────────────────────────────────

(defvar gf-code-review-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c r c") #'gf-code-review-add-comment)
    (define-key m (kbd "C-c r r") #'gf-code-review-refresh)
    m)
  "Keymap for `gf-code-review-mode'.")

;;;###autoload
(define-minor-mode gf-code-review-mode
  "Minor mode to review a Gongfeng MR inline using overlays.

When enabled you will be asked for the MR IID; comments targeting the
current file are then fetched and shown as inline overlays.

Key bindings:
  C-c r c   `gf-code-review-add-comment'   (requires an active region)
  C-c r r   `gf-code-review-refresh'       re-fetch comments"
  :lighter " GF-CR"
  :keymap
  gf-code-review-mode-map
  (if gf-code-review-mode
      (progn
        (gf-code-review--assert-token)
        (unless gf-code-review--mr-iid
          (let ((iid (read-number "MR IID to review (the integer in the MR URL): ")))
            (setq gf-code-review--mr-iid iid)))
        (gf-code-review--ensure-project-id)
        (gf-code-review--fetch-comments))
    ;; Disable: clean up
    (gf-code-review--clear-overlays)
    (when gf-code-review--input-overlay
      (gf-code-review--cancel-comment))
    (setq
     gf-code-review--mr-iid nil
     gf-code-review--mr-id nil
     gf-code-review--project-id nil)))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'gf-code-review)

;;; gf-code-review.el ends here
