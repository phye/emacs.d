;;; code-review-minimal.el --- Minimal Code Review overlay for GitHub/GitLab/Gongfeng -*- lexical-binding: t; -*-

;; Author: phye
;; Version: 0.2.0
;; Keywords: tools, vc, review
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; code-review-minimal is a lightweight minor mode for performing code review
;; directly inside Emacs against GitHub Pull Requests, GitLab Merge Requests,
;; and Gongfeng (Tencent GitLab) MRs.
;;
;; Quick start:
;;   1. Configure authentication:
;;      - GitHub: Set `code-review-minimal-github-token'
;;      - GitLab/Gongfeng: Set `code-review-minimal-gitlab-token'
;;
;;   2. Open any file that belongs to a tracked project, then:
;;        M-x code-review-minimal-mode
;;      The backend is auto-detected from the git remote URL.
;;
;;   3. To add a new comment, select a region, then:
;;        M-x code-review-minimal-add-comment
;;      An overlay input area opens beneath the selection.
;;      Type your comment and press C-c C-c to submit, or C-c C-k to cancel.
;;
;; Supported backends:
;;   - github    : github.com and GitHub Enterprise
;;   - gitlab    : gitlab.com and self-hosted GitLab
;;   - gongfeng  : git.woa.com (Tencent GitLab, API v3)
;;
;; Authentication methods:
;;   - GitHub   : Personal Access Token via Authorization Bearer header
;;   - GitLab   : Personal Access Token via PRIVATE-TOKEN header
;;   - Gongfeng : Private token via PRIVATE-TOKEN header

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'subr-x)

;;;; ─── Customisation ────────────────────────────────────────────────────────

(defgroup code-review-minimal nil
  "Code-review overlays for GitHub/GitLab/Gongfeng Pull/Merge Requests."
  :group 'tools
  :prefix "code-review-minimal-")

(defcustom code-review-minimal-backend nil
  "Backend to use for code review.
If nil, auto-detect from git remote URL.
Valid values: nil (auto), `github', `gitlab', `gongfeng'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "GitHub" github)
                 (const :tag "GitLab" gitlab)
                 (const :tag "Gongfeng (Tencent GitLab)" gongfeng))
  :group 'code-review-minimal)

;; Token configuration (per-backend)
(defcustom code-review-minimal-github-token nil
  "GitHub personal access token.
Create one at: https://github.com/settings/tokens
Required scopes: repo (for private repos), public_repo (for public repos)"
  :type '(choice (const :tag "Not set" nil) (string :tag "Token"))
  :group 'code-review-minimal)

(defcustom code-review-minimal-gitlab-token nil
  "GitLab personal access token.
Create one at: GitLab UI → User Settings → Access Tokens
Required scopes: api (for full access) or read_api + read_repository + write_repository"
  :type '(choice (const :tag "Not set" nil) (string :tag "Token"))
  :group 'code-review-minimal)

(defcustom code-review-minimal-gongfeng-token nil
  "Gongfeng (git.woa.com) private token.
Create one at: Gongfeng UI → User Settings → Access Tokens"
  :type '(choice (const :tag "Not set" nil) (string :tag "Token"))
  :group 'code-review-minimal)

;; API base URLs
(defcustom code-review-minimal-github-base-url "https://api.github.com"
  "Base URL for GitHub API.
For GitHub Enterprise, use: https://your-github-enterprise.com/api/v3"
  :type 'string
  :group 'code-review-minimal)

(defcustom code-review-minimal-gitlab-base-url "https://gitlab.com/api/v4"
  "Base URL for GitLab API.
For self-hosted GitLab, use: https://your-gitlab.com/api/v4"
  :type 'string
  :group 'code-review-minimal)

(defcustom code-review-minimal-gongfeng-base-url "https://git.woa.com/api/v3"
  "Base URL for Gongfeng API."
  :type 'string
  :group 'code-review-minimal)

;; Legacy backward compatibility (for gf-code-review users)
(defcustom code-review-minimal-private-token nil
  "Deprecated. Use `code-review-minimal-gongfeng-token' instead."
  :type '(choice (const :tag "Not set" nil) (string :tag "Token"))
  :group 'code-review-minimal)

(defcustom code-review-minimal-base-url code-review-minimal-gongfeng-base-url
  "Deprecated. Use `code-review-minimal-gongfeng-base-url' instead."
  :type 'string
  :group 'code-review-minimal)

(defcustom code-review-minimal-comment-face 'code-review-minimal-comment-face
  "Face used for comment overlay text."
  :type 'face
  :group 'code-review-minimal)

;;;; ─── Faces ─────────────────────────────────────────────────────────────────

(defface code-review-minimal-comment-face
  '((((background dark))
     :background "#2a2a4a"
     :foreground "#a0c8ff"
     :box (:line-width 1 :color "#5080c0")
     :extend t)
    (t :background "#eef4ff" :foreground "#2040a0" :box (:line-width 1 :color "#8090c0") :extend t))
  "Face for displaying fetched CR comments."
  :group 'code-review-minimal)

(defface code-review-minimal-resolved-face
  '((((background dark)) :foreground "#60c060" :weight bold) (t :foreground "#207020" :weight bold))
  "Face for the resolved status indicator in a comment overlay."
  :group 'code-review-minimal)

(defface code-review-minimal-unresolved-face
  '((((background dark)) :foreground "#60a8ff" :weight bold) (t :foreground "#1040c0" :weight bold))
  "Face for the unresolved/open status indicator in a comment overlay."
  :group 'code-review-minimal)

(defface code-review-minimal-resolved-body-face
  '((((background dark))
     :background "#1a3a1a"
     :foreground "#80c080"
     :box (:line-width 1 :color "#40a040")
     :extend t)
    (t :background "#eeffee" :foreground "#205020" :box (:line-width 1 :color "#60a060") :extend t))
  "Face for the body lines of a resolved CR comment overlay."
  :group 'code-review-minimal)

(defface code-review-minimal-input-face
  '((((background dark))
     :background "#2a3a2a"
     :foreground "#a0ffa0"
     :box (:line-width 1 :color "#50a050")
     :extend t)
    (t :background "#f0fff0" :foreground "#205020" :box (:line-width 1 :color "#70a070") :extend t))
  "Face for the comment-input overlay."
  :group 'code-review-minimal)

(defface code-review-minimal-header-face
  '((((background dark)) :foreground "#80c0ff" :weight bold) (t :foreground "#1040c0" :weight bold))
  "Face for the header line inside a comment overlay."
  :group 'code-review-minimal)

;;;; ─── Backend Detection ─────────────────────────────────────────────────────

(defun code-review-minimal--detect-backend (remote-url)
  "Auto-detect backend from REMOTE-URL.
Returns one of: github, gitlab, gongfeng, or nil."
  (cond
   ;; Gongfeng (Tencent GitLab) - check before generic gitlab
   ((string-match-p "git\.woa\.com" remote-url)
    'gongfeng)
   ;; GitHub
   ((or (string-match-p "github\.com" remote-url)
        (string-match-p "github" remote-url))
    'github)
   ;; Generic GitLab
   ((string-match-p "gitlab" remote-url)
    'gitlab)
   ;; Default to nil if unknown
   (t nil)))

;;;; ─── Per-repo Cache ─────────────────────────────────────────────────────────

(defvar code-review-minimal--iid-cache (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → MR IID (integer).")

(defvar code-review-minimal--backend-cache (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → backend symbol.")

(defun code-review-minimal--git-root ()
  "Return the absolute path to the git root for the current buffer, or nil."
  (when-let ((root (locate-dominating-file (or buffer-file-name default-directory) ".git")))
    (expand-file-name root)))

(defun code-review-minimal--cache-file (filename)
  "Return the path to a per-repo cache file in .git/ directory."
  (when-let ((root (code-review-minimal--git-root)))
    (expand-file-name filename (expand-file-name ".git" root))))

(defun code-review-minimal--load-cached-iid ()
  "Return the persisted MR IID for the current repo, or nil."
  (let ((root (code-review-minimal--git-root)))
    (or (and root (gethash root code-review-minimal--iid-cache))
        (when-let ((file (code-review-minimal--cache-file "code-review-minimal-iid")))
          (when (file-readable-p file)
            (let* ((raw (with-temp-buffer
                          (insert-file-contents file)
                          (string-trim (buffer-string))))
                   (iid (string-to-number raw)))
              (when (and (integerp iid) (> iid 0))
                (when root
                  (puthash root iid code-review-minimal--iid-cache))
                iid)))))))

(defun code-review-minimal--save-iid (iid)
  "Persist IID for the current repo."
  (when-let ((root (code-review-minimal--git-root)))
    (puthash root iid code-review-minimal--iid-cache))
  (when-let ((file (code-review-minimal--cache-file "code-review-minimal-iid")))
    (write-region (number-to-string iid) nil file nil 'silent)))

(defun code-review-minimal--load-cached-backend ()
  "Return the persisted backend for the current repo, or nil."
  (let ((root (code-review-minimal--git-root)))
    (or (and root (gethash root code-review-minimal--backend-cache))
        (when-let ((file (code-review-minimal--cache-file "code-review-minimal-backend")))
          (when (file-readable-p file)
            (let ((backend (with-temp-buffer
                             (insert-file-contents file)
                             (string-trim (buffer-string)))))
              (when (> (length backend) 0)
                (when root
                  (puthash root backend code-review-minimal--backend-cache))
                (intern backend))))))))

(defun code-review-minimal--save-backend (backend)
  "Persist BACKEND for the current repo."
  (when-let ((root (code-review-minimal--git-root)))
    (puthash root backend code-review-minimal--backend-cache))
  (when-let ((file (code-review-minimal--cache-file "code-review-minimal-backend")))
    (write-region (symbol-name backend) nil file nil 'silent)))

;;;; ─── Buffer-local State ────────────────────────────────────────────────────

(defvar-local code-review-minimal--mr-iid nil
  "MR IID (per-project integer id) currently being reviewed.")

(defvar-local code-review-minimal--mr-id nil
  "MR global integer id resolved from `code-review-minimal--mr-iid'.")

(defvar-local code-review-minimal--project-info nil
  "Project info alist with backend-specific keys.
GitHub: ((owner . \"user\") (repo . \"project\"))
GitLab/Gongfeng: ((project-id . \"namespace%2Fproject\"))")

(defvar-local code-review-minimal--current-backend nil
  "The backend symbol currently in use (github, gitlab, gongfeng).")

(defvar-local code-review-minimal--overlays nil
  "List of comment overlays created by `code-review-minimal-mode'.")

(defvar-local code-review-minimal--input-overlay nil
  "The currently active comment-input overlay, if any.")

(defvar-local code-review-minimal--input-prompt-end nil
  "Marker pointing to the end of the prompt in the input buffer.")

;;;; ─── Token Management ──────────────────────────────────────────────────────

(defun code-review-minimal--get-token (backend)
  "Get the authentication token for BACKEND."
  (pcase backend
    ('github code-review-minimal-github-token)
    ('gitlab code-review-minimal-gitlab-token)
    ('gongfeng (or code-review-minimal-gongfeng-token code-review-minimal-private-token))
    (_ nil)))

;;;###autoload
(defun code-review-minimal-set-token (backend token)
  "Interactively set authentication TOKEN for BACKEND.
BACKEND should be one of: github, gitlab, gongfeng."
  (interactive (list (intern (completing-read "Backend: " '("github" "gitlab" "gongfeng")))
                     (read-string "Token: ")))
  (pcase backend
    ('github (setq code-review-minimal-github-token token))
    ('gitlab (setq code-review-minimal-gitlab-token token))
    ('gongfeng (setq code-review-minimal-gongfeng-token token))
    (_ (user-error "Unknown backend: %s" backend)))
  (message "code-review-minimal: token set for %s." backend))

;; Keep old function for backward compatibility
;;;###autoload
(defun code-review-minimal-set-gongfeng-token (token)
  "Set Gongfeng token (backward compatible alias)."
  (interactive (list (read-string "Gongfeng private token: " code-review-minimal-gongfeng-token)))
  (code-review-minimal-set-token 'gongfeng token))

(defun code-review-minimal--assert-token (backend)
  "Signal an error if no token is configured for BACKEND."
  (unless (and (code-review-minimal--get-token backend)
               (not (string-empty-p (code-review-minimal--get-token backend))))
    (user-error "code-review-minimal: Please set your %s token via M-x code-review-minimal-set-token" backend)))

;;;; ─── Remote Parsing ────────────────────────────────────────────────────────

(defun code-review-minimal--git-remote-url ()
  "Return the URL of the `origin' remote."
  (let ((default-directory
         (or (locate-dominating-file (or buffer-file-name default-directory) ".git")
             default-directory)))
    (string-trim (shell-command-to-string "git remote get-url origin 2>/dev/null"))))

;; GitLab/Gongfeng: extract namespace/project from remote URL
(defun code-review-minimal--parse-gitlab-project-path (remote-url)
  "Extract namespace/project from REMOTE-URL (ssh or https)."
  (when remote-url
    (cond
     ;; SSH: git@host:namespace/project.git
     ((string-match "git@[^:]+:\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS: https://host/namespace/project.git
     ((string-match "https?://[^/]+/\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS without .git suffix
     ((string-match "https?://[^/]+/\\(.*[^/]\\)/*$" remote-url)
      (match-string 1 remote-url)))))

;; GitHub: extract owner/repo from remote URL
(defun code-review-minimal--parse-github-repo (remote-url)
  "Parse GitHub REMOTE-URL to get (owner . repo)."
  (when remote-url
    (cond
     ;; SSH: git@github.com:owner/repo.git
     ((string-match "git@github\\.com:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; HTTPS: https://github.com/owner/repo.git
     ((string-match "https?://github\\.com/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; Without .git suffix
     ((string-match "https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)/?$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; GitHub Enterprise SSH
     ((string-match "git@[^:]+:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; GitHub Enterprise HTTPS
     ((string-match "https?://[^/]+/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url))))))

;;;; ─── Backend Selection ─────────────────────────────────────────────────────

(defun code-review-minimal--ensure-backend ()
  "Determine and return the backend to use.
Uses `code-review-minimal-backend' if set, otherwise auto-detects from remote URL.
Caches the result per repository."
  (unless code-review-minimal--current-backend
    (let ((cached (code-review-minimal--load-cached-backend)))
      (if cached
          (setq code-review-minimal--current-backend cached)
        ;; Auto-detect or use configured default
        (let* ((remote (code-review-minimal--git-remote-url))
               (detected (code-review-minimal--detect-backend remote))
               (backend (or code-review-minimal-backend detected)))
          (if backend
              (progn
                (setq code-review-minimal--current-backend backend)
                (code-review-minimal--save-backend backend)
                (message "code-review-minimal: auto-detected %s backend from remote" backend))
            (user-error "code-review-minimal: Cannot detect backend from remote: %s. Please set `code-review-minimal-backend'" remote))))))
  code-review-minimal--current-backend)

;;;; ─── HTTP Helpers ──────────────────────────────────────────────────────────

(defun code-review-minimal--api-url (backend &rest path-segments)
  "Build a full API URL for BACKEND by joining PATH-SEGMENTS."
  (let ((base-url (pcase backend
                    ('github code-review-minimal-github-base-url)
                    ('gitlab code-review-minimal-gitlab-base-url)
                    ('gongfeng code-review-minimal-gongfeng-base-url)
                    (_ code-review-minimal-gitlab-base-url))))
    (concat base-url "/" (mapconcat #'identity path-segments "/"))))

(defun code-review-minimal--make-auth-headers (backend)
  "Create authentication headers for BACKEND."
  (let ((token (code-review-minimal--get-token backend)))
    (pcase backend
      ;; GitHub: Bearer token (GitHub Apps) or token (classic PAT)
      ('github
       `(,(cons "Authorization" (format "Bearer %s" token))
         ("Content-Type" . "application/json; charset=utf-8")
         ("Accept" . "application/vnd.github+json")
         ("X-GitHub-Api-Version" . "2022-11-28")))
      ;; GitLab & Gongfeng: PRIVATE-TOKEN header
      ((or 'gitlab 'gongfeng)
       `(("PRIVATE-TOKEN" . ,token)
         ("Content-Type" . "application/json; charset=utf-8"))))))

(defun code-review-minimal--http-request (backend method url &optional payload callback)
  "Perform async HTTP METHOD request to URL for BACKEND.
PAYLOAD is JSON-encoded as body. CALLBACK receives parsed JSON."
  (code-review-minimal--assert-token backend)
  (let* ((url-request-method method)
         (url-request-extra-headers (code-review-minimal--make-auth-headers backend))
         (url-request-data
          (when payload
            (encode-coding-string (json-encode payload) 'utf-8))))
    (url-retrieve
     url
     (lambda (status)
       (let* ((http-status (code-review-minimal--http-status-code))
              (err (plist-get status :error))
              (resp-body (code-review-minimal--response-body)))
         (cond
          (err
           (message "code-review-minimal: HTTP error %S (URL: %s)" err url))
          ((and http-status (>= http-status 400))
           (message "code-review-minimal: HTTP %d for %s\n  response: %s"
                    http-status url
                    (substring resp-body 0 (min 400 (length resp-body)))))
          (t
           (let ((result (code-review-minimal--parse-response)))
             (when callback
               (funcall callback result)))))))
     nil t)))

(defun code-review-minimal--http-status-code ()
  "Return HTTP status code from url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun code-review-minimal--response-body ()
  "Return response body from url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*$" nil t)
        (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)
      "")))

(defun code-review-minimal--parse-response ()
  "Parse JSON from url-retrieve response buffer."
  (let ((body (code-review-minimal--response-body)))
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string body))
      (error
       (message "code-review-minimal: JSON parse error: %S" err)
       nil))))

;;;; ─── Utility Functions ─────────────────────────────────────────────────────

(defun code-review-minimal--relative-file-path ()
  "Return the path of the current buffer's file relative to git root."
  (when buffer-file-name
    (let* ((root (locate-dominating-file buffer-file-name ".git")))
      (if root
          (file-relative-name buffer-file-name (expand-file-name root))
        (file-name-nondirectory buffer-file-name)))))

(defun code-review-minimal--line-number-at (pos)
  "Return 1-based line number for POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

(defun code-review-minimal--line-end-pos (line)
  "Return buffer position at end of LINE (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-end-position)))

(defun code-review-minimal--clear-overlays ()
  "Remove all comment overlays."
  (mapc #'delete-overlay code-review-minimal--overlays)
  (setq code-review-minimal--overlays nil))

;;;; ─── Backend: GitLab/Gongfeng (Shared API) ─────────────────────────────────

(defun code-review-minimal--gitlab-ensure-project-id ()
  "Set project ID from remote for GitLab/Gongfeng backend."
  (unless (alist-get 'project-id code-review-minimal--project-info)
    (let* ((remote (code-review-minimal--git-remote-url))
           (path (code-review-minimal--parse-gitlab-project-path remote)))
      (if path
          (progn
            (message "code-review-minimal: detected project %s" path)
            (setq code-review-minimal--project-info
                  `((project-id . ,(url-hexify-string path)))))
        (let ((manual (read-string "Project path (e.g. team/project): ")))
          (setq code-review-minimal--project-info
                `((project-id . ,(url-hexify-string manual))))))))
  (alist-get 'project-id code-review-minimal--project-info))

(defun code-review-minimal--gitlab-resolve-mr-id (callback)
  "Resolve MR id and call CALLBACK with it (GitLab/Gongfeng)."
  (if code-review-minimal--mr-id
      (funcall callback code-review-minimal--mr-id)
    (let* ((project-id (code-review-minimal--gitlab-ensure-project-id))
           (url (code-review-minimal--api-url
                 code-review-minimal--current-backend
                 "projects" project-id "merge_request" "iid"
                 (number-to-string code-review-minimal--mr-iid)))
           (buf (current-buffer)))
      (message "code-review-minimal: resolving MR id for IID %d ..." code-review-minimal--mr-iid)
      (code-review-minimal--http-request
       code-review-minimal--current-backend
       "GET" url nil
       (lambda (mr)
         (let ((mr-id (and mr (alist-get 'id mr))))
           (if (not (numberp mr-id))
               (message "code-review-minimal: failed to resolve MR id")
             (with-current-buffer buf
               (setq code-review-minimal--mr-id mr-id))
             (funcall callback mr-id))))))))

;;;; ─── Backend: GitLab/Gongfeng - Fetch Comments ─────────────────────────────

(defun code-review-minimal--gitlab-fetch-comments ()
  "Fetch MR comments and render overlays (GitLab/Gongfeng)."
  (let* ((project-id (code-review-minimal--gitlab-ensure-project-id))
         (mr-iid code-review-minimal--mr-iid)
         (rel-path (code-review-minimal--relative-file-path))
         (buf (current-buffer)))
    (message "code-review-minimal: fetching comments for MR !%d ..." mr-iid)
    (code-review-minimal--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url (concat
                   (code-review-minimal--api-url
                    code-review-minimal--current-backend
                    "projects" project-id "merge_requests"
                    (number-to-string mr-id) "notes")
                   "?per_page=100")))
         (code-review-minimal--http-request
          code-review-minimal--current-backend
          "GET" url nil
          (lambda (notes)
            (with-current-buffer buf
              (code-review-minimal--clear-overlays)
              (if (null notes)
                  (message "code-review-minimal: no comments found")
                (code-review-minimal--gitlab-process-notes notes rel-path))))))))))

(defun code-review-minimal--gitlab-process-notes (notes rel-path)
  "Process GitLab/Gongfeng NOTES and create overlays for REL-PATH."
  (let* ((total (length notes))
         (count 0)
         (by-id (make-hash-table))
         (children (make-hash-table))
         (roots nil))
    ;; Index notes: separate roots from replies
    (dolist (n notes)
      (let ((id (alist-get 'id n))
            (pid (alist-get 'parent_id n)))
        (puthash id n by-id)
        (if pid
            (puthash pid (append (gethash pid children) (list n)) children)
          (push n roots))))
    ;; Reverse for natural order (newest first from API)
    (setq roots (nreverse roots))
    ;; Render threads
    (dolist (root roots)
      (let* ((file-path (alist-get 'file_path root))
             (note-pos (alist-get 'note_position root))
             (latest-pos (and note-pos (alist-get 'latest_position note-pos)))
             (line-num (and latest-pos
                            (or (alist-get 'right_line_num latest-pos)
                                (alist-get 'left_line_num latest-pos))))
             (resolve-state (alist-get 'resolve_state root))
             (resolved (cond ((eql resolve-state 2) t)
                            ((eql resolve-state 1) :json-false)
                            (t nil)))
             (root-id (alist-get 'id root))
             (thread (cons root (gethash root-id children))))
        (when (and (integerp line-num)
                   file-path
                   rel-path
                   (string= file-path rel-path))
          (code-review-minimal--insert-discussion-overlay line-num thread resolved root-id)
          (cl-incf count))))
    (message "code-review-minimal: %d thread(s) in this file, %d total notes." count total)))

;;;; ─── Backend: GitLab/Gongfeng - Post/Update/Resolve ────────────────────────

(defun code-review-minimal--gitlab-post-comment (_beg end body)
  "Post comment on line at END with BODY (GitLab/Gongfeng)."
  (let* ((project-id (code-review-minimal--gitlab-ensure-project-id))
         (mr-iid code-review-minimal--mr-iid)
         (rel-path (code-review-minimal--relative-file-path))
         (end-line (code-review-minimal--line-number-at end))
         (src-buf (current-buffer)))
    (code-review-minimal--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let* ((url (code-review-minimal--api-url
                    code-review-minimal--current-backend
                    "projects" project-id "merge_requests"
                    (number-to-string mr-id) "notes"))
              (payload `((body . ,body)
                         (path . ,rel-path)
                         (line . ,(number-to-string end-line))
                         (line_type . "new"))))
         (code-review-minimal--http-request
          code-review-minimal--current-backend
          "POST" url payload
          (lambda (resp)
            (if (and resp (alist-get 'id resp))
                (progn
                  (message "code-review-minimal: comment posted (id=%s)" (alist-get 'id resp))
                  (with-current-buffer src-buf
                    (code-review-minimal--gitlab-fetch-comments)))
              (message "code-review-minimal: failed to post comment")))))))))

(defun code-review-minimal--gitlab-update-comment (note-id body)
  "Update NOTE-ID with BODY (GitLab/Gongfeng)."
  (let* ((project-id (code-review-minimal--gitlab-ensure-project-id))
         (src-buf (current-buffer)))
    (code-review-minimal--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let* ((url (code-review-minimal--api-url
                    code-review-minimal--current-backend
                    "projects" project-id "merge_requests"
                    (number-to-string mr-id) "notes" (number-to-string note-id)))
              (payload `((body . ,body))))
         (code-review-minimal--http-request
          code-review-minimal--current-backend
          "PUT" url payload
          (lambda (resp)
            (if (and resp (alist-get 'id resp))
                (progn
                  (message "code-review-minimal: note %d updated" note-id)
                  (with-current-buffer src-buf
                    (code-review-minimal--gitlab-fetch-comments)))
              (message "code-review-minimal: failed to update note %d" note-id)))))))))

(defun code-review-minimal--gitlab-resolve-comment (ov)
  "Resolve comment at overlay OV (GitLab/Gongfeng)."
  (let ((note-id (overlay-get ov 'code-review-minimal-note-id))
        (note-body (overlay-get ov 'code-review-minimal-body))
        (project-id (code-review-minimal--gitlab-ensure-project-id))
        (src-buf (current-buffer)))
    (code-review-minimal--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url (code-review-minimal--api-url
                   code-review-minimal--current-backend
                   "projects" project-id "merge_requests"
                   (number-to-string mr-id) "notes" (number-to-string note-id))))
         (code-review-minimal--http-request
          code-review-minimal--current-backend
          "PUT" url
          `((body . ,note-body) (resolve_state . 2))
          (lambda (resp)
            (if (and resp (alist-get 'id resp))
                (progn
                  (message "code-review-minimal: note %d resolved" note-id)
                  (with-current-buffer src-buf
                    (code-review-minimal--gitlab-fetch-comments)))
              (message "code-review-minimal: failed to resolve note %d" note-id)))))))))

;;;; ─── Backend: GitHub ───────────────────────────────────────────────────────

(defun code-review-minimal--github-ensure-project-info ()
  "Set project info from remote for GitHub backend."
  (unless (alist-get 'owner code-review-minimal--project-info)
    (let* ((remote (code-review-minimal--git-remote-url))
           (parsed (code-review-minimal--parse-github-repo remote)))
      (if parsed
          (progn
            (message "code-review-minimal: detected repo %s/%s" (car parsed) (cdr parsed))
            (setq code-review-minimal--project-info
                  `((owner . ,(car parsed))
                    (repo . ,(cdr parsed)))))
        (let ((owner (read-string "GitHub owner/organization: "))
              (repo (read-string "GitHub repository name: ")))
          (setq code-review-minimal--project-info
                `((owner . ,owner)
                  (repo . ,repo))))))))

(defun code-review-minimal--github-fetch-comments ()
  "Fetch PR comments and render overlays (GitHub)."
  (code-review-minimal--github-ensure-project-info)
  (let* ((owner (alist-get 'owner code-review-minimal--project-info))
         (repo (alist-get 'repo code-review-minimal--project-info))
         (pr-number code-review-minimal--mr-iid)
         (rel-path (code-review-minimal--relative-file-path))
         (buf (current-buffer)))
    (message "code-review-minimal: fetching comments for PR #%d ..." pr-number)
    ;; GitHub PR review comments endpoint
    (let ((url (code-review-minimal--api-url
                'github
                "repos" owner repo "pulls" (number-to-string pr-number) "comments")))
      (code-review-minimal--http-request
       'github
       "GET" url nil
       (lambda (comments)
         (with-current-buffer buf
           (code-review-minimal--clear-overlays)
           (code-review-minimal--github-process-comments comments rel-path)))))))

(defun code-review-minimal--github-process-comments (comments rel-path)
  "Process GitHub COMMENTS and create overlays for REL-PATH."
  (let ((count 0))
    (dolist (c comments)
      (let ((path (alist-get 'path c))
            (line (alist-get 'line c))
            (body (alist-get 'body c))
            (id (alist-get 'id c))
            (user (alist-get 'login (alist-get 'user c)))
            (created (alist-get 'created_at c)))
        (when (and path line (string= path rel-path))
          (let* ((note `((author . ((name . ,user)))
                         (body . ,body)
                         (created_at . ,created)))
                 (thread (list note)))
            (code-review-minimal--insert-discussion-overlay line thread nil id)
            (cl-incf count)))))
    (message "code-review-minimal: %d comment(s) in this file" count)))

(defun code-review-minimal--github-post-comment (_beg end body)
  "Post review comment on line at END with BODY (GitHub).
Note: GitHub requires the commit SHA for PR review comments."
  (code-review-minimal--github-ensure-project-info)
  (let* ((owner (alist-get 'owner code-review-minimal--project-info))
         (repo (alist-get 'repo code-review-minimal--project-info))
         (pr-number code-review-minimal--mr-iid)
         (rel-path (code-review-minimal--relative-file-path))
         (line (code-review-minimal--line-number-at end))
         (src-buf (current-buffer)))
    ;; First, get the PR head commit SHA
    (let ((pr-url (code-review-minimal--api-url
                   'github
                   "repos" owner repo "pulls" (number-to-string pr-number))))
      (code-review-minimal--http-request
       'github
       "GET" pr-url nil
       (lambda (pr-data)
         (let ((head-sha (alist-get 'sha (alist-get 'head pr-data))))
           (if (not head-sha)
               (message "code-review-minimal: failed to get PR head commit")
             ;; Post the review comment
             (let ((url (code-review-minimal--api-url
                         'github
                         "repos" owner repo "pulls" (number-to-string pr-number) "comments"))
                   (payload `((body . ,body)
                              (path . ,rel-path)
                              (line . ,line)
                              (side . "RIGHT")
                              (commit_id . ,head-sha))))
               (code-review-minimal--http-request
                'github
                "POST" url payload
                (lambda (resp)
                  (if (and resp (alist-get 'id resp))
                      (progn
                        (message "code-review-minimal: comment posted (id=%s)" (alist-get 'id resp))
                        (with-current-buffer src-buf
                          (code-review-minimal--github-fetch-comments)))
                    (message "code-review-minimal: failed to post comment"))))))))))))

(defun code-review-minimal--github-update-comment (note-id body)
  "Update NOTE-ID with BODY (GitHub).
Note: GitHub uses PATCH to update PR review comments."
  (code-review-minimal--github-ensure-project-info)
  (let* ((owner (alist-get 'owner code-review-minimal--project-info))
         (repo (alist-get 'repo code-review-minimal--project-info))
         (src-buf (current-buffer))
         ;; GitHub review comments use a different endpoint
         (url (code-review-minimal--api-url
               'github
               "repos" owner repo "pulls" "comments" (number-to-string note-id))))
    (code-review-minimal--http-request
     'github
     "PATCH" url `((body . ,body))
     (lambda (resp)
       (if (and resp (alist-get 'id resp))
           (progn
             (message "code-review-minimal: comment %d updated" note-id)
             (with-current-buffer src-buf
               (code-review-minimal--github-fetch-comments)))
         (message "code-review-minimal: failed to update comment %d" note-id))))))

(defun code-review-minimal--github-resolve-comment (_ov)
  "Resolve comment overlay OV (GitHub).
Note: GitHub doesn't have a direct API to 'resolve' review comments.
Resolution is done through the web UI or by marking conversations as resolved."
  (message "code-review-minimal: GitHub review comments are resolved via the web interface"))

;;;; ─── Overlay Rendering ─────────────────────────────────────────────────────

(defun code-review-minimal--render-note (note &optional is-first resolved)
  "Render NOTE alist into propertized string."
  (let* ((author-obj (alist-get 'author note))
         (author (if author-obj
                     (or (alist-get 'name author-obj)
                         (alist-get 'username author-obj)
                         "unknown")
                   "unknown"))
         (created-at (alist-get 'created_at note))
         (body (or (alist-get 'body note) ""))
         (is-resolved (eq resolved t))
         (status-str
          (cond
           ((not is-first) "")
           (is-resolved (propertize " ✓resolved" 'face 'code-review-minimal-resolved-face))
           ((eq resolved :json-false) (propertize " ○open" 'face 'code-review-minimal-unresolved-face))
           (t "")))
         (header (concat
                  (propertize (format "  💬 %s%s"
                                      author
                                      (if created-at
                                          (format "  [%s]" created-at)
                                        ""))
                              'face 'code-review-minimal-header-face)
                  status-str))
         (body-face (if is-resolved
                        'code-review-minimal-resolved-body-face
                      'code-review-minimal-comment-face))
         (body-lines (mapconcat (lambda (l) (concat "  │ " l))
                                (split-string body "\n") "\n")))
    (concat header "\n" (propertize body-lines 'face body-face) "\n")))

(defun code-review-minimal--insert-discussion-overlay (line notes resolved first-note-id)
  "Insert overlay after LINE with NOTES thread."
  (let* ((pos (code-review-minimal--line-end-pos line))
         (ov (make-overlay pos pos nil t nil))
         (first-body (alist-get 'body (car notes)))
         (separator (propertize "  ├────────────────\n" 'face 'code-review-minimal-header-face))
         (text (propertize
                (concat "\n"
                        (mapconcat
                         (lambda (note-and-idx)
                           (code-review-minimal--render-note (car note-and-idx)
                                                     (= (cdr note-and-idx) 0)
                                                     resolved))
                         (cl-loop for n in notes for i from 0 collect (cons n i))
                         separator))
                'cursor 0)))
    (overlay-put ov 'after-string text)
    (overlay-put ov 'code-review-minimal t)
    (overlay-put ov 'code-review-minimal-note-id first-note-id)
    (overlay-put ov 'code-review-minimal-body first-body)
    (overlay-put ov 'code-review-minimal-resolved resolved)
    (push ov code-review-minimal--overlays)))

;;;; ─── Input Overlay ─────────────────────────────────────────────────────────

(defvar code-review-minimal--input-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'code-review-minimal--submit-comment)
    (define-key m (kbd "C-c C-k") #'code-review-minimal--cancel-comment)
    m)
  "Keymap for comment input.")

(defun code-review-minimal--open-input-overlay (beg end &optional edit-note-id initial-body)
  "Open inline input overlay below region BEG..END.
If EDIT-NOTE-ID is non-nil, edit existing note with INITIAL-BODY."
  (when code-review-minimal--input-overlay
    (code-review-minimal--close-input-overlay))
  (let* ((end-pos (save-excursion
                    (goto-char end)
                    (line-end-position)))
         (ov (make-overlay end-pos end-pos nil t nil))
         (ibuf (generate-new-buffer "*code-review-minimal-input*"))
         (editing edit-note-id)
         (prompt (propertize
                  (concat (if editing "\n  ┌─ Edit CR comment " "\n  ┌─ New CR comment ")
                          (propertize "(C-c C-c submit, C-c C-k cancel)"
                                      'face '(:weight normal :slant italic))
                          "\n  │ ")
                  'face 'code-review-minimal-input-face
                  'read-only t
                  'rear-nonsticky t)))
    (overlay-put ov 'code-review-minimal-input t)
    (overlay-put ov 'code-review-minimal-region-beg beg)
    (overlay-put ov 'code-review-minimal-region-end end)
    (overlay-put ov 'code-review-minimal-input-buffer ibuf)
    (when editing
      (overlay-put ov 'code-review-minimal-edit-note-id edit-note-id))
    (setq code-review-minimal--input-overlay ov)
    (with-current-buffer ibuf
      (code-review-minimal-input-mode)
      (insert prompt)
      (setq-local code-review-minimal--input-overlay ov)
      (setq-local code-review-minimal--input-prompt-end (point-marker))
      (when (and editing initial-body)
        (insert initial-body)))
    (let ((win (display-buffer ibuf '(display-buffer-below-selected (window-height . 6)))))
      (when win
        (select-window win)))
    (message "Type your comment, then C-c C-c to submit or C-c C-k to cancel.")))

(define-derived-mode code-review-minimal-input-mode text-mode "CR-Input"
  "Transient mode for entering a code review comment."
  (set-buffer-file-coding-system 'utf-8)
  (use-local-map code-review-minimal--input-map)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state)))

(defun code-review-minimal--get-input-text ()
  "Extract user text from input buffer."
  (when code-review-minimal--input-overlay
    (let ((ibuf (overlay-get code-review-minimal--input-overlay 'code-review-minimal-input-buffer)))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (string-trim
           (buffer-substring-no-properties code-review-minimal--input-prompt-end (point-max))))))))

(defun code-review-minimal--close-input-overlay ()
  "Close input overlay and clean up."
  (when code-review-minimal--input-overlay
    (let* ((ov code-review-minimal--input-overlay)
           (ibuf (overlay-get ov 'code-review-minimal-input-buffer))
           (src-buf (overlay-buffer ov)))
      (delete-overlay ov)
      (setq code-review-minimal--input-overlay nil)
      (when (and src-buf (buffer-live-p src-buf))
        (with-current-buffer src-buf
          (setq code-review-minimal--input-overlay nil)))
      (when (buffer-live-p ibuf)
        (let ((win (get-buffer-window ibuf)))
          (when win
            (delete-window win)))
        (kill-buffer ibuf)))))

(defun code-review-minimal--cancel-comment ()
  "Cancel comment input."
  (interactive)
  (code-review-minimal--close-input-overlay)
  (message "code-review-minimal: comment cancelled."))

(defun code-review-minimal--submit-comment ()
  "Submit comment to API."
  (interactive)
  (let ((body (code-review-minimal--get-input-text)))
    (if (or (null body) (string-empty-p body))
        (message "code-review-minimal: empty comment, not submitting.")
      (let* ((ov code-review-minimal--input-overlay)
             (src-buf (overlay-buffer ov))
             (beg (overlay-get ov 'code-review-minimal-region-beg))
             (end (overlay-get ov 'code-review-minimal-region-end))
             (edit-note-id (overlay-get ov 'code-review-minimal-edit-note-id)))
        (with-current-buffer src-buf
          (if edit-note-id
              (code-review-minimal--update-comment edit-note-id body)
            (code-review-minimal--post-comment beg end body))
          (deactivate-mark)))))
  (code-review-minimal--close-input-overlay))

;;;; ─── Backend Dispatch Functions ────────────────────────────────────────────

(defun code-review-minimal--fetch-comments ()
  "Fetch comments using current backend."
  (pcase code-review-minimal--current-backend
    ('github (code-review-minimal--github-fetch-comments))
    ((or 'gitlab 'gongfeng) (code-review-minimal--gitlab-fetch-comments))
    (_ (error "Unknown backend: %s" code-review-minimal--current-backend))))

(defun code-review-minimal--post-comment (beg end body)
  "Post comment using current backend."
  (pcase code-review-minimal--current-backend
    ('github (code-review-minimal--github-post-comment beg end body))
    ((or 'gitlab 'gongfeng) (code-review-minimal--gitlab-post-comment beg end body))
    (_ (error "Unknown backend: %s" code-review-minimal--current-backend))))

(defun code-review-minimal--update-comment (note-id body)
  "Update comment using current backend."
  (pcase code-review-minimal--current-backend
    ('github (code-review-minimal--github-update-comment note-id body))
    ((or 'gitlab 'gongfeng) (code-review-minimal--gitlab-update-comment note-id body))
    (_ (error "Unknown backend: %s" code-review-minimal--current-backend))))

(defun code-review-minimal--resolve-comment (ov)
  "Resolve comment using current backend."
  (pcase code-review-minimal--current-backend
    ('github (code-review-minimal--github-resolve-comment ov))
    ((or 'gitlab 'gongfeng) (code-review-minimal--gitlab-resolve-comment ov))
    (_ (error "Unknown backend: %s" code-review-minimal--current-backend))))

;;;; ─── Public Commands ───────────────────────────────────────────────────────

(defun code-review-minimal--overlay-at-point ()
  "Return comment overlay at point."
  (let ((found nil))
    (dolist (ov (overlays-in (line-beginning-position) (1+ (line-end-position))))
      (when (and (overlay-get ov 'code-review-minimal)
                 (overlay-get ov 'code-review-minimal-note-id))
        (setq found ov)))
    found))

;;;###autoload
(defun code-review-minimal-set-mr-iid (iid)
  "Set the MR/PR IID for the current repository."
  (interactive (list (read-number "New MR/PR IID: " (or code-review-minimal--mr-iid 0))))
  (setq code-review-minimal--mr-iid iid
        code-review-minimal--mr-id nil)
  (code-review-minimal--save-iid iid)
  (message "code-review-minimal: MR IID set to !%d (persisted)." iid))

;;;###autoload
(defun code-review-minimal-set-backend-for-repo (backend)
  "Set and persist the backend for the current repository.
Use this to override auto-detection."
  (interactive (list (intern (completing-read "Backend: " '("github" "gitlab" "gongfeng")))))
  (setq code-review-minimal--current-backend backend)
  (code-review-minimal--save-backend backend)
  (message "code-review-minimal: backend set to %s (persisted)." backend))

;;;###autoload
(defun code-review-minimal-add-comment (beg end)
  "Add a code review comment for selected region BEG..END."
  (interactive "r")
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (unless code-review-minimal--mr-iid
    (user-error "code-review-minimal: no MR IID set"))
  (code-review-minimal--assert-token code-review-minimal--current-backend)
  (code-review-minimal--open-input-overlay beg end))

;;;###autoload
(defun code-review-minimal-edit-comment ()
  "Edit the code review comment at point."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error "code-review-minimal: no comment overlay on this line"))
    (let ((note-id (overlay-get ov 'code-review-minimal-note-id))
          (note-body (overlay-get ov 'code-review-minimal-body))
          (line (line-beginning-position)))
      (code-review-minimal--open-input-overlay line line note-id note-body))))

;;;###autoload
(defun code-review-minimal-refresh ()
  "Re-fetch and redisplay all comments."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (unless code-review-minimal--mr-iid
    (user-error "code-review-minimal: no MR IID set"))
  (code-review-minimal--assert-token code-review-minimal--current-backend)
  (code-review-minimal--fetch-comments))

;;;###autoload
(defun code-review-minimal-resolve-comment ()
  "Mark the comment at point as resolved."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error "code-review-minimal: no comment overlay on this line"))
    (let ((already (overlay-get ov 'code-review-minimal-resolved)))
      (when (eq already t)
        (user-error "code-review-minimal: comment is already resolved"))
      (code-review-minimal--assert-token code-review-minimal--current-backend)
      (code-review-minimal--resolve-comment ov))))

;;;; ─── Minor Mode ────────────────────────────────────────────────────────────

(defvar code-review-minimal-mode-map
  (make-sparse-keymap)
  "Keymap for `code-review-minimal-mode'.")

;;;###autoload
(define-minor-mode code-review-minimal-mode
  "Minor mode for reviewing code inline using overlays.

When enabled, you will be asked for the MR/PR IID; comments targeting
the current file are fetched and shown inline.

The backend is auto-detected from the git remote URL:
  - github.com → GitHub
  - git.woa.com → Gongfeng
  - Other gitlab hosts → GitLab

Or set `code-review-minimal-backend' to override auto-detection.

Commands:
  `code-review-minimal-add-comment'     - add comment for selected region
  `code-review-minimal-edit-comment'    - edit comment at point
  `code-review-minimal-refresh'         - re-fetch comments
  `code-review-minimal-resolve-comment' - resolve comment at point
  `code-review-minimal-set-backend-for-repo' - change backend for this repo"
  :lighter " CR"
  :keymap code-review-minimal-mode-map
  (if code-review-minimal-mode
      (progn
        ;; Determine backend
        (code-review-minimal--ensure-backend)
        (message "code-review-minimal: using %s backend" code-review-minimal--current-backend)
        ;; Get token for backend
        (code-review-minimal--assert-token code-review-minimal--current-backend)
        ;; Get MR IID
        (unless code-review-minimal--mr-iid
          (let ((cached (code-review-minimal--load-cached-iid)))
            (if cached
                (progn
                  (setq code-review-minimal--mr-iid cached)
                  (message "code-review-minimal: using cached MR IID !%d" cached))
              (let ((iid (read-number "MR/PR IID to review (integer in URL): ")))
                (setq code-review-minimal--mr-iid iid)
                (code-review-minimal--save-iid iid)))))
        ;; Fetch comments
        (code-review-minimal--fetch-comments))
    ;; Disable
    (code-review-minimal--clear-overlays)
    (when code-review-minimal--input-overlay
      (code-review-minimal--cancel-comment))
    (setq code-review-minimal--mr-iid nil
          code-review-minimal--mr-id nil
          code-review-minimal--project-info nil
          code-review-minimal--current-backend nil)))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'code-review-minimal)

;;; code-review-minimal.el ends here
