;;; phye-init-treesit-fold.el --- treesit-fold packages and custom ranges  -*- lexical-binding: t; -*-

;;; Commentary:
;; treesit-fold / treesit-fold-indicators setup plus custom fold-range
;; functions that keep the header/closing keywords of a block visible.

;;; Code:

(use-package
 treesit-fold
 :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold" :rev newest)
 :defer t
 :custom (treesit-fold-line-count-show t))

(use-package
 treesit-fold-indicators
 :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold" :rev newest))

(defun phye/treesit-fold--child-of-types (node types)
  "Return the first direct child of NODE whose type is in TYPES."
  (seq-find (lambda (c) (member (treesit-node-type c) types))
            (treesit-node-children node)))

(defun phye/treesit-fold--range-between (node offset open-types close-types)
  "Build a fold range between two keyword children of NODE.
OPEN-TYPES is a list of child type strings marking the start keyword;
the fold begins at the end of the first match.  CLOSE-TYPES is a list
of child type strings marking the end keyword; the fold ends at the
start of the first match, or at the end of NODE when CLOSE-TYPES is
nil.  OFFSET is applied to the resulting range via
`treesit-fold--cons-add'.  Returns nil when the required children are
missing."
  (let ((open (phye/treesit-fold--child-of-types node open-types))
        (close (and close-types
                    (phye/treesit-fold--child-of-types node close-types))))
    (when (and open (or (null close-types) close))
      (let ((end-pos (if close
                         (treesit-node-start close)
                       (treesit-node-end node))))
        ;; If END-POS is preceded by (optional) trailing whitespace and
        ;; a newline, back up to the newline so the following line
        ;; (e.g. elif/else/fi/esac, or the next sibling clause) stays
        ;; on its own line even when folded.
        (save-excursion
          (goto-char end-pos)
          (skip-chars-backward " \t")
          (when (and (> (point) (point-min))
                     (eq (char-before) ?\n))
            (setq end-pos (1- (point)))))
        (treesit-fold--cons-add
         (cons (treesit-node-end open) end-pos)
         offset)))))

(defun phye/treesit-fold-bash-if-body (node offset)
  "Fold only the \"then\" body of a bash if_statement NODE.
Keeps the if/elif/else/fi lines visible.  OFFSET is forwarded to
`phye/treesit-fold--range-between'."
  (phye/treesit-fold--range-between
   node offset '("then") '("fi" "else_clause" "elif_clause")))

(defun phye/treesit-fold-bash-elif-body (node offset)
  "Fold only the \"then\" body of a bash elif_clause NODE.
An elif_clause has no `fi'/`else'/`elif' child (those are siblings
under the parent if_statement), so the fold extends to the end of the
elif_clause itself, which tree-sitter places right after the body's
last statement -- keeping the following keyword line visible.  OFFSET
is forwarded to `phye/treesit-fold--range-between'."
  (phye/treesit-fold--range-between node offset '("then") nil))

(defun phye/treesit-fold-bash-else-body (node offset)
  "Fold only the body of an else_clause NODE, keeping the `else' line visible.
OFFSET is forwarded to `phye/treesit-fold--range-between'."
  (phye/treesit-fold--range-between node offset '("else") nil))

(defun phye/treesit-fold-bash-case-body (node offset)
  "Fold only the body of a bash case_statement NODE.
Keeps the `case ... in' and `esac' lines visible.  OFFSET is forwarded
to `phye/treesit-fold--range-between'."
  (phye/treesit-fold--range-between node offset '("in") '("esac")))

(with-eval-after-load 'treesit-fold
  (push '(block_sequence_item . treesit-fold-range-seq)
        (alist-get 'yaml-mode treesit-fold-range-alist))
  (push '(try_statement . treesit-fold-range-seq)
        (alist-get 'python-mode treesit-fold-range-alist))
  (push '(if_statement . treesit-fold-range-seq)
        (alist-get 'python-mode treesit-fold-range-alist))
  (push `(if_statement . ,#'phye/treesit-fold-bash-if-body)
        (alist-get 'bash-ts-mode treesit-fold-range-alist))
  (push `(case_statement . ,#'phye/treesit-fold-bash-case-body)
        (alist-get 'bash-ts-mode treesit-fold-range-alist))
  (push `(elif_clause . ,#'phye/treesit-fold-bash-elif-body)
        (alist-get 'bash-ts-mode treesit-fold-range-alist))
  (push `(else_clause . ,#'phye/treesit-fold-bash-else-body)
        (alist-get 'bash-ts-mode treesit-fold-range-alist)))

(provide 'phye-init-treesit-fold)
;;; phye-init-treesit-fold.el ends here
