;;; -*- lexical-binding: t -*-

(require 'url)
(require 'subr-x)

(message "Default directory: %s" default-directory)
(message "Command-line default-directory: %s" command-line-default-directory)

(defun xml-get (sxml elem n)
  (if (consp sxml)
      (named-let iter ((head (cdr sxml))
                       (i 1))
        (if (consp head)
            (if (and (consp (car head))
                     (eq (caar head) elem))
                (if (= i n)
                    (car head)
                  (iter (cdr head) (1+ i)))
              (iter (cdr head) i))
          nil))
    nil))

(defun kh/commit-from-xml (xml)
  (caddr (cl-reduce
          (lambda (alist pair)
            (xml-get alist (car pair) (cdr pair)))
          '((body . 1) (div . 1) (div . 1) (table . 1) (tr . 3) (td . 1) (a . 1))
          :initial-value xml)))

(defun kh/emacs-get-commit ()
  (let ((xml (with-current-buffer
                 (url-retrieve-synchronously "https://git.savannah.gnu.org/cgit/emacs.git/commit/")
               (goto-char url-http-end-of-headers)
               (libxml-parse-xml-region (point) (point-max)))))
    (kh/commit-from-xml xml)))

(defun kh/emacs-master-hash (url)
  (let* ((file (make-temp-file "emacs-"))
         (_ (url-copy-file url file t))
         (hash (with-temp-buffer
                 (call-process "guix" nil t nil "hash" file)
                 (buffer-substring (point-min) (1- (point-max))))))
    (delete-file file)
    hash))

(message "Obtaining current time...")
(setq emacs-time (format-time-string "%s"))

(message "Getting newest emacs commit...")
(setq emacs-latest-commit (kh/emacs-get-commit))

(message "Calculating hash of emacs source...")
(setq emacs-latest-hash
      (kh/emacs-master-hash
       (concat "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-"
               emacs-latest-commit ".tar.gz")))

(let ((buff (get-buffer-create "temp"))
      (file "trisk/packages/emacs-master.scm"))
  (with-current-buffer buff
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (when (re-search-forward "(define emacs-git-commit \"\\(.+\\)\"" nil t)
      (message "Changing commit to: %s" emacs-latest-commit)
      (replace-match emacs-latest-commit nil nil nil 1))
    (when (re-search-forward "(define emacs-git-hash \"\\(.+\\)\"" nil t)
      (message "Changing hash to: %s" emacs-latest-hash)
      (replace-match emacs-latest-hash nil nil nil 1))
    (when (re-search-forward "(define emacs-git-time \"\\(.+\\)\"" nil t)
      (message "Changing time to: %s" emacs-time)
      (replace-match emacs-time nil nil nil 1))
    (write-region nil nil file)))

(message "Done!")
