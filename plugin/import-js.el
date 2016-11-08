;;; import-js.el --- Import Javascript dependencies -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Henric Trotzig and Kevin Kehl
;;
;; Author: Kevin Kehl <kevin.kehl@gmail.com>
;; URL: http://github.com/trotzig/import-js/
;; Package-Requires: ((emacs "24"))
;; Version: 0.1
;; Keywords: javascript

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the MIT license, see:
;; http://github.com/trotzig/import-js/blob/master/LICENSE

;;; Commentary:

;; Quick start:
;; run-import-js
;;
;; Bind the following commands:
;; import-js-import
;; import-js-goto
;;
;; For a detailed introduction see:
;; http://github.com/trotzig/import-js/blob/master/README.md

;;; Code:

(require 'json)

(defvar import-js-buffer nil "Current import-js process buffer")
(defvar import-js-project-root "." "Root of your project")
(defvar import-buffer nil "The current buffer under operation")

(defun import-js-send-input (&rest opts)
  (let ((path buffer-file-name)
        (temp-buffer (generate-new-buffer "import-js")))
    (cd (shell-quote-argument import-js-project-root))
    (apply 'call-process `("importjs"
                           ,path
                           ,temp-buffer
                           nil
                           ,@opts
                           ,path))
    (revert-buffer t t t)
    (let ((out (with-current-buffer temp-buffer (buffer-string))))
      (kill-buffer temp-buffer)
      out)))

(defun import-js-word-at-point ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:_")
    (let ((beg (point)) module)
      (skip-chars-forward "A-Za-z0-9:_")
      (setq module (buffer-substring beg (point)))
      module)))

;;;###autoload
(defun import-js-import ()
  (interactive)
  (save-some-buffers)
  (import-js-send-input "word" "--overwrite" (import-js-word-at-point)))

;;;###autoload
(defun import-js-fix ()
  (interactive)
  (save-some-buffers)
  (import-js-send-input "fix" "--overwrite"))

;;;###autoload
(defun import-js-goto ()
  (interactive)
  (let ((goto-list (json-read-from-string
                    (import-js-send-input "goto" (import-js-word-at-point)))))
    (find-file (import-js-locate-goto-file (cdr (assoc 'goto goto-list))))))

(defun import-js-locate-goto-file (goto-value)
  (let ((goto-file goto-value))
    (if (file-name-absolute-p goto-value)
        (let ((goto-located nil)
              (js-suffixes '(".js" ".jsx")))
          (if (file-directory-p goto-value)
              (setf goto-located (locate-file "index" `(,goto-value) js-suffixes))
            (setf goto-located (locate-file goto-value '("/") js-suffixes)))

          (if goto-located (setf goto-file goto-located))))

    goto-file))

(provide 'import-js)
;;; import-js.el ends here
