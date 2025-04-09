;;; labs.el --- Iximius Labs controls
;;; Commentary: Manages Iximius Labs playgrounds and authoring materials.
;;; Iximius Labs integration for Emacs
;;;
;; Copyright (C) 2025 Maksym Prokopov
;;
;; Author: Maksym Prokopov <mprokopov@gmail.com>
;; Maintainer: Maksym Prokopov <mprokopov@gmail.com>
;; Created: March 14, 2025
;; Modified: March 14, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mprokopov/iximiuz-labs.el
;; Package-Requires: ((emacs "24.3"))

;;; Code:

(defgroup labs nil
  "Iximiuz Labs nifties."
  :group 'tools
  :group 'convenience
  :version "24.1")

(defvar labs-playground-id ""
  "Current playground id.")

(defconst labs-playground-types
  '("tutorial" "challenge" "course" "skill-path")
  "List of available playground types.")

(defcustom labs-playground-type
  "challenge" "Current playground-type."
  ;; :type '(symbol)
  :group 'labs
  :type '(choice
                (const :tag "Tutorial" "tutorial")
                (const :tag "Challenge" "challenge")
                (const :tag "Course" "course")
                (const :tag "Skill-Path" "skill-path")))

(defun labs-kill-ring-material-name ()
  "Put material name into kill ring."
  (interactive)
  (let ((material-name (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory (buffer-file-name))))))
    (kill-new material-name)))

(defun labs-kill-ring-material-name-and-replace ()
  "Put material name into kill ring and replace - with _."
  (interactive)
  (let ((material-name (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory (buffer-file-name))))))
    (setq material-name (replace-regexp-in-string "-" "_" material-name))
    (kill-new material-name)))

(defun labs-terminate-playground ()
  "Terminate current playground."
  (interactive)
  (if (string= labs-playground-id "")
      (message "No playground ID found")
    (let ((buffer (make-comint "Labs Playgrounds" "labctl" nil
                               "playground" "stop" labs-playground-id)))
      (with-current-buffer buffer
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (string= event "finished\n")
                 (message "Playground terminated"))))))))))

(defun labs-get-playground-id ()
  "Extract playground ID from the second line of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)  ; Move to the second line
    (if (looking-at "\\([0-9a-f]\\{24\\}\\)")  ; Match 24-character hex ID
        (match-string 1)
      (message "No playground ID found"))))

(defun labs-fetch-playgrounds ()
  "Fetch playgrogunds from Iximius Labs."
  (interactive)
  (and (get-buffer "*Labs Playgrounds*")
       (kill-buffer "*Labs Playgrounds*"))
  (let ((buf (make-comint "Labs Playgrounds" "labctl" nil "playground" "list")))
    (with-current-buffer buf
      (let ((proc (get-buffer-process buf)))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (setq labs-playground-id (labs-get-playground-id))
                 (message (format "Playgrounds fetched: %s" labs-playground-id))))))))))
  (switch-to-buffer "*Labs Playgrounds*"))

(defun labs-ssh-iterm ()
  "SSH to Iximius Labs playground.
Make sure labctl is installed and configured."
  (interactive)
  (let* ((playground labs-playground-id)
         (command (format "labctl ssh %s" playground))
         (script (format
                  "tell application \"iTerm\"
create tab with default profile window 0 command \"%s\"
activate
end tell" command)))
    (do-applescript script)))


(defun labs-extract-kind-value ()
  "Find \=kind: <value>\= in current buffer and return <value>."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "kind:\\s-*\\(.*\\)" nil t)
        (let ((value (match-string 1)))
          (message "Found kind: %s" value)
          value)
      (message "No 'kind:' entry found")
      nil)))

(defun labs-push-current-material ()
  "Push current material to Iximius Labs."
  (interactive)
  (let ((type (labs-extract-kind-value)) ; or challenge
        (parent-dir
         (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))))
         (message (format "Pushing %s" parent-dir))
         (let ((default-directory (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
           (message (format "Pushing %s" default-directory))
           (make-comint "Labs" "labctl" nil "content" "push" type parent-dir "-f"))
         (pop-to-buffer "*Labs*")))

(defun labs-browse-current-material ()
  "Open current material in browser."
  (interactive)
  (let* ((type (labs-extract-kind-value)) ; or challenge
         (parent-dir
         (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))))
    (browse-url(format "https://labs.iximiuz.com/%s/%s" (concat type "s") parent-dir))))

(defun labs-create-material (material)
  "Create new material MATERIAL."
  (interactive "sMaterial: ")
  (message "Creating %s of type %s" material labs-playground-type)
  (let ((buffer (make-comint "Labs" "labctl" nil "content" "create" labs-playground-type material "--no-sample")))
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (message "Material created")))))))
    (pop-to-buffer "*Labs*")))

(defun labs-remove-current-material ()
  "Remove material MATERIAL."
  (interactive)
  (let* ((type (labs-extract-kind-value))
         (buffer (make-comint "Labs" "labctl" nil "content" "remove" type labs-playground-id)))
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (message "Material removed")))))))
    (pop-to-buffer "*Labs*")))

(defun labs-list-materials (TYPE)
  "List materials of type TYPE."
  (interactive "sType: ")
  (let ((buffer (make-comint "Labs" "labctl" nil "content" "list" "--kind" TYPE)))
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (message "Materials listed")))))))
    (pop-to-buffer "*Labs*")))

;;;###autoload
(define-minor-mode labs-mode
  "Minor mode for Iximiuse Labs."
  :lighter "Labs"
  :require 'labs
  :group 'labs
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c p") 'labs-push-current-material)
            (define-key map (kbd "C-c c b") 'labs-browse-current-material)
            (define-key map (kbd "C-c c t") 'labs-terminate-playground)
            map))

(provide 'labs)
;;; labs.el ends here
