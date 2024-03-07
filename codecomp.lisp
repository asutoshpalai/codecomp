(defpackage :lem-codecomp
  (:use :cl :lem)
  (:documentation "Package for Lem code completion functionality."))

(in-package :lem-codecomp)

(defvar *overlay-start* nil)
(defvar *overlay-end* nil)

(define-key *global-keymap* "C-x C-i" 'llm-code-suggestion)
(define-key *global-keymap* "C-x C-o" 'hello)

(defun get-previous-code (end)
  "returns 20 lines starting from the current point"
  (with-point ((start end :temporary))
    (line-offset start (max -20 (- 1 (line-number-at-point start))))
    (line-start start)
    (points-to-string start end)))

(defun get-buffer-language (buffer)
  (let ((mode-name (string (lem/language-mode:buffer-language-mode buffer))))
    (message (format nil "~a" mode-name))
    (cond
      ((string= mode-name "LISP-MODE") "lisp")
      ((string= mode-name "PYTHON-MODE") "python")
      ((string= mode-name "GO-MODE") "go")
      (t (error (format nil "unknown language mode ~a"  mode-name))))))

(defun accept-suggestion ()
  ; (message "accepting suggestions")
  (when *overlay-start*
    (let ((comp-text (points-to-string *overlay-start* *overlay-end*)))
      (with-point ((start *overlay-start*))
        (remove-overlay-text)
        (insert-string start comp-text)))))

(defun key-press-hook (key)
  (message (format nil "~a" key))
  (cond
    ((match-key key :sym "Tab") (accept-suggestion))
    ((match-key key :sym "NopKey") nil)
    (t (remove-overlay-text))))

(defun show-overlay-text (point text)
  ; (message (format nil "point after insertion: ~a" point))
  (let* ((end-point (copy-point point :left-inserting))
         (start-point (copy-point point :right-inserting)))
    (insert-string point text :sticky-attribute 'lem:syntax-comment-attribute)
    (character-offset end-point (length text))
    (move-point point start-point)
    ;(setf *overlay* (make-overlay point end-point 'lem:syntax-comment-attribute))
    (setf *overlay-start* start-point)
    (setf *overlay-end* end-point)
    (add-hook *input-hook* 'key-press-hook)))

(defun trim-till-string (completion-text partial-code)
  ;; TODO: improve the logic so that. E.g. if the last line appears in multiple
  ;; places, exapand the search region.
  (let* ((last-line (string-left-trim '(#\Tab #\Space)
                                      (car (split-sequence:split-sequence #\Newline 
                                                                     partial-code
                                                                     :from-end t 
                                                                     :count 1))))
         (last-line-pos (search last-line completion-text)))
    (when last-line-pos
      ; (show-message (format nil "last line: ~a" last-line))
      (subseq completion-text (+ (length last-line) last-line-pos)))))
  
(defun remove-overlay-text ()
  (when *overlay-start*
    ; (message "removing overlays between ~a ~a" *overlay-start* *overlay-end*)
    (remove-hook *input-hook* 'key-press-hook)
    (delete-between-points *overlay-start* *overlay-end*)
    (delete-point *overlay-start*)
    (delete-point *overlay-end*)
    (setf *overlay-start* nil)
    (setf *overlay-end* nil)))      

(define-command llm-code-suggestion (point) ((lem:current-point))
  (remove-overlay-text)
  (let* ((partial-code (get-previous-code point))
        (repo-path (namestring 
                    (lem-core/commands/project:find-root (buffer-directory))))
        (lang (get-buffer-language (point-buffer point)))
        (completion-text (lem-codecomp/client:code-complete repo-path 
                                                         lang
                                                         partial-code)))
    ; (message (format nil "compleiton done ~a" completion-text))
    (show-overlay-text point (or (trim-till-string completion-text partial-code) ""))))