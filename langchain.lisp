(defpackage :lem-codecomp/client
  (:use :cl)
  (:export :code-complete))

(in-package :lem-codecomp/client)

(defun langchain-invoke (input &key (path "/code_complete")
                                    (endpoint "http://localhost:8000"))
  (let ((body (yason:with-output-to-string* ()
                  (yason:encode (alexandria:alist-hash-table
                               `(("input" . ,input)))))))
    (lem:message (format nil "json: ~a" body))
      (dex:post (format nil "~A~A/invoke" endpoint path) 
                :content body
                :headers '(("content-type" . "application/json"))
                :force-string t
                :read-timeout 600)))

(defun code-complete (repo-path lang partial-code)
  (let* ((input (alexandria:alist-hash-table 
                `(("repo_path" . ,repo-path)
                  ("language" . ,lang)
                  ("partial_code" . ,partial-code)))) 
         (res (langchain-invoke input))
         (res-json (yason:parse res)))
    (gethash "output" res-json)))
    