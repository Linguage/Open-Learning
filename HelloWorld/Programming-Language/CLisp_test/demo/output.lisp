;; output.lisp

;; 输出到 CSV 文件
(defun output-to-csv (filename result)
  (with-open-file (stream filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (format stream "Result~%")
    (format stream "~A~%" result)))
