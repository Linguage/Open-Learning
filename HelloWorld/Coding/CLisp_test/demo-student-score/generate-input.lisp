;; generate-input.lisp

;; 生成输入文件
(defun generate-input (filename)
  (with-open-file (stream filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    ;; 写入表头
    (format stream "Name,StudentID,Chinese,Math,English,Physics,Chemistry~%")
    ;; 写入学生信息和成绩
    (loop for i from 1 to 10
          do (format stream "Student-~D,202200~D,~D,~D,~D,~D,~D~%"
                      i i (random 100) (random 100) (random 100) (random 100) (random 100)))))

;; 从输入文件中读取学生信息和成绩
(defun read-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

;; 解析输入文件中的一行数据
;; 解析输入文件中的一行数据
(defun parse-line (line)
  (let ((fields (split-sequence #\, line)))
    (list (first fields)
          (second fields)
          (map 'list #'parse-integer (rest (rest fields))))))

;; 拆分行为字段
(defun split-sequence (separator sequence)
  (loop for start = 0 then (1+ end)
        as end = (or (position separator sequence :start start)
                     (length sequence))
        collect (subseq sequence start end)
        while (< end (length sequence))))

