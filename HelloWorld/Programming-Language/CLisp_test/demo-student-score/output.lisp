;; output.lisp

;; 按总分排序学生
(defun sort-students-by-total-score (students)
  (sort students #'> :key #'(lambda (student) (cdr (assoc :total student)))))

;; 输出到 CSV 文件
(defun output-to-csv (filename students)
  (with-open-file (stream filename
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    ;; 写入表头
    (format stream "Name,Total,Average~%")
    ;; 写入每个学生的信息
    (loop for student in students
          do (format stream "~A,~A,~A~%"
                      (car student)
                      (cdr (assoc :total student))
                      (cdr (assoc :average student))))))

