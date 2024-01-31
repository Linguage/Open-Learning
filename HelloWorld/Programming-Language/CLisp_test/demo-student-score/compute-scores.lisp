;; compute-scores.lisp

;; 计算每个学生的总分和每门课的平均分
(defun compute-scores (students)
  (loop for student in students
        collect (cons (car student)
                      (list :total (reduce #'+ (cdr (cdr student)))
                            :average (map 'list #'average (cdr (cdr student)))))))

;; 计算平均值
(defun average (scores)
  (/ (reduce #'+ scores) (length scores)))
