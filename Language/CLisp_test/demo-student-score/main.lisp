;; main.lisp

;; 加载其他模块
(load "generate-input.lisp")
(load "compute-scores.lisp")
(load "output.lisp")

;; 主程序
(defun main ()
  ;; 生成输入文件
  (generate-input "input.csv")
  ;; 从输入文件中读取学生信息和成绩
  (let ((students (read-input "input.csv")))
    ;; 计算每个学生的总分和每门课的平均分
    (setf students (compute-scores students))
    ;; 按总分排序学生
    (setf students (sort-students-by-total-score students))
    ;; 输出结果到 CSV 文件
    (output-to-csv "output.csv" students)))

;; 执行主程序
(main)
