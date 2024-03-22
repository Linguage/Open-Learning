;; main.lisp

;; 加载其他模块
(load "calculations.lisp")
(load "output.lisp")

;; 主程序
(defun main ()
  (let ((result (calculate 5 10))) ; 调用计算函数
    (output-to-csv "output.csv" result))) ; 输出结果到 CSV 文件

;; 执行主程序
(main)
