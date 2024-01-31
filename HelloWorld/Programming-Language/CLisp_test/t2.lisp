(setq *print-case* :capitalize)

; 数字格式
(format t "Number with commas ~:d" 10000)

(format t "PI to 5 characters ~5f" 3.141593)

(format t "PI to 4 decimals ~,4f" 3.141593)

(format t "10 Percent ~,,2f" .10)

(format t "10 Dollars ~$ ~%" 10)

;函数计算

(format t "(rem 9 4) = ~d ~%" (rem 9 4))

(format t "(mod 9 4) = ~d ~%" (mod 9 4))

(format t "(expt 4 2) = ~d ~%" (expt 4 2))

(format t "(sqrt 81) = ~d ~%" (sqrt 81))

(format t "(exp 1) = ~d ~%" (exp 1))

; 逻辑函数

(format t "(eq 'dog 'dog) = ~d ~%" (eq 'dog 'dog))

(format t "(floor 5.5) = ~d ~%" (floor 5.5))
(format t "(ceiling 5.5) = ~d ~%" (ceiling 5.5))

(format t "(max 5 10) = ~d ~%" (max 5 10))
(format t "(min 5 10) = ~d ~%" (min 5 10))

(format t "(oddp 15) = ~d ~%" (oddp 15))
(format t "(evenp 15) = ~d ~%" (evenp 15))

(format t "(numberp a) = ~d ~%" (numberp 'a))

(format t "(null nil) = ~d ~%" (null nil))

(format t "(null 0) = ~d ~%" (null 0))