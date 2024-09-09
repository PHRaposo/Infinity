(in-package :infinity)

(defun infinity-ser (accumul counter n-max)
 (let ((partial-results 
        (append
         accumul
         (list
          (+
           (nth (nth counter (om::arithm-ser 0 n-max 2))
                  accumul)
           (- (nth counter accumul)
                    (nth (1+ counter) accumul)))     
          (+
           (nth (nth counter (om::arithm-ser 1 n-max 2))
                   accumul)
           (- (nth (1+ counter) accumul)
                    (nth counter accumul)))))))
 (cond ((= (length partial-results) n-max) (write partial-results))
           ((> (length partial-results) n-max) (om::first-n partial-results n-max))
    (t (infinity-ser partial-results (1+ counter) n-max)))))

(om::defmethod! infinity-series ((midics list) (n integer)) 
    :initvals '( (6700 6800) 4096)
	:indoc '("List of midics" "Number of notes") 
	:icon 01
	:doc "Generates a Infinity Series with <n> notes." 	
(infinity-ser midics 0 n))

(om::defmethod! infinity-canons ((infinity-series list))
    :initvals '((6700 6800 6600 6900 6800 6700 6500 7000 6600 6900 6700 6800))
	:indoc '("List of midics") 
	:icon 01
	:doc "Generates four canons derived from the infinity series:
      - transposition1: notes 2, 4, 6, 8, etc...
      - mirror (inversion): notes 1, 3, 5, 7, etc...
      -  transposition2: notes 4, 8, 12, 16, 20, etc...
      - replica: notes 1, 5, 9, 13, 17, etc..." 
    :numouts 4	
 (let ((transposition1 (om::posn-match infinity-series (om::arithm-ser 1 (1- (length infinity-series)) 2)))
        (mirror (om::posn-match infinity-series (om::arithm-ser 0 (1- (length infinity-series)) 2)))
        (transposition2 (om::posn-match infinity-series (om::arithm-ser 3 (1- (length infinity-series)) 4)))
        (replica (om::posn-match infinity-series (om::arithm-ser 0 (1- (length infinity-series)) 4))))
 (values transposition1 mirror transposition2 replica)))

;;; ================================ ;;;
;;; FROM OM-TRISTAN 3.4 ;;;

(defun tristan-positions (input1 input2)
  (let ((index 0) res)
    (dolist (n input1)
      (if (eq input2 n)  (push index res)) 
      (setq index (1+ index)))
    (nreverse res)))

;;; ================================ ;;;

#|
(om::defmethod! infinity-rhythms ((midics list) (unit number))
    :initvals '( (6700 6800 6600 6900 6800 6700 6500 7000) 1/16)
    :indoc '("List of midics" "rhythmic-unit") 
    :icon 01
    :doc "Generates a infinity rhythmic series." 
    :numouts 2
 (let* ((infinity-r (infinity-ser '(1 2) 0 (length midics)))
         (zero-positions (tristan-positions infinity-r 0))
         (chords-grace (mapcar #'list zero-positions (om::om+ 1 zero-positions)))
         (infinity-length (om::arithm-ser 0 (1- (length infinity-r)) 1))
         (grace-positions (om::subs-posn infinity-length zero-positions chords-grace))
         (remove-dups (remove-if #'(lambda (n) (member n (om::flat chords-grace))) grace-positions))
         (dur-list (om::om* unit (remove 0 infinity-r)))
         (infinity-with-grace (om::posn-match midics remove-dups)))
(values dur-list infinity-with-grace)))

(defun recursive-sum (s1 s2 n-recursions)
 (let ((result (+ s1 s2))) 
(if (zerop n-recursions)
    (1+ result)
     (recursive-sum s2 result (1- n-recursions)))))

(defun get-fibo-limit (seed1 seed2 n-layers n-periods)
 (recursive-sum seed1 seed2 (+ n-layers (* 2 n-periods))))

(defvar *pattern* '(0 1 2 1 2 3 2 1 2 3 4 3 2 3 2 1))

(defun get-length-layers (n-layers result)
 (let ((length-layer (expt 2 n-layers)))
  (if (= 1 length-layer)
      (om::x-append result length-layer)
      (get-length-layers (1- n-layers)
                                    (om::x-append result length-layer)))))
                                    
(defun get-layer (length-layer pattern results)
 (cond ((< (length results) length-layer)
            (get-layer length-layer 
                           (om::om+ 2 pattern) 
                           (om::x-append results (om::om+ 2 pattern))))
           (t results)))

(defun all-layers-positions (length-layers results)
 (if (= (length length-layers) 1)
     results
      (all-layers-positions (cdr length-layers) 
                                      (om::x-append (list (get-layer (first length-layers) *pattern* *pattern*))
                                                              results))))
  
           

(if (= 1 n-layers)
     result
  (recursive-posn (1- n-layers)      
                            (




;(defun get-layers (fibonacci n-layers n-periods result)
; (let* ((pattern '(0 1 2 1 2 3 2 1))
;         (first-layer (om::posn-match fibonacci)) 

|#

#|
;(defun first-layer

(defmethod! fibo-rhythms ((seed1 number) (seed2 number) (limit number) (n-layers integer) (n-periods integer) (n-repet integer) (unit number))
     :initvals '( 1 2 90 4 4 4 1/16)
    :indoc '("number" "number" "number"  "integer" "integer" "integer" "number" ) 
    :icon 01
    :doc "Generates a infinity rhythmic series based on the Fibonacci series. 
Accepts as arguments: 
- 3 argumentos of the Fibonacci series <seed1>, <seed2> and <limit>;
- Number of layers <n-layers>;
- Numbers of periods <n-periods>;
- Number of repetitions of each period <n-repet>;
- Rhythmic unit (1/16 for sixteenth note, 1/8 for eighth note, 1/4 for quarter note, etc...).
Output: a list of Voice objects.
" 
 (let* ((fibonacci-series (om::fibo-ser seed1 seed2 limit)

(defun fibo-rhythms (fib-ser n-layers)
 (if (or (and (< (length fib-ser) 7))
                  (= n-layers 4))
  
           (and (< (length fib-ser) 6)
                   (= n-layers 3))

          (and (< (length fib-ser) 5)
                  (= n-layers 2))

         (and (< (length fib-ser) 4)
                      (= n-layers 1))
      (error "Increase the <limit> argument from the fibonacci series to use the selected number of layers.")
 ;;; CONDITIONAL - LAYERS = 1, 2, 3 OR 4
(let (
RECURSIVE-FUNCTION
 END: n-layers=1 length = 4
           n-layers=2 length = 5
           n-layers=3 length = 6
           n-layers=4 length = 7
OTHERWISE
TODO-> TRASCRIBE-PATCH
|#

 

  
