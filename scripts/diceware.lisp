#!/usr/bin/env cl

(ql:quickload '(:trivial-download :ironclad))

(defparameter +diceware-url+
  "http://world.std.com/~reinhold/diceware.wordlist.asc")

(defparameter +diceware-path+
  #p"~/.scripts/diceware.txt")

(unless (probe-file +diceware-path+)
  (trivial-download:download +diceware-url+ +diceware-path+))

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defparameter +list+ (slurp-file +diceware-path+))

(setf ironclad:*prng* (ironclad:make-prng :fortuna))

(defun digit ()
  (write-to-string (1+ (ironclad:strong-random 6 ironclad:*prng*))))

(defun random-word ()
  (reduce #'(lambda (digit-a digit-b)
              (concatenate 'string digit-a digit-b))
          (loop for i from 1 to 5 collecting (digit))))

(defun extract-word (word)
  (let* ((pos (search word +list+))
         (newline (position #\Newline +list+ :start pos)))
    (subseq +list+ (+ 6 pos) newline)))

(defun generate-password (length)
  (reduce #'(lambda (word-a word-b)
              (concatenate 'string word-a (string #\Space) word-b))
          (loop for i from 1 to length collecting (extract-word (random-word)))))

(print (generate-password 10))
(terpri)
