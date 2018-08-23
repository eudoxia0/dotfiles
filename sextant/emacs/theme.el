;;;; Theme settings

;;; Rainbow parentheses customization

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#00877C" :weight normal))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#b58900" :weight normal))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#6c71c4" :weight normal))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#cb4b16" :weight normal))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#859900" :weight normal))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#b58900" :weight normal))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#6c71c4" :weight normal))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#cb4b16" :weight normal))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#859900" :weight normal)))))

;;; General theme options

(setq custom-safe-themes t)

(load-theme 'brin t)
