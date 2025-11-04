;; Turn on readline.
(use-modules (ice-9 readline))
(activate-readline)

;; Customize the prompt.
(use-modules (system repl common))
(repl-default-option-set! 'prompt "> ")
