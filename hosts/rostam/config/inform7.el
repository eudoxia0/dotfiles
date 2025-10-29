;;; inform7.el --- Major mode for working with Inform 7 files

;; Copyright (C) 2020 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/GuiltyDolphin/inform7-mode
;; Git-Repository: git://github.com/GuiltyDolphin/inform7-mode.git
;; Created: 2020-04-11
;; Version: 0.1.1
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (s "1.12.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; inform7-mode provides a major mode for interacting with files
;; written in Inform 7 syntax.
;;
;; For more information see the README.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;
;;;;; Font Lock ;;;;;
;;;;;;;;;;;;;;;;;;;;;


(require 'font-lock)

(defgroup inform7-faces nil
  "Faces used in Inform 7 mode."
  :group 'inform7
  :group 'faces)

(defface inform7-string-face
  '((t . (:inherit font-lock-string-face :weight bold :foreground "#004D99")))
  "Face for Inform 7 strings."
  :group 'inform7-faces)

(defface inform7-substitution-face
  '((t . (:inherit variable-pitch :slant italic :foreground "#3E9EFF")))
  "Face for Inform 7 substitutions embedded in text."
  :group 'inform7-faces)

(defface inform7-rule-name-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for Inform 7 standard rules rule names."
  :group 'inform7-faces)

(defface inform7-statement-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for Inform 7 statement keywords."
  :group 'inform7-faces)

(defface inform7-kind-face
    '((t . (:inherit font-lock-type-face)))
    "Face for Inform 7 kinds, rules names, and actions --- any type that can be used as a value or assigned to a variable.")

(defface inform7-variable-face
    '((t . (:inherit font-lock-variable-name-face)))
    "Face for Inform 7 variables of all sorts.")

(defface inform7-property-face
    '((t . (:inherit font-lock-variable-name-face)))
    "Face for Inform 7 relations, property names.")

(defface inform7-value-face
    '((t . (:inherit font-lock-constant-face)))
    "Face for Inform 7 values and property keyword values."
    :group 'inform7-faces)

(defface inform7-heading-volume-face
    '((t . (:inherit inform7-heading-book-face :height 1.4)))
    "Face for Inform 7 volume headings."
    :group 'inform7-faces)

(defface inform7-heading-book-face
  '((t . (:inherit inform7-heading-part-face :height 1.3)))
  "Face for Inform 7 book headings."
  :group 'inform7-faces)

(defface inform7-heading-part-face
  '((t . (:inherit inform7-heading-chapter-face :height 1.2)))
  "Face for Inform 7 part headings."
  :group 'inform7-faces)

(defface inform7-heading-chapter-face
  '((t . (:inherit inform7-heading-section-face :height 1.1)))
  "Face for Inform 7 chapter headings."
  :group 'inform7-faces)

(defface inform7-heading-section-face
  '((t . (:inherit variable-pitch :weight bold)))
  "Face for Inform 7 section headings."
  :group 'inform7-faces)


(defmacro inform7--make-regex-bol (re)
    "Produce a regular expression for matching RE at the beginning of the line.
Ignores whitespace."
    `(rx (seq bol (* space) ,re)))

(defmacro inform7--make-regex-heading (keyword)
  "Produce a regular expression for matching headings started by the given KEYWORD."
  `(inform7--make-regex-bol
       (seq ,keyword (one-or-more blank) (one-or-more print) eol)))

(defconst inform7-regex-heading
  (inform7--make-regex-heading (or "Volume" "Book" "Part" "Chapter" "Section"))
  "Regular expression for an Inform 7 heading.")

(defconst inform7-regex-heading-volume
  (inform7--make-regex-heading "Volume")
  "Regular expression for an Inform 7 volume heading.")

(defconst inform7-regex-heading-book
  (inform7--make-regex-heading "Book")
  "Regular expression for an Inform 7 book heading.")

(defconst inform7-regex-heading-part
  (inform7--make-regex-heading "Part")
  "Regular expression for an Inform 7 part heading.")

(defconst inform7-regex-heading-chapter
  (inform7--make-regex-heading "Chapter")
  "Regular expression for an Inform 7 chapter heading.")

(defconst inform7-regex-heading-section
  (inform7--make-regex-heading "Section")
  "Regular expression for an Inform 7 section heading.")

(defconst inform7-regex-substitution-maybe-open
  "\\[\\(?:[^]]\\|\\n\\)*\\]?+"
  "Regular expression for matching a substitution embedded in an Inform 7 string (which may not be closed).")

(defconst inform7-regex-string-maybe-open
  (format "\"\\(?:%s\\|[^\"]\\|\\n\\)*\"?+" inform7-regex-substitution-maybe-open)
  "Regular expression for matching an Inform 7 string (which may not be closed).")

(defconst inform7-regex-comment
    (rx (or "[" "(") (one-or-more (not (or ")" "]"))) (or ")" "]")))

(defconst inform7-regex-values
    (rx (one-or-more blank) (group digit (zero-or-more (any digit ".:"))))
    "Regular expression for matching Inform 7 values such as numbers.")

(rx-define inform7-name (one-or-more (not (any ";" "." ":"))))

(defmacro inform7--make-keyword-list (&rest keywords)
    "Generate a regex for a list of keywords, delimited so they won't be highlighted as parts of other words."
    `(rx (seq
             (or bol string-start (any ".:;()[]" blank space))
             (or ,@keywords)
             (or eol string-end symbol-end (any ".:;()[]" blank space)))))

(defconst inform7-regex-action-keywords
    (inform7--make-keyword-list
        "Understand"
        "requires"
        "as"
        "an action with past participle"
        "an action"
        (seq "applying to " (or "nothing" "one" "two"))
        "and"
        "requiring"
        "action has"
        "try"
        "silently"
        "repeatedly"
        "action"
        "is not listed"
        "is listed"
        "before"
        "after"
        "for"
        "for the"
        "while"
        "when"
        "unless"
        "suceeds"
        "fails"
        "make no decision"
        "instead"
        "rulebook"
        "-based"
        "have outcomes"
        "judgement"
        "Judgement"))

(defconst inform7-regex-action-words
    (inform7--make-keyword-list
        (seq (one-or-more graph) "ing")
        (seq "to be " (optional "able to ") (one-or-more graph))
        "that"
        ;; prepositions that usually go with verbs
        "to"
        "aboard"
        "about"
        "above"
        "according to"
        "across"
        "after"
        "against"
        "ahead of"
        "along"
        "amid"
        "amidst"
        "among"
        "around"
        "aside"
        "athwart"
        "atop"
        "before"
        "behind"
        "below"
        "beneath"
        "beside"
        "between"
        "beyond"
        "concerning"
        "down"
        "during"
        "for"
        "from"
        "following"
        "far from"
        "in front of"
        "in place of"
        "inside"
        "instead of"
        "into"
        "near"
        "next to"
        "off"
        "on"
        "on top of"
        "onto"
        "opposite"
        "out"
        "outside"
        "over"
        "past"
        "prior to"
        "toward"
        "towards"
        "under"
        "underneath"
        "until"
        "upon"
        "versus"
        "with"
        "within"
        "without"))

(defconst inform7-regex-builtin-actions
    (inform7--make-keyword-list
        "taking inventory"
        "taking"
        "listing"
        "removing it from"
        "dropping"
        "putting it on"
        "inserting it into"
        "waiting"
        "going"
        "entering"
        "exiting"
        "getting off"
        "looking"
        "examining"
        "looking under"
        "searching"
        "consulting it about"
        "locking it with"
        "unlocking it with"
        "switching on"
        "switching off"
        "opening"
        "closing"
        "wearing"
        "taking off"
        "eating"
        "giving it to"
        "showing it to"
        "waking"
        "throwing it at"
        "attacking"
        "kissing"
        "answering"
        "telling"
        "asking"
        "asking which do you mean"
        "touching"
        "waving"
        "pulling"
        "pushing"
        "turning"
        "pushing"
        "squeezing"
        "saying yes"
        "saying no"
        "burning"
        "waking up"
        "thinking"
        "smelling"
        "listening to"
        "tasting"
        "cutting"
        "jumping"
        "tying"
        "drinking"
        "saying sorry"
        "swearing"
        "swearing"
        "swinging"
        "rubbing"
        "setting"
        "waving hands"
        "buying"
        "singing"
        "climbing"
        "sleeping"
        "quitting the game"
        "saving the game"
        "restoring the game"
        "restarting the game"
        "verifying the story file"
        "requesting the story file version"
        "switching the story transcript"
        "preferring"
        "requesting the pronoun meanings"
        "switching score notification"))

(defconst inform7-regex-builtin-variables
    (inform7--make-keyword-list
        "turn count"
        "true"
        "false"
        "time of day"
        "story author"
        "story creation year"
        "story description"
        "story genre"
        "story headline"
        "release number"
        "the score"
        "topic understood"
        "player's command"
        "indefinite article"
        "second noun"
        "current scene"
        "the location"
        "something"
        "someone"
        "anyone"
        ; you're nobody till somebody loves you.. and that's me -- Mr. New Vegas
        "anybody"
        "somebody"
        "nobody"
        "nothing"
        "everything"
        "anything"
        "anywhere"
        "everywhere"
        "nowhere"
        "noun"
        "actor"
        "person asked"
        "reason the action failed"
        "item described"
        "the action"
        ; "the room" should be here, but it clashes with the kind name in an ugly way
        "it"
        "It"
        "her"
        "him"
        "them"))

(defconst inform7-regex-adjective-keywords
    (inform7--make-keyword-list
        "can be"
        "or"
        "has a"
        "are"
        "either"))

(defconst inform7-regex-calculated-adjective-statement
    (inform7--make-regex-bol "Definition:"))

(defconst inform7-regex-calculated-adjective-keywords
    (inform7--make-keyword-list "yes" "no" "rather than" "says so"))

(defconst inform7-regex-adjective-words
    (inform7--make-keyword-list
        "behavior"
        (seq graph (one-or-more graph) "ed")
        (seq (one-or-more graph) "ble")))

(defconst inform7-regex-builtin-adjectives
    (inform7--make-keyword-list
        "adjacent"
        "an Inform library animate object"
        "an Inform library container"
        "an Inform library door"
        "an Inform library male"
        "an Inform library supporter"
        "closed"
        "concealed"
        "dark"
        "edible"
        "empty"
        "enterable"
        "even"
        "female"
        "fixed in place"
        "going on"
        "handled"
        "improper-named"
        "inedible"
        "initially carried"
        "invisible"
        "lighted"
        "lit"
        "lockable"
        "locked"
        "male"
        "marked for listing"
        "mentioned"
        "negative"
        "neuter"
        "non-empty"
        "non-recurring"
        "odd"
        "off-stage"
        "on-stage"
        "opaque"
        "open"
        "openable"
        "plural-named"
        "portable"
        "positive"
        "privately-named"
        "proper-named"
        "pushable between rooms"
        "recurring"
        "scenery"
        "singular-named"
        "switched off"
        "switched on"
        "touchable"
        "transparent"
        "unconcealed"
        "undescribed"
        "unlit"
        "unlocked"
        "unmarked for listing"
        "unmentioned"
        "unopenable"
        "untouchable"
        "unvisited"
        "visible"
        "visited"
        "wearable"))

(defconst inform7-regex-rule-keywords
    (inform7--make-keyword-list
        "Rule"
        "rules"
        "rule"
        "This is"
        "listed in"
        "follow"
        "consider"
        "abide by"
        "result of"
        "outcome of"
        "make no decision"
        "succeeds"
        "fails"
        "with result"
        "succeeded"
        "failed"
        "ignore"
        "reinstate"
        "reject the result"
        "accept the result"
        "substitute"
        "restore the original"
        "move"
        "first"
        "last"
        "not listed"))

(defconst inform7-regex-standard-rule
    (inform7--make-regex-bol
        (seq (or "After"
                 "Before"
                 "Check"
                 "Carry out"
                 "Every"
                 "Instead of"
                 "Report"
                 "When"
                 "Persuasion"
                 "Procedural"
                 ; we needed to be able to check case for some other stuff, so
                 ; now we've gotta do this:
                 "after"
                 "before"
                 "check"
                 "carry out"
                 "every"
                 "instead of"
                 "report"
                 "when"
                 "persuasion")
            word-boundary))
    "Regular expression for matching a standard Inform 7 rule.")

(defconst inform7-regex-general-statements
    (inform7--make-regex-bol (or "Use" "Include" "version" "by" "Release")))

(defconst inform7-regex-general-keywords
    (inform7--make-keyword-list
        "do nothing"
        "of"
        "allow access"
        "deny access"
        "delete"
        "blank"
        "award"
        "play"
        "specifies"
        "case insensitively"
        "matches"
        "does not match"
        "replace"
        "cut"
        "reject"
        "regular expression"
        "line number"
        "lines"
        "paragraph number"
        "paragraphs"
        "character number"
        "characters"
        "word number"
        "entries"
        "entry"
        "add"
        "remove"
        "truncate"
        "extend"
        "sort"
        "order"
        "reverse order"
        "rotate"
        "backwards"
        "words"
        "all but"
        "all except"
        "almost all"
        "almost no"
        "each"
        "exactly"
        "the number of"
        "implicitly"
        "the description"
        "The description"
        "the initial appearance"
        "in the presence of"
        "be"
        "scope"
        "whether"
        "any"
        "in"
        "into"
        "onto"
        "on"
        "enter"
        "exit"
        "is"
        "is a"
        "are"
        "if"
        "which"
        "who"
        "that"
        "on"
        "none"
        "some"
        "under half"
        "at least"
        "the opposite of"
        "at most"
        "greater than"
        "less than"
        "fewer than"
        "more than"
        "not"
        "random"
        "random chance"
        "off"
        "every"
        "resume the game"
        "say"
        "now"
        "update"
        "all"
        "otherwise"
        "else"
        "ifdef"
        "ifndef"
        "a kind of"
        "usually"
        "end the game"
        "begin"
        "end"
        "display"
        "group"
        "together"
        "giving"
        "handled"))

(defconst inform7-regex-builtin-kinds
    (inform7--make-keyword-list
        "truth state"
        "scene"
        "time"
        "recurring scene"
        "figure-name"
        "figure name"
        (seq "Figure"
            (one-or-more
                (or "of" "and" "the" "a"
                    (any " -.")
                    digit
                    (seq upper (one-or-more (or lower (any "'-.")))))))
        "list"
        "table-name"
        "table name"
        (seq "Table"
            (one-or-more
                (or "of" "and" "the" "a"
                    (any " -.")
                    digit
                    (seq upper (one-or-more (or lower (any "'-.")))))))
        "text"
        "indexed text"
        "number"
        "value"
                                        ; directions
        "north"
        "northwest"
        "west"
        "southwest"
        "south"
        "southeast"
        "east"
        "northeast"
        "up"
        "down"
        "out"
        "inside"
        "outside"
                                        ; singular
        "object"
        "direction"
        "room"
        "region"
        "thing"
        "door"
        "container"
        "vehicle"
        "player's holdall"
        "supporter"
        "backdrop"
        "device"
        "person"
        "man"
        "woman"
        "animal"
                                        ; plural
        "objects"
        "directions"
        "rooms"
        "regions"
        "things"
        "doors"
        "containers"
        "vehicles"
        "player's holdalls"
        "supporters"
        "backdrops"
        "devices"
        "people"
        "men"
        "women"
        "animals"))

; Ideally I should refactor so that all the classes are done in terms of priority, but...
(defconst inform7-regex-override-keywords
    (inform7--make-keyword-list
        ; etc
        "to the nearest"
        "minutes"
        "seconds"
        "hours"
        "stored action"
        "timer rings"
        "turns"
        "the action of"
        "the current action"
        "the plural of"
        "printed plural name"
        "matched text"
        ; kind keywords
        "is a kind of"
        ; variable keywords
        "let"
        "change"
        "increase"
        "decrease"
        "that varies"
        ; phrase/function keywords
        "To"))

(defconst inform7-regex-room-keywords
    (inform7--make-keyword-list
        "is a room"
        "best route"
        "number of moves"
        "use fast route-finding"
        "use slow route-finding"
        "from"))

(defconst inform7-regex-decide-keywords
    (inform7--make-keyword-list
        "decide"
        "whether"
        "what"
        "stop"
        "decide no"
        "decide yes"
        "decide on"
        "for deciding"
        "includes"))

(defconst inform7-regex-inventory-keywords
    (inform7--make-keyword-list
        "carried by"
        "contents"
        "inventory"))

(defconst inform7-regex-loop-keywords
    (inform7--make-keyword-list
        "whole"
        "row"
        "rows"
        "blank"
        "filled"
        "repeat through"
        "choose"
        "running through"
        (seq "running from " inform7-name " to")
        "next"
        "break"
        "reverse order"
        "order"
        "repeat"
        "with"
        "including"
        "excluding"
        "giving"
        "prefacing"
        "suppressing"))

(defconst inform7-regex-math-keywords
    (inform7--make-keyword-list
        "plus"
        "minus"
        "times"
        "multiplied by"
        "divided by"
        "remainder after dividing"
        "total"))

(defconst inform7-regex-relation-keywords
    (inform7--make-keyword-list
        "relates"
        "various"
        "one"
        "each other"
        "another"
        "in groups"
        "The verb"
        "implies"
        "means"
        "relation"))

(defconst inform7-regex-builtin-relations
    (inform7--make-keyword-list
        ; infinitive form
        "to have"
        "to contain"
        "to support"
        "to carry"
        "to wear"
        "to unlock"
        "to conceal"
        "to be part of"
        "to be adjacent to"
        "to hold"
        "to provide"
        "to enclose"

        ; whatever this is
        "has"
        "contains"
        "supports"
        "carries"
        "wears"
        "unlocks"
        "conceals"
        "part of"
        "parts of"
        "adjacent to"
        "holds"
        "provides"
        "encloses"))

(defconst inform7-regex-called-clause
    (rx "("
        (group (or "called " "matched as "))
        (group inform7-name)
        ")"))

(defconst inform7-regex-argument-clause
    (rx "("
        (group inform7-name) " - " (group (one-or-more (not (any ";" "." ":" ")"))))
        ")"))

(defconst inform7-regex-scene-keywords
    (inform7--make-keyword-list
        "begins"
        "ends"
        "happily"
        "sadly"
        "happening"
        "has happened"
        "has not happened"
        "has ended"
        "has not ended"
        "ended in"
        "did not end"))

(defun inform7--match-inside (outer matcher facespec)
    "Match inside the match OUTER with MATCHER, fontifying with FACESPEC."
    (let ((preform `(progn
                        (goto-char (match-beginning 0))
                        (match-end 0))))
        `(,outer . '(,matcher ,preform nil (0 ,facespec t)))))

(defvar inform7-font-lock-keywords
  `(
       ;; types (N.B.: english words are irregular so this is an imperfect guess)
       (,inform7-regex-action-words . 'font-lock-function-name-face)
       (,inform7-regex-adjective-words . 'font-lock-function-name-face)

       ;; inform7 builtins
       (,inform7-regex-builtin-actions (0 'font-lock-builtin-face t))
       (,inform7-regex-builtin-adjectives (0 'font-lock-builtin-face t))

       (,inform7-regex-builtin-kinds (0 'font-lock-type-face t))
       (,inform7-regex-builtin-relations (0 'font-lock-builtin-face t))

       ;; keywords
       (,inform7-regex-action-keywords (0 'inform7-statement-face t))
       (,inform7-regex-adjective-keywords (0 'inform7-statement-face t))
       (,inform7-regex-calculated-adjective-keywords . 'inform7-statement-face)
       (,inform7-regex-calculated-adjective-statement . 'inform7-statement-face)
       (,inform7-regex-rule-keywords . 'inform7-statement-face)
       (,inform7-regex-general-keywords . 'inform7-statement-face)
       (,inform7-regex-general-statements . 'inform7-statement-face)
       (,inform7-regex-room-keywords (0 'inform7-statement-face t))
       (,inform7-regex-decide-keywords (0 'inform7-statement-face t))
       (,inform7-regex-inventory-keywords (0 'inform7-statement-face t))
       (,inform7-regex-loop-keywords (0 'inform7-statement-face t))
       (,inform7-regex-math-keywords (0 'inform7-statement-face t))
       (,inform7-regex-relation-keywords (0 'inform7-statement-face t))
       (,inform7-regex-scene-keywords (0 'inform7-statement-face t))

       ; special stuff
       (,inform7-regex-override-keywords (0 'inform7-statement-face t))
       (,inform7-regex-builtin-variables (0 'font-lock-constant-face t))

       ;; standard rules
       (,inform7-regex-standard-rule (0 'inform7-rule-name-face t))

       ;; values other than strings
       (,inform7-regex-values (0 'inform7-value-face t))

       ;; headings
       (,inform7-regex-heading-volume (0 'inform7-heading-volume-face t))
       (,inform7-regex-heading-book (0 'inform7-heading-book-face t))
       (,inform7-regex-heading-part (0 'inform7-heading-part-face t))
       (,inform7-regex-heading-chapter (0 'inform7-heading-chapter-face t))
       (,inform7-regex-heading-section (0 'inform7-heading-section-face t))

       ;; comments
       (,inform7-regex-comment 0 'font-lock-comment-face t)

       ;; these override comments
       (,inform7-regex-called-clause (1 'inform7-statement-face t))
       (,inform7-regex-called-clause (2 'font-lock-variable-name-face t))

       (,inform7-regex-argument-clause (1 'font-lock-variable-name-face t))
       (,inform7-regex-argument-clause (2 'inform7-kind-face t))
       ;; strings
       (,inform7-regex-string-maybe-open 0 'inform7-string-face t)
       ;; substitutions
       ,(inform7--match-inside inform7-regex-string-maybe-open inform7-regex-substitution-maybe-open `'inform7-substitution-face)
       )
  "Syntax highlighting for Inform 7 files.")

(defun inform7-extend-region ()
    "Extend the search region when updating font lock to include an entire Inform 7 statement, delimited by periods."
    ; Get global variables from font-lock
    (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
    (save-excursion
        (goto-char font-lock-beg)
        (let ((found (or (re-search-backward "\\." nil t) (point-min))))
            (goto-char font-lock-end)
            (when (re-search-forward "\\." nil t)
                (setq font-lock-end (point)))
            (setq font-lock-beg found))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; imenu Support ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(require 's)

(defun inform7--normalise-heading-string (heading)
  "Normalise the heading string HEADING.

This does the following: removes extra whitespace."
  (s-collapse-whitespace (s-trim heading)))

(defun inform7-imenu-create-flat-index ()
  "Produce a flat imenu index for the current buffer.
See `imenu-create-index-function' for details."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward inform7-regex-heading nil t)
        (let ((heading (inform7--normalise-heading-string (match-string-no-properties 0)))
              (pos (match-beginning 0)))
          (setq index (append index (list (cons heading pos)))))))
    index))


;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Indentation ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;


(defun inform7--in-string-p (point)
  "Return non-NIL if POINT is in a string."
  (let ((face (get-text-property point 'face)))
    (memq face '(inform7-substitution-face inform7-string-face))))

(defun inform7--in-comment-p (point)
  "Return non-NIL if POINT is in a comment."
  (let ((face (get-text-property point 'face)))
    (eq face 'font-lock-comment-face)))

(defun inform7--goto-line (line)
  "Move point to the beginning of LINE."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun inform7--line-starts-indent (line)
  "Return non-NIL if LINE should cause subsequent lines to indent."
  (save-excursion
    (inform7--goto-line line)
    (looking-at-p "^.*:[[:space:]]*$")))

(defun inform7--line-indentation (line)
  "Return the horizontal indentation for LINE."
  (save-excursion
    (inform7--goto-line line)
    (current-indentation)))

(defun inform7--max-indentation ()
  "The maximum indentation for the current line."
  ;; first line has max indentation 0
  (if (eq (line-number-at-pos) 1) 0
    (let* ((previous-line (save-excursion (forward-line -1) (line-number-at-pos)))
           (prev-indent (inform7--line-indentation previous-line)))
      (if (inform7--line-starts-indent previous-line)
          (+ tab-width prev-indent)
        prev-indent))))

(defun inform7-indent-line ()
  "Indent the current line as Inform 7 code."
  (interactive)
  (if (or (inform7--in-string-p (point)) (inform7--in-comment-p (point)))
      ;; no indentation cycles if in a string or comment
      'noindent
    (indent-to (inform7--max-indentation))))


;;;;;;;;;;;;;;;;;;;;;;
;;;;; Major Mode ;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode inform7-mode text-mode
    "Inform7"
    "Major mode for editing Inform 7 files."

    ;; Comments
    (setq-local comment-start "[")
    (setq-local comment-end "]")
    (setq-local comment-start-skip "\\[[[:space:]]*")
    (setq-local comment-column 0)
    (setq-local comment-auto-fill-only-comments nil)
    (setq-local comment-use-syntax nil)

    ;; Indenting
    (setq-local tab-width 4)
    (setq-local indent-line-function 'inform7-indent-line)

    ;; Font Lock
    (setq-local font-lock-defaults
        '(inform7-font-lock-keywords
             ;; fontify syntax (not just keywords)
             nil
             ;; ignore case of keywords
             nil
             ((?\" . ".")            ; quote
                 (?\\ . "."))           ; backslashes don't escape
             (font-lock-multiline . t)))

    ;(add-hook 'font-lock-extend-region-functions 'inform7-extend-region)

    ;; We're doing a ton of syntax highlighting now so add some settings to make it performant
    (setq-local jit-lock-stealth-time 16
	jit-lock-defer-contextually t
	jit-lock-stealth-nice 0.5)

    ;; imenu support
    (setq imenu-create-index-function
        #'inform7-imenu-create-flat-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\(ni\\|i7\\)\\'" . inform7-mode)) ; Inform 7 source files (aka 'Natural Inform')
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.i7x\\'" . inform7-mode))           ; Inform 7 extension files


(provide 'inform7)
;;; inform7.el ends here
