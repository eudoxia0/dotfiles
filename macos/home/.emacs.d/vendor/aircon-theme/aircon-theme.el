;;; aircon-theme.el --- Cool and legible light theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2024  Free Software Foundation, Inc.

;; Version: 0.0.6
;; Author: Gregory Chamberlain <greg@cosine.blue>
;; Maintainer: Gregory Chamberlain <~chambln/public-inbox@lists.sr.ht>
;; URL: https://git.sr.ht/~chambln/aircon-theme.el
;; Keywords: faces
;; Package-Requires: ((emacs "24.4"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Aircon is a clean and high contrast custom theme for Emacs.  It
;; consists of a white (#ffffff) background and mostly blues, purples
;; and greens.  Highlights are subtle but easily visible.  The active
;; mode-line is white on blue to distinguish it from inactive
;; mode-lines which are dark blue on light grey.

;;; Code:

(defgroup aircon nil
  "Cool and legible light theme."
  :prefix "aircon-"
  :group 'faces)

(deftheme aircon "Cool and legible light theme.")

(defconst aircon-brick     "#8d2934")   ; error
(defconst aircon-cello     "#243867")   ; builtin
(defconst aircon-eden      "#18566e")   ; constant
(defconst aircon-ghost     "#a3a4ae")
(defconst aircon-goblin    "#327038")   ; string, success
(defconst aircon-grape     "#412f7e")   ; function name
(defconst aircon-hibiscus  "#9a2d71")   ; special
(defconst aircon-lilac     "#5f4f93")   ; variable
(defconst aircon-marigold  "#b88325")   ; warning
(defconst aircon-midnight  "#010e2c")
(defconst aircon-sandstone "#615855")   ; comment
(defconst aircon-sapphire  "#2e4d98")   ; keyword
(defconst aircon-scorpion  "#595959")   ; shadow
(defconst aircon-shark     "#303645")

(defface aircon-athens
  '((((class color) (min-colors 89)) (:background "#e2e3ea")))
  "Aircon athens.")
(defface aircon-blush
  '((((class color) (min-colors 89)) (:background "#eea5a7")))
  "Aircon blush.")
(defface aircon-brick
  `((((class color) (min-colors 89)) (:foreground ,aircon-brick)))
  "Aircon brick.")
(defface aircon-cello
  `((((class color) (min-colors 89)) (:foreground ,aircon-cello)))
  "Aircon cello.")
(defface aircon-eden
  `((((class color) (min-colors 89)) (:foreground ,aircon-eden)))
  "Aircon eden.")
(defface aircon-fjord
  '((((class color) (min-colors 89)) (:background "#49587b")))
  "Aircon fjord.")
(defface aircon-goblin
  `((((class color) (min-colors 89)) (:foreground ,aircon-goblin)))
  "Aircon goblin.")
(defface aircon-grape
  `((((class color) (min-colors 89)) (:foreground ,aircon-grape)))
  "Aircon grape.")
(defface aircon-haze
  '((((class color) (min-colors 89)) (:background "#c4cbdc")))
  "Aircon haze.")
(defface aircon-header
  `((((class color) (min-colors 89))
     (:inherit (aircon-athens) :foreground ,aircon-shark)))
  "Aircon header.")
(defface aircon-hibiscus
  `((((class color) (min-colors 89)) (:foreground ,aircon-hibiscus)))
  "Aircon hibiscus.")
(defface aircon-lilac
  `((((class color) (min-colors 89)) (:foreground ,aircon-lilac)))
  "Aircon lilac.")
(defface aircon-linen
  '((((class color) (min-colors 89)) (:background "#f7e8ca")))
  "Aircon linen.")
(defface aircon-mango
  '((((class color) (min-colors 89))
     (:background "#ffcb63" :foreground "#422c00")))
  "Aircon mango.")
(defface aircon-marigold
  `((((class color) (min-colors 89)) (:foreground ,aircon-marigold)))
  "Aircon marigold.")
(defface aircon-prelude
  '((((class color) (min-colors 89)) (:background "#d5cded")))
  "Aircon prelude.")
(defface aircon-sandstone
  `((((class color) (min-colors 89)) (:foreground ,aircon-sandstone)))
  "Aircon sandstone.")
(defface aircon-sapphire
  `((((class color) (min-colors 89)) (:foreground ,aircon-sapphire)))
  "Aircon sapphire.")
(defface aircon-scorpion
  `((((class color) (min-colors 89)) (:foreground ,aircon-scorpion)))
  "Aircon scorpion.")

(custom-theme-set-faces
 'aircon

 '(default ((t (:background "#ffffff" :foreground "#000000"))))
 `(cursor  ((t (:background ,aircon-cello))))
 '(fringe  ((t (:inherit (default)))))

 `(mode-line                  ((t (:inherit (variable-pitch aircon-fjord)
                                   :foreground "#ffffff"
                                   :box ,aircon-midnight))))
 `(mode-line-inactive         ((t (:inherit (aircon-header mode-line)
                                   :box ,aircon-ghost))))
 '(mode-line-highlight        ((t (:inherit (mode-line)))))

 '(compilation-mode-line-fail ((t (:inherit error))))
 '(compilation-error          ((t (:inherit error))))

 `(help-key-binding ((t (:inherit (aircon-header)
                         :box (:color ,aircon-ghost :line-width (1 . -1))))))

 '(shadow       ((t (:inherit (aircon-scorpion)))))
 '(error        ((t (:inherit (bold aircon-brick)))))
 '(warning      ((t (:inherit (bold aircon-marigold)))))
 '(success      ((t (:inherit (bold aircon-goblin)))))

 '(escape-glyph   ((t (:inherit (aircon-brick)))))
 '(homoglyph      ((t (:inherit (aircon-brick)))))

 '(highlight           ((t (:inherit (aircon-haze)))))
 '(region              ((t (:inherit (aircon-athens)))))
 '(secondary-selection ((t (:inherit (aircon-prelude)))))
 '(trailing-whitespace ((t (:inherit (aircon-blush)))))
 '(isearch             ((t (:inherit (aircon-mango)))))
 '(isearch-fail        ((t (:inherit (aircon-blush)))))
 '(lazy-highlight      ((t (:inherit (aircon-linen)))))
 '(tooltip             ((t (:inherit (aircon-linen)))))
 '(match               ((t (:inherit (aircon-linen)))))
 '(show-paren-match    ((t (:inherit (aircon-mango)))))
 '(show-paren-mismatch ((t (:inherit (aircon-blush)))))
 '(show-paren-match-expression ((t (:inherit (aircon-linen)))))

 '(font-lock-builtin-face       ((t (:inherit (aircon-cello bold)))))
 '(font-lock-comment-face       ((t (:inherit (aircon-sandstone italic)))))
 '(font-lock-constant-face      ((t (:inherit (aircon-eden)))))
 '(font-lock-function-name-face ((t (:inherit (aircon-grape)))))
 '(font-lock-keyword-face       ((t (:inherit (aircon-sapphire bold)))))
 '(font-lock-string-face        ((t (:inherit (aircon-goblin)))))
 '(font-lock-type-face          ((t (:inherit (aircon-hibiscus bold)))))
 '(font-lock-variable-name-face ((t (:inherit (aircon-lilac)))))
 '(font-lock-warning-face       ((t (:inherit (warning)))))
 '(font-lock-doc-face           ((t (:inherit (italic aircon-goblin)))))

 '(link                         ((t (:inherit (aircon-sapphire underline)))))
 '(link-visited                 ((t (:inherit (aircon-lilac link)))))
 '(textsec-suspicious           ((t (:inherit (aircon-blush aircon-brick)))))

 '(minibuffer-prompt            ((t (:inherit (aircon-cello bold)))))

 '(completions-common-part      ((t (:inherit (aircon-linen)))))
 '(completions-first-difference ((t (:inherit (aircon-mango)))))

 `(flymake-error      ((t (:underline (:style wave :color ,aircon-brick)))))
 `(flymake-note       ((t (:underline (:style wave :color ,aircon-sapphire)))))
 `(flymake-warning    ((t (:underline (:style wave :color ,aircon-marigold)))))

 '(flyspell-duplicate ((t (:inherit flymake-warning))))
 '(flyspell-incorrect ((t (:inherit flymake-error))))

 '(diff-header            ((t (:inherit (aircon-header)))))
 '(diff-file-header       ((t (:inherit (diff-header) :weight bold))))
 '(diff-added             ((t (:background "#c4face" :foreground "#143c1d"))))
 '(diff-indicator-added   ((t (:inherit (diff-added bold)))))
 '(diff-refine-added      ((t (:background "#88cd98" :foreground "#0c2912 "))))
 '(diff-removed           ((t (:background "#ffcccc" :foreground "#4b1313"))))
 '(diff-indicator-removed ((t (:inherit (diff-removed bold)))))
 '(diff-refine-removed    ((t (:background "#ef9d9d" :foreground "#3a0a0a "))))
 '(diff-error             ((t (:inherit (error)))))

 '(diary                  ((t (:inherit (aircon-header)))))

 '(change-log-date           ((t (:inherit (aircon-scorpion)))))
 '(change-log-list           ((t (:inherit (aircon-eden)))))
 '(change-log-name           ((t (:inherit (aircon-lilac)))))
 '(change-log-acknowledgment ((t (:inherit (aircon-scorpion)))))
 '(log-view-message          ((t (:inherit (change-log-acknowledgment)))))
 '(log-view-commit-body      ((t (:inherit (aircon-cello)))))
 '(log-view-file             ((t (:inherit (aircon-header)))))

 '(vc-dir-directory          ((t (:inherit (dired-directory)))))
 '(vc-dir-file               ((t (:inherit (default)))))
 '(vc-dir-header             ((t (:inherit (aircon-sapphire bold)))))
 '(vc-dir-header-value       ((t (:inherit (aircon-lilac)))))
 '(vc-dir-status-edited      ((t (:inherit (bold)))))

 '(magit-bisect-bad              ((t (:inherit error))))
 '(magit-bisect-good             ((t (:inherit success))))
 '(magit-bisect-skip             ((t (:inherit warning))))

 '(magit-diffstat-added          ((t (:inherit (aircon-goblin)))))
 '(magit-diffstat-removed        ((t (:inherit (aircon-brick)))))

 '(magit-section-heading           ((t (:inherit (aircon-sapphire bold)))))
 '(magit-section-heading-selection ((t (:inherit (aircon-mango)))))
 '(magit-section-highlight         ((t (:inherit (highlight)))))

 '(magit-blame-heading           ((t (:inherit aircon-header))))
 '(magit-blame-highlight         ((t (:inherit aircon-header))))
 '(magit-blame-margin            ((t (:inherit aircon-header))))

 '(magit-branch-local            ((t (:inherit (aircon-eden)))))
 '(magit-branch-remote           ((t (:inherit (aircon-goblin)))))

 '(magit-hash                    ((t (:inherit (change-log-acknowledgment)))))
 '(magit-tag                     ((t (:inherit (aircon-marigold)))))

 '(magit-log-author              ((t (:inherit (change-log-name)))))
 '(magit-log-date                ((t (:inherit (change-log-date)))))

 '(magit-diff-base               ((t (:inherit aircon-linen))))
 '(magit-diff-base-highlight     ((t (:inherit aircon-mango))))
 '(magit-diff-added              ((t (:inherit diff-added))))
 '(magit-diff-added-highlight    ((t (:inherit diff-refine-added))))
 '(magit-diff-removed            ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight  ((t (:inherit diff-refine-removed))))
 '(magit-diff-whitespace-warning ((t (:inherit trailing-whitespace))))
 '(magit-diff-file-heading       ((t (:inherit vc-dir-status-edited))))
 '(magit-diff-file-heading-selection ((t (:inherit (aircon-mango)))))

 '(magit-diff-hunk-heading               ((t (:inherit (aircon-header)))))
 `(magit-diff-hunk-heading-highlight     ((t (:background ,aircon-ghost))))
 '(magit-diff-context                    ((t (:inherit (aircon-scorpion)))))
 '(magit-diff-context-highlight          ((t (:inherit (aircon-header)))))
 '(magit-diff-file-heading-highlight     ((t (:inherit (aircon-header)))))
 '(magit-diff-revision-summary-highlight ((t (:inherit (aircon-header)))))

 '(dired-directory      ((t (:inherit (aircon-cello bold)))))
 '(dired-header         ((t (:inherit (dired-directory)))))
 '(dired-symlink        ((t (:inherit (italic link)))))
 '(dired-broken-symlink ((t (:inherit (aircon-brick dired-symlink)))))
 '(dired-ignored        ((t (:inherit (shadow)))))
 '(dired-mark           ((t (:inherit (aircon-mango) :background "#ffffff"))))
 '(dired-marked         ((t (:inherit (aircon-mango)))))
 '(dired-perm-write     ((t (:inherit (aircon-goblin)))))
 '(dired-flagged        ((t (:inherit (aircon-blush)))))
 '(dired-special        ((t (:inherit (aircon-hibiscus)))))

 '(eshell-ls-archive    ((t (:inherit aircon-cello))))
 '(eshell-ls-backup     ((t (:inherit dired-ignored))))
 '(eshell-ls-directory  ((t (:inherit dired-directory))))
 '(eshell-ls-executable ((t (:inherit aircon-hibiscus))))
 '(eshell-ls-missing    ((t (:inherit dired-broken-symlink))))
 '(eshell-ls-readonly   ((t (:inherit aircon-sandstone))))
 '(eshell-ls-symlink    ((t (:inherit dired-symlink))))
 '(eshell-prompt        ((t (:inherit minibuffer-prompt))))

 '(ansi-color-black   ((t (:foreground "#000000"))))
 '(ansi-color-white   ((t (:foreground "#ffffff"))))
 `(ansi-color-blue    ((t (:foreground ,aircon-sapphire))))
 `(ansi-color-cyan    ((t (:foreground ,aircon-eden))))
 `(ansi-color-green   ((t (:foreground ,aircon-goblin))))
 `(ansi-color-magenta ((t (:foreground ,aircon-hibiscus))))
 `(ansi-color-red     ((t (:foreground ,aircon-brick))))
 `(ansi-color-yellow  ((t (:foreground ,aircon-marigold))))

 '(elisp-shorthand-font-lock-face
   ((t (:inherit (font-lock-variable-name-face italic)))))

 '(erc-pal-face            ((t (:inherit aircon-hibiscus :weight bold))))
 '(erc-button              ((t (:inherit button))))
 '(erc-keyword-face        ((t (:inherit aircon-sapphire))))
 '(erc-current-nick-face   ((t (:inherit aircon-sapphire))))
 '(erc-dangerous-host-face ((t (:inherit warning))))
 '(erc-direct-msg-face     ((t (:inherit aircon-cello :weight normal))))
 '(erc-error-face          ((t (:inherit error))))
 '(erc-input-face          ((t (:inherit aircon-lilac))))
 '(erc-nick-default-face   ((t (:inherit aircon-sapphire :weight bold))))
 '(erc-nick-msg-face       ((t (:inherit aircon-sapphire :weight bold))))
 '(erc-notice-face         ((t (:inherit aircon-sandstone))))
 '(erc-prompt-face         ((t (:inherit minibuffer-prompt))))
 '(erc-timestamp-face      ((t (:inherit aircon-eden))))

 '(eww-invalid-certificate ((t (:inherit error))))
 '(eww-valid-certificate   ((t (:inherit success))))

 '(message-header-name         ((t (:inherit (aircon-sapphire bold)))))
 '(message-header-newsgroups   ((t (:inherit (aircon-cello bold)))))

 '(message-header-other        ((t (:inherit (aircon-sandstone)))))
 '(message-header-subject      ((t (:inherit (aircon-goblin bold)))))
 '(message-header-to           ((t (:inherit (aircon-grape)))))
 '(message-header-cc           ((t (:inherit (aircon-lilac)))))
 '(message-header-xheader      ((t (:inherit (aircon-hibiscus)))))
 '(message-separator           ((t (:inherit (aircon-header)))))
 '(message-mml                 ((t (:inherit (aircon-goblin)))))

 '(gnus-group-mail-1-empty     ((t (:inherit (aircon-hibiscus)))))
 '(gnus-group-mail-3-empty     ((t (:inherit (aircon-lilac)))))

 '(gnus-summary-selected       ((t (:inherit (aircon-athens)))))
 '(gnus-summary-normal-read    ((t (:inherit (aircon-scorpion)))))
 '(gnus-summary-normal-ancient ((t (:inherit (aircon-sandstone)))))
 '(gnus-summary-normal-ticked  ((t (:inherit (aircon-sapphire bold)))))
 '(gnus-summary-cancelled      ((t (:inherit (aircon-linen error)))))

 '(gnus-header-name            ((t (:inherit (message-header-name)))))
 '(gnus-header-from            ((t (:inherit (message-header-to)))))
 '(gnus-header-subject         ((t (:inherit (message-header-subject)))))
 '(gnus-header-content         ((t (:inherit (message-header-other)))))

 '(gnus-cite-1                 ((t (:inherit (aircon-cello)))))

 '(elfeed-search-date-face         ((t (:inherit (calendar-month-header)))))
 '(elfeed-search-feed-face         ((t (:inherit (aircon-eden)))))
 '(elfeed-search-last-update-face  ((t (:inherit (elfeed-search-date-face)))))
 '(elfeed-search-tag-face          ((t (:inherit (aircon-goblin)))))
 '(elfeed-search-title-face        ((t (:inherit (aircon-scorpion)))))
 '(elfeed-search-unread-count-face ((t (:inherit (default)))))
 '(elfeed-search-unread-title-face ((t (:inherit (bold default)))))
 '(elfeed-log-date-face            ((t (:inherit (elfeed-search-date-face)))))
 '(elfeed-log-debug-level-face     ((t (:inherit (aircon-brick)))))
 '(elfeed-log-error-level-face     ((t (:inherit (error)))))
 '(elfeed-log-info-level-face      ((t (:inherit (aircon-sapphire)))))
 '(elfeed-log-warn-level-face      ((t (:inherit (warning)))))

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aircon)

;;; aircon-theme.el ends here
