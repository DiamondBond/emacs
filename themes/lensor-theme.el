;;; lensor-theme.el --- A palateable dark theme -*- lexical-binding:t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Place this file in emacs-user-directory, "~/.emacs.d/",
;; then load from `~/.emacs` init file with (load-theme 'lensor).

;;; Code:

(deftheme lensor
  "A dark theme for Emacs based on my preference :^).")

;;; Color definitions
(let ((*foreground* "#ffffff")
      (*background* "#161718")

      (*link*         "#2cabff")
      (*link-visited* "#2cabee")

      (*foreground-secondary* "#fcfcfc")
      (*lensor-grey-1* "#e4ebe8")
      (*lensor-grey-2* "#babebb")
      (*lensor-grey-3* "#7f8388")
      (*lensor-grey-4* "#515456")
      (*lensor-grey-5* "#353535")

      (*lensor-bright-red*        "#e71111")
      (*lensor-dark-red*          "#b40000")
      (*lensor-light-red*         "#f58864")
      (*lensor-light-light-red*   "#ffb4a0")
      (*lensor-bright-orange*     "#f86911")
      (*lensor-bright-yellow*     "#fffd80")
      (*lensor-yellow*            "#ffee20")
      (*lensor-light-yellow*      "#f5eba1")
      (*lensor-bright-green*      "#2fdd69")
      (*lensor-green*             "#2da50e")
      (*lensor-light-green*       "#0aff8d")
      (*lensor-light-light-green* "#a3f5c1")
      (*lensor-light-blue-green*  "#80fffd")
      (*lensor-blue-green*        "#6bdcce")
      (*lensor-light-blue*        "#ceffff")
      (*lensor-bright-blue*       "#2cabff")
      (*lensor-blue*              "#135be2")
      (*lensor-beautiful-blue*    "#028090")
      (*lensor-dark-blue*         "#091626")
      (*lensor-bright-cyan*       "#00ffff")
      (*lensor-cyan*              "#00e2e2")
      (*lensor-bright-magenta*    "#dd64f4")
      (*lensor-magenta*           "#a033e0")

      (*lensor-bisque-1* "#ffebbb")
      (*lensor-bisque-2* "#fea80c"))

  (custom-theme-set-faces
   'lensor

   ;;; Basic faces
   `(default
      ((t (:background
           , *background*
           :foreground
           , *foreground*
           ))))
   `(cursor ((t (:background , *lensor-yellow*))))

   ;;; GUI
   `(fringe ((t (:background , *background*))))

   ;;; Highlighting/Selection
   `(shadow ((t (:foreground , *lensor-grey-2*))))
   `(highlight
     ((t (:background
          ,"#000000"
          :foreground
          , *lensor-bright-orange*
          ))))
   `(region ((t (:background , *lensor-grey-5*))))
   `(secondary-selection ((t (:background , *lensor-dark-blue*))))
   `(isearch
     ((t (:background
          , *lensor-bright-orange*
          :foreground
          , *foreground*
          ))))
   `(lazy-highlight
     ((t (:background
          ,"#000000"
          :foreground
          , *lensor-bright-orange*
          ))))

   ;;; Whitespace mode
   `(trailing-whitespace ((t (:background , *lensor-bright-red*))))

   ;;; Font lock
   `(font-lock-builtin-face       ((t (:foreground , *lensor-bright-magenta*))))
   `(font-lock-comment-face       ((t (:foreground , *lensor-grey-3*))))
   `(font-lock-preprocessor-face  ((t (:foreground , *lensor-grey-2*))))
   `(font-lock-constant-face      ((t (:foreground , *lensor-light-blue*))))
   `(font-lock-number-face        ((t (:inherit font-lock-constant-face))))
   `(font-lock-function-name-face ((t (:foreground , *lensor-bisque-1*))))
   `(font-lock-keyword-face       ((t (:foreground , *lensor-bright-magenta*))))
   `(font-lock-string-face        ((t (:foreground , *lensor-yellow*))))
   `(font-lock-escape-face        ((t (:foreground , *lensor-bright-yellow*))))
   `(font-lock-type-face          ((t (:foreground , *lensor-bright-blue*))))
   `(font-lock-operator-face      ((t (:foreground , *lensor-light-green*))))
   `(font-lock-delimiter-face     ((t (:foreground , *lensor-grey-1*))))
   `(font-lock-property-face      ((t (:foreground , *lensor-bright-yellow*))))
   `(font-lock-variable-name-face ((t (:foreground , *lensor-light-blue-green*))))

   ;;; Button and links
   `(link         ((t (:underline t :foreground , *link*))))
   `(link-visited ((t (:underline t :foreground , *link-visited*))))

   ;;; Mode line
   `(mode-line
     ((t (:background
          , *lensor-grey-2*
          :foreground
          , *background*
          :box
          (:line-width -1 :style released-button)
          ))))
   `(eglot-mode-line ((t (:inherit mode-line))))
   `(mode-line-inactive
     ((t (:box
          (:line-width -1 :style released-button)
          :background
          , *lensor-grey-4*
          :foreground
          , *lensor-grey-2*
          ))))

   ;;; Tab bar
   `(tab-bar
     ((t
       (:background
        , *background*
        :foreground
        , *foreground*
        ))))
   `(tab-bar-tab
     ((t
       (:box
        (:line-width
         2
         :style
         released-button
         :color
         , *lensor-grey-3*)
        :background
        , *lensor-grey-4*
        ))))
   `(tab-bar-tab-inactive
     ((t
       (:box
        (:line-width 2 :style released-button)
        :inherit
        tab-bar
        ))))

   ;;; Bookmarks
   `(bookmark-face
     ((t (:background
          , *background*
          :foreground
          , *lensor-bright-orange*
          ))))

   `(compilation-mode-line-fail ((t (:foreground , *lensor-dark-red*))))
   `(compilation-mode-line-run  ((t (:foreground , *lensor-bright-orange*))))
   `(compilation-mode-line-exit ((t (:foreground , *lensor-dark-red*))))

   ;;; Escape and prompt faces
   `(minibuffer-prompt ((t (:foreground , *lensor-grey-2*))))
   `(escape-glyph      ((t (:foreground , *lensor-bisque-2*))))
   `(homoglyph         ((t (:foreground , *lensor-bisque-2*))))
   `(error             ((t (:foreground , *lensor-bright-red*))))
   `(warning           ((t (:foreground , *lensor-bright-orange*))))
   `(success           ((t (:foreground , *lensor-green*))))

   ;;; ANSI Color Code faces
   `(ansi-color-black
     ((t (:background
          , *background*
          :foreground
          , *background*
          ))))
   `(ansi-color-red
     ((t (:background
          , *lensor-dark-red*
          :foreground
          , *lensor-dark-red*
          ))))
   `(ansi-color-green
     ((t (:background
          , *lensor-green*
          :foreground
          , *lensor-green*
          ))))
   `(ansi-color-yellow
     ((t (:background
          , *lensor-yellow*
          :foreground
          , *lensor-yellow*
          ))))
   `(ansi-color-blue
     ((t (:background
          , *lensor-blue*
          :foreground
          , *lensor-blue*
          ))))
   `(ansi-color-magenta
     ((t (:background
          , *lensor-magenta*
          :foreground
          , *lensor-magenta*
          ))))
   `(ansi-color-cyan
     ((t (:background
          , *lensor-cyan*
          :foreground
          , *lensor-cyan*
          ))))
   `(ansi-color-white
     ((t (:background
          , *foreground-secondary*
          :foreground
          , *foreground-secondary*
          ))))
   `(ansi-color-brightblack
     ((t (:background
          , *lensor-grey-3*
          :foreground
          , *lensor-grey-3*
          ))))
   `(ansi-color-brightred
     ((t (:background
          , *lensor-bright-red*
          :foreground
          , *lensor-bright-red*
          ))))
   `(ansi-color-brightgreen
     ((t (:background
          , *lensor-bright-green*
          :foreground
          , *lensor-bright-green*
          ))))
   `(ansi-color-brightyellow
     ((t (:background
          , *lensor-bright-yellow*
          :foreground
          , *lensor-bright-yellow*
          ))))
   `(ansi-color-brightblue
     ((t (:background
          , *lensor-bright-blue*
          :foreground
          , *lensor-bright-blue*
          ))))
   `(ansi-color-brightmagenta
     ((t (:background
          , *lensor-bright-magenta*
          :foreground
          , *lensor-bright-magenta*
          ))))
   `(ansi-color-brightcyan
     ((t (:background
          , *lensor-bright-cyan*
          :foreground
          , *lensor-bright-cyan*
          ))))
   `(ansi-color-brightwhite
     ((t (:background
          , *foreground*
          :foreground
          , *foreground*
          ))))

   ;;; Line number current line
   `(line-number-current-line
     ((t (:inherit
          'default
          :foreground
          , *lensor-yellow*
          ))))

   ;;; imenu
   `(imenu-list-entry-face-0 ((t (:foreground , *lensor-light-blue*))))
   `(imenu-list-entry-face-1 ((t (:foreground , *foreground*))))
   `(imenu-list-entry-face-2 ((t (:foreground , *lensor-grey-2*))))
   `(imenu-list-entry-face-3 ((t (:foreground , *lensor-grey-3*))))
   `(imenu-list-entry-subalist-face-0 ((t (:foreground , *lensor-yellow*))))
   `(imenu-list-entry-subalist-face-1 ((t (:foreground , *lensor-bisque-2*))))
   `(imenu-list-entry-subalist-face-2 ((t (:foreground , *lensor-bright-blue*))))
   `(imenu-list-entry-subalist-face-3 ((t (:foreground , *lensor-bright-green*))))
   ))

;;(custom-theme-set-variables
;; 'lensor
;; ;;'(variable-name EXPRESSION)
;; )

(provide-theme 'lensor)

;;; lensor-theme.el ends here
