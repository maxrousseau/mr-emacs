;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
					 '(("tt" "$1? |" "test" nil nil nil "/Users/mrousseau/code/mr-emacs/snippets/org-mode/test" nil nil)
					   ("nb" "** $1\n\n$2\n\n#+BEGIN_SRC\n$3\n#+END_SRC" "new noteblock" nil nil nil "/Users/mrousseau/code/mr-emacs/snippets/org-mode/note-block" nil nil)
					   ("newim" "#+CAPTION: $1\n#+NAME:   $2\n#+ATTR_HTML: :width 500px\n[[~/code/ebrain/notes/media/$3]]\n" "add a new image" nil nil nil "/Users/mrousseau/code/mr-emacs/snippets/org-mode/new-org-image" nil nil)
					   ("cc" "$1 \\{\\{c1::$2\\}\\} $3 |" "cloze-card" nil nil nil "/Users/mrousseau/code/mr-emacs/snippets/org-mode/cloze_card" nil nil)
					   ("bc" "$1? | $2" "basic-card" nil nil nil "/Users/mrousseau/code/mr-emacs/snippets/org-mode/basic-card" nil nil)))


;;; Do not edit! File generated at Fri Apr 19 14:52:28 2024
