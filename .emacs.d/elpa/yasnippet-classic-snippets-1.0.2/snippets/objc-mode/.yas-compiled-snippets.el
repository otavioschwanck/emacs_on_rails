;;; Compiled snippets and support files for `objc-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'objc-mode
                     '(("prop" "- (${1:id})${2:foo}\n{\n    return $2;\n}\n\n- (void)set${2:$(capitalize text)}:($1)aValue\n{\n    [$2 autorelease];\n    $2 = [aValue retain];\n}\n$0" "foo { ... } ; setFoo { ... }" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/objc-mode/prop" nil nil)))


;;; Do not edit! File generated at Tue Jul  9 21:16:27 2019
