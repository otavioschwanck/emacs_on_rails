                             _____________

                               EMBRACE.EL

                              Junpeng Qiu
                             _____________


Table of Contents
_________________

1 Overview
2 Usage
.. 2.1 Example
.. 2.2 Screencasts
.. 2.3 `embrace-change' and `embrace-delete'
.. 2.4 `embrace-add'
3 Customization
.. 3.1 Adding More Semantic Units
.. 3.2 Adding More Surrounding Pairs
.. 3.3 Disable Help Message
.. 3.4 Example Settings
4 For `evil-surround' Users
.. 4.1 Where `embrace' is better
.. 4.2 Why not use together?
5 Contributions
6 Related Packages


Add/Change/Delete pairs based on [expand-region].

For `evil-surround' integration, see [evil-embrace].


[expand-region] https://github.com/magnars/expand-region.el

[evil-embrace] https://github.com/cute-jumper/evil-embrace.el


1 Overview
==========

  This package is heavily inspired by [evil-surround] (which is a port
  of the vim plugin [surround.vim]). But instead of using `evil' and its
  text objects, this package relies on another excellent package
  [expand-region].

  For Emacs users who don't like `evil' and thus don't use
  `evil-surround', `embrace' provides similar commands that can be found
  in `evil-surround'. `Evil' is absolutely *not* required. For
  `evil-surround' users, `embrace' can make your `evil-surround'
  commands even better! (Have you noticed that `evil-surround' doesn't
  work on many custom pairs?)


[evil-surround] https://github.com/timcharper/evil-surround

[surround.vim] https://github.com/tpope/vim-surround

[expand-region] https://github.com/magnars/expand-region.el


2 Usage
=======

  There are three commands: `embrace-add', `embrace-change' and
  `embrace-delete' that can add, change, and delete surrounding pairs
  respectively. You can bind these commands to your favorite key
  bindings.

  There is also a dispatch command `embrace-commander'. After invoking
  `embrace-commander', you can hit:
  - `a' for `embrace-add'
  - `c' for `embrace-change'
  - `d' for `embrace-delete'


2.1 Example
~~~~~~~~~~~

  It might be a little hard for users who have no experience in `evil'
  and `evil-surround' to understand what `embrace' can do. So let's give
  an example to show what `embrace' can do fist. You can look at the
  following sections to see the meaning of key bindings. In this
  example, I bind C-, to `embrace-commander'. Assume we have following
  text in `c-mode' and the cursor position is indicated by `|':
  ,----
  | fo|o
  `----

  Press C-, a w ' to add '' to the current word:
  ,----
  | 'fo|o'
  `----

  Press C-, a q { to add {} to outside of the quotes:
  ,----
  | {'fo|o'}
  `----

  Press C-, c ' " to change the '' to "":
  ,----
  | {"fo|o"}
  `----

  Press C-, c { t, and then enter the tag: body class="page-body", to
  change the {} to a tag:
  ,----
  | <body class="page-body">"fo|o"</body>
  `----

  Press C-, c t f, and enter the function name `bar' to change the tag
  to a function call:
  ,----
  | bar("fo|o")
  `----

  Press C-, d f to remove the function call:
  ,----
  | "fo|o"
  `----

  If you're an `evil-surround' user, you might notice that the last
  command can't be achieved by `evil-surround'. However, it works in
  `embrace'! And yes, you can find even more examples in which
  `evil-surround' doesn't work while `embrace' works!


2.2 Screencasts
~~~~~~~~~~~~~~~

  For non `evil-mode' users, use the following settings (they will be
  explained later):
  ,----
  | (global-set-key (kbd "C-,") #'embrace-commander)
  | (add-hook 'org-mode-hook #'embrace-org-mode-hook)
  `----

  Open an org-mode file, we can perform the following pair changing:

  [./screencasts/embrace.gif]

  For `evil-mode' users, here is a similar screencast (see
  [evil-embrace] for more details):

  [https://github.com/cute-jumper/evil-embrace.el/blob/master/screencasts/evil-embrace.gif]


[evil-embrace] https://github.com/cute-jumper/evil-embrace.el


2.3 `embrace-change' and `embrace-delete'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  These two commands can change and delete the surround pair
  respectively. For `evil-surround' users, `embrace-change' is similar
  to `cs' and `embrace-delete' is similar to `ds'.

  The surrounding pair is specified by a key, which is very similar to
  the key used for Vim's text objects. For example, `(' stands for the
  surrounding pair `(' and `)', and `{' stands for the surrouding pair,
  `{' and `}'. The default key mappings are shown below:
   Key  Left             right
  --------------------------------
   (    "("              ")"
   )    "( "             " )"
   {    "{"              "}"
   }    "{ "             " }"
   [    "["              "]"
   ]    "[ "             " ]"
   >    "<"              ">"
   "    "\""             "\""
   '    "\'"             "\'"
   `    "`"              "`"
   t    "<foo bar=100>"  "</foo>"
   f    "func("          ")"

  Note that for `t' and `f' key, the real content is based on the
  user's input. Also, you can override the closing quote when
  entering a ` (backquote) in emacs-lisp to get a ' (apostrophe)
  instead of a ` (backquote) by using
  `embrace-emacs-lisp-mode-hook' (see below).


2.4 `embrace-add'
~~~~~~~~~~~~~~~~~

  This command is similar to `evil-surround''s `ys' command. We need to
  enter a key for the semantic unit to which we want to add a
  surrounding pair. The semantic unit is marked by the functions
  provided by `expand-region'.

  Here is the default mapping:
   key  mark function
  -----------------------------
   w    er/mark-word
   s    er/mark-symbol
   d    er/mark-defun
   p    er/mark-outside-pairs
   P    er/mark-inside-pairs
   q    er/mark-outside-quotes
   Q    er/mark-inside-quotes
   .    er/mark-sentence
   h    er/mark-paragraph

  After pressing a key to select the semantic unit, you can press
  another key to add the surrounding pair, which is the same as
  `embrace-change' and `embrace-delete'.


3 Customization
===============

3.1 Adding More Semantic Units
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  You can modify the variable `embrace-semantic-units-alist' and note
  that this variable is buffer-local so it is better to change the value
  in a hook:
  ,----
  | (add-hook 'text-mode-hook
  |     (lambda ()
  |        (add-to-list 'embrace-semantic-units-alist '(?e . er/mark-email))))
  `----


3.2 Adding More Surrounding Pairs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Use the command `embrace-add-pair' to add a pair:
  ,----
  | (embrace-add-pair key left right)
  `----

  The change is also buffer-local, so wrap it in a hook function:
  ,----
  | (add-hook 'LaTeX-mode-hook
  |     (lambda ()
  |        (embrace-add-pair ?e "\\begin{" "}")))
  `----

  If you want add something like the `t' key for the tag, you can look
  at the function `embrace-add-pair-regexp' in the source code.

  Note that if you're using `embrace-add-pair' to add an existing key,
  then it will replace the old one.


3.3 Disable Help Message
~~~~~~~~~~~~~~~~~~~~~~~~

  If you find the help message annoying, use the following code to
  disable it:
  ,----
  | (setq embrace-show-help-p nil)
  `----


3.4 Example Settings
~~~~~~~~~~~~~~~~~~~~

  I recommend binding a convenient key for `embrace-commander'. For
  example,
  ,----
  | (global-set-key (kbd "C-,") #'embrace-commander)
  `----

  We have defined several example hook functions that provide additional
  key bindings which can be used in different major modes. Right now
  there are hooks for `LaTeX-mode', `org-mode', `ruby-mode' (including
  `enh-ruby-mode') and `emacs-lisp-mode':

  `LaTeX-mode':
   Key  Left      Right
  ----------------------
   =    \verb|    |
   ~    \texttt{  }
   *    \textbf{  }

  `org-mode':
   Key  Left              Right
  ------------------------------------------
   =    =                 =
   ~    ~                 ~
   *    *                 *
   _    _                 _
   +    +                 +
   k    `@@html:<kbd>@@'  `@@html:</kbd>@@'

  `ruby-mode' and `enh-ruby-mode':
   Key  Left  Right
  ------------------
   #    #{     }
   d    do     end

  `emacs-lisp-mode':
   Key  Left  Right
  ------------------
   `    `      '

  To use them:
  ,----
  | (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  | (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  | (add-hook 'ruby-mode-hook 'embrace-ruby-mode-hook) ;; or enh-ruby-mode-hook
  | (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)
  `----

  The code of two of the hooks above (which are defined in `embrace.el'):
  ,----
  | (defun embrace-LaTeX-mode-hook ()
  |   (dolist (lst '((?= "\\verb|" . "|")
  |                  (?~ "\\texttt{" . "}")
  |                  (?/ "\\emph{" . "}")
  |                  (?* "\\textbf{" . "}")))
  |     (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  | (defun embrace-org-mode-hook ()
  |   (dolist (lst '((?= "=" . "=")
  |                  (?~ "~" . "~")
  |                  (?/ "/" . "/")
  |                  (?* "*" . "*")
  |                  (?_ "_" . "_")
  |                  (?+ "+" . "+")
  |                  (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
  |     (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  `----

  You can define and use your own hook function similar to the code
  above.

  Welcome to add some settings for more major modes.


4 For `evil-surround' Users
===========================

4.1 Where `embrace' is better
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  From the previous example, you can see that `embrace' actually
  replicates all the funcionalities provided in `evil-surround' and it
  can even do more than `evil-surround'. Actually, they are quite
  different. Since `embrace' uses `expand-region' behind the scene, you
  can expect it to work as long as `expand-region' works. Unlike
  `evil-surround', which is restricted to the pre-defined text objects,
  `embrace' can define nearly arbitrary surrounding pairs and three core
  commands always work. On the contratry, you get nearly no
  customization in `evil-surround': custom pairs don't work in `cs' or
  `ds' if you don't have a corresponding text object defined (they work
  in `ys').

  *TL;DR*: `embrace' is more customizable.


4.2 Why not use together?
~~~~~~~~~~~~~~~~~~~~~~~~~

  Sure! You can make `embrace' and `evil-surround' work together. Look
  at [evil-embrace]!


[evil-embrace] https://github.com/cute-jumper/evil-embrace.el


5 Contributions
===============

  This package is still in early stage, but it is quite usable right
  now. More functions can be added and the evil integration is not
  perfect yet. Contributions are always welcome!


6 Related Packages
==================

  - [evil-embrace]
  - [expand-region]
  - [evil-surround]
  - [change-inner]
  - [smartparens]


[evil-embrace] https://github.com/cute-jumper/evil-embrace.el

[expand-region] https://github.com/magnars/expand-region.el

[evil-surround] https://github.com/timcharper/evil-surround

[change-inner] https://github.com/magnars/change-inner.el

[smartparens] https://github.com/Fuco1/smartparens
