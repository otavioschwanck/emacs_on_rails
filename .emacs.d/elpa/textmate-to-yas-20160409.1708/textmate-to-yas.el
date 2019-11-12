;;; textmate-to-yas.el --- Import Textmate macros into yasnippet syntax
;;
;; Filename: textmate-to-yas.el
;; Description: Import Textmate macros into yasnippet syntax
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Oct 20 15:08:50 2010 (-0500)
;; Version: 0.21
;; Last-Updated: Fri Jun 29 12:22:42 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1747
;; URL: https://github.com/mlf176f2/textmate-to-yas.el/
;; Keywords: Yasnippet Textmate
;; Compatibility: Tested with Windows Emacs 23.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; * Importing A Textmate bundle from the Textmate SVN url
;; This is done with the command `textmate-import-svn-from-url'.
;; * Importing from an unzipped Textmate tmBundle
;; This is done with the command `textmate-import-bundle'.  You need to
;; specify both the root directory of the bundle ant the parent modes for
;; importing (like text-mode).
;; *Example function for importing Sata snippets into Yasnippet
;; 
;; *** textmate-import-svn-get-pkgs
;; =(textmate-import-svn-get-pkgs)=
;; 
;;  - Gets textmate bundles from svn
;; 
;; *** textmate-import-svn-snippets
;; =(textmate-import-svn-snippets SNIPPET-URL PLIST TEXTMATE-NAME)=
;; 
;; *Imports snippets based on textmate svn tree.
;; 
;; *** textmate-regexp-to-emacs-regexp
;; =(textmate-regexp-to-emacs-regexp REXP)=
;; 
;;  - Convert a textmate regular expression to an emacs regular expression (as much as possible)
;; 
;; *** textmate-yas-menu
;; =(textmate-yas-menu PLIST &optional MODE-NAME)=
;; 
;;  - Builds `yas-define-menu'from info.plist file
;; 
;; *** textmate-yas-menu-get-items
;; =(textmate-yas-menu-get-items TXT)=
;; 
;; Gets items from TXT and puts them into a list
;; 
;; *** yas---t/
;; =(yas---t/ TEXTMATE-REG TEXTMATE-REP &optional TEXTMATE-OPTION T-TEXT)=
;; 
;;  - Textmate like mirror.  Uses textmate regular expression and textmate formatting.
;; 
;; *** yas-format-match-?-buf
;; =(yas-format-match-\?-buf TEXT &optional STRING EMPTY-MISSING
;; START-POINT STOP-POINT)=
;; 
;;  - Recursive call to temporary buffer to replace conditional formats.
;; 
;; *** yas-getenv
;; =(yas-getenv VAR)=
;; 
;;  - Gets environment variable or customized variable for Textmate->Yasnippet conversion
;; 
;; *** yas-replace-match
;; =(yas-replace-match TEXT &optional STRING
;; TREAT-EMPTY-MATCHES-AS-MISSING-MATCHES SUBEXP)=
;; 
;;  - yas-replace-match is similar to emacs replace-match but using Textmate formats
;; 
;; *** yas-text-on-moving-away
;; =(yas-text-on-moving-away DEFAULT-TEXT)=
;; 
;;  - Changes text when moving away AND original text has not changed
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 9-May-2013    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    Added better auto-installing support for textmate-to-yas.
;; 3-May-2013    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    Better fix for requiring 'textmate-to-yas in Emacs 24+.  It should
;;    download textmate-to-yas if it isn't present.
;; 3-May-2013    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    Added textmate-to-yas automatic package installation for emacs 24
;; 18-Oct-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    Bug fix for yasnippet backward compatibility.
;; 18-Sep-2012      
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    Backward compatibility update.
;; 12-Sep-2012      
;;    Last-Updated: Fri Jun 29 12:22:42 2012 (-0500) #1747 (Matthew L. Fidler)
;;    First attempt to support 0.8x.  Currently untested.
;; 29-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 12:14:15 2012 (-0500) #1742 (Matthew L. Fidler)
;;    Should convert binary plists with either perl and plutil.pl or
;;    Mac OSX and plutil
;; 29-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 10:28:02 2012 (-0500) #1725 (Matthew L. Fidler)
;;    Will not import Textmate snippets that cannot be translated to
;;    emacs regular expressions.
;; 29-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 29 10:01:07 2012 (-0500) #1716 (Matthew L. Fidler)
;;    Added yas/define-menu support. Should fix issue #2
;; 27-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Jun 27 14:49:05 2012 (-0500) #1522 (Matthew L. Fidler)
;;    @capitaomorte suggested more customization.  Implemented.
;; 27-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Jun 27 13:49:04 2012 (-0500) #1510 (Matthew L. Fidler)
;;    Added the ability to transform bundle specific transformations
;; 22-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 22 13:06:56 2011 (-0600) #1502 (Matthew L. Fidler)
;;    Another small fix for quoted parenthesis \}
;; 22-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 22 12:55:43 2011 (-0600) #1499 (Matthew L. Fidler)
;;    Added a fix for Textmate imports to avoid yasnippet bug.
;;    See: https://github.com/capitaomorte/yasnippet/issues/197
;; 21-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Nov 21 12:37:16 2011 (-0600) #1494 (Matthew L. Fidler)
;;    Tested with bsd-tar on windows and changed some extraction behaviors.
;; 01-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Tue Feb  8 08:58:39 2011 (-0600) #1473 (Matthew L. Fidler)
;;    Changed `yas/ma' so that it applies the mirrors upon moving away.
;; 31-Mar-2011    Matthew L. Fidler
;;    Last-Updated: Tue Feb  8 08:58:39 2011 (-0600) #1473 (Matthew L. Fidler)
;;    Add `yas/editing-field-num-p'
;; 08-Feb-2011    Matthew L. Fidler
;;    Last-Updated: Tue Feb  8 08:58:04 2011 (-0600) #1471 (Matthew L. Fidler)
;;    Added autoload cookies.
;; 28-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov 28 18:22:04 2010 (-0600) #1466 (Matthew L. Fidler)
;;    Bug-fix for names.
;; 28-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov 28 15:14:17 2010 (-0600) #1463 (Matthew L. Fidler)
;;    bug fix for yas/t/ when $1 doesn't exist.
;; 12-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Fri Nov 12 17:00:33 2010 (-0600) #1457 (Matthew L. Fidler)
;;    Added #bindings back.
;; 10-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Wed Nov 10 06:57:05 2010 (-0600) #1412 (Matthew L. Fidler)
;;    Bug fix to Textmate to Emacs regular expression matching.
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 23:46:39 2010 (-0600) #1341 (Matthew L. Fidler)
;;    Added error fix for TextMate formats (upper and lower case when match isn't found.)
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 23:11:41 2010 (-0600) #1333 (Matthew L. Fidler)
;;    Bug fix for complicated yas/t/ snippets not converting the \ character to \\.
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 23:02:51 2010 (-0600) #1328 (Matthew L. Fidler)
;;    yas/t/ bugfix for missing text.
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 22:58:35 2010 (-0600) #1326 (Matthew L. Fidler)
;;    Added error handler when guessing modes.
;; 09-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  9 20:14:39 2010 (-0600) #1315 (Matthew L. Fidler)
;;    Added drag and drop support for Github tar.gz files.  Requires Yasnippet to be running.
;; 06-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov  6 10:49:48 2010 (-0500) #1215 (Matthew L. Fidler)
;;    Changed name.
;; 06-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov  6 08:40:34 2010 (-0500) #1210 (Matthew L. Fidler)
;;    Handle nested conditional replacements.  For example (?3:one:(?2:two:none))
;; 05-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Fri Nov  5 16:28:05 2010 (-0500) #1052 (Matthew L. Fidler)
;;    Textmate import file handles errors gracefully.
;; 05-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Fri Nov  5 14:33:00 2010 (-0500) #1050 (Matthew L. Fidler)
;;
;;    Added better textmate support by providing translations for
;;    mirrors. Requires the directive # type: command available in the
;;    SVN version of yasnippet.
;;
;; 05-Nov-2010
;;    Last-Updated: Fri Nov  5 09:59:30 2010 (-0500) #898 (US041375)
;;    Changed textmate-replace-in-string with replace-regexp-in-string
;; 04-Nov-2010
;;    Last-Updated: Thu Nov  4 12:38:32 2010 (-0500) #535 (us041375)
;;    Changed extension from .yasnippet to what the package is in a svn-import.
;; 04-Nov-2010
;;    Last-Updated: Thu Nov  4 10:55:27 2010 (-0500) #525 (us041375)
;;    replace-in-string changed to textmate-replace-in-string.  May be missing on some systems.
;; 01-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov  1 16:19:16 2010 (-0500) #447 (Matthew L. Fidler)
;;    Bug fix for expand-env
;; 01-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov  1 15:16:01 2010 (-0500) #442 (Matthew L. Fidler)
;;    Added more supported tags.
;; 01-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov  1 13:27:13 2010 (-0500) #413 (Matthew L. Fidler)
;;    Took out #scope pseudo-directive.
;; 01-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Mon Nov  1 12:04:30 2010 (-0500) #385 (Matthew L. Fidler)
;;    Added more file extensions.
;; 28-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 28 14:45:28 2010 (-0500) #375 (Matthew L. Fidler)
;;    Removed bindings.  They are currently causing problems...
;; 28-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 28 11:14:35 2010 (-0500) #354 (Matthew L. Fidler)
;;    Added completed import of svn bundle message.
;; 28-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 28 10:56:55 2010 (-0500) #348 (Matthew L. Fidler)
;;    Bug fix to allow files to be .yasnippet instead of _yasnippet files.
;; 27-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Wed Oct 27 23:11:33 2010 (-0500) #342 (Matthew L. Fidler)
;;    Added fix to allow files to pass for directories in `textmate-import-bundle'
;; 27-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Wed Oct 27 15:58:57 2010 (-0500) #338 (Matthew L. Fidler)
;;    Added import from svn.textmate.org using url package.  Use `textmate-import-svn-url'
;; 27-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Wed Oct 27 14:34:53 2010 (-0500) #259 (Matthew L. Fidler)
;;    Added a guess-mode function to take out prompting for modes.
;; 25-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Mon Oct 25 10:17:48 2010 (-0500) #110 (Matthew L. Fidler)
;;    Bug fix for .yas-parents.
;; 25-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Mon Oct 25 10:12:22 2010 (-0500) #97 (Matthew L. Fidler)
;;    Changed import rmate and stata to mirror new textmate-import function
;; 25-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Mon Oct 25 09:59:31 2010 (-0500) #94 (Matthew L. Fidler)
;;    Changed parent-mode to a prompt and uses .yas-parents as in SVN trunk of yasnippet.
;; 22-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Fri Oct 22 09:42:57 2010 (-0500) #82 (Matthew L. Fidler)
;;    Bugfix for ${1:default} expressions
;; 22-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Fri Oct 22 09:34:06 2010 (-0500) #79 (Matthew L. Fidler)
;;    Added ability to choose mode by function or mode-name
;; 21-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 21 16:10:52 2010 (-0500) #61 (Matthew L. Fidler)
;;    Selected text bugfix
;; 21-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 21 15:54:16 2010 (-0500) #56 (Matthew L. Fidler)
;;    Now handles key-bindings as well.
;; 21-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 21 13:34:30 2010 (-0500) #26 (Matthew L. Fidler)
;;    Added a fix to take out spaces in textmate bundles file name translations.
;; 21-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 21 13:29:00 2010 (-0500) #19 (Matthew L. Fidler)
;;
;;    Updated import to find groupings before or after orderings in
;;    the info.plist.
;;
;; 21-Oct-2010    Matthew L. Fidler
;;    Last-Updated: Thu Oct 21 09:05:30 2010 (-0500) #9 (Matthew L. Fidler)
;;
;;    Added a yas/root-directory of the current directory if
;;    undefined.  Allows to be run from the command line by just
;;    loading this file
;;
;; 21-Oct-2010    Matthew L. Fidler
;;    Added optional transformation function.
;; 20-Oct-2010    Matthew L. Fidler
;;    Bug fix -- added mode.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; Add $TM_COMMENT_START
;; Add $TM_ORGANIZATION_NAME


;;
;; TODO: Fix yas/env when they are transformations...
(require 'yasnippet nil t)
(require 'url)
(provide 'texmate-import)
(provide 'textmate-import)
(provide 'texmate-to-yas)
(provide 'textmate-to-yas)

(defvar textmate-to-yas-backward-compatability
  '((yas/expand-snippet yas-expand-snippet)
    (yas/active-field-overlay yas--active-field-overlay)
    (yas/wrap-around-region yas-wrap-around-region)
    (yas/moving-away-p yas-moving-away-p)
    (yas/expand yas-expand)
    (yas/modified-p yas-modified-p)     
    (yas/moving-away-p yas-moving-away-p)
    (yas/text yas-text)
    (yas/skip-and-clear-or-delete-char yas-skip-and-clear-or-delete-char)
    (yas/snippet-fields yas--snippet-fields)
    (yas/snippets-at-point yas--snippets-at-point)
    (yas/update-mirrors yas--update-mirrors)
    (yas/fallback-behavior yas-fallback-behavior)
    (yas/minor-mode yas-minor-mode)
    (yas/field-probably-deleted-p yas--field-probably-deleted-p)
    (yas/field yas-field)
    (yas/field-text-for-display yas--field-text-for-display)
    (yas/snippet-control-overlay yas--snippet-control-overlay)
    (yas/exit-snippet yas-exit-snippet)
    (yas/check-commit-snippet yas--check-commit-snippet)
    (yas/define-snippets yas--check-commit-snippet)
    (yas/field-probably-deleted-p yas--field-probably-deleted-p)
    (yas/after-exit-snippet-hook yas-after-exit-snippet-hook))
  "Yasnippet backward compatability functions used in textmate-to-yas.el")

(defvar textmate-import-backward nil
  "textmate-import use backward compatability?")

;; Add backward compatability when needed.
(mapc
 (lambda(what)
   (unless (eval `(or (boundp ',(nth 1 what)) (fboundp ',(nth 1 what))))
     (if (eval `(functionp ',(nth 0 what)))
         (progn
           (setq textmate-import-backward t)
           (eval `(defalias ',(nth 1 what) ',(nth 0 what))))
       (eval `(defvaralias ',(nth 1 what) ',(nth 0 what))))))
 textmate-to-yas-backward-compatability)

(defgroup textmate-import nil
  "* Textmate import"
  :group 'yasnippet)




(defcustom textmate-use-define-menu t
  "* Use `yas/define-menu' or `yas-define-menu' (0.8+) instead of placing the menu choice in the group tag."
  :type 'boolean
  :group 'textmate-import)

(defcustom textmate-import-plutil.pl
  (concat (file-name-directory (or
                                load-file-name
                                buffer-file-name)) "plutil.pl")
  "plutil.pl path to deal with binary plists."
  :type 'file
  :group 'textmate-import)

(if (or (not (boundp 'yas/root-directory)) (not (boundp 'yas-snippet-dirs)))
    (setq yas-snippet-dirs "./") ; Should already be defined by yasnippet.
  )
(when (and (boundp 'yas/root-directory) (not (boundp 'yas-snippet-dirs)))
  (defalias 'yas/root-directory 'yas-snippet-dirs))

(defun textmate-import-get-property (name start stop)
  "* Get property from plist"
  (let ( (val-start nil) (val-stop nil) (content nil) )
    (goto-char start)
    (when (search-forward (concat "<key>" name "</key>") stop 't)
      (when (search-forward "<string>")
        (setq val-start (point))
        (when (search-forward "</string>")
          (setq val-stop (match-beginning 0))
          (setq content (buffer-substring val-start val-stop)))))
    (symbol-value 'content)))

(defcustom textmate-import-key-bindings nil
  "Import key bindings into the snippets."
  :type 'boolean
  :group 'textmate-import)

(defcustom textmate-default-key-prefix "C-c C-y"
  "When importing key codes, this prefix is used as well.

For example if a Textmate snippet specifies a key of Meta-A, the emacs key specified is
`textmate-default-key-prefix' added to the detected key.  With the default C-c C-y, this
becomes:

C-c C-y M-a
"
  :type 'string
  :group 'textmate-import
  )
(defcustom textmate-key-to-emacs-key-known
  '(
    ("&lt;" "<")        ;
    ("&gt;" ">")
    ("[@]\\(.\\)" "M-\\1")
    ("\\^\\(.\\)" "C-\\1")
    (?\C-a "C-a")
    (?\C-b "C-b")
    (?\C-c "C-c")
    (?\C-d "C-d")
    (?\C-e "C-e")
    (?\C-f "C-f")
    (?\C-g "C-g")
    (?\C-h "C-h")
    (?\C-i "C-i")
    (?\C-j "C-j")
    (?\C-k "C-k")
    (?\C-l "C-l")
    (?\C-m "C-m")
    (?\C-n "C-n")
    (?\C-o "C-o")
    (?\C-p "C-p")
    (?\C-q "C-q")
    (?\C-r "C-r")
    (?\C-s "C-s")
    (?\C-t "C-t")
    (?\C-u "C-u")
    (?\C-v "C-v")
    (?\C-w "C-w")
    (?\C-x "C-x")
    (?\C-y "C-y")
    (?\C-z "C-z"))
  "Textmate Key to Emacs Key"
  :group 'textmate-import
  :type '(repeat
          (list (sexp :tag "Texmate Key/Regular Expression for Key")
                (string :tag "Emacs Replacement"))))

(defmacro texmate-import-unsupported-snippet-p (snippet)
  "Determines if the snippet is supported by emacs."
  `(string-match (regexp-opt textmate-regexp-emacs-unsupported t) ,snippet ))

(defcustom textmate-regexp-emacs-unsupported
  '("(?=" "(?!" "(?<=" "(?<!" "(?>" "(?<" "(?'" "(?i" "(?m" "(?x"
    "(?#")
  "Emacs Unsupported Regular Expression"
  :group 'textmate-import
  :type '(repeat (string :tag "Textmate Expression")))

(defcustom textmate-regexp-to-emacs-regexp-known
  '(;Textmate  Emacs
    ("\\A" "\\\`") ;; Beginning of String ->
    ("\\Z" "\\\'") ;; End of String (or before newline at end of file)
    ("\\z" "\\\'") ;; End of String
    ("\\s" "\\s-")
    ("\\S" "\\S-")
    ("\\w" "\\sw")
    ("\\W" "\\Sw")
    
    ("\\d" "[0-9]")
    ("\\D" "[^0-9]")
    ("\\n" "\n")
    ("\\t" "\t")
    ;; ascii, alnum, blank, cntrl, digit, graph, lower
    ;; multibyte, nonascii, print, punct, space, unibyte, upper
    ;; word, xdigit
    ;; I'm not sure negation works the same [:^ascii:].  I will assume so for
    ;; now.
    ("\\p{Alphanum}" "[A-Za-z0-9]")
    ("\\p{^Alphanum}" "[^A-Za-z0-9]")
    ("\\h" "[0-9a-fA-F]")
    ("\\H" "[^0-9a-fA-F]"))
  "Known Textmate to Emacs regular expression translations."
  :group 'textmate-import
  :type '(repeat
          (list (string :tag "Textmate Expression")
                (string :tag "Emacs Expression"))))

;;;###autoload
(defun textmate-regexp-to-emacs-regexp (rexp)
  "* Convert a textmate regular expression to an emacs regular expression (as much as possible)"
  (save-match-data
    (let (ret
          case-fold-search)
      (with-temp-buffer
        ;; Emacs http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-of-Regexps.html#Syntax-of-Regexps
        ;; Textmate http://manual.macromates.com/en/drag_commands#drag_commands
        
        ;; \w => \w
        ;; \W => \W
        ;; \s => \s- (Whitespace)
        ;; \S => \S-
        ;; \d => [0-9] Decimal digit character
        ;; \D => [^0-9] Non-decimal digit character
        ;; \h => [0-9a-fA-F] Hexadecimal digit character
        ;; \H => [^0-9a-fA-F] Not Hexadecimal digit character
        ;; \p{Alnum} => [A-Za-z0-9] Alphanumeirc
        ;; \p{^Alnum} => [^A-Za-z0-9] Alphanumeirc
        ;; {n,m} => \{n,m\}
        ;; {,n} => \{,n\}
        ;; \{n,\} => \{n,\}
        (insert rexp)
        ;; Deal with expressions where [\w]  is inside of the brackets...  Should do some sexp expression handling to be precise.
        (goto-char (point-min))
        (while (re-search-forward "\\[\\^\\(.*?\\)\\\\\\([wsdh]\\)\\(.*?\\)\\]\\([+*?]*\\|{.*?}\\)" nil t)
          (replace-match (format "(?:\\\\%s\\4|[^\\1\\3]\\4)" (upcase (match-string 2)) 't)))
        
        (goto-char (point-min))
        (while (re-search-forward "\\[\\^\\(.*?\\)\\\\\\([WSDH]\\)\\(.*?\\)\\]\\([+*?]*\\|{.*?}\\)" nil t)
          (replace-match (format "(?:\\\\%s\\4|[^\\1\\3]\\4)" (downcase (match-string 2)) 't)))
        
        (goto-char (point-min))
        (while (re-search-forward "\\[\\^\\(.*?\\)\\\\\\([WSDHwsdh]\\)\\(.*?\\)\\]\\([+*?]*\\|{.*?}\\)" nil t)
          (replace-match "(?:\\\\\\2\\4|[^\\1\\3]\\4)" 't))
        ;; The following needs to be changed (){} |
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile
                                    (regexp-opt '("\\\\"
                                                  "\\("
                                                  "\\)"
                                                  "\\|"
                                                  "\\{"
                                                  "\\}"
                                                  ) 't
                                                    )) nil t)
          (replace-match (string (cond
                                  ( (string= (match-string 0) "\\(") ?\C-a)
                                  ( (string= (match-string 0) "\\)") ?\C-b)
                                  ( (string= (match-string 0) "\\{") ?\C-c)
                                  ( (string= (match-string 0) "\\}") ?\C-d)
                                  ( (string= (match-string 0) "\\|") ?\C-e)
                                  ( (string= (match-string 0) "\\\\") ?\C-f)
                                  )) nil 't)
          )
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile (regexp-opt '("(" ")" "{" "}" "|") 't)) nil t)
          (replace-match (concat "\\" (match-string 0)) nil 't))
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile (regexp-opt (list (string ?\C-a) (string ?\C-b) (string ?\C-c) (string ?\C-d) (string ?\C-e) (string ?\C-f)) 't)) nil t)
          (replace-match (cond
                          ( (string= (match-string 0) (string ?\C-a)) "(")
                          ( (string= (match-string 0) (string ?\C-b)) ")")
                          ( (string= (match-string 0) (string ?\C-c)) "{")
                          ( (string= (match-string 0) (string ?\C-d)) "}")
                          ( (string= (match-string 0) (string ?\C-f)) "\\\\")
                          ( (string= (match-string 0) (string ?\C-e)) "|")) 't 't))
        (mapc
         (lambda(x)
           (goto-char (point-min))
           (while (re-search-forward (regexp-quote (nth 0 x)) nil 't)
             (replace-match (nth 1 x) 't 't)))
         textmate-regexp-to-emacs-regexp-known)
        (setq ret (concat "\\(?:" (buffer-substring-no-properties (point-min) (point-max)) "\\)"))))))

(defcustom textmate-import-convert-known-expressions
  `(
    ("&lt;" "<")
    ("&gt;" ">")
    ("[$][{]\\([0-9]+\\):[$]TM_SELECTED_TEXT[}]" ,(format "${\\1:`%s`}"
                                                          (if (boundp 'yas-selected-text)
                                                              "yas-selected-text"
                                                            "yas/selected-text")))
    ("[$][{]\\([0-9]+\\)[}]" "$\\1")
    ("[$][{]TM_SELECTED_TEXT:\\([^\\}]*\\)[}]" ,(format "`(or %s \"\\1\")`"
                                                        (if (boundp 'yas-selected-text)
                                                            "yas-selected-text"
                                                          "yas/selected-text")))
    ("[$][{]TM_SELECTED_TEXT[}]" ,(format "`(or %s \"\")`"
                                          (if (boundp 'yas-selected-text)
                                              "yas-selected-text"
                                            "yas/selected-text")))
    ("[$]TM_SELECTED_TEXT" ,(format "`(or %s \"\")`"
                                    (if (boundp 'yas-selected-text)
                                        "yas-selected-text"
                                      "yas/selected-text")))
    ;; See http://manual.macromates.com/en/environment_variables.html
    ("[$]TM_YEAR" "`(format-time-string \"%Y\")`")
    ("[$]TM_CURRENT_LINE" "`yas-current-line`")
    ("[$]TM_CURRENT_WORD" "`yas-current-word`")
    ("[$]TM_DIRECTORY" "`yas-current-dir`")
    ("[$]TM_FILEPATH" "`yas-current-path`")
    ("[$]TM_LINE_INDEX" "`yas-current-column`")
    ;; Unsupported:
    ;; TM_SOFT_TABS, TM_SUPPORT_PATH, TM_TAB_SIZE
    
    ;; There are situations where we want our placeholder text
    ;; mirrored but with slight changes or where we want some text to
    ;; appear depending on the value/presence of a placeholder.
    
    ;; We can accomplish this by doing a regular expression
    ;; substitution on the placeholder text (when mirroring it). The
    ;; syntax for this is: ${<<tab stop>>/<<regexp>>/<<format>>/<<options>>}.
    
    
    ;; Also see http://manual.macromates.com/en/drag_commands#drag_commands
    
    ;; TM_DROPPED_FILE -- relative path of the file dropped (relative
    ;; to the document directory, which is also set as the current
    ;; directory).
    
    ;; TM_DROPPED_FILEPATH -- the absolute path of the file dropped.
    
    ;; TM_MODIFIER_FLAGS -- the modifier keys which were held down
    ;; when the file got dropped. This is a bitwise OR in the form:
    ;; SHIFT|CONTROL|OPTION|COMMAND (in case all modifiers were down).
    
    ("[$][{]\\([A-Za-z].*?\\):[$]TM_FULLNAME[}]" "`(or (yas-getenv \"\\1\") (user-full-name))`") ;
    
    ("[$][{]\\([A-Za-z].*?\\):[$]TM_CURRENT_LINE[}]" "`(or (yas-getenv \"\\1\") yas-current-line)`") ;
    ("[$][{]\\([A-Za-z].*?\\):[$]TM_CURRENT_WORD[}]" "`(or (yas-getenv \"\\1\") yas-current-word)`") ;
    ("[$]TM_FULLNAME" "`(user-full-name)`")
                                        ;    ("`date +[+]\\(.*?\\)`" "`(format-time-string \"\\1\")`")
    
    ;; Unknown environment commands.  They can be taught!
    ("[$][{]\\([A-Za-z].*?\\):\\(\\(?:.*?[\\\\][}]\\)*.*?\\)[}]" "`(or (yas-getenv \"\\1\") \"\\2\")`") ;
    ("[$][{]\\([A-Za-z].*?\\)[}]" "`(or (yas-getenv \"\\1\") \"\")`"))
                                        ;  "*Textmate import convert
                                        ;  known expressions"
  "Known Textmate Expressions"
  :group 'textmate-import
  :type '(repeat
          (list (regexp :tag "Regular Expression for Textmate known tag")
                (string :tag "Replacement string"))))

(defvar textmate-import-convert-env-lst '()
  "List to convert Textmate Environmental variables to customizable fields.")

(defun textmate-import-convert-template-t (begin-text max)
  "* Subroutine to convert regular expressions to (yas---t expressions)"
  (let (
        (str (buffer-substring-no-properties (point) max))
        (lst '())
        ret
        )
    (delete-region (point) max)
    (replace-match (format "%s(yas---t/ " begin-text) 't)
    (while (string-match (eval-when-compile (regexp-quote "\\/")) str)
      (setq str (replace-match (string ?\C-a) 't 't str)))
    (setq lst (split-string str "/"))
    (setq ret
          ( mapconcat
            (lambda(x)
              (let ((val x))
                (setq val (replace-regexp-in-string (eval-when-compile (regexp-quote (string ?\C-a))) "\\/" val 't 't))
                (setq val (replace-regexp-in-string "\\\\" "\\\\" val 't 't))
                (setq val (replace-regexp-in-string "\\\\\\\\n" "\\n" val 't 't))
                (setq val (replace-regexp-in-string "\\\\\\\\t" "\\t" val 't 't))
                (setq val (replace-regexp-in-string "\"" "\\\"" val 't 't))
                (setq val (concat "\"" val "\""))
                (symbol-value 'val)))
            lst " "))
    (insert ret)))

(defun textmate-import-convert-template (template)
  "* Converts template to Yasnippet template"
  (let (ret max p1 not-found txt i lst tmp tmp0)
    (with-temp-buffer
      (insert template)
      ;; First replace duplicate transformations like
      ;; \begin{${1:enumerate}}
      ;; $0
      ;; \end{${1:enumerate}}
      ;;
      ;; Should be
      ;;
      ;; \begin{${1:enumerate}}
      ;; $0
      ;; \end{$1}
      (while (re-search-forward "[$]{\\([0-9]\\):" nil t)
        (goto-char (match-beginning 0))
        (setq p1 (point))
        (forward-char 1)
        (save-match-data
          (save-excursion
            (with-syntax-table c-mode-syntax-table
              (forward-sexp 1)
              (setq tmp (buffer-substring-no-properties p1 (point))))))
        (goto-char (match-end 0))
        (setq tmp0 (match-string 1))
        (save-excursion
          (while (search-forward tmp nil t)
            (replace-match (concat "$" tmp0) t t))))
      
      ;; Now replace Textmate mirrors $(1/reg/expr)
      (goto-char (point-min))
      (while (re-search-forward "\\([$][{][0-9]+\\)/" nil t)
        (setq max (save-match-data (save-excursion
                                     (goto-char (match-beginning 0))
                                     (forward-char 1)
                                     (with-syntax-table text-mode-syntax-table
                                       (forward-sexp 1))
                                     (backward-char 1)
                                     (insert ")")
                                     (- (point) 1)
                                     )))
        (textmate-import-convert-template-t "\\1:$" max ))
      ;; Now do ${ENVIROMENT_VAR/reg/format/opt} fields
      (goto-char (point-min))
      (while (re-search-forward "[$][{]\\([^0-9][^/\n]*\\)?/" nil t)
        (setq max (save-match-data
                    (save-excursion
                      (goto-char (match-beginning 0))
                      (forward-char 1)
                      (with-syntax-table text-mode-syntax-table
                        (forward-sexp 1))
                      (backward-char 1)
                      (setq pt (point))
                      (cond
                       ( (string= "TM_SELECTED_TEXT" (match-string 1))
                         (insert (format " (or %s \"\"))`"
                                         (if (boundp 'yas-selected-text)
                                             "yas-selected-text"
                                           "yas/selected-text")))
                         )
                       ( (string= "TM_CURRENT_LINE" (match-string 1))
                         (insert " yas-current-line)`"))
                       ( (string= "TM_CURRENT_WORD" (match-string 1))
                         (insert " yas-current-word)`"))
                       ( (string= "TM_DIRECTORY" (match-string 1))
                         (insert " yas-current-dir)`"))
                       ( (string= "TM_FILEPATH" (match-string 1))
                         (insert " yas-current-path)`"))
                       ( (string= "TM_LINE_INDEX" (match-string 1))
                         (insert " yas-current-column)`"))
                       ( 't
                         (insert (format " (or (yas-getenv \"%s\") \"\"))`" (match-string 1)))))
                      pt)))
        (textmate-import-convert-template-t "`" max ))
      (mapc (lambda(x)
              (goto-char (point-min))
              (while (re-search-forward (nth 0 x) nil t)
                (when (save-match-data (string-match "yas-getenv" (nth 1 x)))
                  (add-to-list 'textmate-import-convert-env-lst (match-string 1)))
                (replace-match (nth 1 x) 't nil)))
            textmate-import-convert-known-expressions)
      (goto-char (point-min))
      (setq max "0")
      (while (re-search-forward "[$][{]?\\([0-9]+\\)" nil t)
        (setq max (match-string 1)))
      (setq max (+ 1 (string-to-number max)))
      (while (search-forward "`(or yas[/-]selected-text \"\")`" nil t)
        (replace-match (format "${%s:`%s`}" max
                               (if (boundp 'yas-selected-text)
                                   "yas-selected-text"
                                 "yas/selected-text")) 't 't))
      
      ;; Now replace (yas---t/ "".*) with the appropriate list
      (setq i 0)
      (goto-char (point-min))
      (setq lst "(setq yas---t-lst (list ")
      (while (re-search-forward "(yas---t/ \"" nil t)
        (setq p1 (- (point) 1))
        (goto-char (match-beginning 0))
        (with-syntax-table text-mode-syntax-table
          (forward-sexp 1))
        (setq lst (concat lst "\n\t(list " (buffer-substring-no-properties p1 (point))))
        (delete-region (match-beginning 0) (point))
        (insert (format "(apply 'yas---t/ (nth %s yas---t-lst))" i))
        (setq i (+ i 1))
        )
      (unless (string= "(setq yas---t-lst (list " lst)
        (setq lst (concat lst "))\n"))  
        (goto-char (point-min))
        (insert (format "(%syas/expand-snippet \""
                        (if (boundp 'yas-selected-text)
                            "yas-expand-snippet"
                          "yas/expand-snippet")))
        (setq p1 (point))
        (while (search-forward "\\" nil t)
          (replace-match "\\\\" t t))
        (goto-char p1)
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" 't 't))
        (goto-char (point-max))
        (insert "\")")
        (goto-char (point-min))
        (insert lst))
      ;; Fix the condition ${#:{ to display correctly
      (goto-char (point-min))
      (while (re-search-forward "[$][{]\\([0-9]+\\)[:][{]" nil t nil)
        (replace-match "${\\1:`\"{\"`"))
      (goto-char (point-min))
      (while (search-forward "\\}" nil t)
        (replace-match "`\"}\"`"))
      (setq ret (buffer-substring (point-min) (point-max))))
    (symbol-value 'ret)))

(defvar textmate-yas-known-uuid nil
  "Known UUIDs for current import.")

;;;###autoload
(defun textmate-yas-submenu (lst submenus space mode-name)
  (let ((ret "")
        submenu
        tmp
        (n-submenu 0)
        (n-items 0)
        (all-missing t)
        (separator-useless t)
        (last-comment nil)
        (que-separator nil))
    (mapc
     (lambda(uuid)
       (cond
        ((member uuid (cadr (assoc mode-name textmate-yas-known-uuid)))
         (when que-separator
           (setq ret (concat ret "\n" space "(yas-separator)"))
           (setq que-separator nil))
         (setq ret (concat ret "\n" space "(yas-item " uuid ")"))
         (setq separator-useless nil)
         (setq last-comment nil)
         (setq n-items (+ n-items 1)))
        ((string-match "^-+$" uuid)
         (unless separator-useless
           (setq que-separator t)
           (setq separator-useless t)))
        ((progn (setq submenu (assoc uuid submenus)) submenu)
         ;; Submenu Found
         (setq tmp (textmate-yas-submenu (nth 2 submenu) submenus
                                         (concat space (make-string 14 ? ))
                                         mode-name))
         (if (string= tmp "")
             (progn
               (concat ret "\n" space ";; Nothing matches submenu " (nth 1 submenu))
               (setq last-comment t))
           (when que-separator
             (setq ret (concat ret "\n" space "(yas-separator)"))
             (setq que-separator nil))
           (with-temp-buffer
             (insert tmp)
             (goto-char (point-min))
             (when (looking-at "\n *\\([(;]\\)")
               (replace-match "\\1"))
             (setq tmp (buffer-substring (point-min) (point-max))))
           (setq ret (concat ret "\n" space "(yas-submenu \"" (nth 1 submenu) "\"\n" space "("
                             tmp "))"))
           (setq last-comment nil)
           (setq n-submenu (+ n-submenu 1))))
        (t
         (setq ret (concat ret "\n" space
                           ";; Could not figure out what to do with " uuid))
         (setq last-comment t))))
     lst)
    (when last-comment
      (setq ret (concat ret "\n" space "")))
    (cond
     ((and (= n-submenu 0) (= n-items 0)) ;; Nothing found.
      (setq ret ""))
     ((and (= n-submenu 1) (= n-items 0)) ;; Only one submenu.
      (with-temp-buffer
        (insert ret)
        (goto-char (point-min))
        (when (re-search-forward "(yas-submenu \"\\(.*?\\)\"[ \t\n]*(" nil t)
          (replace-match (concat";; \\1 only has one submenu\n" space))
          (goto-char (point-max))
          (when (re-search-backward ")[ \t\n]*)" nil t)
            (replace-match "")))
        (setq ret (buffer-substring (point-min) (point-max))))))
    (symbol-value 'ret)))

(defun textmate-yas-menu-get-items (txt)
  "Gets items from TXT and puts them into a list"
  (let ((ret '()))
    (with-temp-buffer
      (insert txt)
      (goto-char (point-min))
      (while (re-search-forward "<string>\\([^-\n]+-[^-\n]+-[^-\n]+-[^-\n]+-[^-\n]+\\|-+\\)</string>" nil t)
        (add-to-list 'ret (match-string 1))))
    (symbol-value 'ret)))

(defun textmate-yas-menu (plist &optional  mode-name)
  "* Builds `yas-define-menu'from info.plist file"
  (if mode-name
      (let ((ret (concat ";; Translated Menu from textmate-to-yas.el\n(yas-define-menu '" mode-name "\n" (make-string 17 ? ) "'("))
            p1 p2
            main-menu
            menu
            items
            name
            ignore-items
            (sub-menus '()))
        (with-temp-buffer
          (insert plist)
          (goto-char (point-min))
          (when (re-search-forward "<key>mainMenu</key>" nil t)
            (setq p2 (point))
            (when (re-search-forward "<key>items</key>" nil t)
              (when (re-search-forward "<array>" nil t)
                (setq p1 (point))
                (when (re-search-forward "</array>" nil t)
                  (setq main-menu (textmate-yas-menu-get-items (buffer-substring p1 (point))))
                  (when (re-search-forward "<key>submenus</key>" nil t)
                    (while (re-search-forward "<key>\\([^-\n]+-[^-\n]+-[^-\n]+-[^-\n]+-[^-\n]+\\)</key>" nil t)
                      (setq uuid (match-string 1))
                      (when (re-search-forward "<key>\\(items\\|name\\)</key>")
                        (setq items "")
                        (setq name "")
                        ;; Get items and names
                        (flet ((f ()
                                  (if (string= "items" (match-string 1))
                                      (when (re-search-forward "<array>" nil t)
                                        (setq p1 (point))
                                        (when (re-search-forward "</array>" nil t)
                                          (setq items (textmate-yas-menu-get-items (buffer-substring p1 (point))))))
                                    (when (re-search-forward "<string>\\(.*?\\)</string>" nil t)
                                      (setq name (match-string 1))))))
                          (f) (f))
                        (add-to-list 'sub-menus `(,uuid ,name ,items))))
                    (goto-char p2)
                    (when (re-search-forward "<key>excludedItems</key>" nil t)
                      (when (re-search-forward "<array>" nil t)
                        (setq p1 (point))
                        (when (re-search-forward "</array>" nil t)
                          (setq ignore-items
                                (concat "'("
                                        (mapconcat
                                         (lambda(x) x)
                                         (textmate-yas-menu-get-items (buffer-substring p1 (point)))
                                         " ") ")")))))))))))
        (setq ret (concat ret  (textmate-yas-submenu main-menu sub-menus (make-string 19 ? ) mode-name) ")\n" (make-string 17 ? ) ignore-items  ")"))
        (symbol-value 'ret))
    ;; Setup directories
    (mapc (lambda(x)
            (let ((new-dir (if (eq (type-of 'yas-snippet-dirs) 'symbol)
                               (nth 0 yas-snippet-dirs)
                             yas-snippet-dirs))
                  (mode (nth 0 x))
                  yas-setup
                  fc
                  (menu ""))
              (setq yas-setup (expand-file-name ".yas-setup.el"
                                                (expand-file-name mode new-dir)))
              (setq fc "")
              (when (file-readable-p yas-setup)
                (setq fc (with-temp-buffer
                           (insert-file-contents yas-setup)
                           (buffer-substring (point-min) (point-max)))))
              (with-temp-file yas-setup
                (insert fc)
                (goto-char (point-max))
                (unless (search-backward "(require 'textmate-to-yas" nil t)
                  (insert "(require 'textmate-to-yas nil t)(if (and (or (not (fboundp 'yas---t/)) (not (featurep 'textmate-to-yas)) (not (package-installed-p 'textmate-to-yas))) (fboundp 'package-install))(require 'package)(add-to-list 'package-archives '(\"marmalade\" .\"http://marmalade-repo.org/packages/\"))(package-initialize) (package-install 'textmate-to-yas))\n"))
                (insert (textmate-yas-menu plist mode))
                (insert "\n")
                (emacs-lisp-mode)
                (indent-region (point-min) (point-max)))))
          textmate-yas-known-uuid)))

(defun textmate-get-group (uuid plist)
  "* Gets group from textmate info.plist file"
  (let (group start stop)
    (with-temp-buffer
      (insert plist)
      (goto-char (point-min))
      (when (search-forward (concat "<string>" uuid "</string>") nil t)
        (when (search-backward "<dict>")
          (setq start (point)))
        (when (search-forward "</dict>")
          (setq stop (point)))
        (setq group (textmate-import-get-property "name" start stop))))
    (symbol-value 'group)))

(defun textmate-import-convert-to-xml (file)
  "Converts a binary plist to XML"
  (cond
   ((and (eq system-type 'darwin)
         (executable-find "plutil"))
    (message "%s" (shell-command-to-string (concat "plutil -convert xml1 " file)))
    file)
   ((and (executable-find "perl")
         (file-readable-p textmate-import-plutil.pl))
    (message "%s" (shell-command-to-string (concat "perl " textmate-import-plutil.pl " " file)))
    (if (file-exists-p (concat file ".text"))
        (progn
          (delete-file file)
          (rename-file (concat file ".text") file))
      (let ((plist file))
        (when (string-match ".plist" plist)
          (setq plist (replace-match ".text.plist" t t plist)))
        (unless (string= file plist)
          (delete-file file)
          (rename-file plist file)))))))

(defun textmate-import-file (file new-dir &optional mode original-author plist transform-function parent-modes)
  "* Imports textmate file"
  (message "Importing %s " file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (re-search-forward "<\\?xml" nil t)
      (message "%s is in binary format, attempting to convert!" file)
      (delete-region (point-min) (point-max))
      (textmate-import-convert-to-xml file)
      (insert-file-contents file))
    (goto-char (point-min))
    (if (re-search-forward "<\\?xml" nil t)
        (textmate-import-current-buffer new-dir plist file original-author mode transform-function parent-modes)
      (message "Binary file conversion failed, cannot import %s" file))))

(defun textmate-import-guess-possiblities (p-quote match-string)
  "* Guesses possible modes..."
  (add-to-list p-quote (intern (concat match-string "-mode")))
  (add-to-list p-quote (intern (concat (downcase match-string) "-mode")))
  (add-to-list p-quote (intern (concat (upcase match-string) "-mode")))
  (when (< 1 (length match-string))
    (add-to-list p-quote (intern (concat (upcase (substring match-string 0 1))
                                         (downcase (substring match-string 1)) "-mode")))))
(defvar textmate-import-saved-guesses '()
  "Saved guesses for textmate import")
(defvar textmate-import-saved-ess '())
(defun textmate-import-guess-mode (scope-o &optional snippet-q)
  "* Guesses mode based on Textmate scope."
  (if (not scope)
      '(text-mode)
    (if (assoc scope-o textmate-import-saved-guesses)
        (let ((ret (nth 1 (assoc scope-o textmate-import-saved-guesses))))
          (when (memq 'ess-mode ret)
            (when (string-match "# *scope: *.*" (symbol-value snippet-q))
              (set snippet-q
                   (replace-match
                    (concat
                     (match-string 0 (symbol-value snippet-q))
                     (format "\n# condition: (string= \"%s\" ess-language)"
                             (nth 1 (assoc scope-o textmate-import-saved-ess))))
                    't 't (symbol-value snippet-q))))
            ;; Take out any Ess keybindings.  They are hard to translate...
            (when (string-match "\n# *binding:.*" (symbol-value snippet-q))
              (set snippet-q (replace-match "" 't 't (symbol-value snippet-q)))))
          (symbol-value 'ret))
      (let (
            (possible-modes '())
            (tmp '())
            (scope scope-o))
        (when (string-match "\\([A-Za-z0-9]+\\)[.]tmbundle" scope)
          (textmate-import-guess-possiblities 'possible-modes (match-string 1 scope)))
        (while (string-match "[.]\\([A-Za-z0-9]+\\)\\>" scope)
          (textmate-import-guess-possiblities 'possible-modes (match-string 1 scope))
          (setq scope (replace-match "" nil nil scope)))
        (setq tmp (remove-if-not
                   #'(lambda(x) (fboundp x)) possible-modes))
        (setq possible-modes '())
        (mapc (lambda(x)
                (with-temp-buffer
                  (condition-case error
                      (progn
                        (funcall x)
                        (add-to-list 'possible-modes major-mode)
                        ;; Handle Ess's strange handling of modes.
                        (when (and snippet-q (eq 'ess-mode major-mode))
                          (add-to-list 'textmate-import-saved-ess (list scope-o ess-language))
                          (when (string-match "# *scope: *.*" (symbol-value snippet-q))
                            (set snippet-q
                                 (replace-match
                                  (concat
                                   (match-string 0 (symbol-value snippet-q))
                                   (format "\n# condition: (string= \"%s\" ess-language)" ess-language))
                                  't 't (symbol-value snippet-q))))
                          ;; Take out any Ess keybindings.  They are hard to translate...
                          (when (string-match "\n# *binding:.*" (symbol-value snippet-q))
                            (set snippet-q (replace-match "" 't 't (symbol-value snippet-q))))))
                    (error
                     (message "[textmate-to-yas] Error Guessing mode: %s" (error-message-string error))))))
              tmp)
        (unless possible-modes
          (setq possible-modes (list (intern (completing-read (format "Emacs Mode (Textmate scope: %s): " scope-o) '())))))
        (add-to-list 'textmate-import-saved-guesses (list scope-o possible-modes))
        (message "Guessed the possible modes: %s" possible-modes)
        (symbol-value 'possible-modes)))))

(defun textmate-import-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let (
        (case-fold-search 't)
        (existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))

(defun textmate-import-current-buffer (new-dir &optional plist  buffer-name original-author mode-string-or-function   transform-function parent-modes ext)
  "* Changes Textmate (current buffer) plist to yas snippet."
  (let (
        (case-fold-search t)
        (start nil)
        (stop nil)
        (val-start nil)
        (val-stop nil)
        (content nil)
        (trigger nil)
        (uuid nil)
        (name nil)
        (scope nil)
        (group nil)
        (type "")
        (snippet "")
        (binding "")
        (mode "")
        (env "")
        (fc "")
        (bfn (or buffer-name (buffer-file-name)))
        (debug-on-error t)
        (yas (or ext ".yasnippet")))
    (when (string-match "/?\\([^/]*\\)[.][^.]*$" bfn)
      (setq bfn (concat (match-string 1 bfn) yas)))
    (while (string-match "[^A-Za-z0-9_.]" bfn)
      (setq bfn (replace-match "_" nil nil bfn)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<dict>" nil t)
        (setq start (point))
        (when (search-forward "</dict>" nil t)
          (setq stop (point))
          (setq content (textmate-import-get-property "content" start stop))
          (setq key (textmate-import-get-property "tabTrigger" start stop))
          (setq uuid (textmate-import-get-property "uuid" start stop))
          (setq name (textmate-import-get-property "name" start stop))
          (setq scope (textmate-import-get-property "scope" start stop))
          (setq group (textmate-get-group uuid plist))
          (setq binding (if textmate-import-key-bindings
                            (textmate-import-get-property "keyEquivalent" start stop)
                          nil))
          (when binding
            ;; Need to convert bindings.
            (setq binding (downcase binding))
            (mapc (lambda(x)
                    (let ((start 0) len)
                      (while (string-match (if (stringp (nth 0 x)) (nth 0 x) (string (nth 0 x))) binding start)
                        (setq len (length binding))
                        (setq binding (replace-match (concat (nth 1 x) " ") t nil binding))
                        (setq start (+ (- (length binding) len) (match-end 0))))))
                  textmate-key-to-emacs-key-known)
            (setq binding (concat textmate-default-key-prefix " " binding)))
          (if (texmate-import-unsupported-snippet-p content)
              (progn
                (message "Snippet %s cannot be imported because it has Textmate specific features that have no equivalent in emacs" name))
            (setq snippet (textmate-import-convert-template content))
            ;; Get Environment
            (when (string-match "\\<yas-current-line\\>" snippet)
              (setq env (concat env " (yas-current-line (buffer-substring (point-at-bol) (point-at-eol))) ")))
            (when (string-match "\\<yas-current-word\\>" snippet)
              (setq env (concat env " (yas-current-word (buffer-substring (save-excursion (skip-syntax-backward \"w\") (point) (save-excursion (skip-syntax-forward \"w\") (point))) ")))
            (when (string-match "\\<yas-current-dir\\>" snippet)
              (setq env (concat env " (yas-current-dir (if (buffer-file-name) (file-name-directory (buffer-file-name)) \"\")) ")))
            (when (string-match "\\<yas-current-path\\>" snippet)
              (setq env (concat env " (yas-current-path (if (buffer-file-name) (buffer-file-name) \"\")) ")))
            (when (string-match "\\<yas-current-column\\>" snippet)
              (setq env (concat env " (yas-current-column (if (current-column) (current-column) \"\")) ")))
            (when (string-match "(yas-expand-snippet" snippet)
              (setq type "\n# type: command")
              ;; Add environment to expand/snippet on command snippets.
              (unless (string= "" env)
                (goto-char (point-max))
                (when (re-search-backward ")" nil t)
                  (insert "nil nil \"")
                  (insert (replace-regexp-in-string "\"" "\\\"" env 't 't))
                  (insert "\""))))
            (setq snippet (concat "# -*- mode: snippet -*-"
                                  "\n# uuid: " uuid
                                  "\n# contributor: Translated from textmate snippet by textmate-import.el"
                                  "\n# contributor: Imported by " (user-full-name)
                                  (if original-author
                                      (concat "\n# contributor: Original Author " original-author)
                                    "")
                                  (if (string= env "") ""
                                    (concat "\n# expand-env : (" env ")"))
                                  "\n# name: " name
                                  (if (not key)
                                      ""
                                    (concat "\n# key: " key))
                                  (if (not binding)
                                      ""
                                    (concat "\n# binding:" binding))
                                  "\n# scope: " scope
                                  type
                                  (if (and group (not textmate-use-define-menu))
                                      (concat "\n# group: " group)
                                    "")
                                  "\n# --\n"
                                  snippet))
            (when transform-function
              (setq snippet (apply transform-function (list snippet))))
            (cond
             ( (functionp mode-string-or-function)
               (setq mode (list (funcall mode-string-or-function snippet))))
             ( (stringp mode-string-or-function)
               (setq mode (list mode-string-or-function)))
             ( 't
               (setq mode (mapcar (lambda(x) (format "%s" x)) (textmate-import-guess-mode scope 'snippet)))))
            ;; (setq new-dir (concat new-dir mode))
            (mapc (lambda(m)
                    (textmate-import-add-to-alist
                     'textmate-yas-known-uuid
                     `(,m ,(append (list uuid)
                                   (cadr (assoc m textmate-yas-known-uuid)))))
                    (unless (string= m "")
                      (setq m (concat m "/")))
                    (when (not (file-exists-p (concat new-dir "/" m)))
                      (make-directory (concat new-dir "/" m) 't))
                    (with-temp-file (concat new-dir "/" m "/" bfn)
                      (set-buffer-file-coding-system 'raw-text)
                      (insert snippet)
                      (goto-char (point-min))
                      (when (re-search-forward "# *scope:.*\n" nil t)
                        (replace-match "")))
                    (if (not parent-modes)
                        (setq parent-modes "text-mode"))
                    (when (and parent-modes (not (string= parent-modes "")))
                      (unless (file-exists-p (concat new-dir "/" m ".yas-parents"))
                        (with-temp-file (concat new-dir "/" m ".yas-parents")
                          (insert parent-modes))))
                    
                    (when (and textmate-import-convert-env-lst (> (length textmate-import-convert-env-lst) 0))
                      (let ( (fc "")
                             (defg (format "(defgroup yas-%s nil \"%s snippet options\" :group 'yasnippet)" (substring m 0 -1)  (substring m 0 -1)))
                             (defc (format "(defcustom yas-%senv/%%s nil \"%s environment variable %%s.  May be customized here instead of having the environment value specified.  This customization takes precedence over any environmental variable.\"\n  :type 'string\n  :group 'yas-%s)\n" (substring m 0 -1) (substring m 0 -1) (substring m 0 -1))))
                        (setq fc "")
                        (when (file-exists-p (concat new-dir "/" m ".yas-setup.el"))
                          (setq fc (with-temp-buffer (insert-file-contents (concat new-dir "/" m "/.yas-setup.el"))
                                                     (buffer-substring (point-min) (point-max)))))
                        (with-temp-file (concat new-dir "/" m ".yas-setup.el")
                          (insert fc)
                          (goto-char (point-max))
                          (unless (search-backward "(require 'textmate-to-yas" nil t)
                            (insert "(require 'textmate-to-yas nil t)(if (and (or (not (fboundp 'yas---t/)) (not (featurep 'textmate-to-yas)) (not (package-installed-p 'textmate-to-yas))) (fboundp 'package-install))(require 'package)(add-to-list 'package-archives '(\"marmalade\" .\"http://marmalade-repo.org/packages/\"))(package-initialize) (package-install 'textmate-to-yas))\n"))
                          (goto-char (point-max))
                          (unless (search-backward defg nil t)
                            (insert defg)
                            (insert "\n"))
                          (mapc (lambda(txt)
                                  (goto-char (point-max))
                                  (unless (search-backward (format defc txt txt) nil t)
                                    (insert (format defc txt txt))
                                    (insert "\n")))
                                textmate-import-convert-env-lst)))))
                  mode)
            (setq textmate-import-convert-env-lst '())))))))

;;;###autoload
(defun textmate-import-drag-and-drop (uri &rest ignore)
  "* Drag and drop interface to import files."
  (let ((f (dnd-get-local-file-name uri t)) ret)
    (when (and yas-minor-mode
               (string-match "[/\\\\]\\([^\n/\\\\-]*?\\)-\\([^\n/\\\\.]*?\\)\\([.]tmbundle\\)\\(.*\\)\\([.]tar[.]gz\\)$" uri)
               (yes-or-no-p (format "Would you like to import %s git-hub tarball into Yasnippet?" f)))
      (textmate-import-git-tar.gz f "text-mode")
      (setq ret 't))
    (symbol-value 'ret)))

;;;###autoload
(defadvice dnd-open-local-file (around textmate-import-drag-and-drop activate)
  "* Drag Textmate git-hub tar.gz files to import into Yasnippet."
  (unless (textmate-import-drag-and-drop (ad-get-arg 0))
    ad-do-it))

;;;###autoload
(defadvice dnd-open-file (around textmate-import-drag-and-drop activate)
  "* Drag Textmate git-hub tar.gz files to import into Yasnippet."
  (unless (textmate-import-drag-and-drop (ad-get-arg 0))
    ad-do-it))

;;;###autoload
(defun textmate-import-git-tar.gz (file parent-modes)
  "* Imports a TextMate git-hub bundle."
  (interactive "fTextmate GIThub .tar.gz file: \nsParent Modes: ")
  (setq textmate-yas-known-uuid nil)
  (setq textmate-import-convert-env-lst nil)
  (let (original-author
        (gz (executable-find "gzip"))
        (tar (executable-find "tar"))
        (rm (executable-find "rm"))
        (cmd (if (fboundp 'shell-command-to-string) 'shell-command-to-string 'exec-to-string))
        (pwd (if (buffer-file-name) (file-name-directory (buffer-file-name)) (expand-file-name "./")))
        temp-dir new-file
        new-dir)
    (save-excursion
      (if (not (and gz tar rm))
          (error "Can't find gzip or tar.  Can't decompress")
        (if (not (string-match "[/\\\\]\\([^\n/\\\\-]*?\\)-\\([^\n/\\\\.]*?\\)\\([.]tmbundle\\)\\(.*\\)\\([.]tar[.]gz\\)$" file))
            (error "Does not seem to be a tar ball from Github.")
          (setq original-author (format "%s (Package %s from Github t, ver %s)"
                                        (match-string 1 file)
                                        (match-string 2 file)
                                        (match-string 4 file)))
          (save-match-data
            (message "Decompressing tar ball")
            (setq temp-dir (make-temp-file "textmate-import" 't))
            (cd temp-dir)
            (message "%s" (format "%s -d %s " gz file))
            (message "%s" (apply cmd (list (format "%s -d %s" gz file))))
            (message "%s" (format "%s -xvf %s" tar (substring file 0 -3)))
            (message "%s" (apply cmd (list (format "%s -xvf %s" tar (substring file 0 -3)))))
            (message "%s" (format "%s %s " gz (substring file 0 -3)))
            (message "%s" (apply cmd (list (format "%s %s" gz (substring file 0 -3))))))
          (setq new-file (concat temp-dir "/" (match-string 1 file)
                                 "-" (match-string 2 file) (match-string 3 file)
                                 (match-string 4 file) "/"))
          (message "textmate-import-bundle: %s %s %s" new-file parent-modes original-author)
          (textmate-import-bundle new-file parent-modes original-author))))
    (cd temp-dir)
    (message "%s" (apply cmd (list (format "%s -rf %s" rm temp-dir))))))

;;;###autoload
(defun textmate-import-bundle (dir parent-modes &optional original-author yas-dir mode transform-function)
  "Imports textmate bundle to new-dir.  Mode may be a string or a function determining which mode to place files in..."
  (interactive "fTextmate Bundle Directory: \nsParent Modes: ")
  (setq textmate-yas-known-uuid nil)
  (setq textmate-import-convert-env-lst nil)
  (let ((new-dir (if (eq (type-of 'yas-snippet-dirs) 'symbol)
                     (nth 0 yas-snippet-dirs)
                   yas-snippet-dirs))
        snip-dir snips plist)
    
    (unless (or (string= (substring new-dir -1) "/")
                (string= (substring new-dir -1) "\\"))
      (setq new-dir (concat new-dir "/")))
    (message "Snippet Output Directory %s" new-dir)
    (when (file-exists-p (expand-file-name "info.plist" dir))
      (setq plist (with-temp-buffer
                    (insert-file-contents (expand-file-name "info.plist" dir))
                    (goto-char (point-min))
                    (unless (re-search-forward "<\\?xml" nil t)
                      (message "info.plist is in binary format, attempting to convert!")
                      (delete-region (point-min) (point-max))
                      (textmate-import-convert-to-xml (expand-file-name "info.plist" dir))
                      (insert-file-contents (expand-file-name "info.plist" dir)))
                    (goto-char (point-min))
                    (if (re-search-forward "<\\?xml" nil t)
                        (buffer-substring (point-min) (point-max))
                      nil))))
    (if (not plist)
        (message "Cannot convert binary plist, aborting snippet import.")
      (setq snip-dir (expand-file-name "Snippets" dir))
      (message "Snippet Dir located in: %s" snip-dir)
      (when (file-exists-p snip-dir)
        (let ((default-directory snip-dir))
          (setq snips
                (apply 'append
                       (mapcar #'(lambda (ext)
                                   (file-expand-wildcards (concat "*." ext) t))
                               (list
                                "tmSnippet"
                                "plist"
                                "tmCommand"
                                "tmMacro")))))
        (message "Snippets: %s" snips)
        (unless (not (file-exists-p new-dir))
          (mapc (lambda(x)
                  (textmate-import-file x new-dir mode original-author plist transform-function parent-modes))
                snips))
        (when textmate-use-define-menu
          (textmate-yas-menu plist))
        (message "Finished importing %s" dir)))))
(defun textmate-import-stata (dir &optional new-dir)
  "*Example function for importing Sata snippets into Yasnippet"
  (message "Importing Stata bundle dir %s" dir)
  (textmate-import-bundle dir "text-mode" "Timothy Beatty" new-dir))

(defun textmate-import-rmate (dir &optional new-dir)
  "* Example Function for importing Rmate into Yasnippet"
  (message "Importing Rmate Bundle dir %s" dir)
  (textmate-import-bundle dir "text-mode" "Hans-Peter Suter" new-dir))

(defvar textmate-import-svn-url "http://svn.textmate.org/trunk/Bundles/"
  "* Url for Textmate svn")
(defvar textmate-import-svn-pkgs-cache nil
  "* Cached list of Textmate svn bundles")

(defun textmate-import-svn-get-pkgs ()
  "* Gets textmate bundles from svn"
  (if textmate-import-svn-pkgs-cache
      (symbol-value 'textmate-import-svn-pkgs-cache)
    (let ((buf (url-retrieve-synchronously textmate-import-svn-url))
          (lst '()))
      (save-excursion
        (set-buffer buf)
        (goto-char (point-min))
        (while (re-search-forward "\"\\([%A-Z0-9_a-z]+\\)[.]tmbundle/\"" nil t)
          (add-to-list 'lst (match-string 1)))
        (kill-buffer (current-buffer)))
      (setq textmate-import-svn-pkgs-cache
            (mapcar (lambda(x) (replace-regexp-in-string "%20" " " x)) lst))
      (symbol-value 'textmate-import-svn-pkgs-cache))))

(defun textmate-import-snippets-supported (textmate-url)
  "Check to see if snippets are supported"
  (let ((buf (url-retrieve-synchronously textmate-url))
        (ret nil))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (setq ret (re-search-forward "\"Snippets/\"" nil t))
      (kill-buffer (current-buffer)))))

(defun textmate-import-svn-snippets (snippet-url plist textmate-name)
  "*Imports snippets based on textmate svn tree."
  (message "Fetching %s" snippet-url)
  (setq textmate-yas-known-uuid nil)
  (setq textmate-import-convert-env-lst nil)
  (let ((ext textmate-name)
        buf
        (snippets '())
        (new-dir (if (eq (type-of 'yas-snippet-dirs) 'symbol)
                     (nth 0 yas-snippet-dirs)
                   yas-snippet-dirs))
        (default-buffer-file-coding-system 'utf-8)
        (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
    (setq buf (url-retrieve-synchronously snippet-url))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]*[.]\\(?:tmSnippet\\|plist\\|tmCommand\\|tmMacro\\)\\)\"" nil 't)
        (add-to-list 'snippets (match-string 1)))
      (kill-buffer (current-buffer)))
    (while (string-match "[^A-Za-z0-9_.]+" ext)
      (setq ext (replace-match "_" 't 't ext)))
    (setq ext (concat "." ext))
    (mapc (lambda(x)
            (message "Fetching %s" (concat snippet-url x))
            (setq buf (url-retrieve-synchronously (concat snippet-url x)))
            (save-excursion
              (set-buffer buf)
              (goto-char (point-min))
              (unless (re-search-forward "<\\?xml" nil t)
                (message "This is in binary format. Attempting to convert.")
                (let ((tmp-file (make-temp-file "textmate-to-yas-convert-")))
                  (write-file tmp-file)
                  (textmate-import-convert-to-xml tmp-file)
                  (delete-region (point-min) (point-max))
                  (insert-file-contents tmp-file)
                  (delete-file tmp-file)))
              (goto-char (point-min))
              (if (re-search-forward "<\\?xml" nil t)
                  (textmate-import-current-buffer new-dir plist
                                                  (replace-regexp-in-string "%3c" "<"
                                                                            (replace-regexp-in-string "%20" " " x))
                                                  nil
                                                  nil
                                                  nil
                                                  nil
                                                  ext)
                (message "Error converting to xml format."))
              (kill-buffer (current-buffer)))
            (message "Imported %s" (replace-regexp-in-string "%3c" "<"
                                                             (replace-regexp-in-string
                                                              "%20" " " x)))
            (sleep-for 1))
          snippets)
    (when textmate-use-define-menu
      (textmate-yas-menu plist))
    (yas-reload-all)))
;;;###autoload
(defun textmate-import-svn-from-url ()
  "* Imports a textmate bundle and extracts snippets from `textmate-import-svn-url'"
  (interactive)
  (let (
        (textmate-name (completing-read "Textmate package: " (textmate-import-svn-get-pkgs) nil 't))
        textmate-url
        temp-dir
        buf
        plist
        )
    (setq textmate-url (concat textmate-import-svn-url (replace-regexp-in-string " " "%20" textmate-name) ".tmbundle/"))
    (if (not (textmate-import-snippets-supported textmate-url))
        (progn
          (setq textmate-import-svn-pkgs-cache (remove-if
                                                #'(lambda(x) (string= textmate-name x))
                                                textmate-import-svn-pkgs-cache))
          (error "This Textmate package has no snippets"))
      (message "Fetching %s" (concat textmate-url "info.plist"))
      (setq buf (url-retrieve-synchronously (concat textmate-url "info.plist")))
      (save-excursion
        (set-buffer buf)
        (goto-char (point-min))
        (unless (re-search-forward "<\\?xml" nil t)
          (message "This is in binary format. Attempting to convert.")
          (let ((tmp-file (make-temp-file "textmate-to-yas-convert-")))
            (write-file tmp-file)
            (textmate-import-convert-to-xml tmp-file)
            (delete-region (point-min) (point-max))
            (insert-file-contents tmp-file)
            (delete-file tmp-file)))
        (goto-char (point-min))
        (if (re-search-forward "<\\?xml" nil t)
            (setq plist (buffer-substring (point-min) (point-max)))
          (message "Cannot convert info.plist to xml; Aborting snippet import.")
          (setq plist nil))
        (kill-buffer (current-buffer)))
      (sleep-for 1)
      (when plist
        (textmate-import-svn-snippets (concat textmate-url "Snippets/") plist textmate-name))
      (message "Completed loading snippets from textmate package %s" textmate-name))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet helper functions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas-getenv (var)
  "* Gets environment variable or customized variable for Textmate->Yasnippet conversion"
  (let (
        (bvar (intern (format "yas-%s/env/%s" (or yas-extra-modes yas/mode-symbol major-mode) var))))
    (if (boundp bvar)
        (if (symbol-value bvar)
            bvar
          (getenv var))
      (getenv var))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros for yas-replace-match
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro yas-format-match-ulm (match-number &optional string downcase)
  "* Helper macro to change textmate match-string \\u$1 to the correct expression"
  `(if (> (length (match-string ,match-number ,string)) 1)
       (concat (,(if downcase 'downcase 'upcase) (substring (match-string ,match-number ,string) 0 1))
               (substring (match-string ,match-number ,string) 1))
     (upcase (match-string ,match-number ,string))))

(defmacro yas-format-match-UE (text &optional string downcase)
  "* Helper macro to emulate Textmate case folding in replacement that is \\U\\E and \\U\\L"
  (let (
        (lst (make-symbol "lst"))
        (ret (make-symbol "ret"))
        (md2 (make-symbol "md2"))
        (md (make-symbol "md"))
        (lst2 (make-symbol "lst2"))
        (ret2 (make-symbol "ret2"))
        (start (make-symbol "start"))
        (num (make-symbol "num"))
        (mtch (make-symbol "mtch")))
    `(let (case-fold-search
           ,lst
           ,ret
           (,md (match-data))
           ,md2)
       (setq ,lst (split-string ,text ,(if downcase "\\\\L" "\\\\U") 't))
       (setq ,ret (pop ,lst))
       (setq ,ret (concat ,ret
                          (mapconcat
                           (lambda(x)
                             (let (,lst2 ,ret2 (,start 0) ,num ,mtch)
                               (setq ,lst2 (split-string x "\\\\E" 't))
                               (setq ,ret2 (,(if downcase 'downcase 'upcase) (pop ,lst2)))
                               (while (string-match "[$]\\([0-9]+\\)" ,ret2 ,start)
                                 (setq ,num (string-to-number (match-string 1 ,ret2)))
                                 (setq ,md2 (match-data)) ; Save match in current string
                                 (set-match-data ,md) ; Set match to actual match we are replacing.
                                 (if (not (match-string ,num ,string))
                                     (setq ,mtch "")
                                   (setq ,mtch (,(if downcase 'downcase 'upcase) (match-string ,num ,string)))) ; get the match data and make it upper case.
                                 (set-match-data ,md2) ; Put the match data in ret2 back.
                                 (setq ,start (+ (match-beginning 0) (length ,mtch)))
                                 (setq ,ret2 (replace-match ,mtch 't 't ,ret2))
                                 )
                               ;; Put extra \E values back in.
                               (setq ,ret2 (concat ,ret2 (mapconcat (lambda(y) y) ,lst2 "\\E")))
                               (symbol-value ',ret2)))
                           ,lst
                           "")))
       (set-match-data ,md)
       (symbol-value ',ret))))

(defmacro yas-format-match-u (text &optional string downcase)
  "* Macro to replace \\u$1 (or \\l$1) with the correct expansion"
  (let (
        (md (make-symbol "md"))
        (md2 (make-symbol "md2"))
        (num (make-symbol "num"))
        (ret (make-symbol "ret"))
        (start (make-symbol "start"))
        (mtch (make-symbol "mtch"))
        )
    `(let (
           (,md (match-data))
           ,md2 ,num ,mtch
           case-fold-search
           (,ret ,text)
           (,start 0)
           )
       (while (string-match ,(if downcase "\\\\l[$]\\([0-9]+\\)" "\\\\u[$]\\([0-9]+\\)") ,ret ,start)
         (setq ,num (string-to-number (match-string 1 ,ret)))
         (setq ,md2 (match-data))
         (set-match-data ,md)
         (setq ,mtch (yas-format-match-ulm ,num ,string ,(if downcase 't 'nil))) ;;downcase
         (set-match-data ,md2)
         (setq start (+ (match-beginning 0) (length ,mtch)))
         (setq ,ret (replace-match ,mtch 't 't ,ret))
         )
       (set-match-data ,md)
       (symbol-value ',ret))))

(defun yas-format-match-?-buf (text &optional string empty-missing start-point stop-point)
  "* Recursive call to temporary buffer to replace conditional formats."
  ;; I don't believe recursive functions can be macros.
  (let ((start (or start-point (point-min)))
        (stop (or stop-point (point-max)))
        insert other num
        (md (match-data))
        md2
        p1 p2 p3)
    (goto-char start)
    (while (re-search-forward "([?]\\([0-9]+\\)[:]" stop t)
      (setq p1 (match-beginning 0))
      (setq p2 (match-end 0))
      (goto-char p1)
      (with-syntax-table text-mode-syntax-table ;; Go to end of this conditional statement.
        (forward-sexp 1))
      (setq p3 (point))
      (set-match-data md)
      ;; Recursive call, should get rid of nested conditional statements
      (yas-format-match-?-buf text string empty-missing p2 p3)
      (goto-char p1)
      (when (looking-at "([?]\\([0-9]+\\)[:]\\(\\(?:.\\|\n\\)*?\\)\\(?:[:]\\(\\(?:.\\|\n\\)*?\\))\\|)\\)")
        (setq other "")
        (setq num (string-to-number (match-string 1)))
        (setq insert (match-string 2))
        (if (match-string 3)
            (setq other (match-string 3)))
        (setq md2 (match-data))
        (set-match-data md)
        (if (match-string num string)
            (if (string= "" (match-string num string))
                (setq mtch other)
              (setq mtch insert))
          (setq mtch other)
          )
        (set-match-data md2)
        (replace-match mtch 't 't)))
    ;; Restore original match.
    (set-match-data md)))

(defmacro yas-format-match-? (text &optional string empty-missing)
  "* Replaces conditional statements (?3:insertion:otherwise) or (?3:insertion).
Also tries to handle nested conditional statements like (?1:$0:(?2:\\t$0))
"
  (let ((ret (make-symbol "ret")))
    `(let ((,ret ,text))
       (with-temp-buffer
         (insert ,ret)
         (yas-format-match-?-buf ,text ,string ,empty-missing)
         (setq ,ret (buffer-substring-no-properties (point-min) (point-max))))
       (symbol-value ',ret))))
(defmacro yas-format-match-$ (text &optional string)
  "* Replace $1 with the appropriate match."
  (let (
        (ret (make-symbol "ret"))
        (md (make-symbol "md"))
        (start (make-symbol "start"))
        (md2 (make-symbol "md2"))
        (num (make-symbol "num"))
        (mtch (make-symbol "mtch"))
        )
    `(let (
           (,ret ,text)
           (,md (match-data))
           (,start 0)
           ,md2 ,num ,mtch)
       (while (string-match "[$]\\([0-9]+\\)" ,ret ,start)
         (setq ,num (string-to-number (match-string 1 ,ret)))
         (setq ,md2 (match-data))
         (set-match-data ,md)
         (setq ,mtch (match-string ,num ,string))
         (set-match-data ,md2)
         (setq ,start (+ (match-beginning 0) (length ,mtch)))
         (if ,mtch
             (setq ,ret (replace-match ,mtch 't 't ,ret))
           (setq ,ret (replace-match "" t t ,ret))))
       (set-match-data ,md)
       (symbol-value ',ret))))

(defmacro yas-format-match (text &optional string treat-empty-matches-as-missing-matches)
  "* Use Textmate style format strings to replace match data."
  (let ((ret (make-symbol "ret")))
    `(let (,ret)
       (setq ,ret (yas-format-match-UE ,text ,string))
       (setq ,ret (yas-format-match-UE ,ret ,string 't))
       (setq ,ret (yas-format-match-u ,ret ,string))
       (setq ,ret (yas-format-match-u ,ret ,string 't))
       (setq ,ret (yas-format-match-? ,ret ,string ,treat-empty-matches-as-missing-matches))
       (setq ,ret (yas-format-match-$ ,ret ,string))
       (symbol-value ',ret))))

(defun yas-replace-match (text &optional string treat-empty-matches-as-missing-matches subexp)
  "* yas-replace-match is similar to emacs replace-match but using Textmate formats"
  (replace-match (yas-format-match text string treat-empty-matches-as-missing-matches) 't 't string subexp))

;;;###autoload
(defun yas---t/ (textmate-reg textmate-rep &optional textmate-option t-text)
  "* Textmate like mirror.  Uses textmate regular expression and textmate formatting."
  (let ((option (or textmate-option ""))
        (ret (or t-text yas---text ""))
        (start 0)
        (fix "")
        (reg (textmate-regexp-to-emacs-regexp textmate-reg))
        mtch
        case-fold-search)
    (when (string-match "[iI]" option)
      (setq case-fold-search 't) ;; Case insensitive search
      )
    (when (string-match "[sS]" option)
      ;; Treat string as a single line.
      )
    (when (string-match "[mM]" option)
      ;; Treat string as multiple lines.  Instead of matching ^ or $ to
      ;; the beginning or ending of the string, it matches the
      ;; beginning or ending of the line.
      
      ;; In theory, the default behavior is to match the beginning and
      ;; ending of the string.
      
      
      ;; Currently this does NOTHING.
      )
    (cond
     ( (string-match "[gG]" option) ;; Global replace
                                        ;       (esn-message "%s" reg)
       (while (string-match reg ret start)
         (setq mtch (yas-format-match textmate-rep ret))
                                        ;         (esn-message "Match String %s,%s" (match-string 0 ret) mtch)
         (setq start (+ (match-beginning 0) (length mtch)))
         (setq ret (replace-match mtch t t ret))))
     ( 't ;; Replace first occurrence
       (when (string-match reg ret)
         (setq ret (yas-replace-match textmate-rep ret)))))
    (symbol-value 'ret)))
;;(message "%s" (yas---t/ "\\\\\\w+\\{(.*?)\\}|\\\\(.)|(\\w+)|([^\\w\\\\]+)" "(?4:_:\\L$1$2$3)" "g" "SUBSUBSECTION name"))
;;(message "%s" (yas---t/ "\\w+\\((.*)\\)|.*" "$1" "" "name(me)"))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for yas---text-on-moving-away and yas-ma
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas-ma-was-modified nil)
(make-variable-buffer-local 'yas-ma-was-modified)
;;;###autoload
(defun yas-text-on-moving-away (default-text)
  "* Changes text when moving away AND original text has not changed"
  (cond
   ((and (not yas-modified-p) yas-moving-away-p)
    (if (string= "" default-text)
        (progn
          (yas-skip-and-clear-or-delete-char)
          (when (boundp 'yas---text)
            (setq yas---text ""))
          (when (boundp 'text)
            (setq text ""))
          (let ((snippet (car (yas--snippets-at-point))))
            (when snippet
              (yas--update-mirrors snippet))))
      (insert default-text)
      (let ((snippet (car (yas--snippets-at-point))))
        (when snippet
          (yas--update-mirrors snippet)))))))

(defalias 'yas-ma 'yas-text-on-moving-away)
(defalias 'yas-emld 'yas---text-on-moving-away)

(defun yas-editing-field-num-p (&optional arg)
  "Which field is active?"
  (interactive)
  (let* ((arg (or arg 0))
         (snippet (first (yas--snippets-at-point)))
         (active-field (overlay-get yas--active-field-overlay (if textmate-import-backward
                                                                  'yas/field
                                                                'yas--field)))
         (live-fields (remove-if #'(lambda (field)
                                     (and (not (eq field active-field))
                                          (yas--field-probably-deleted-p snippet field)))
                                 (yas--snippet-fields snippet)))
         (active-field-pos (position active-field live-fields)))
    (= active-field-pos arg)))

(defvar yas---t-lst '()
  "Variable for expanding textmate transformations with Yasnippet")

;;(textmate-import-bundle "C:/tmp/rmate/swissr-rmate.tmbundle-7d026da" "text-mode")
                                        ;(textmate-import-stata "c:/tmp/Stata.tmbundle/")
                                        ;(setq debug-on-error 't)
                                        ;(setq debug-on-quit 't)
                                        ;(textmate-import-bundle "c:/tmp/textmate-php.tmbundle-b7dd4ef/" "text-mode html-mode")


;;https://github.com/subtleGradient/javascript-tools.tmbundle
(provide 'textmate-to-yas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; textmate-to-yas.el ends here
