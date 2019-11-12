;;; Compiled snippets and support files for `nxml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/ul" nil nil)
                       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/tr" nil nil)
                       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/title" nil nil)
                       ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/th" nil nil)
                       ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/td" nil nil)
                       ("tag" "<${1:tag}>\n  $2\n</$1>$0" "<tag> \\n...\\n</tag>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/tag.2l" nil nil)
                       ("tag" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/tag.1l" nil nil)
                       ("table" "<table>\n  $0\n</table>" "<table>...</table>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/table" nil nil)
                       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/style" nil nil)
                       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/span" nil nil)
                       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/quote" nil nil)
                       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/pre" nil nil)
                       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/p" nil nil)
                       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/ol" nil nil)
                       ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/name" nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/link" nil nil)
                       ("li" "<li>$1</li>" "<li>...</li>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/li" nil nil)
                       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/input" nil nil)
                       ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/img" nil nil)
                       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>" "<html xmlns=\"...\">...</html>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/html" nil nil)
                       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/href" nil nil)
                       ("hr" "<hr />" "<hr />" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/hr" nil nil)
                       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/head" nil nil)
                       ("form" "<form method=\"$1\" action=\"$2\">\n  $0\n</form>" "<form method=\"...\" action=\"...\"></form>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/form" nil nil)
                       ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/div" nil nil)
                       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/code" nil nil)
                       ("br" "<br />" "<br />" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/br" nil nil)
                       ("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/body" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("h6" "<h6>$1</h6>" "<h6>...</h6>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h6" nil nil)
                       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h5" nil nil)
                       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h4" nil nil)
                       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h3" nil nil)
                       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h2" nil nil)
                       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil
                        ("header")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/header/h1" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/meta/meta" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil
                        ("meta")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/meta/doctype.xhtml1_transitional" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil
                        ("meta")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/meta/doctype.xhtml1_strict" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil
                        ("meta")
                        nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/nxml-mode/meta/doctype" nil nil)))


;;; Do not edit! File generated at Tue Jul  9 21:16:27 2019
