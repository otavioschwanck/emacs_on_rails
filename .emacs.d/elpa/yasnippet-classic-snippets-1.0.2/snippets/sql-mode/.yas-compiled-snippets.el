;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
                     '(("references" "REFERENCES ${1:TableName}([${2:ColumnName}])" "REFERENCES ..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/references" nil nil)
                       ("create" "CREATE PROCEDURE [${1:dbo}].[${2:Name}]\n(\n		$3		$4		= ${5:NULL}		${6:OUTPUT}\n)\nAS\nBEGIN\n$0\nEND\nGO" "create procedure ..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/create.1" nil nil)
                       ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}]\n(\n		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}\n$0\n	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3])\n)\nGO" "create table ..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/create" nil nil)
                       ("constraint" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}])" "CONSTRAINT [..] FOREIGN KEY ..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/constraint.1" nil nil)
                       ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}])" "CONSTRAINT [..] PRIMARY KEY ..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/constraint" nil nil)
                       ("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}" ", ColumnName ColumnType NOT NULL..." nil nil nil "/home/otavio/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets/sql-mode/column" nil nil)))


;;; Do not edit! File generated at Tue Jul  9 21:16:27 2019
