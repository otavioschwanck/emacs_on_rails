# Emacs on Rails

A Emacs configuration file to use on Ruby on Rails Development.

## Requirements

- ripgrep silversearcher-ag
- Emacs 26 (OBS: On ubuntu 18.04 or lower, you need to use a PPA)
- Rbenv (optional)

## Quick Start

```
git clone https://github.com/otavioschwanck/emacs_on_rails.git ~/.emacs.d
cp ~/.emacs.d/user.el.example ~/.emacs.d/user_config/user.el

Open the emacs and run: `M-x all-the-icons-install-fonts`

```

## Features

- Full integrated with Ruby
- Syntax checking with rubocop and reek
- Autocomplete using ruby-server (robe)
- Snippets
- Terminal, rails console and server inside the editor
- Navigation made for Ruby on Rails
- Rspec integrated
- Fast
- Customizable (editing /user_config/user.el)

## Keybindings

For Projectile-rails, use this keybindings: https://github.com/asok/projectile-rails#the-keymap-is-unbound-by-default-the-following-keybinding-assume-that-youve-bound-it-to-c-c-r-see-keymap-prefix-section-for-details

(Prefix: `C-c r`)

For Rspec-mode, use this keybindigs: https://github.com/pezra/rspec-mode#rspec-verifiable-mode

(Prefix: `C-c ,`)

Other keybindigs:

| Key | Description |
| --- | ----------  |
| `C-c r` | Projectile Rails prefix |
| `C-c p` | Projectile prefix |
| `C-.` | Find file on project |
| `C-,` | Find opened buffers on project |
| `C-RET` | Search for bookmark.  You can manage a created bookmark with M-o |
| `C-c g` | MaGIT.  To learn more about magit: https://www.youtube.com/watch?v=vQO7F2Q9DwA. |
| `M-2` | Expand-region.  Useful do select things |
| `M-o` | Switch windows |
| `C-x w u` | Undo window changes |
| `C-x w r` | Redo window changes |
| `C-x C-r` | Search for recent files |
| `C-x w m` | Set a bookmark |
| `C-x w b` | Search for bookmarks |
| `C-c w` | Copy current buffer class name |
| `C-c {` | Toggle ruby block |
| `C-c r '` | Run `robe-start` to get better auto-completes |
| `C-q` | Toggle snippets.  Example:  type def and press `C-q` |
| `C-c c` | Just start some text on the screen.  Then, just jump to it. |
| `C-c C-v` | Works on web mode.  Can make tags easily.  See: https://www.emacswiki.org/emacs/ZenCoding |

## Daily use techniques

- Search and replace in entire project

Just run `C-c /`, search for the text and press `C-c C-o`.  To make the buffer editable, press `C-x C-q`.  Do your changes and press `C-c C-c` to confirm.

## Some plugins documentations

- https://github.com/joaotavora/yasnippet
- https://github.com/asok/projectile-rails
- https://github.com/bbatsov/projectile
- https://github.com/dgutov/robe

## New to emacs

If you never used Emacs, i strongly recommend you do the basic tutorial.  After open Emacs, just run C-h t (Ctrl + H, t) and follow do the exercises.

## Screenshots

![whick-key](https://i.imgur.com/gjwGuqn.png)
![auto-complete](https://i.imgur.com/vRYK8pu.pnghttps://i.imgur.com/gjwGuqn.png)
