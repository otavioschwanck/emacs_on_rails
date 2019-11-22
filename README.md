# Emacs on Rails

Configurações do Emacs para desenvolver em Rails.

Cheatsheet e Guia de instalação se encontra no PDFRails (Busca padrão, por model, 

# Instalação

## Linux Mint 19 / Ubuntu 18.04

```
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26 silversearcher-ag
cd
git clone https://github.com/otavioschwanck/emacs_on_rails.git
cd emacs_on_rails
cp .emacs ~/ && cp -r .emacs.d ~/ -f
```

## Fedora

```
sudo dnf install -y emacs
cd
git clone https://github.com/otavioschwanck/emacs_on_rails.git
cd emacs_on_rails
cp .emacs ~/ && cp -r .emacs.d ~/ -f
```

## Debian 10

```
sudo apt install flatpak
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.gnu.emacs
cd
git clone https://github.com/otavioschwanck/emacs_on_rails.git
cd emacs_on_rails
cp .emacs ~/ && cp -r .emacs.d ~/ -f
```

Abra o emacs, e execute: `M-x all-the-icons-install-fonts`

## Principais Features

- Editor com integração completa com Ruby
- Checagem de erros de sintaxe
- Integração com Rubocop
- Autocomplete avançado
- Snippets
- Terminal, Rails Server e Console direto no editor
- Navegação avançada pensando no Rails (Busca padrão, por model, controller, feature, etc)
- Execução de testes direto no editor, podendo ir para linha que deu erro rapidamente.
- Super rápido

## Vídeo com dicas e demonstração:

- https://www.youtube.com/watch?v=ARwgXfwtyQk

## Screenshots

![screenshot](https://github.com/otavioschwanck/emacs_on_rails/blob/master/dashboard.png?raw=true)
![screenshot2](https://github.com/otavioschwanck/emacs_on_rails/blob/master/auto_complete.png?raw=true)

## Demonstração de macro com emacs:

![demonstration](https://github.com/otavioschwanck/emacs_on_rails/blob/master/macro%20example.gif?raw=true)

