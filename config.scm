#!/usr/bin/env guile
!#
;;; config.scm --- Deploy config files

;; Copyright © 2015, 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  3 Mar 2015

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script may be used to list, show info and deploy (create
;; symlinks to) various configuration files.

;;; Code:

(use-modules
 (ice-9 format)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-37)
 (al configs)
 (al files)
 (al links)
 (al messages)
 (al places)
 (al sources)
 (al utils))

(set-locale)


;;; Configs

(define (my-repo name)
  (string-append "https://github.com/alezost/" name ".git"))

(define %main-configs
  (list
   (config* #:name "config"
            #:source (source* #:uri (my-repo "config")
                              #:directory (config-file))
            #:links (list
                     (link* #:filename (bin-file "config")
                            #:target (config-file "config.scm"))))
   (config* #:name "guile"
            #:source (source* #:uri (my-repo "guile-config")
                              #:directory (config-file "guile")))
   (config* #:name "guix"
            #:source (source* #:uri (my-repo "guix-config")
                              #:directory (guix-config-file)))
   (config* #:name "shepherd"
            #:source (source* #:uri (my-repo "shepherd-config")
                              #:directory (config-file "shepherd"))
            #:links (list
                     (link* #:filename (home-file ".config/shepherd/init.scm")
                            #:target (config-file "shepherd/init.scm"))))
   (config* #:name "guile-daemon"
            #:source (source* #:uri (my-repo "guile-daemon-config")
                              #:directory (config-file "guile-daemon"))
            #:links (list
                     (link* #:filename (home-file ".config/guile-daemon/init.scm")
                            #:target (config-file "guile-daemon/init.scm"))))
   (config* #:name "shell"
            #:source (source* #:uri (my-repo "shell-config")
                              #:directory (config-file "shell"))
            #:links (list
                     (link* #:filename (home-file ".inputrc")
                            #:target (config-file "shell/bash/inputrc"))
                     (link* #:filename (home-file ".bashrc")
                            #:target (config-file "shell/bash/bashrc"))
                     (link* #:filename (home-file ".bash_profile")
                            #:target (config-file "shell/bash/profile"))))
   (config* #:name "emacs"
            #:source (source* #:uri (my-repo "emacs-config")
                              #:directory (config-file "emacs"))
            #:links (list
                     (link* #:filename (home-file ".emacs.d/init.el")
                            #:target (config-file "emacs/init/init.el"))))
   (config* #:name "stumpwm"
            #:source (source* #:uri (my-repo "stumpwm-config")
                              #:directory (config-file "stumpwm"))
            #:links (list
                     (link* #:filename (home-file ".stumpwmrc")
                            #:target (config-file "stumpwm/init.lisp"))))
   (config* #:name "conkeror"
            #:source (source* #:uri (my-repo "conkeror-config")
                              #:directory (config-file "conkeror"))
            #:links (list
                     (link* #:filename (home-file ".conkerorrc")
                            #:target (config-file "conkeror/init.js"))))

   (config* #:name "top"
            #:links (list
                     (link* #:filename (home-file ".toprc")
                            #:target (config-file "top/toprc"))))
   (config* #:name "sbcl"
            #:links (list
                     (link* #:filename (home-file ".sbclrc")
                            #:target (config-file "sbcl/sbclrc"))))
   (config* #:name "git"
            #:links (list
                     (link* #:filename (home-file ".gitconfig")
                            #:target (config-file "git/gitconfig"))))
   (config* #:name "lirc"
            #:links (list
                     (link* #:filename (home-file ".lircrc")
                            #:target (config-file "lirc/lircrc"))))
   (config* #:name "mime"
            #:links (list
                     (link* #:filename (home-file ".mime.types")
                            #:target (config-file "mime/mime.types"))))
   (config* #:name "rtorrent"
            #:links (list
                     (link* #:filename (home-file ".rtorrent.rc")
                            #:target (config-file "rtorrent/rc"))))
   (config* #:name "tvtime"
            #:links (list
                     (link* #:filename (home-file ".tvtime")
                            #:target (config-file "tvtime"))))
   (config* #:name "mplayer"
            #:links (list
                     (link* #:filename (home-file ".mplayer")
                            #:target (config-file "mplayer"))))

   (config* #:name "mpv"
            #:links (list
                     (link* #:filename (home-file ".config/mpv")
                            #:target (config-file "mpv"))))
   (config* #:name "youtube-dl"
            #:links (list
                     (link* #:filename (home-file ".config/youtube-dl")
                            #:target (config-file "youtube-dl"))))
   (config* #:name "openbox"
            #:links (list
                     (link* #:filename (home-file ".config/openbox")
                            #:target (config-file "openbox"))))
   (config* #:name "dunst"
            #:links (list
                     (link* #:filename (home-file ".config/dunst")
                            #:target (config-file "dunst"))))
   (config* #:name "zathura"
            #:links (list
                     (link* #:filename (home-file ".config/zathura")
                            #:target (config-file "zathura"))))
   (config* #:name "fontconfig"
            #:links (list
                     (link* #:filename (home-file ".config/fontconfig")
                            #:target (config-file "fontconfig"))))

   (config* #:name "gtk"
            #:links (list
                     (link* #:filename (home-file ".gtkrc-2.0")
                            #:target (config-file "gtk/gtkrc-2.0"))
                     (link* #:filename (home-file ".config/gtk-3.0")
                            #:target (config-file "gtk/3.0"))))

   (config* #:name "X"
            #:links (list
                     (link* #:filename (home-file ".Xmodmap")
                            #:target (config-file "X/Xmodmap"))
                     (link* #:filename (home-file ".Xresources")
                            #:target (config-file "X/Xresources"))
                     (link* #:filename (home-file "XTerm")
                            #:target (config-file "X/XTerm"))))))

(define %secret-configs
  (catch #t
    (lambda () (primitive-load (secret-config-file "config.scm")))
    (const '())))

(define %configs
  (append %main-configs %secret-configs))

(define (configs-names)
  "Return list of all available config names."
  (map config-name %configs))

(define (configs-links)
  "Return list of all available config links."
  (append-map config-links %configs))


;;; Command-line args

(define (show-help)
  (display "Usage: config OPTION [CONFIG]...
List, show info or deploy available/specified configurations.")
  (display "\n
Options:
  -h, --help        display this help and exit
  -l, --list        list available configurations and exit
  -O, --list-old    list old configuration files and exit
  -o, --ls-old      perform 'ls -l' on the old configuration files and exit
  -D, --delete-old  delete old configuration files and exit
  -s, --show        show info of the specified configurations
  -f, --fetch       fetch (git clone) source of the specified configurations
  -d, --deploy      deploy (create symlinks) the specified configurations")
  (display "\n
If '--show', '--fetch' or '--deploy' option is used and no configuration
is specified, then all available ones will be shown, fetched or
deployed.  '--fetch' and '--deploy' can be specified together.")
  (newline))

(define* (list-strings strings #:key title
                       (proc (lambda (s)
                               (format #t "~{~a~%~}" (sort s string-ci<)))))
  "Display list of STRINGS using PROC."
  (when title
    (display title)
    (newline))
  (proc strings))

(define (show-configs-names)
  (list-strings (configs-names)
                #:title "Available configurations:"))

(define (call-with-old-files proc)
  "Call PROC on old configuration files."
  (let ((files (old-files)))
    (if (null? files)
        (display "There are no old configuration files.\n")
        (proc (old-files)))))

(define (show-old-files)
  (call-with-old-files
   (lambda (files)
     (list-strings files
                   #:title "Old configuration files:"))))

(define (ls-old-files)
  "Perform 'ls -l' on the old configuration files."
  (call-with-old-files
   (lambda (files)
     (list-strings
      files
      #:title "Old configuration files:"
      #:proc (lambda (files)
                   (apply system*
                          "ls" "-l" "--directory" "--color=auto"
                          (sort files string-ci<)))))))

(define (delete-old-files)
  (call-with-old-files (cut map delete-file-recursively <>)))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\l "list") #f #f
                (lambda _
                  (show-configs-names)
                  (exit 0)))
        (option '(#\O "list-old") #f #f
                (lambda _
                  (show-old-files)
                  (exit 0)))
        (option '(#\o "ls-old") #f #f
                (lambda _
                  (ls-old-files)
                  (exit 0)))
        (option '(#\D "delete-old") #f #f
                (lambda _
                  (delete-old-files)
                  (exit 0)))
        (option '(#\s "show") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action 'show seed)))
        (option '(#\f "fetch") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action 'fetch seed)))
        (option '(#\d "deploy") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action 'deploy seed)))))

(define (parse-args args)
  "Return alist of options from command-line ARGS."
  (reverse
   (args-fold args %options
              (lambda (opt name arg seed)
                (print-error "Unrecognized option: '~a'" name)
                seed)
              (lambda (arg seed)
                (alist-cons 'config arg seed))
              '())))



(define (old-filename filename)
  "Return name of an old config file that should be backed up."
  (string-append filename "-old"))

(define (old-unique-filename filename)
  "Return unique name of an old config file that should be backed up."
  (unique-filename (old-filename filename)))

(define (old-files)
  "Return list of old config files."
  (append-map (lambda (link)
                (find-matching-files
                 (old-filename (link-filename link))))
              (configs-links)))

(define deploy-config*
  (cut deploy-config <> old-unique-filename))

(define (options->values name opts)
  "Return list of values for NAME from OPTS alist.

Example:

  (options->values 'a '((a . 1) (b . 2) (a . 3)))  =>  (1 3)"
  (filter-map (match-lambda
                ((key . value)
                 (and (eq? key name) value))
                (_ #f))
              opts))

(define options->config-names
  (cut options->values 'config <>))

(define options->action-names
  (cut options->values 'action <>))

(define (action-names->action names)
  "Return config procedure from action NAMES."
  (let ((show?   (memq 'show names))
        (fetch?  (memq 'fetch names))
        (deploy? (memq 'deploy names)))
    (cond
     (show? show-config)
     ((and fetch? deploy?)
      (lambda (config)
        (fetch-config config)
        (deploy-config* config)))
     (fetch? fetch-config)
     (deploy? deploy-config*)
     (else #f))))

(define (lookup-config name)
  "Return config record from '%configs' list by its NAME."
  (find (lambda (config)
          (equal? (config-name config) name))
        %configs))

(define (lookup-configs names)
  "Return config records from '%configs' list by their NAMES."
  (filter-map
   (lambda (name)
     (or (lookup-config name)
         (begin (print-error "No '~a' configuration was found" name)
                #f)))
   names))

(define (main arg0 . args)
  (let* ((opts         (parse-args args))
         (config-names (options->config-names opts))
         (action-names (options->action-names opts))
         (configs      (if (null? config-names)
                           %configs
                           (lookup-configs config-names)))
         (action       (or (action-names->action action-names)
                           (leave "\
No action is specified, try --help for more information"))))
    (map action configs)))

(when (batch-mode?)
  (apply main (command-line)))

;;; config.scm ends here
